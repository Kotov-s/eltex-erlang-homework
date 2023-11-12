%%% @author Andrey Kotov
%%% @since 12.11.2023
%%% @doc This module manages <b>keylists</b>.
%%% It provides functions to start and stop keylists, get the names of all keylists, and stop the keylist manager itself.
%%% @end

-module(keylist_mgr).

-export([loop/1, init/1, start/0]).
-export([start_child/1, stop_child/1, get_names/0, stop/0]).

-type restart_type() :: permanent | temporary.

-record(state, {
    children = []   :: list({atom(), pid()}) | [], 
    restart = []    :: list(pid()) | []
}).


%% @doc Initializes the keylist manager.
%% This function sets the process flag to trap exits, registers the process with the given name, and starts the loop.
%% @end
init(Name) ->
    process_flag(trap_exit, true),
    register(Name, self()),
    loop(#state{}).

-spec start() -> {ok, pid(), reference()} | {already_exists}.
%% @doc Starts the keylist manager.
%% This function checks if the keylist manager is already running, and if not, starts it.
%% @end
start() ->
    case whereis(keylist_mgr) of
        undefined -> 
            {Pid, MonitorRef} = spawn_monitor(?MODULE, init, [?MODULE]),
            {ok, Pid, MonitorRef};
        _ ->
            {already_exists}
    end.

-spec start_child(#{name := atom(), restart := restart_type()}) -> no_return().
%% @doc Starts a child keylist.
start_child(Params) ->
    ?MODULE ! {self(), start_child, Params}.

-spec stop_child(atom()) -> no_return().
%% @doc Stops a child keylist.
stop_child(Name) ->
    ?MODULE ! {self(), stop_child, Name}.

-spec get_names() -> no_return().
%% @doc Gets the names of all child keylists.
get_names() ->
    ?MODULE ! {self(), get_names}.

-spec stop() -> no_return().    
%% @doc Stops the keylist manager.
stop() ->
    ?MODULE ! {stop}.

%% @doc The main loop of the keylist manager.
%% This function receives and handles messages to start and stop child keylists, get the names of all child keylists, and stop the keylist manager itself.
%% @end
loop(#state{children = Children, restart = Restarts} = State) ->
    receive
        {From, start_child, #{name := Name, restart := Restart} = _Params } ->
            Res = proplists:lookup(Name, Children),
            case Res of
                none ->
                    {ok, Pid} = keylist:start_link(Name),
                    case Restart of
                        permanent -> 
                            NewState = State#state{children = [{Name, Pid} | Children], restart = [Pid|Restarts]};
                        temporary ->
                            NewState = State#state{children = [{Name, Pid} | Children]}    
                    end,    
                    case From==self() of
                        true -> io:format("Process with name ~p, Pid= ~p started~n", [Name, Pid]);
                        false -> From ! {ok, Pid}
                    end,
                    loop(NewState);
                _ ->
                    From ! {already_exists},
                    loop(State)
            end;
        {From, stop_child, Name} ->
            Res = proplists:lookup(Name, Children),
            case Res of
                none ->
                    From ! {not_found},
                    loop(State);                
                _ ->
                    Name ! {stop, Name},
                    NewState = State#state{children = proplists:delete(Name, Children)},
                    From ! {ok, stop_child},
                    loop(NewState)

            end;
        {From, get_names} ->
            From ! Children,
            loop(State);
        stop ->
            exit(stop_proc);
        {'EXIT', Pid, Reason} -> 

            case lists:keyfind(Pid, 2, Children) of
                false -> 
                    io:format("Received an exit signal from an unknown process with pid = ~p and reason = ~p~n", [Pid, Reason]),
                    loop(State);
                {Name, Pid} ->
                    case lists:member(Pid, Restarts) of
                        true ->
                            NewState = State#state{children = proplists:delete(Name, Children)},
                            start_child(#{name => Name, restart => permanent});
                        false ->
                            NewState = State#state{children = proplists:delete(Name, Children)}
                    end,    
                    io:format("Process with name = ~p, pid = ~p exited with reason ~p~n", [Name, Pid, Reason]),
                    loop(NewState)
            end            
    end.