-module(keylist_mgr).

-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2]).

%% API
-export([start/0, stop/0]).
-export([start_child/1, get_names/0, stop_child/1]).

-record(state, {
    children = []   :: list({atom(), pid()}) | [], 
    restart = []    :: list(pid()) | []
}).

-type start_mon_ret() :: 
    {ok, {Pid :: pid(), MonRef :: reference()}} | ignore | {error, Reason :: term()}.
-type restart_type() :: permanent | temporary.

%% API

-spec start() ->  start_mon_ret() | {already_exists}.
%% @doc Starts the keylist manager.
%% This function checks if the keylist manager is already running, and if not, starts it.
%% @end
start() ->
    case whereis(?MODULE) of
        undefined ->
            gen_server:start_monitor({local, ?MODULE}, ?MODULE, [], []);
        _ ->
            {already_exists}
    end.

-spec stop() -> stopped.
%% @doc Stops the keylist manager.
stop() -> 
    gen_server:call(?MODULE, stop).

-spec start_child(#{name := atom(), restart := restart_type()}) -> ok.
%% @doc Starts a child keylist.
start_child(Params) -> 
    gen_server:call(?MODULE, {self(), start_child, Params}).

-spec stop_child(atom()) -> ok.
%% @doc Stops a child keylist.
stop_child(Name) ->
    gen_server:call(?MODULE, {self(), stop_child, Name}).

-spec get_names() -> ok.
%% @doc Gets the names of all child keylists.
get_names() -> 
    gen_server:cast(?MODULE, {self(), get_names}).

%% Callbacks

%% @hidden
start_child_process(From, Name, Restart, #state{children = Children, restart = Restarts} = State) ->
    case keylist:start_link(Name) of
        {ok, Pid} ->
            lists:foreach(fun({KeylistName, _KeylistPid}) -> KeylistName ! {added_new_child, Pid, Name} end, Children),
            NewState = 
                case Restart of
                    permanent -> 
                        State#state{children = [{Name, Pid} | Children], restart = [Pid|Restarts]};
                    temporary ->
                        State#state{children = [{Name, Pid} | Children]}    
                end,    
            case From==self() of
                true -> io:format("Process with name ~p, Pid= ~p started~n", [Name, Pid]);
                false -> From ! {ok, Pid}
            end;
        {error, Reason} ->
            NewState = State, 
            io:format("Error: ~p", [Reason]);
        ignore ->
            NewState = State, 
            io:format("~p", [ignore])  
    end,
    NewState.

%% @hidden
handle_call({From, start_child, #{name := Name, restart := Restart}}, _From, #state{children = Children} = State) -> 
    NewState = 
        case proplists:lookup(Name, Children) of
            none ->
            start_child_process(From, Name, Restart, State);
            _ ->
                case From==self() of
                    true -> io:format("Already exists");
                    false -> From ! {already_exists}
                end,
                State
        end,
    {reply, ok, NewState};

handle_call(stop, _From, #state{children = Children} = State) ->
    lists:foreach(fun({Name, _Pid}) -> keylist:stop(Name) end, Children),
    {stop, normal, stopped, State};

handle_call({From, stop_child, Name}, _From, #state{children = Children, restart = Restarts} = State) -> 
    case proplists:lookup(Name, Children) of
        none ->
            From ! {not_found},
            NewState = State;                
        _ ->
            NewState = State#state{children = proplists:delete(Name, Children), restart = lists:delete(whereis(Name), Restarts)},
            keylist:stop(Name),
            From ! {ok, stop_child}
    end,
    {reply, ok, NewState}.

%% @hidden
handle_info({'EXIT', Pid, Reason},  #state{children = Children, restart = Restarts} = State) -> 
    NewState = 
        case lists:keyfind(Pid, 2, Children) of
            false -> 
                io:format("Received an exit signal from an unknown process with pid = ~p and reason = ~p~n", [Pid, Reason]),
                State;
            {Name, Pid} ->
                case lists:member(Pid, Restarts) of
                    true ->
                        start_child_process(self(), Name, permanent, State#state{children = proplists:delete(Name, Children), restart = lists:delete(Pid, Restarts)});
                    false ->
                        State#state{children = proplists:delete(Name, Children)}
                end
        end,
    {noreply, NewState}.
   
%% @hidden
handle_cast({From, get_names}, #state{children = Children} = State) -> 
    From ! Children,
    {noreply, State}.

%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
