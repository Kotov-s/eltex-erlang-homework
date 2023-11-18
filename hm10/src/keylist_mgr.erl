-module(keylist_mgr).

-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_info/2]).

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
    gen_server:call(?MODULE, {start_child, Params}).

-spec stop_child(atom()) -> ok.
%% @doc Stops a child keylist.
stop_child(Name) ->
    gen_server:call(?MODULE, {stop_child, Name}).

-spec get_names() -> ok.
%% @doc Gets the names of all child keylists.
get_names() -> 
    gen_server:call(?MODULE, {get_names}).

%% Callbacks

%% @hidden
handle_call({start_child, #{name := Name, restart := Restart}}, _From, #state{children = Children} = State) -> 
    NewState = 
        case proplists:lookup(Name, Children) of
            none ->
                start_child_process(Name, Restart, State);
            _ ->
                io:format("Already exists"),
                State
        end,
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    % lists:foreach(fun({Name, _Pid}) -> keylist:stop(Name) end, Children),
    {stop, normal, stopped, State};

handle_call({stop_child, Name}, _From, #state{children = Children, restart = Restarts} = State) -> 
    Msg = 
        case proplists:lookup(Name, Children) of
            none ->
                NewState = State,
                {not_found};                
            _ ->
                NewState = State#state{children = proplists:delete(Name, Children), restart = lists:delete(whereis(Name), Restarts)},
                keylist:stop(Name),
                {ok, stop_child}
        end,
    {reply, Msg, NewState};

handle_call({get_names}, _From, #state{children = Children} = State) -> 
    {reply, Children, State}.

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
                        TempState = State#state{children = proplists:delete(Name, Children), restart = lists:delete(Pid, Restarts)},
                        start_child_process(Name, permanent, TempState);
                    false ->
                        State#state{children = proplists:delete(Name, Children)}
                end
        end,
    {noreply, NewState}.

    
%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%% @hidden
start_child_process(Name, Restart, #state{children = Children, restart = Restarts} = State) ->
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
            io:format("Process with name ~p, Pid= ~p started~n", [Name, Pid]);      
        {error, Reason} ->
            NewState = State, 
            io:format("Error: ~p", [Reason]);
        ignore ->
            NewState = State, 
            io:format("~p", [ignore])  
    end,
    NewState.