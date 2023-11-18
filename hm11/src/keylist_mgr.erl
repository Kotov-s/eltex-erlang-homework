-module(keylist_mgr).

-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_info/2]).

%% API
-export([start/0, stop/0]).
-export([start_child/1, get_names/0, stop_child/1, monitor_child/1]).

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


-spec monitor_child(#{name := atom(), restart := restart_type()}) -> ok.
%% @doc Starts a child keylist.
monitor_child(Params) -> 
    gen_server:call(?MODULE, {self(), start_monitor, Params}).

-spec stop_child(atom()) -> ok.
%% @doc Stops a child keylist.
stop_child(Name) ->
    gen_server:call(?MODULE, {self(), stop_child, Name}).

-spec get_names() -> ok.
%% @doc Gets the names of all child keylists.
get_names() -> 
    gen_server:call(?MODULE, {self(), get_names}).

%% Callbacks

%% @hidden
handle_start_process(From, Pid, Name, Restart, #state{children = Children, restart = Restarts} = State)->
    lists:foreach(fun({KeylistName, _KeylistPid}) -> KeylistName ! {added_new_child, Pid, Name} end, Children),
    NewState = 
        case Restart of
            permanent -> 
                State#state{children = [{Name, Pid} | Children], restart = [Pid|Restarts]};
            temporary ->
                State#state{children = [{Name, Pid} | Children]}    
        end,    
    case From==self() of
        true -> io:format("Process with name = ~p, Pid = ~p started~n", [Name, Pid]);
        false -> From ! {ok, Pid}
    end,
    NewState.

%% @hidden
handle_restart_process(RestartFun, Reason, Pid, #state{children = Children, restart = Restarts} = State)->
    NewState = 
        case lists:keyfind(Pid, 2, Children) of
            false -> 
                io:format("Received an exit signal from process with pid = ~p and reason = ~p~n", [Pid, Reason]),
                State;
            {Name, Pid} ->
                case lists:member(Pid, Restarts) of
                    true ->
                        TempState = State#state{children = proplists:delete(Name, Children), restart = lists:delete(Pid, Restarts)},
                        start_child_process(RestartFun, self(), Name, permanent, TempState);
                    false ->
                        State#state{children = proplists:delete(Name, Children)}
                end
        end,
    NewState.

%% @hidden
start_child_process(StartFun, From, Name, Restart, State) ->
    NewState = 
        case StartFun(Name) of
            {ok, {Pid, _MonRef}} ->
                handle_start_process(From, Pid, Name, Restart, State);            
            {ok, Pid} ->
                handle_start_process(From, Pid, Name, Restart, State);
            {error, Reason} ->
                io:format("Error: ~p", [Reason]),
                State;
            ignore ->
                io:format("~p", [ignore]),
                State  
        end,
    NewState.

%% @hidden
handle_call({From, MonitorOrLink, #{name := Name, restart := Restart}}, _From, #state{children = Children} = State) -> 
    NewState = 
        case proplists:lookup(Name, Children) of
            none ->
                case MonitorOrLink of
                    start_monitor -> start_child_process(fun keylist:start_monitor/1, From, Name, Restart, State);
                    start_child -> start_child_process(fun keylist:start_link/1, From, Name, Restart, State)
                end;
            _ ->
                case From==self() of
                    true -> io:format("Already exists");
                    false -> From ! {already_exists}
                end,
                State
        end,
    {reply, {ok, whereis(Name)}, NewState};

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
    {reply, ok, NewState};

handle_call({From, get_names}, _From, #state{children = Children} = State) -> 
    From ! Children,
    {reply, Children, State}.

%% @hidden
handle_info({'EXIT', Pid, Reason}, State) -> 
    io:format("Linked process ~p exited eith reason:~p~n", [Pid, Reason]),
    NewState = handle_restart_process(fun keylist:start_link/1, Reason, Pid, State),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) -> 
    io:format("Monitored process ~p exited eith reason:~p~n", [Pid, Reason]),
    NewState = handle_restart_process(fun keylist:start_monitor/1, Reason, Pid, State),
    {noreply, NewState}.
   
%% @hidden
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
