-module(keylist_mgr).

-behaviour(gen_server).

-include("item.hrl").

%% Callbacks
-export([init/1, handle_call/3, handle_info/2]).

%% API
-export([start/0, stop/0]).
-export([get_names/0, start_child/1]).

-record(state, {
    children = [] :: list({atom(), pid()}) | []
}).


%% API

-spec start() -> gen_server:start_ret() | {already_exists}.
%% @doc Starts the keylist manager.
%% This function checks if the keylist manager is already running, and if not, starts it.
%% @end
start() ->
    case whereis(?MODULE) of
        undefined ->
            gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
        _ ->
            {already_exists}
    end.

-spec stop() -> stopped.
%% @doc Stops the keylist manager.
stop() -> 
    gen_server:call(?MODULE, stop).

-spec get_names() -> list().
%% @doc Gets the names of all child keylists.
get_names() -> 
    gen_server:call(?MODULE, {get_names}).

-spec start_child(atom()) -> {ok, pid()}.
%% @doc Starts keylists via keylist_sup.
start_child(Name) ->
    gen_server:call(?MODULE, {start_child, Name}).

%% Callbacks

%% @hidden
init([]) ->
    io:format("Starting keylist_mgr~n"),
    ets:new(state, [public, named_table, {keypos, #item.key}]),
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(stop, _From,  State) ->
    {stop, normal, stopped, State};
    
handle_call({get_names}, _From, #state{children = Children} = State) -> 
    {reply, Children, State};

handle_call({start_child, Name}, _From, State) ->
    {ok, Pid} = supervisor:start_child(keylist_sup, [Name]),
    {reply, {ok, Pid}, State}.

%% @hidden
handle_info({register, Name, Pid}, #state{children = Children} = _State) -> 
    io:format("keylist_mgr: trying to register process with Name = ~p, Pid = ~p~n", [Name, Pid]),
    monitor(process, Pid),
    {noreply,  #state{children = [{Name, Pid} | Children]}};

handle_info({'DOWN', _Ref, process, Pid, Reason}, #state{children = Children} = State) -> 
    io:format("keylist_mgr: monitored process ~p down with reason: ~p~n", [Pid, Reason]),
    {noreply, State#state{children = lists:keydelete(Pid, 2, Children)}}.