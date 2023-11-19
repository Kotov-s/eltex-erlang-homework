-module(keylist).

-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3]).

%% API
-export([start_link/1, start_monitor/1, stop/1]).
-export([add/4, is_member/2, take/2, find/2, delete/2, show_list/1, handle_info/2]).

-record(state, {
    list = []   :: list(#state{}) | [], 
    counter = 0 :: integer()
}).

-type start_mon_ret() :: 
    {ok, {Pid :: pid(), MonRef :: reference()}} | ignore | {error, Reason :: term()}.
-type start_ret() ::
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.


%% API

-spec start_link(atom()) -> start_ret().
%% @doc Starts a link for the keylist.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

-spec start_monitor(atom()) -> start_mon_ret().
%% @doc Starts a monitor for the keylist.
start_monitor(Name) ->
    gen_server:start_monitor({local, Name}, ?MODULE, [], []).

-spec stop(atom()) -> ok.
%% @doc Stops the keylist.
stop(Name) ->
    gen_server:stop(Name).

-spec add(atom(), any(), any(), any()) -> {ok, integer()}.
%% @doc Adds a key-value pair to the keylist.
add(Name, Key, Value, Comment) ->
    gen_server:call(Name, {add, Key, Value, Comment}).

-spec is_member(atom(), any()) -> {ok, boolean(), integer()}.
%% @doc Checks if a key is a member of the keylist.  
is_member(Name, Key) ->
    gen_server:call(Name, {is_member, Key}). 
 
-spec take(atom(), any()) -> {ok, not_found | {any(), any(), any()}, integer()}.
%% @doc Takes a key-value pair from the keylist.
take(Name, Key) ->
    gen_server:call(Name, {take, Key}).

-spec find(atom(), any()) -> {ok, not_found | {any(), any(), any()}, integer()}.
%% @doc Finds a key-value pair in the keylist. 
find(Name, Key) ->
    gen_server:call(Name, {find, Key}). 

-spec delete(atom(), any()) -> {ok, list(), integer()}.
%% @doc Deletes a key-value pair from the keylist.
delete(Name, Key) ->
    gen_server:call(Name, {delete, Key}). 

-spec show_list(atom()) -> ok.
%% @doc Shows the list of key-value pairs in the keylist.
show_list(Name) ->
    gen_server:call(Name, {show_list}).     

%% CALLBACK

%% @hidden    
handle_call({add, Key, Value, Comment}, _From, #state{list = List, counter = Counter} = State) ->
    NewState = State#state{list = [{Key, Value, Comment} | List], counter = Counter + 1},
    {reply, {ok, NewState#state.counter}, NewState};

handle_call({is_member, Key}, _From, #state{list = List, counter = Counter} = State) ->
    Res = lists:keymember(Key, 1, List),
    NewState = State#state{counter = Counter + 1},
    {reply, {ok, Res, NewState#state.counter}, NewState};

handle_call({take, Key}, _From, #state{list = List, counter = Counter} = State) ->
    Msg = 
        case lists:keytake(Key, 1, List) of
            false ->
                NewState = State#state{counter = Counter + 1},
                {ok, not_found, NewState#state.counter};
            {_, Found, NewList} ->
                NewState = State#state{list = NewList, counter = Counter + 1},
                {ok, Found, NewState#state.counter}
        end,
    {reply, Msg, NewState};

handle_call({find, Key}, _From, #state{list = List, counter = Counter} = State) ->
    NewState = State#state{counter = Counter + 1},
    Msg = 
        case lists:keyfind(Key, 1, List) of
            false ->
                {ok, not_found, NewState#state.counter};
            Res ->
                {ok, Res, NewState#state.counter}
        end,
    {reply, Msg, NewState};

handle_call({delete, Key}, _From, #state{list = List, counter = Counter} = State) ->
    Res = lists:keydelete(Key, 1, List),
    NewState = State#state{counter = Counter + 1, list = Res},
    {reply, {ok, Res, NewState#state.counter}, NewState};


handle_call({show_list}, _From, #state{list = List} = State) ->
    {reply, {ok, List}, State}.

%% @hidden
handle_info({added_new_child, Pid, Name}, State) ->
    io:format("Info from process ~p: new keylist created with Pid = ~p, Name = ~p~n", [self(), Pid, Name]),
    {noreply, State}.

    
%% @hidden
init([]) ->
    process_flag(trap_exit, true), 
    {ok, #state{}}.