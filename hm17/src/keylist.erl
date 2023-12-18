-module(keylist).

-behaviour(gen_server).

-include("item.hrl").

%% Callbacks
-export([init/1, handle_call/3, handle_info/2]).

%% API
-export([start_link/1, start_monitor/1, stop/1]).
-export([add/4, is_member/2, take/2, find/2, delete/2, show_list/1, match/2, match_object/2, select/2]).

-record(state, {
    counter = 0 :: integer()
}).

%% API

-spec start_link(atom()) -> gen_server:start_ret().
%% @doc Starts a link for the keylist.
start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Name, []).

-spec start_monitor(atom()) -> gen_server:start_mon_ret().
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
 
-spec take(atom(), any()) -> {ok, not_found | {item, any(), any(), any()}, integer()}.
%% @doc Takes a key-value pair from the keylist.
take(Name, Key) ->
    gen_server:call(Name, {take, Key}).

-spec find(atom(), any()) -> {ok, not_found | {item, any(), any(), any()}, integer()}.
%% @doc Finds a key-value pair in the keylist. 
find(Name, Key) ->
    gen_server:call(Name, {find, Key}). 

-spec delete(atom(), any()) -> {ok, integer()}.
%% @doc Deletes a key-value pair from the keylist.
delete(Name, Key) ->
    gen_server:call(Name, {delete, Key}). 

-spec show_list(atom()) -> {list(), integer()}.
%% @doc Shows the list of key-value pairs in the keylist.
show_list(Name) ->
    gen_server:call(Name, {show_list}).     

-spec match(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
%% @doc an API for ets:match/2
match(Name, Pattern) ->
    gen_server:call(Name, {match, Pattern}).   

-spec match_object(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
%% @doc an API for ets:match_object/2
match_object(Name, Pattern) ->
    gen_server:call(Name, {match_object, Pattern}).   

-spec select(Name :: atom(), Filter :: fun()) -> {ok, list()}.
%% @doc an API for ets:select/2, but the second argument is a function, not an ets:match_spec().
select(Name, Filter) ->
    gen_server:call(Name, {select, Filter}).   

%% CALLBACK

%% @hidden
init(Name) -> 
    % process_flag(trap_exit, true),
    keylist_mgr ! {register, Name, self()}, 
    {ok, #state{}}.

%% @hidden    
handle_call({add, Key, Value, Comment}, _From, #state{counter = Counter} = State) ->
    ets:insert(state, #item{key = Key, value = Value, comment = Comment}),
    {reply, {ok, Counter + 1}, State#state{counter = Counter + 1}};

handle_call({is_member, Key}, _From, #state{counter = Counter} = State) ->
    case ets:lookup(state, Key) of
        [] -> {reply, {ok, false, Counter + 1}, State#state{counter = Counter + 1}};
        _ -> {reply, {ok, true, Counter + 1}, State#state{counter = Counter + 1}} 
    end;

handle_call({take, Key}, _From, #state{counter = Counter} = State) ->
    case ets:lookup(state, Key) of
        [] ->
            {reply, {ok, not_found, Counter + 1}, State#state{counter = Counter + 1}};
        [Res] ->
            ets:delete(state, Key),
            {reply, {ok, Res, Counter + 1}, State#state{counter = Counter + 1}}
    end;

handle_call({find, Key}, _From, #state{counter = Counter} = State) ->
    case ets:lookup(state, Key) of
        [] ->
            {reply, {ok, not_found, Counter + 1}, State#state{counter = Counter + 1}};
        [Res] ->
            {reply, {ok, Res, Counter + 1}, State#state{counter = Counter + 1}}
    end;

handle_call({delete, Key}, _From, #state{counter = Counter} = State) ->
    ets:delete(state, Key),
    {reply, {ok, Counter + 1}, State#state{counter = Counter + 1}};

handle_call({show_list}, _From, #state{counter = Counter} = State) ->
    {reply, {lists:flatten(ets:match(state, '$1')), Counter + 1}, State#state{counter = Counter + 1}};

handle_call({match, Pattern}, _From, State) ->
    {reply, {ok, ets:match(state, Pattern)}, State};

handle_call({match_object, Pattern}, _From, State) ->
    {reply, {ok, ets:match_object(state, Pattern)}, State};

handle_call({select, Filter}, _From, State) ->
    Res = ets:select(state, ets:fun2ms(Filter)),
    {reply, {ok, Res}, State}.

%% @hidden 
handle_info({added_new_child, Pid, Name}, State) ->
    io:format("Info from process ~p: new keylist created with Pid = ~p, Name = ~p~n", [self(), Pid, Name]),
    {noreply, State}.
