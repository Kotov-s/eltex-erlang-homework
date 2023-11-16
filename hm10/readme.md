# Реализуйте логику модулей `keylist.erl` и `keylist_mgr.erl` с помощью `gen_server`

## Листинг `keylist_mgr.erl`
```erlang
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
```

## Листинг `keylist.erl`
```erlang
-module(keylist).

-behaviour(gen_server).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2]).

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
    gen_server:call(Name, {self(), add, Key, Value, Comment}).

-spec is_member(atom(), any()) -> {ok, boolean(), integer()}.
%% @doc Checks if a key is a member of the keylist.  
is_member(Name, Key) ->
    gen_server:call(Name, {self(), is_member, Key}). 
 
-spec take(atom(), any()) -> {ok, not_found | {any(), any(), any()}, integer()}.
%% @doc Takes a key-value pair from the keylist.
take(Name, Key) ->
    gen_server:call(Name, {self(), take, Key}).

-spec find(atom(), any()) -> {ok, not_found | {any(), any(), any()}, integer()}.
%% @doc Finds a key-value pair in the keylist. 
find(Name, Key) ->
    gen_server:call(Name, {self(), find, Key}). 

-spec delete(atom(), any()) -> {ok, list(), integer()}.
%% @doc Deletes a key-value pair from the keylist.
delete(Name, Key) ->
    gen_server:call(Name, {self(), delete, Key}). 

-spec show_list(atom()) -> ok.
%% @doc Shows the list of key-value pairs in the keylist.
show_list(Name) ->
    gen_server:cast(Name, {self(), show_list}).     

%% CALLBACK

%% @hidden
init([]) -> 
    {ok, #state{}}.

%% @hidden    
handle_call({From, add, Key, Value, Comment}, _From, #state{list = List, counter = Counter} = State) ->
    NewState = State#state{list = [{Key, Value, Comment} | List], counter = Counter + 1},
    From ! {ok, NewState#state.counter},
    {reply, {ok, NewState#state.counter}, NewState};

handle_call({From, is_member, Key}, _From, #state{list = List, counter = Counter} = State) ->
    Res = lists:keymember(Key, 1, List),
    NewState = State#state{counter = Counter + 1},
    From ! {ok, Res, NewState#state.counter},
    {reply, {ok, Res, NewState#state.counter}, NewState};

handle_call({From, take, Key}, _From, #state{list = List, counter = Counter} = State) ->
    Msg = 
        case lists:keytake(Key, 1, List) of
            false ->
                NewState = State#state{counter = Counter + 1},
                {ok, not_found, NewState#state.counter};
            {_, Found, NewList} ->
                NewState = State#state{list = NewList, counter = Counter + 1},
                {ok, Found, NewState#state.counter}
        end,
    From ! Msg,
    {reply, Msg, NewState};

handle_call({From, find, Key}, _From, #state{list = List, counter = Counter} = State) ->
    NewState = State#state{counter = Counter + 1},
    Msg = 
        case lists:keyfind(Key, 1, List) of
            false ->
                {ok, not_found, NewState#state.counter};
            Res ->
                {ok, Res, NewState#state.counter}
        end,
    From ! Msg,
    {reply, Msg, NewState};

handle_call({From, delete, Key}, _From, #state{list = List, counter = Counter} = State) ->
    Res = lists:keydelete(Key, 1, List),
    NewState = State#state{counter = Counter + 1, list = Res},
    From ! {ok, Res, NewState#state.counter},
    {reply, {ok, Res, NewState#state.counter}, NewState}.

%% @hidden
handle_cast({From, show_list}, State) ->
    From ! {ok, State},
    {noreply, State}.

handle_info({added_new_child, Pid, Name}, State) ->
    io:format("Info from process ~p: new keylist created with Pid = ~p, Name = ~p~n", [self(), Pid, Name]),
    {noreply, State}.
```

Модули `keylist.erl` и `keylist_mgr.erl` теперь используют `gen_server`. При создании нового процесса `keylist`, `keylist_mgr` отправляет всем существующим процессам `keylist` сообщение `{added_new_child, NewPid, NewName}`. Процесс `keylist` логирует это сообщение. Кроме того, были обновлены спецификации для `keylist.erl` и `keylist_mgr.erl`.

# Тестирование

## Проверка на обработку сообщения о создании нового `keylist`'а
```erlang
74> keylist_mgr:start().
{ok,{<0.267.0>,#Ref<0.855329242.104333314.31087>}}
75> keylist_mgr:start_child(#{name => key1, restart => permanent}).
ok
76> keylist_mgr:start_child(#{name => key2, restart => permanent}).
Info from process <0.269.0>: new keylist created with Pid = <0.271.0>, Name = key2
ok
77> keylist_mgr:start_child(#{name => key3, restart => permanent}).
Info from process <0.271.0>: new keylist created with Pid = <0.273.0>, Name = key3
Info from process <0.269.0>: new keylist created with Pid = <0.273.0>, Name = key3
ok
78> keylist_mgr:start_child(#{name => key4, restart => permanent}).
Info from process <0.271.0>: new keylist created with Pid = <0.275.0>, Name = key4
Info from process <0.269.0>: new keylist created with Pid = <0.275.0>, Name = key4
Info from process <0.273.0>: new keylist created with Pid = <0.275.0>, Name = key4
ok
79> exit(whereis(key4), "no reason").
Process with name key4, Pid= <0.277.0> started
Info from process <0.273.0>: new keylist created with Pid = <0.277.0>, Name = key4
Info from process <0.271.0>: new keylist created with Pid = <0.277.0>, Name = key4
Info from process <0.269.0>: new keylist created with Pid = <0.277.0>, Name = key4
true
```

## Проверка работы измененного модуля `keylist`

```erlang
83> keylist:add(key1, 1, "one", "number").
{ok,1}
84> keylist:add(key1, 2, "two", "number").
{ok,2}
85> keylist:add(key1, 3, "three", "number").
{ok,3}
86> keylist:is_member(key1, 1).
{ok,true,4}
87> keylist:is_member(key1, 5).
{ok,false,5}
88> keylist:take(key1, 5).
{ok,not_found,6}
89> keylist:take(key1, 3).
{ok,{3,"three","number"},7}
90> keylist:find(key1, 2).
{ok,{2,"two","number"},8}
91> keylist:find(key1, 5).
{ok,not_found,9}
92> keylist:show_list(key1).
ok
93> flush().
Shell got {ok,1}
Shell got {ok,2}
Shell got {ok,3}
Shell got {ok,true,4}
Shell got {ok,false,5}
Shell got {ok,not_found,6}
Shell got {ok,{3,"three","number"},7}
Shell got {ok,{2,"two","number"},8}
Shell got {ok,not_found,9}
Shell got {ok,{state,[{2,"two","number"},{1,"one","number"}],9}}
ok
```

## Проверка работы измененного модуля `keylist_mgr`

```erlang
94> keylist_mgr:stop_child(key3).
Received an exit signal from an unknown process with pid = <0.273.0> and reason = normal
ok
95> flush().
Shell got {ok,stop_child}
ok
96> keylist_mgr:get_names().
ok
97> flush().
Shell got [{key4,<0.277.0>},{key2,<0.271.0>},{key1,<0.269.0>}]
ok
98> keylist_mgr:stop().
stopped
99> whereis(keylist_mgr).
undefined
100> whereis(key1).
undefined
101> whereis(key2).
undefined
102> whereis(key4).
undefined
```