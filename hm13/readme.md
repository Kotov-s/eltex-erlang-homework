# 1 Часть

Из [`keylist_mgr`](src/keylist_mgr.erl) создана таблица ETS в которой хранятся рекорды.

В модуле была создана следующая таблица `ets:new(state, [public, named_table, {keypos, #item.key}])`, типа `set` (тип по умолчанию) с контролем доступа `public`.

> Выберите тип таблицы, который кажется более подходящим. Будет ваша таблица `private`, `public`, `protected`? Прокомментируйте ваш выбор.

- Тип `set` был выбран, потому что есть потребность в том, чтобы каждый ключ в таблице был уникален, и каждому ключу соответствовало одно значение (т.к. в модуле `keylist` используются операции с данными таблицы по ключу). Поэтому не были выбраны таблицы типа bag или `duplicate_bag`. Не был выбран тип `ordered_set`, потому что нет необходимости в том, чтобы данные в таблице сортировались.

- Выбран контроль доступа `public`, потому что он позволяет любому процессу читать или записывать данные в таблицу. Это нужно для того чтобы `keylist` процессы могли записывать и читать информацию из таблицы.

# 2 Часть

## match/match_object/select

В модуле [`keylist`](src/keylist.erl) были добавлены API для `match`, `match_object` и `select`:

```erlang
-spec match(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
match(Name, Pattern) ->
    gen_server:call(Name, {match, Pattern}).   

-spec match_object(Name :: atom(), Pattern :: ets:match_pattern()) -> {ok, list()}.
match_object(Name, Pattern) ->
    gen_server:call(Name, {match_object, Pattern}).   

-spec select(Name :: atom(), Filter :: fun()) -> {ok, list()}.
select(Name, Filter) ->
    gen_server:call(Name, {select, Filter}).   

handle_call({match, Pattern}, _From, State) ->
    {reply, {ok, ets:match(state, Pattern)}, State};

handle_call({match_object, Pattern}, _From, State) ->
    {reply, {ok, ets:match_object(state, Pattern)}, State};

handle_call({select, Filter}, _From, State) ->
    Res = ets:select(state, ets:fun2ms(Filter)),
    {reply, {ok, Res}, State}.    
```

### Тестирование

```erlang
6> keylist:add(key1, 1, "one", "comm").
{ok, 1}
7> keylist:add(key1, 2, "two", "comm").
{ok, 2}
8> keylist:add(key1, 3, "three", "comm").
{ok, 3}
9> keylist:add(key1, 4, "four", "comm").
{ok, 4}
10> keylist:match(key1, '$1').
{ok,[[#item{key = 1,value = "one",comment = "comm"}],
     [#item{key = 2,value = "two",comment = "comm"}],
     [#item{key = 4,value = "four",comment = "comm"}],
     [#item{key = 3,value = "three",comment = "comm"}]]}
12> keylist:match(key1, #item{key='$1', value="two", comment='_'}).
{ok,[[2]]}
13> keylist:match_object(key1, #item{key='_', value="two", comment='_'}).
{ok,[#item{key = 2,value = "two",comment = "comm"}]}
14> keylist:select(key1, fun({_I, K, V, C}) when K >= 3 -> [K, V, C] end ).
{ok,[[4,"four","comm"],[3,"three","comm"]]}
44> keylist:select(key1, fun({_I, K, [H|T], C}) when [H] =:= "t" -> [K, [H|T], C] end ).
{ok,[[2,"two","comm"],[3,"three","comm"]]}
47> keylist:select(key1, fun({_I, K, V, C}) when length(V) =:= 3 -> [K, V, C] end ).
{ok,[[1,"one","comm"],[2,"two","comm"]]}
```

Все работает корректно.

## DETS

```erlang
1> -record(person, {id, name, age, gender}).
ok
2> dets:open_file(table, [{keypos, #person.id}, {file, "table"}]).
{ok,table}
3> dets:insert(table, #person{id = 1, name = "Elena", age = 26, gender = female}).
ok
4> dets:insert(table, #person{id = 2, name = "Oleg", age = 34, gender = male}).
ok
5> dets:insert(table, #person{id = 3, name = "Zhenya", age = 23, gender = male}).
ok
6> dets:select(table, ets:fun2ms(fun({_Person, Id, Name, Age, Gender}) when Gender =:= male -> {Id, Name, Age, Gender} end)).
[{2,"Oleg",34,male},{3,"Zhenya",23,male}]
7> dets:close(table).
ok
8> dets:select(table, ets:fun2ms(fun({_Person, Id, Name, Age, Gender}) when Gender =:= male -> {Id, Name, Age, Gender} end)).
** exception error: bad argument
     in function  dets:select/2
        called as dets:select(table,
                              [{{'$1','$2','$3','$4','$5'},
                                [{'=:=','$5',male}],
                                [{{'$2','$3','$4','$5'}}]}])
9> {ok, Ref} = dets:open_file("/Erlang/Homework/hm13/src/table").    
{ok,#Ref<0.2394846824.3227779074.57813>}
10> dets:lookup(Ref, 1).
[{person,1,"Elena",26,female}]
11> exit(self()).
** exception exit: <0.99.0>
12> dets:select(table, ets:fun2ms(fun({_Person, Id, Name, Age, Gender}) when Gender =:= male -> {Id, Name, Age, Gender} end)).
** exception error: bad argument
     in function  dets:select/2
        called as dets:select(table,
                              [{{'$1','$2','$3','$4','$5'},
                                [{'=:=','$5',male}],
                                [{{'$2','$3','$4','$5'}}]}])
```

Основываясь на официальной документации Erlang: 

> Dets tables must be opened before they can be updated or read, and when finished they must be properly closed. 

> A Dets table is closed when the process which opened the table terminates.

Это означает, что ошибка в двух случаях произошла из-за того что `Dets` была не открыта (т.к. для того чтобы производить действия с `Dets` таблицами, нужно открыть их). В первом случае она была закрыта нами. Во втором она была закрыта, по причине того что процесс завершился + она не была открыта в новом процессе.