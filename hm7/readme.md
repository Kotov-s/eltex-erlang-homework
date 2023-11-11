# 1 Часть

Была создана функция loop/1:

```erlang
-record(state, {list = [], counter = 0}).

loop(#state{list = List, counter = Counter} = State) ->
    receive
        {From, add, Key, Value, Comment} ->
            NewState = State#state{list = [{Key, Value, Comment} | List], counter = Counter + 1},
            From ! {ok, NewState#state.counter},
            loop(NewState);
        {From, is_member, Key} ->
            Res = lists:keymember(Key, 1, List),
            NewState = State#state{counter = Counter + 1},
            From ! {ok, Res, NewState#state.counter},
            loop(NewState);
        {From, take, Key} ->
            Res = lists:keytake(Key, 1, List),
            case Res of
                false ->
                    NewState = State#state{counter = Counter + 1},
                    From ! {ok, not_found, NewState#state.counter},
                    loop(NewState);
                _ ->
                    {_, Found, NewList} = Res,
                    NewState = State#state{list = NewList, counter = Counter + 1},
                    From ! {ok, Found, NewState#state.counter},
                    loop(NewState)
            end;
        {From, find, Key} ->
            Res = lists:keyfind(Key, 1, List),
            NewState = State#state{counter = Counter + 1},
            case Res of
                false ->
                    From ! {ok, not_found, NewState#state.counter},
                    loop(NewState);
                _ ->
                    From ! {ok, Res, NewState#state.counter},
                    loop(NewState)
            end;
        {From, delete, Key} ->
            Res = lists:keydelete(Key, 1, List),
            NewState = State#state{counter = Counter + 1, list = Res},
            From ! {ok, Res, NewState#state.counter},
            loop(NewState);
        {From, show_list} ->
            NewState = State#state{counter = Counter + 1},
            From ! {ok, List, NewState#state.counter},
            loop(NewState)
    end.
```

# 2 Часть (start_monitor/1, start_link/1)

## Функции start_monitor/1 и start_link/1:

```erlang
start_monitor(Name) ->
    {Pid, MonitorRef} = spawn_monitor(?MODULE, init, [Name]),
    {ok, Pid, MonitorRef}.

start_link(Name) ->
    Pid = spawn_link(?MODULE, init, [Name]),
    {ok, Pid}.

init(Name) ->
    register(Name, self()),
    loop(#state{}).
```

## Eshell:
```erlang
1> c("keylist.erl").
{ok,keylist}
2> self().
<0.85.0>
3> keylist:start_monitor(monitored).
{ok,<0.93.0>,#Ref<0.1955628830.3059482625.17106>}
4> keylist:start_link(linked).
{ok,<0.95.0>}
5> linked ! {self(), add, 1, "banana", "fruit"}.
{<0.85.0>,add,1,"banana","fruit"}
6> linked ! {self(), add, 2, "apple", "fruit"}.
{<0.85.0>,add,2,"apple","fruit"}
7> linked ! {self(), add, 3, "potato", "vegetable"}.
{<0.85.0>,add,3,"potato","vegetable"}
8> linked ! {self(), is_member, 3}.
{<0.85.0>,is_member,3}
9> linked ! {self(), is_member, 4}.
{<0.85.0>,is_member,4}
10> linked ! {self(), take, 3}.
{<0.85.0>,take,3}
11> linked ! {self(), find, 2}.
{<0.85.0>,find,2}
12> linked ! {self(), find, 3}.
{<0.85.0>,find,3}
13> linked ! {self(), delete, 2}.
{<0.85.0>,delete,2}
14> flush().
Shell got {ok,1}
Shell got {ok,2}
Shell got {ok,3}
Shell got {ok,true,4}
Shell got {ok,false,5}
Shell got {ok,{3,"potato","vegetable"},6}
Shell got {ok,{2,"apple","fruit"},7}
Shell got {ok,not_found,8}
Shell got {ok,[{1,"banana","fruit"}],9}
ok
```

Как видно, функция работает корректно.

## 3 Часть

## monitored
```erlang
15> self().
<0.85.0>
16> exit(whereis(monitored), "Some reason").
true
17> self().
<0.85.0>
18> flush().
Shell got {'DOWN',#Ref<0.1955628830.3059482625.17106>,process,<0.93.0>,
                  "Some reason"}
ok
```

Мониторы позволяют процессу отслеживать завершение другого процесса без завершения самого себя. Когда процесс, который мониторится, завершается, процесс-монитор получит сообщение о его завершении. 

И идентификатор процесса в Esell'e не поменялся, потому что мы мониторили процесс и получили сообщение, поэтому мы только получили сообщение о его падении. 

## linked

```erlang
19> self().
<0.85.0>
20> exit(whereis(linked), "Some reason").
** exception exit: "Some reason"
21> self().
<0.112.0>
22> flush().
ok
```

В Eshell'e pid изменился, потому что процессы (linked и Eshell процессы) были связаны. 
В Erlang, когда процесс "падает" (то есть завершается из-за ошибки), все процессы, которые с ним связаны, также "падают" (если trap_exit в false).

# 4 Часть

```erlang
23> process_flag(trap_exit, true).
false
24> self().
<0.112.0>
25> keylist:start_link(linked).
{ok,<0.118.0>}
26> exit(whereis(linked), "Some reason").
true
27> flush().
Shell got {'EXIT',<0.118.0>,"Some reason"}
ok
28> self().
<0.112.0>
```

Однако процессы могут "ловить" выходные сигналы от связанных процессов. Если в процессе установить trap_exit в true, он не будет завершаться, когда связанный процесс "падает". Вместо этого, он получит сообщение в форме {'EXIT', From, Reason}.

# 5 Часть

```erlang
1> process_flag(trap_exit, false).
false
2> keylist:start_link(linked1).
{ok,<0.88.0>}
3> keylist:start_link(linked2).
{ok,<0.90.0>}
4> self().
<0.85.0>
5> exit(whereis(linked1), "Some reason").
** exception exit: "Some reason"
6> self().
<0.93.0>
7> process_info(<0.90.0>). 
undefined
```

Т.к. флаг trap_exit установлен в false, то завершение связанного процесса приводит к завершению текущего. Также был завершен и процесс linked2, т.к. когда процесс завершается (в данном случае, это процесс Eshell’a), все процессы, которые с ним связаны, также завершаются. Это включает в себя процесс linked2, который был связан с процессом Eshell’a.

Т.е. произошло следующее:

1. Начальное состояние (все процессы работают):
```
Eshell <0.85.0> ---- linked1 <0.88.0>
         |
         ---- linked2 <0.90.0>
```
2. Процесс linked1 завершается:
```
Eshell <0.85.0> ---- X linked1 <0.88.0>
         |
         ---- linked2 <0.90.0>
```
3. Процесс Eshell завершается, так как он связан с процессом linked1:
```
X Eshell <0.85.0> ---- X linked1 <0.88.0>
         |
         ---- linked2 <0.90.0>
```
4. Процесс linked2 также завершается, так как он связан с процессом Eshell:
```
X Eshell <0.85.0> ---- X linked1 <0.88.0>
         |
         ---- X linked2 <0.90.0>
```