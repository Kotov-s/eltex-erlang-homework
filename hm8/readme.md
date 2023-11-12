# 1 Часть

Была создана функция `loop/1`

```erlang
loop(#state{children = Childrens} = State) ->
    receive
        {From, start_child, Name} ->
            Res = proplists:lookup(Name, Childrens),
            case Res of
                none ->
                    {ok, Pid} = keylist:start_link(Name),
                    NewState = State#state{children = [{Name, Pid} | Childrens]},
                    From ! {ok, Pid},
                    loop(NewState);
                _ ->
                    From ! {already_exists},
                    loop(State)
            end;
        {From, stop_child, Name} ->
            Res = proplists:lookup(Name, Childrens),
            case Res of
                none ->
                    From ! {not_found},
                    loop(State);                
                _ ->
                    Name ! {stop, Name},
                    NewState = State#state{children = proplists:delete(Name, Childrens)},
                    From ! {ok, stop_child},
                    loop(NewState)

            end;
        {From, get_names} ->
            From ! Childrens,
            loop(State);
        stop ->
            exit(stop_proc);
        {'EXIT', Pid, Reason} -> 
            case lists:keyfind(Pid, 2, Childrens) of
                false -> 
                    io:format("Received an exit signal from an unknown process with pid = ~p and reason = ~p~n", [Pid, Reason]),
                    loop(State);
                {Name, Pid} ->
                    io:format("Process with name = ~p, pid = ~p exited with reason ~p~n", [Name, Pid, Reason]),
                    NewState = State#state{children = proplists:delete(Name, Childrens)},
                    loop(NewState)
            end            
    end.
```


# 2 Часть

Были созданы функции `start/0`, `init/1`
```erlang
init(Name) ->
    process_flag(trap_exit, true),
    register(Name, self()),
    loop(#state{}).

start() ->
    case whereis(keylist_mgr) of
        undefined -> 
            {Pid, MonitorRef} = spawn_monitor(?MODULE, init, [?MODULE]),
            {ok, Pid, MonitorRef};
        _ ->
            {already_exists}
    end.
```


Проверка работоспособности:
```erlang
2> self().
<0.85.0>
3> keylist_mgr:start().
{ok,<0.97.0>,#Ref<0.3620107624.3457941505.215103>}
4> keylist_mgr ! {self(), start_child, keylist1}.
{<0.85.0>,start_child,keylist1}
5> keylist_mgr ! {self(), start_child, keylist2}.
{<0.85.0>,start_child,keylist2}
6> keylist_mgr ! {self(), start_child, keylist3}.
{<0.85.0>,start_child,keylist3}
7> keylist3 ! {self(), add, 1, "banana", "fruit"}.
{<0.85.0>,add,1,"banana","fruit"}
8> keylist3 ! {self(), add, 2, "apple", "fruit"}.
{<0.85.0>,add,2,"apple","fruit"}
9> keylist3 ! {self(), add, 3, "potato", "veg"}.
{<0.85.0>,add,3,"potato","veg"}
10> keylist3 ! {self(), is_member, 3}.
{<0.85.0>,is_member,3}
11> keylist3 ! {self(), is_member, 4}.
{<0.85.0>,is_member,4}
12> keylist3 ! {self(), find, 2}.
{<0.85.0>,find,2}
13> keylist3 ! {self(), take, 2}.
{<0.85.0>,take,2}
14> keylist3 ! {self(), delete, 3}.
{<0.85.0>,delete,3}
15> flush().
Shell got {ok,<0.99.0>}
Shell got {ok,<0.101.0>}
Shell got {ok,<0.103.0>}
Shell got {ok,1}
Shell got {ok,2}
Shell got {ok,3}
Shell got {ok,true,4}
Shell got {ok,false,5}
Shell got {ok,{2,"apple","fruit"},6}
Shell got {ok,{2,"apple","fruit"},7}
Shell got {ok,[{1,"banana","fruit"}],8}
ok
```

Как видно, процесс `keylist3` обрабатывает сообщения, работает со списком и возвращает ожидаемый результат

# 3 Часть

## Завершите процесс keylist1 с помощью функции exit(Pid, Reason).

```erlang
7> self().
<0.85.0>
8> exit(whereis(keylist1), "No reason").
Process with name = keylist1, pid = <0.92.0> exited with reason "No reason"
true
9> self().
<0.85.0>
10> keylist2 ! {self(), add, 1, "apple", "fruit"}.
{<0.85.0>,add,1,"apple","fruit"}
11> keylist3 ! {self(), add, 1, "apple", "fruit"}.
{<0.85.0>,add,1,"apple","fruit"}
12> flush().
Shell got {ok,1}
Shell got {ok,1}
ok
```

Как видно, процессы `keylist2` и `keylist3` не упали. Это произошло потому, что у процесса `keylist_mgr` был установлен флаг `process_flag(trap_exit, true)`, который позволяет ему получать сообщения о падении связанных процессов, не падая при этом самому. 

`Pid` в `Eshell` не изменился, даже несмотря на падение мониторируемого процесса. `Eshell` является процессом-монитором и при падении мониторируемого процесса он получает сообщение о падении, но сам при этом не падает.

`keylist_mgr` вывел сообщение о том, что процесс `keylist1` завершился.


## Завершите процесс keylist_mgr с помощью функции exit(Pid, Reason).

```erlang

2> self().
<0.85.0>
3> keylist_mgr:start().
{ok,<0.93.0>,#Ref<0.808809307.2522087425.111480>}
4> exit(whereis(keylist_mgr), "no reason").
Received an exit signal from an unknown process with pid = <0.85.0> and reason = "no reason"
true
5> self().
<0.85.0>
6> flush().
ok
7> exit(whereis(keylist_mgr), kill).
true
8> flush().
Shell got {'DOWN',#Ref<0.808809307.2522087425.111480>,process,<0.93.0>,killed}
ok
9> self().
<0.85.0>
```
При попытке завершения процесса `keylist_mgr` с причиной отличной от `kill`, процесс не будет убит, потому что, тк включен флаг `process_flag(trap_exit, true)`, то процесс преобразует сигналы выхода в сообщения формата `{'EXIT', From, Reason}` и продолжает работу. 
`kill` является неотловимым сигналом выхода, поэтому процесс завершается.

## Завершите процесс keylist_mgr с помощью функции keylist_mgr ! stop.

```erlang
2> self().
<0.85.0>
3> keylist_mgr:start().
{ok,<0.97.0>,#Ref<0.1934997725.2808086532.261532>}
4> keylist_mgr ! {self(), start_child, keylist1}.
{<0.85.0>,start_child,keylist1}
5> keylist_mgr ! {self(), start_child, keylist2}.
{<0.85.0>,start_child,keylist2}
6> keylist_mgr ! {self(), start_child, keylist3}.
{<0.85.0>,start_child,keylist3}
7> flush().
Shell got {ok,<0.99.0>}
Shell got {ok,<0.101.0>}
Shell got {ok,<0.103.0>}
ok
8> keylist_mgr ! stop.
stop
9> flush().
Shell got {'DOWN',#Ref<0.1934997725.2808086532.261532>,process,<0.97.0>,
                  stop_proc}
ok
10> whereis(keylist1).
undefined
11> whereis(keylist2).
undefined
12> whereis(keylist3).
undefined
13> self().
<0.85.0>
```

`Pid` не изменился потому что мы мониторим процесс `keylist_mgr`, это означает, что если он упадет, то нам только придет сообщение о том, что процесс, который мы мониторим, упал, что и произошло.

Процессы `keylist1`, `keylist2`, `keylist3` также завершились, потому что они связаны с процессом `keylist_mgr` и умирают, когда он умирает он (если только причина не `normal`).

Также нам пришло сообщение о завершении процесса, потому что мы его мониторим.
