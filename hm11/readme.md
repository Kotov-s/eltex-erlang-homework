# 1. Напишите тесты для keylist.erl с использованием модуля eunit и тестовых генераторов

[Модуль тестирования](src/keylist_test.erl)

Результат работы модуля тестирования:
```erlang
30> c(keylist_test).
{ok,keylist_test}
31> eunit:test(keylist_test).
keylist_test.erl:28:<0.539.0>: keylist module testing
keylist_test.erl:55:<0.545.0>: keylist_mgr module testing
  2 tests passed.
ok
```
Как видно, тестирование прошло успешно и ошибок не обнаружено.

# 2. В keylist_mgr в handle_info у вас должна осуществляться обработка 'EXIT' и 'DOWN'

Добавлен функционал для обработки 'EXIT' и 'DOWN':
```erlang
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
handle_info({'EXIT', Pid, Reason}, State) -> 
    io:format("Linked process ~p exited eith reason:~p~n", [Pid, Reason]),
    NewState = handle_restart_process(fun keylist:start_link/1, Reason, Pid, State),
    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) -> 
    io:format("Monitored process ~p exited eith reason:~p~n", [Pid, Reason]),
    NewState = handle_restart_process(fun keylist:start_monitor/1, Reason, Pid, State),
    {noreply, NewState}.
```


# 3 Часть

## Завершить keylist1
```erlang
128> keylist_mgr:start().
{ok,{<0.336.0>,#Ref<0.337724386.1954807810.186992>}}
129> keylist_mgr:start_child(#{name => keylist1, restart => permanent}).
{ok,<0.338.0>}
130> keylist_mgr:start_child(#{name => keylist2, restart => temporary}).
Info from process <0.338.0>: new keylist created with Pid = <0.340.0>, Name = keylist2
{ok,<0.340.0>}
131> keylist_mgr:start_child(#{name => keylist3, restart => temporary}).
Info from process <0.338.0>: new keylist created with Pid = <0.342.0>, Name = keylist3
Info from process <0.340.0>: new keylist created with Pid = <0.342.0>, Name = keylist3
{ok,<0.342.0>}
132> exit(whereis(keylist1), "NotNormalReason").
Linked process <0.338.0> exited eith reason:"NotNormalReason"
true
Process with name = keylist1, Pid = <0.344.0> started
Info from process <0.342.0>: new keylist created with Pid = <0.344.0>, Name = keylist1
Info from process <0.340.0>: new keylist created with Pid = <0.344.0>, Name = keylist1
133> keylist_mgr:get_names().
[{keylist1,<0.344.0>},
 {keylist3,<0.342.0>},
 {keylist2,<0.340.0>}]
134> exit(whereis(keylist2), "NotNormalReason").
Linked process <0.340.0> exited eith reason:"NotNormalReason"
true
135> whereis(keylist1).
<0.344.0>
136> whereis(keylist2).
undefined
138> whereis(keylist3).
<0.342.0>
```

Как видно, тк процесс `keylist1` был `permanent` процессом, то он был перезапущен модулем `kelist_mgr`. Если бы он был как и процесс `keylist2` `temporary`, то в случае если бы он аварийно завершился, его бы не перезапустил `kelist_mgr`. 

## Добавьте в любой keylist handle_call timer:sleep(10000).

```erlang
202> self().
<0.497.0>
203> keylist:add(keylist1, 1, "one", "number").
** exception exit: {timeout,{gen_server,call,
                                        [keylist1,{<0.497.0>,add,1,"one","number"}]}}
     in function  gen_server:call/2 (gen_server.erl, line 386)
204> self().
<0.507.0>
205> whereis(keylist1).
<0.503.0>
206> whereis(keylist3).
<0.505.0>
207> whereis(keylist_mgr).
<0.500.0>

208> keylist:show_list(keylist1).
ok
209> flush(). % В конечном счете handle_call отработал и добавил элемент в массив
Shell got {ok,{state,[{1,"one","number"}],1}} 
```

По умолчанию, `gen_server:call/2` ожидает ответа в течение 5 секунд. Если `gen_server` не отвечает в течение этого времени, `gen_server:call/2` сгенерирует исключение `timeout`.

Чтобы исправить эту проблему, можно увеличить тайм-аут для `gen_server:call/3`, передав время ожидания в качестве третьего аргумента (`gen_server:call(Name, {self(), add, Key, Value, Comment}, infinity).` но в данном случае можно и 11000 миллисекунд поставить). Это означает, что `gen_server:call/3` будет ждать заданное время ответа от `gen_server`. 

Процессы `keylist_mgr`, `keylist1`, `keylist2` продолжают работать, как видно из команды `whereis`. 

`timer:sleep(10000)` приостанавливает выполнение процесса на 10 секунд, но это не вызывает исключение. Это просто делает процесс "спящим" на указанное количество времени. Процесс `keylist1` продолжает работать после того, как он "просыпается".


## Добавьте в любой keylist handle_call 1/0 (деление на 0).

```erlang
219> self().
<0.531.0>
220> keylist:add(keylist1, 6, "six", "number").
=ERROR REPORT==== 18-Nov-2023::00:40:52.755000 ===
** Generic server keylist1 terminating
** Last message in was {<0.531.0>,add,6,"six","number"}
** When Server state == {state,[],0}
** Reason for termination ==
** {badarith,[{keylist,handle_call,3,[{file,"keylist.erl"},{line,78}]},
              {gen_server,try_handle_call,4,
                          [{file,"gen_server.erl"},{line,1113}]},
              {gen_server,handle_msg,6,[{file,"gen_server.erl"},{line,1142}]},
              {proc_lib,init_p_do_apply,3,
                        [{file,"proc_lib.erl"},{line,241}]}]}
** Client <0.531.0> stacktrace
** [{gen,do_call,4,[{file,"gen.erl"},{line,259}]},
    {gen_server,call,2,[{file,"gen_server.erl"},{line,382}]},
    {erl_eval,do_apply,7,[{file,"erl_eval.erl"},{line,750}]},
    {shell,exprs,7,[{file,"shell.erl"},{line,782}]},
    {shell,eval_exprs,7,[{file,"shell.erl"},{line,738}]},
    {shell,eval_loop,4,[{file,"shell.erl"},{line,723}]}]

Linked process <0.530.0> exited eith reason:{badarith,
                                             [{keylist,handle_call,3,
                                               [{file,"keylist.erl"},
                                                {line,78}]},
                                              {gen_server,try_handle_call,4,
                                               [{file,"gen_server.erl"},
                                                {line,1113}]},
                                              {gen_server,handle_msg,6,
                                               [{file,"gen_server.erl"},
                                                {line,1142}]},
                                              {proc_lib,init_p_do_apply,3,
                                               [{file,"proc_lib.erl"},
                                                {line,241}]}]}
=CRASH REPORT==== 18-Nov-2023::00:40:52.755000 ===
  crasher:
    initial call: keylist:init/1
    pid: <0.530.0>
    registered_name: keylist1
    exception error: an error occurred when evaluating an arithmetic expression
      in function  keylist:handle_call/3 (keylist.erl, line 78)
      in call from gen_server:try_handle_call/4 (gen_server.erl, line 1113)
      in call from gen_server:handle_msg/6 (gen_server.erl, line 1142)
    ancestors: [keylist_mgr,<0.497.0>,<0.84.0>,<0.70.0>,<0.65.0>,<0.69.0>,
                  <0.64.0>,kernel_sup,<0.47.0>]
    message_queue_len: 0
    messages: []
    links: [<0.500.0>]
    dictionary: []
    trap_exit: false
    status: running
    heap_size: 376
    stack_size: 28
    reductions: 9274
  neighbours:

** exception exit: {{badarith,[{keylist,handle_call,3,
                                        [{file,"keylist.erl"},{line,78}]},
                               {gen_server,try_handle_call,4,
                                           [{file,"gen_server.erl"},{line,1113}]},
                               {gen_server,handle_msg,6,
                                           [{file,"gen_server.erl"},{line,1142}]},
                               {proc_lib,init_p_do_apply,3,
                                         [{file,"proc_lib.erl"},{line,241}]}]},
                    {gen_server,call,
                                [keylist1,{<0.531.0>,add,6,"six","number"}]}}
     in function  gen_server:call/2 (gen_server.erl, line 386)
Process with name = keylist1, Pid = <0.534.0> started
Info from process <0.505.0>: new keylist created with Pid = <0.534.0>, Name = keylist1
221> self().
<0.535.0>
222> whereis(keylist1).
<0.534.0>
223> whereis(keylist3).
<0.505.0>
224> whereis(keylist_mgr).
<0.500.0>


240> keylist:add(keylist1, 1, "one", "number"). % После добавления try/catch
Attempted division by zero
{ok,1}
```

Деление на ноль приводит к исключению `badarith`, поскольку деление на ноль не допускается в Erlang. Это исключение приведет к завершению процесса (в нашем случае он перезапустился модулем `keylist_mgr`, тк данный процесс был `permanent`), в котором оно произошло. Также завершился и процесс вызывающий его (Eshell). 

Исправить это можно добавив обработку исключений `try/catch` вокруг операции, которая может вызвать исключение. Это позволит перехватить исключение и обработать его:

```erlang
try 1/0 of
    _ -> ok
catch
    error:badarith ->
        io:format("Attempted division by zero~n")
end,
```