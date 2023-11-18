# Реализуйте логику модулей `keylist.erl` и `keylist_mgr.erl` с помощью `gen_server`

## [keylist_mgr.erl](src/keylist_mgr.erl)

## [keylist.erl](src/keylist.erl)

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