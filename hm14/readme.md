Был реализован модуль [`door`](src/door.erl). Было добавлено новое состояние `suspended` а также возможность изменить код, когда состояние `open`. Также были написаны спеки и документация для этого модуля.

Результат тестирования в Eshell:

```erlang
1> {ok, Pid} = door:start_link([1, 2, 3, 4]).
{ok,<0.87.0>}
2> door:enter(Pid, 1).
{ok,next}
3> door:enter(Pid, 2).
{ok,next}
4> door:enter(Pid, 3).
{ok,next}
5> door:enter(Pid, 4).
{ok,open}
6>  door:set_new_code(Pid, 1).
{ok,next}
7> door:set_new_code(Pid, 2).
{ok,next}
8> door:approve_code(Pid).
A new code [1,2] has been set
{ok,locked}
9> door:set_new_code(Pid, 1).
An incorrect command has been entered for locked state.
{error,bad_command}
10> door:enter(Pid, 2).
{ok,next}
11> door:enter(Pid, 2).
{error,wrong_code}
12> door:enter(Pid, 2).
{ok,next}
13> door:enter(Pid, 2).
{error,wrong_code}
14> door:enter(Pid, 2).
{ok,next}
15> door:enter(Pid, 2).
{error,wrong_code}
16> door:enter(Pid, 2).
suspended
17> door:enter(Pid, 2).
You can't do anything since you tried to enter code too many times
{error,bad_command}
Try again
18> door:enter(Pid, 1).
{ok,next}
19> door:enter(Pid, 2).
{ok,open}
Timeout
```

Также модуль `door` был протестирован с помощью тестового модуля [`door_test`](src/door_test.erl). 

Результат EUnit тестирования:

```
56> eunit:test(door_test).
door_test.erl:29:<0.826.0>: Testing locked state...
door_test.erl:47:<0.830.0>: Testing open state...
door_test.erl:76:<0.834.0>: Testing suspended state...
door_test.erl:109:<0.838.0>: Testing open state timeout...
  All 4 tests passed.
ok
```

Как видно, тестирования прошли успешно.

<!-- 
- eunit: Result from instantiator is not a test -- https://return.co.de/blog/articles/eunit-result-instantiator-not-test/
- Timeout control in EUnit -- https://erlang.org/pipermail/erlang-questions/2009-June/044338.html 
-->
