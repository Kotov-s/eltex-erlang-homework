# 1 Часть

```erlang
1> Fac = fun recursion:fac/1.
fun recursion:fac/1
2> Fac(5).
120

3> TailFac = fun recursion:tail_fac/1.
fun recursion:tail_fac/1
4> TailFac(5).
120
```
Все работает корректно.

# 2 Часть

## 1. Анонимная функция для умножения двух элементов

```erlang
8> Multiply = fun(X, Y) -> X * Y end.
#Fun<erl_eval.41.105768164>
9> Multiply(20, 9).
180
```
Анонимная функция работает и выдает корректный результат.

## 2. `converter:to_rub/1` -> анонимная  функция

```erlang
22> ToRub = fun ({usd, Amount}) when is_integer(Amount), Amount>0  -> {ok, Amount * 75,5}; ({euro, Amount})  when is_integer(Amount), Amount>0  -> {ok, 80 * Amount}; ({lari, Amount}) when is_integer(Amount), Amount>0  -> {ok, 29 * Amount}; ({peso, Amount}) when is_integer(Amount), Amount>0 -> {ok, 3 * Amount}; ({krone, Amount}) when is_integer(Amount), Amount>0 -> {ok, 10 * Amount}; (_) -> {error, badarg} end.
#Fun<erl_eval.42.105768164>
23> ToRub({usd, 100}).
{ok,7500,5}
24> ToRub({peso, 12}).
{ok,36}
25> ToRub({yene, 30}).
{error,badarg}
26> ToRub({euro, -15}).
{error,badarg}
```

Функция выдает ожидаемые и корректные результаты, аналогичные тому, что выдавала функция `to_rub/1`.

## 3. Заголовочный файл `person.hrl`


### Заголовочный файл `person.hrl`

```erlang
-record(person, {
  id,
  name,
  age,
  gender
}).

-define(MALE, male).

-define(FEMALE, female).
```

### `filter/2` - Функция фильтрации списка персон

```erlang
filter(Fun, Persons) -> lists:filter(Fun, Persons).

my_filter(Fun, Persons) -> my_filter(Fun, Persons, []).

my_filter(_Fun, [], Acc) -> lists:reverse(Acc);
my_filter(Fun, [Person|Persons], Acc) -> 
    case Fun(Person) of 
        true -> my_filter(Fun, Persons, [Person|Acc]);
        false -> my_filter(Fun, Persons, Acc)
    end.
```


### `all/2` - Функция проверки, что все персоны подходят под условие

```erlang
all(Fun, Persons) -> lists:all(Fun, Persons).

my_all(_Fun, []) -> true;
my_all(Fun, [Person|Persons]) -> 
    case Fun(Person) of
        true -> my_all(Fun, Persons);
        false -> false
    end.

```


### `any/2` Функция проверки, что хотя бы одна персона подходит под условие 

```erlang
is_female(#person{gender = Gender}) -> Gender =:= ?FEMALE.

any(Fun, Persons) -> lists:any(Fun, Persons).


my_any(_Fun, []) -> false;
my_any(Fun, [Person|Persons]) -> 
    case Fun(Person) of
        false -> my_any(Fun, Persons);
        true -> true
    end.
```

### `update/2` Функция обновления данных персон

```erlang
update(Fun, Persons) -> lists:map(Fun, Persons).

my_update(Fun, Persons) -> my_update(Fun, Persons, []).

my_update(_Fun, [], Acc) -> lists:reverse(Acc);
my_update(Fun, [Person|Persons], Acc) -> my_update(Fun, Persons, [Fun(Person)|Acc]).
```

### `get_average_age/1` - Функция подсчета среднего возраста персон из списка

Листинг двух реализаций функции `get_average_age/1` (с использованием `lists:foldl` и без нее)
```erlang
get_average_age([]) -> {error, badarg};
get_average_age(Persons) when is_list(Persons) ->
    {AgeCount, PersonCount} = lists:foldl(fun(#person{age = Age}, {AgeCount, PersonCount}) -> {AgeCount + Age, PersonCount + 1} end, {0, 0}, Persons),
    case PersonCount of
        0 -> {error, badarg}; %
        _ -> AgeCount/PersonCount
    end.

my_get_average_age([]) -> {error, badarg};
my_get_average_age(Persons) when is_list(Persons) -> my_get_average_age(Persons, {0, 0}).

my_get_average_age([], {Age, Persons}) when Persons /= 0 -> Age / Persons;
my_get_average_age([#person{age = Age}|Persons], {AgeCount, PersonsCount}) ->
    my_get_average_age(Persons, {AgeCount + Age, PersonsCount + 1});

my_get_average_age(_, _) -> {error, badarg}.
```

Результат работы функций:
```erlang 
3> rr("person.hrl").
[person]
4> Persons = [#person{id=1, name="Bob", age=23, gender=male}, 
      #person{id=2, name="Kate", age=20, gender=female},
      #person{id=3, name="Jack", age=34, gender=male},
      #person{id=4, name="Nate", age=54, gender=female}].
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
5> EmptyList = [].
[]

7> persons:my_get_average_age(Persons).
32.75
8> persons:my_get_average_age(EmptyList). % обработка пустого списка
{error,badarg}
```

```erlang
10> persons:get_average_age(Persons).
32.75
11> persons:get_average_age(EmptyList).
{error,badarg}
```


## Результаты

Используется тот же список записей из второй работы:
```erlang
Persons = [#person{id=1, name="Bob", age=23, gender=male},
      #person{id=2, name="Kate", age=20, gender=female},
      #person{id=3, name="Jack", age=34, gender=male},
      #person{id=4, name="Nate", age=54, gender=female}].
```
 - Получите список из персон старше 30 лет
```erlang
16> persons:filter(fun(#person{age = Age}) -> Age >= 30 end, Persons).
[#person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
```

```erlang
18> persons:my_filter(fun(#person{age = Age}) -> Age >= 30 end, Persons).
[#person{id = 4,name = "Nate",age = 54,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male}]
```
 - Получите список из мужчин

```erlang
% Используется функция из модуля persons.erl
% is_male(#person{gender = Gender}) -> Gender =:= ?MALE.
53> persons:filter(fun persons:is_male/1, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 3,name = "Jack",age = 34,gender = male}] 
```

```erlang
54> persons:my_filter(fun persons:is_male/1, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 3,name = "Jack",age = 34,gender = male}]
```

 - Проверьте, что в списке есть хотя бы одна женщина

```erlang
% Используется функция из модуля persons.erl
% is_female(#person{gender = Gender}) -> Gender =:= ?FEMALE.
34> persons:any(fun persons:is_female/1, Persons).
true
35> persons:my_any(fun persons:is_female/1, Persons).
true
```

 - Проверьте, что в списке все старше 20 (включая)

```erlang
20> persons:all(fun(#person{age = Age}) -> Age >= 20 end, Persons).
true
21> persons:my_all(fun(#person{age = Age}) -> Age >= 20 end, Persons).
true
```

 - Проверьте, что в списке все младше 30 (включая)
```erlang
22> persons:all(fun(#person{age = Age}) -> Age =< 30 end, Persons).
false
23> persons:my_all(fun(#person{age = Age}) -> Age =< 30 end, Persons).
false
```

 - Обновите возраст (+1) персоне с именем Jack

```erlang
48>persons:update(fun(Person = #person{name = "Jack", age = Age}) -> Person#person{age = Age+1}; (Persons) -> Persons end, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 35,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]

49> persons:my_update(fun(Person = #person{name = "Jack", age = Age}) -> Person#person{age = Age+1}; (Persons) -> Persons end, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 35,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
```

 - Обновите возраст (-1) всем женщинам

```erlang
50> persons:update(fun(Person = #person{gender = female, age = Age}) -> Person#person{age = Age-1}; (Persons) -> Persons end, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 19,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 53,gender = female}]

51> persons:my_update(fun(Person = #person{gender = female, age = Age}) -> Person#person{age = Age-1}; (Persons) -> Persons end, Persons).
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 19,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 53,gender = female}]
```

# Функция `catch_all/1`

```erlang
-module(exceptions).

-export([catch_all/1]).

catch_all(Action) when is_function(Action, 0) ->
  try Action() of
    Result -> {ok, Result}
  catch
    throw:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      throw;
    error:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      error;
    exit:Reason ->
      io:format("Action ~p failed, reason ~p ~n", [Action, Reason]),
      exit;
    _:_ ->
      io:format("We covered all cases so this line will never be printed~n"),
      "Never will happen"
  end.
```

Результат работы:
```erlang
7> c("exceptions.erl").
{ok,exceptions}
8> exceptions:catch_all(fun() -> 1/0 end).
Action #Fun<erl_eval.43.105768164> failed, reason badarith
error
9> exceptions:catch_all(fun() -> throw(custom_exceptions) end).
Action #Fun<erl_eval.43.105768164> failed, reason custom_exceptions
throw
10> exceptions:catch_all(fun() -> exit(killed) end).
Action #Fun<erl_eval.43.105768164> failed, reason killed
exit
11> exceptions:catch_all(fun() -> erlang:error(runtime_exception) end).
Action #Fun<erl_eval.43.105768164> failed, reason runtime_exception
error
```

Т.е. функция `catch_all/1` принимает функцию без аргументов и пытается её выполнить. Если функция успешно выполняется, то возвращается результат этой функции. Если при выполнении функции возникает исключение, то `catch_all/1` перехватывает его и выводит сообщение об ошибке.

1. `exceptions:catch_all(fun() -> 1/0 end).` Здесь функция пытается поделить 1 на 0, что вызывает ошибку `badarith`. 

2. `exceptions:catch_all(fun() -> throw(custom_exceptions) end).` Здесь функция генерирует исключение типа `throw` с причиной `custom_exceptions`. 

3. `exceptions:catch_all(fun() -> exit(killed) end).` Здесь функция генерирует исключение типа `exit` с причиной `killed`. 

4. `exceptions:catch_all(fun() -> erlang:error(runtime_exception) end).` Здесь функция генерирует исключение типа `error` с причиной `runtime_exception`. 

Во всех случаях функция перехватывает исключения и выводит сообщения об ошибке.