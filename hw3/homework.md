## 1 Часть

```erlang
4> Persons = [#person{id=1, name="Bob", age=23, gender=male},
      #person{id=2, name="Kate", age=20, gender=female},
      #person{id=3, name="Jack", age=34, gender=male},
      #person{id=4, name="Nate", age=54, gender=female}].
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male },
 #person{id = 4,name = "Nate",age = 54,gender = female}]
```

Создаем список записей `Persons`.

### 1

```erlang
5> [_, SecondPerson, _, _] = Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
6> SecondPerson.
#person{id = 2,name = "Kate",age = 20,gender = female}
```

`SecondPerson` сопоставляется со вторым элементом списка `Persons` и становится равным `#person{id = 2,name = "Kate",age = 20,gender = female}`. Прочерки обозначают анонимную переменную, чье значение нам не важно. 

### 2

```erlang
7> SecondAge = SecondPerson#person.age.
20
8> SecondName = SecondPerson#person.name.
"Kate"
```

Обращаемся к полям `name` и `age` в записи `SecondPerson` типа person, для того чтобы извлечь поля `age` и `name` из данной записи и присвоить их значения переменным `SecondAge` и `SecondName`.

### 3

```erlang
9> [_, #person{name = SecondName, age = SecondAge} | _Rest] = Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
10> SecondName.
"Kate"
11> SecondAge.
20
12> _Rest.
[#person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
```

Здесь сопоставление с образцом проходит успешно, потому что второй элемент списка `Persons` имеет имя "Kate" и возраст 20, что соответствует значениям `SecondName` и `SecondAge`.


### 4

```erlang
14> Persons == [#person{id = 1,name = "Bob",age = 23,gender = male},
    #person{id = 2,name = "Kate",age = 20,gender = female},
    #person{id = 3,name = "Jack",age = 34,gender = male},
    #person{id = 4,name = "Nate",age = 54,gender = female}].
true 
```

Можно убедиться, что список записей `Persons` не изменился.

### 5

```erlang
15> SecondPerson#person{age = 21}.
#person{id = 2,name = "Kate",age = 21,gender = female}
16> Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
17> SecondPerson.
#person{id = 2,name = "Kate",age = 20,gender = female}
```

Команда номер 15 создаст новую запись на основе `SecondPerson`, но с возрастом, равным 21. Однако это не изменит оригинальную запись `SecondPerson` или список `Persons`, поскольку Erlang не допускает изменения данных.

## 2 Часть 

```erlang
19> Persons = [#{id => 1, name => "Bob", age => 23, gender => male},
    #{id => 2, name => "Kate", age => 20, gender => female},
    #{id => 3, name => "Jack", age => 34, gender => male},
    #{id => 4, name => "Nate", age => 54, gender => female}].
[#{id => 1,name => "Bob",age => 23,gender => male},
 #{id => 2,name => "Kate",age => 20,gender => female},
 #{id => 3,name => "Jack",age => 34,gender => male},
 #{id => 4,name => "Nate",age => 54,gender => female}]
```

Создаем список карт (Maps) `Persons`.

### 1

```erlang
20> [FirstPerson | _] = Persons.
[#{id => 1,name => "Bob",age => 23,gender => male},
 #{id => 2,name => "Kate",age => 20,gender => female},
 #{id => 3,name => "Jack",age => 34,gender => male},
 #{id => 4,name => "Nate",age => 54,gender => female}]
21> FirstPerson.
#{id => 1,name => "Bob",age => 23,gender => male}
```

Переменная `FirstPerson` была сопоставлена с первым элементом списка карт `Persons` и становится равной `#{id => 1,name => "Bob",age => 23,gender => male}`. Прочерк обозначает анонимную переменную, те ее значение для нас не важно.

### 2

```erlang
22> [_, _, #{name := Name, age := Age},  _] = Persons.
[#{id => 1,name => "Bob",age => 23,gender => male},
 #{id => 2,name => "Kate",age => 20,gender => female},
 #{id => 3,name => "Jack",age => 34,gender => male},
 #{id => 4,name => "Nate",age => 54,gender => female}]
23> Name.
"Jack"
24> Age.
34
``` 

Переменные `Name` и `Age` сопоставляются со значениями по ключам `name` и `age` третьего элемента списка карт и становятся равными `"Jack"` и `34`.

### 3

```erlang
25> [_First, _Second, #{name := Name, age := Age} | _Rest] = Persons.
[#{id => 1,name => "Bob",age => 23,gender => male},
 #{id => 2,name => "Kate",age => 20,gender => female},
 #{id => 3,name => "Jack",age => 34,gender => male},
 #{id => 4,name => "Nate",age => 54,gender => female}]
26> _First.
#{id => 1,name => "Bob",age => 23,gender => male}
27> _Second.
#{id => 2,name => "Kate",age => 20,gender => female}
28> Name.
"Jack"
29> Age.
34
30> _Rest.
[#{id => 4,name => "Nate",age => 54,gender => female}]
```

Здесь команда завершилась успешно, потому что значения переменных `Name` и `Age` равны значениям ключей `name` и `age` третьего элемента списка карт `Persons`.  

### 4

```erlang
31> Persons.
[#{id => 1,name => "Bob",age => 23,gender => male},
 #{id => 2,name => "Kate",age => 20,gender => female},
 #{id => 3,name => "Jack",age => 34,gender => male},
 #{id => 4,name => "Nate",age => 54,gender => female}]
```

Как видно, список карт `Persons` не изменился после всех операций, потому что в Erlang'e переменные не меняются. 

### 5
```erlang
32> FirstPerson#{age := 24}.
#{id => 1,name => "Bob",age => 24,gender => male}
33> Persons.
[#{id => 1,name => "Bob",age => 23,gender => male},
 #{id => 2,name => "Kate",age => 20,gender => female},
 #{id => 3,name => "Jack",age => 34,gender => male},
 #{id => 4,name => "Nate",age => 54,gender => female}]
34> FirstPerson.
#{id => 1,name => "Bob",age => 23,gender => male}
```
Была создана новая карта, но с уже другим возрастом (не 23, а 24). При этом (как и ожидалось) это не изменило ни карту `FirstPerson` ни список карт `Persons`.

### 6
```erlang
35> FirstPerson#{address := "Mira 31"}.
** exception error: bad key: address
     in function  maps:update/3
        called as maps:update(address,"Mira 31",
                              #{id => 1,name => "Bob",age => 23,gender => male})
        *** argument 3: not a map
     in call from erl_eval:'-expr/6-fun-0-'/2 (erl_eval.erl, line 311)
     in call from lists:foldl/3 (lists.erl, line 1594)


36> NewPerson = FirstPerson#{address => "Mira 30"}.
#{id => 1,name => "Bob",address => "Mira 30",age => 23,
  gender => male}
37> NewPerson#{address := "Mira 31"}.
#{id => 1,name => "Bob",address => "Mira 31",age => 23,
  gender => male}
```
Ошибка возникла из-за того что ключа address не существовало.
В Erlang оператор := предназначен для сопоставления с образцом (и изменения данных) и требует наличия ключа в карте. При этом с помощью оператора => можно добавить новую пару ключ-значение (в новый список). И этот список уже можно изменить (создав новый с другими значениями) с помощью оператора :=.

## 3 Часть

Листинг программы `converter.erl`:

```erlang
% Атрибут, который определяет имя модуля
-module(converter).

% Атрибут, который делает доступными функции для вызова извне (в данном случае функцию to_rub)
% Арность (целое число после названия функции) - количество параметров функции. 
-export([to_rub/1]).

to_rub({usd, Amount}) when Amount > 0, is_integer(Amount) -> 
    io:format("Convert ~p to rub, amount ~p~n", [usd, Amount]),
    {ok, Amount*75.5};

to_rub({euro, Amount}) when Amount > 0, is_integer(Amount) ->
    io:format("Convert ~p to rub, amount ~p~n", [euro, Amount]),    
    {ok, Amount*80};

to_rub({lari, Amount}) when Amount > 0, is_integer(Amount) ->
    io:format("Convert ~p to rub, amount ~p~n", [lari, Amount]),    
    {ok, Amount*29};

to_rub({peso, Amount}) when Amount > 0, is_integer(Amount) ->
    io:format("Convert ~p to rub, amount ~p~n", [peso, Amount]),    
    {ok, Amount*3};

to_rub({krone, Amount}) when Amount > 0, is_integer(Amount) ->
    io:format("Convert ~p to rub, amount ~p~n", [krone, Amount]),    
    {ok, Amount*10};

% Вывод ошибки (т.е. если будет введено что-то иное, то отработает эта функция)
to_rub(_) ->
    io:format("Bad arguments~n"),
    {error, badarg}.
```

Результаты работы:
```erlang

67>c("converter.erl"). % Компиляция модуля 
{ok,converter}

68> converter:to_rub({usd, 100}).
Convert usd to rub, amount 100
{ok,7550.0}

69> converter:to_rub({peso, 12}).
Convert peso to rub, amount 12
{ok,36} 

70> converter:to_rub({yene, 30}). % Вывод ошибки, потому что не с чем сопоставить атом yene 
Bad arguments
{error,badarg}

71> converter:to_rub({euro, -15}). % Вывод ошибки, потому что в охранных условиях есть проверка на то что число больше 0 
Bad arguments
{error,badarg}

72> converter:to_rub({euro, 15.2}). % Вывод ошибки, потому что в охранных условиях есть проверка на целое
Bad arguments
{error,badarg}
```

Как видно, программа работает корректно и выводит ошибки, когда ввели не целое, меньше нуля или неверный тип валюты. 

Проверка на верно введенную валюту производится благодаря сопоставлению с образцом. 
В моем модуле также есть охранные условия (guards) для проверки, на то что `Amount` является целым числом и больше нуля. 
Функция `to_rub(_)` будет вызвана и вернет `{error, badarg}`, если все предыдущие функции `to_rub/1` не смогли обработать аргумент. 
