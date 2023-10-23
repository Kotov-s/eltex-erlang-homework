## 1 Часть

```erlang
1> Persons = [{person, 1, "Bob", 23, male},
            {person, 2, "Kate", 20, female},
            {person, 3, "Jack", 34, male},
            {person, 4, "Nate", 54, female}].
[{person,1,"Bob",23,male},
 {person,2,"Kate",20,female},
 {person,3,"Jack",34,male},
 {person,4,"Nate",54,female}]

2> [First|Rest] = Persons.
[{person,1,"Bob",23,male},
 {person,2,"Kate",20,female},
 {person,3,"Jack",34,male},
 {person,4,"Nate",54,female}]

3> First.
{person,1,"Bob",23,male}

4> Rest.
[{person,2,"Kate",20,female},
 {person,3,"Jack",34,male},
 {person,4,"Nate",54,female}]
```

Мы выполняем  операцию pattern matching, для того чтобы разделить список `Persons` на первый элемент и остаток (они же head и tail или голова и хвост). Поэтому переменная `First` будет равна первому элементу списка `Persons`, а переменная `Rest` будет равна новому списку начиная со второго элемента списка `Persons`.

```erlang
5> [Second|Rest1] = Rest.
[{person,2,"Kate",20,female},
 {person,3,"Jack",34,male},
 {person,4,"Nate",54,female}]

6> Second.
{person,2,"Kate",20,female}

7> Rest1.
[{person,3,"Jack",34,male},{person,4,"Nate",54,female}]
```

Происходит то же самое, что и в предыдущем случае: переменная `Second` связывается с первым элементом списка `Rest` (или со вторым элементом оригинального списка `Persons`), все остальное связывается со второй переменной `Rest1`.

```erlang
8> [Third, Fourth | Rest2] = Rest1.
[{person,3,"Jack",34,male},{person,4,"Nate",54,female}]

9> Third.
{person,3,"Jack",34,male}

10> Fourth.
{person,4,"Nate",54,female}

11> Rest2.
[]
```

Поскольку в списке `Rest1` находится всего 2 элемента, то после того как они извлекаются, чтобы сопоставится с переменным `Third` и `Fourth`, список `Rest1` становится пустым. И именно этот пустой список и сопоставляется с переменной `Rest2`.

```erlang
12> Persons.
[{person,1,"Bob",23,male},
 {person,2,"Kate",20,female},
 {person,3,"Jack",34,male},
 {person,4,"Nate",54,female}]
13> Persons == [{person,1,"Bob",23,male},
                {person,2,"Kate",20,female},
                {person,3,"Jack",34,male},
                {person,4,"Nate",54,female}].
true
```

Убеждаемся, что список `Persons` не изменился.

## 2 Часть

Был создан файл заголовка `person.hrl` со следующей записью:

```erlang
-record(person, {id,
                name,
                age,
                gender}).
```

```erlang
1> rr("person.hrl").
[person]
```

Запись была считана из файла заголовка с помощью команды rr("person.hrl") (rr - read records). 

```erlang
2> Persons = [#person{id=1, name="Bob", age=23, gender=male},
   #person{id=2, name="Kate", age=20, gender=female},
   #person{id=3, name="Jack", age=34, gender=male},
   #person{id=4, name="Nate", age=54, gender=female}].
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
```

Создание записи, которая сопоставляется с переменной Persons

### 1.

```erlang
3> [FirstPerson|_] = Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
4> FirstPerson.
#person{id = 1,name = "Bob",age = 23,gender = male}
```

Первый элемент списка Persons (а именно запись `#person{id = 1, name = “Bob”, age = 23, gender = male`), сопоставляется с неинициализированной переменной `FirstPerson`, которая становится равной этому элементу. Прочерк означает анонимную переменную. Таким образом показываем, что мы не заинтересованы в значении этой переменной и ее данные нам не важны.

### 2.

```erlang
5> [_, SecondPerson, _, _] = Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
6> SecondPerson.
#person{id = 2,name = "Kate",age = 20,gender = female}
```

`SecondPerson` сопоставляется со вторым элементом списка Persons. Остальные элементы игнорируются. 

### 3.

```erlang
7> [_, _, SecondPerson | _] = Persons.
** exception error: no match of right hand side value [#person{id = 1,name = "Bob",age = 23,gender = male},
                                                       #person{id = 2,name = "Kate",age = 20,gender = female},
                                                       #person{id = 3,name = "Jack",age = 34,gender = male},
                                                       #person{id = 4,name = "Nate",age = 54,gender = female}]
```

Появляется ошибка, потому что `SecondPerson` (переменная, чьё значение равно `#person{id = 2,name = "Kate",age = 20,gender = female}`) сопоставляется с `#person{id = 3,name = "Jack",age = 34,gender = male}`.

При этом все будет работать, если вместо `SecondPerson` использовать неинициализированную переменную `ThirdPerson` (или равную `#person{id = 3,name = "Jack",age = 34,gender = male}`). 

```erlang
4>[_, _, ThirdPerson | _] = Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nata",age = 54,gender = female}]
5> ThirdPerson.
#person{id = 3,name = "Jack",age = 34,gender = male}
```

### 4.

```erlang
8> SecondName = SecondPerson#person.name.
"Kate"
9> SecondAge = SecondPerson#person.age.
20
```

Эти команды извлекают имя и возраст из записи `SecondPerson` и присваивают их переменным `SecondName` и `SecondAge` соответственно.

```erlang
10> Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
11> Persons == [#person{id = 1,name = "Bob",age = 23,gender = male},
     #person{id = 2,name = "Kate",age = 20,gender = female},
     #person{id = 3,name = "Jack",age = 34,gender = male},
     #person{id = 4,name = "Nate",age = 54,gender = female}].
true
```

Проверяем и убеждаемся, что список записей Persons не изменился.

```erlang
12> SecondPerson#person{age = 21}.
#person{id = 2,name = "Kate",age = 21,gender = female}
13> SecondPerson.
#person{id = 2,name = "Kate",age = 20,gender = female}
14> Persons.
[#person{id = 1,name = "Bob",age = 23,gender = male},
 #person{id = 2,name = "Kate",age = 20,gender = female},
 #person{id = 3,name = "Jack",age = 34,gender = male},
 #person{id = 4,name = "Nate",age = 54,gender = female}]
```

Эта команда создает новую запись на основе записи `SecondPerson`, но с возрастом, установленным в 21 год. Однако это не изменяет исходную запись `SecondPerson` или список `Persons`. В Erlang'e переменные не меняются.

## 3 Часть

![Alt text](../hm1/pics/3part/1.png)

При помо­щи приставки $ получаем ASCII значение символа:

![Alt text](../hm1/pics/3part/2.png)

![Alt text](../hm1/pics/3part/3.png)

В Erlang строки представляют собой списки целых чисел, где каждое целое число - это код ASCII соответствующего символа. Поэтому все эти записи выводят идентичный результат. 

При этом 

```erlang
[97, 98, 99]. % вернет "abc"
[1, 97, 98, 99]. % вернет [1,97,98,99]
% Erlang будет печатать списки чисел как числа, когда хотя бы одно из них не может представлять букву
```

![Alt text](../hm1/pics/3part/4.png)

Binary более эффективны в плане потреблен ия памяти.
Их недостатком в сравнении со списками является потеря простоты, когда дело доходит до сопоставления с образцом и манипуляций. Следовательно, предпочтительно использовать binary для хранения текста, который не будет подвергаться обработке слишком часто, или когда сбережение памяти становится настоящей проблемой.