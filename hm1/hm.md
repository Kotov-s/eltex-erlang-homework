## 1. Создайте список кортежей, которые содержат данные о 4х человек.

![Alt text](pics/1part/1.png)

Создаем список кортежей и кортежа

![Alt text](pics/1part/2.png)

Создаем новый список, где `Persons` является первым элементом (головой), а `[NewPerson] `является остальной частью списка (хвостом). В итоге получаем список списков.

![Alt text](pics/1part/3.png)

Создаем новый список, добавляя `NewPerson` в начало списка `Persons`. В результате `NewPerson` становится первым элементом нового списка (головой).

![Alt text](pics/1part/4.png)

Ошибка, потому что оператор `++` используется для объединения двух списков, а `NewPerson` не является списком.

![Alt text](pics/1part/5.png)

Создаем новый список, добавляя `NewPerson` в начало списка `Persons`. В этом случае `NewPerson` обернут в список, поэтому оператор `++` может объединить два списка вместе

![Alt text](pics/1part/7.png)

Убеждаемся, что список `Persons` после всех операций неизменился.

## 2. Выполните команды и обратите внимание на результаты полученных команд. Объясните ошибки в некоторых выражениях. 

Erlang поддерживает следующие арифметические и логические операции

![Alt text](pics/1.png)
![Alt text](pics/2.png)

![Alt text](pics/2part/1.png)

![Alt text](pics/2part/2.png)

/ - это деление с плавающей запятой

![Alt text](pics/2part/3.png)

div  - это целочисленное деление

![Alt text](pics/2part/4.png)

rem (remainder, остаток) - это взятие остатка по модулю 

![Alt text](pics/2part/5.png)

Это битовые операция И (AND) и OR, которые применяется к двоичным числам 10 (в десятичной системе это 2) и 01 (в десятичной системе это 1).

![Alt text](pics/2part/6.png)

Ошибка выводится потому что a - это атом, а с атомами допустимы только операции сранения

![Alt text](pics/2part/7.png)

Эта строка вызовет ошибку из-за переполнения. Оператор bsl выполняет битовый сдвиг влево. Выражение (1 bsl 64) сдвигает единицу на 64 позиции влево, что приводит к очень большому числу, на которое первая единица также пытается сдвинуться влево.

## 3. Напишите слово “Night” как 

![Alt text](pics/3part/1.png)

При помо­щи приставки $ получаем ASCII значение символа:

![Alt text](pics/3part/2.png)

![Alt text](pics/3part/3.png)

В Erlang строки представляют собой списки целых чисел, где каждое целое число - это код ASCII соответствующего символа. Поэтому все эти записи выводят идентичный результат. 

При этом 

```erlang
[97, 98, 99]. % вернет "abc"
[1, 97, 98, 99]. % вернет [1,97,98,99]
% Erlang будет печатать списки чисел как числа, когда хотя бы одно из них не может представлять букву
```

![Alt text](pics/3part/4.png)

Binary более эффективны в плане потреблен ия памяти.
Их недостатком в сравнении со списками является потеря простоты, когда дело доходит до сопоставления с образцом и манипуляций. Следовательно, предпочтительно использовать binary для хранения текста, который не будет подвергаться обработке слишком часто, или когда сбережение памяти становится настоящей проблемой.