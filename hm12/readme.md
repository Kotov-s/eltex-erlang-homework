## string:tokens/2
- `string:tokens/2`: Разделяет строку на токены по заданным разделителям. Принимает строку и строку разделителей. Возвращает список токенов.

```erlang
1>  Data = "John,Doe,30".
"John,Doe,30"
2> string:tokens(Data, ",").
["John","Doe","30"]
```

## string:join/2

- `string:join/2`: Объединяет список строк в одну строку с заданным разделителем. Принимает список строк и строку разделителя. Возвращает строку.

```erlang
3> List = ["Мой","дядя","самых","честных","правил"].
["Мой","дядя","самых","честных","правил"]
4> string:join(List, " ").
"Мой дядя самых честных правил"
```

## string:strip/1, string:strip/2

- `string:strip/1`: Удаляет пробелы в начале и конце строки.
- `string:strip/2`: Удаляет пробелы в конце, начале или в начале и конце строки.
- `string:strip/3`: Удаляет пробелы или другие символы из строки. Функция принимает три аргумента: строку, направление (left, right, both) и символ.

```erlang
1> string:strip("         строка   ").
"строка"
2> string:strip("         строка   ", left).
"строка   "
3> string:strip("         строка   ", right).
"         строка"
4> string:strip("         строка   ", both).
"строка"
5> string:strip("....строка...", both, $.).
"строка"
6> string:strip("....строка...", left, $.).
"строка..."
7> string:strip("....строка...", right, $.).
"....строка"
```

## string:to_upper/1, string:to_lower/1

- `string:to_upper/1`: Преобразует строку в верхний регистр. Принимает строку. Возвращает строку. 
- `string:to_lower/1`: Преобразует строку в нижний регистр. Принимает строку. Возвращает строку.

```erlang
16> string:to_upper("My uncle’s goodness is extreme").
"MY UNCLE’S GOODNESS IS EXTREME"
17> string:to_lower("My uncle’s goodness is extreme").
"my uncle’s goodness is extreme"

1> Str1 = "Hello".
"Hello"
2> Str2 = "HeLLO".
"HeLLO"
3> string:to_upper(Str1) =:= string:to_upper(Str2). %% Пример применения 
true
```

## string:to_integer/1 и erlang:list_to_integer/1

- `string:to_integer/1`: Преобразует строку в целое число. Принимает строку. Возвращает кортеж из целого числа и остатка строки. 
- `erlang:list_to_integer/1`: Преобразует строку в целое число. Принимает строку. Возвращает целое число.

```erlang
4> string:to_integer("10000").
{10000,[]}
5> string:to_integer("1000 раз").
{1000," раз"}
6> string:to_integer("в 1000 раз").
{error,no_integer}

7> list_to_integer("1000").
1000
8> list_to_integer("1000 раз").
** exception error: bad argument
     in function  list_to_integer/1
        called as list_to_integer("1000 раз")
        *** argument 1: not a textual representation of an integer
```

## erlang:byte_size/1

- `erlang:byte_size/1`: Возвращает целое число, которое представляет количество байтов, необходимых для хранения данной Bitstring.

```erlang
10> byte_size(<<"My uncle’s goodness is extreme">>).
30
```

## erlang:split_binary/2

- `erlang:split_binary/2`: Разделяет Bitstring на две части. Принимает Bitstring и позицию. Возвращает кортеж из двух Bitstring.

```erlang
29> split_binary(<<"My uncle's goodness is extreme">>, 3).
{<<"My ">>,<<"uncle's goodness is extreme">>}
```

## erlang:binary_part/3

- `erlang:binary_part/3`: Возвращает часть Bitstring. Принимает Bitstring, начальную позицию и длину. Возвращает Bitstring.

```erlang
22> binary_part(<<"My uncle’s">>, 3, 5).
<<"uncle">>
```

## binary:split/2

- `binary:split/2`: Разделяет Bitstring на части по разделителю. Принимает Bitstring и разделитель. Возвращает список Bitstring.

```erlang
27> binary:split(<<"My uncle's goodness is extreme">>, <<" ">>).
[<<"My">>,<<"uncle's goodness is extreme">>]
28> binary:split(<<"My uncle's goodness is extreme">>, [<<" ">>, <<"'">>], [global]).
[<<"My">>,<<"uncle">>,<<"s">>,<<"goodness">>,<<"is">>,
 <<"extreme">>]
```

## binary:match/2, binary:matches/3

- `binary:match/2`: Ищет подстроку в Bitstring. Принимает Bitstring и подстроку. Возвращает позицию и длину совпадения.
- `binary:matches/3`: Ищет подстроку/подстроки в Bitstring. Принимает Bitstring и подстроку и возможно для binary:matches/3. Возвращает список совпадений.

```erlang
32> binary:match(<<"My uncle's goodness is extreme">>, <<"s ">>).
{9,2}
33> binary:matches(<<"My uncle's goodness is extreme">>, <<"s ">>).
[{9,2},{18,2},{21,2}]
```

## binary:replace/3

- `binary:replace/3`: Заменяет вхождение подстроки в Bitstring. Принимает Bitstring, подстроку и замену. Возвращает Bitstring.

```erlang
35> binary:replace(<<"My uncle’s goodness is extreme">>, <<"’">>, <<"'">>).
<<"My uncle's goodness is extreme">>
36> binary:replace(<<"My uncle's goodness is extreme">>, <<"s">>, <<"">>, [global]).
<<"My uncle' goodne i extreme">>
```

## binary_to_list/1

- `binary_to_list/1`: Преобразует Bitstring в список. Принимает Bitstring. Возвращает список.

```erlang
36> binary_to_list(<<"My uncle's goodness is extreme">>).
"My uncle's goodness is extreme"
```

## list_to_binary/1

- `list_to_binary/1`: Преобразует список в Bitstring. Принимает список. Возвращает Bitstring.

```erlang
38> list_to_binary("My uncle's goodness is extreme").
<<"My uncle's goodness is extreme">>
```

## lists:flatten/1

- `lists:flatten/1`: Берет список, который может содержать другие списки внутри себя, и "выравнивает" его, то есть преобразует в один простой список без вложенных списков. Принимает список. Возвращает список.

```erlang
43> lists:flatten(["Мой","дядя","самых","честных","правил"]).
"Мойдядясамыхчестныхправил"
```