# 1. В Eshell выполните List comprehensions

## Целые числа от 1 до 10, которые делятся на три
```erlang
9> [X || X <- lists:seq(1, 10), X rem 3 =:= 0].
[3,6,9]
```

## Удаление всех нецелы= чисел из списка + возведение в квадрат

```erlang
13> [ X * X || X <- [1, "hello", 100, boo, "boo", 9], is_integer(X)].   
[1,10000,81]
```

# 2. Прокомментируйте код и возникшие ошибки 

## 2.1. <<X:4,Y:2>> = <<42:6>>.

```erlang
13> <<X:4,Y:2>> = <<42:6>>.
<<42:6>>
14> X.
10
15> Y.
2
16>2#101010.
42
```

- 42₁₀ = 101010₂
- `X` = 1010₂ = 10₁₀, тк `X:4` означает, что для переменной `X` используется 4 бита
- `Y` = 10₂ = 2₁₀, тк `Y:2` означает, что для переменной `Y` используется 2 бита

Сопоставление происходит успешно, потому что и с левой и с правой стороны суммарно используется 6 бит

## 2.2. <<C:4,D:4>> = <<1998:6>>. 

```erlang
18> <<C:4,D:4>> = <<1998:6>>.
** exception error: no match of right hand side value <<14:6>>
```

Сопоставление не успешно, потому что слева 8 бит, а справа 6

## 2.3. <<C:4,D:2>> = <<1998:8>>. 

```erlang
17> <<C:4,D:2>> = <<1998:8>>.
** exception error: no match of right hand side value <<"Î">>
```

Сопоставление не успешно, потому что слева 6 бит, а справа 8

## 2.4. Вариант правильной записи 

```erlang
34> <<C:4, D:2, E:2>> = <<1998:8>>.
<<"Î">>
35> C.
12
36> D.
3
37> E.
2
38> $Î.
206
```
Сопоставление происходит без ошибок, потому что слева и справа битность одинаковая.

- 1998₁₀ = 0111 1100 1110₂, но тк используется только 8 бит, то первые 4 бита обрезаются (0111 1100 1110₂ -> 1100 1110₂ = 206₁₀ = $Î). 
- C = 1100₂ = 12₁₀, т.к. для нее используется 4 бита  
- D = 11₂ = 3₁₀, т.к. для нее используется 2 бита  
- E = 10₂ = 2₁₀, т.к. для нее используется 2 бита 

# 3. protocol.erl, `ipv4/1`


```erlang

ipv4(<<Version:4, _Rest/bitstring >>) when Version =/= 4 ->
    throw("Incorrect version of ip");

ipv4(<<Version:4, IHL:4, ToS:8, TotalLength:16,
 	Identification:16, Flags:3, FragOffset:13,
 	TimeToLive:8, Protocol:8, Checksum:16,
 	SourceAddress:32, DestinationAddress:32,
 	OptionsAndPadding:((IHL-5)*32)/bits,
 	RemainingData/bytes >>
) when Version =:= 4 ->
	io:format("Received data ~p ~n", [RemainingData]),
	#ipv4{
		version = Version,
        ihl = IHL,
        tos = ToS,
        total_length = TotalLength,
        identification = Identification,
        flags = Flags,
        frag_offset = FragOffset,
        time_to_live = TimeToLive,
        protocol = Protocol,
        checksum = Checksum,
        source_address = SourceAddress,
        destination_address = DestinationAddress,
        options_and_padding = OptionsAndPadding,
		remaining_data = RemainingData
    };

ipv4(_) ->
    throw("Incorrect argument").
```

Результаты:

```erlang
45> DataWrongFormat = <<4:4, 6:4, 0:8, 0:3>>.
46> DataWrongVer = <<6:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13,0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.


47> protocol:ipv4(DataWrongVer).
** exception throw: "Incorrect version of ip"
     in function  protocol:ipv4/1 (protocol.erl, line 49)
48> protocol:ipv4(DataWrongFormat).
** exception throw: "Incorrect argument"
     in function  protocol:ipv4/1 (protocol.erl, line 52)

50> Data1 = <<4:4, 6:4, 0:8, 232:16, 0:16, 0:3, 0:13, 0:8, 0:8, 0:16, 0:32, 0:32, 0:32, "hello" >>.
<<70,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,
  101,108,108,111>>
51> protocol:ipv4(Data1).
Received data <<"hello">>
{ipv4,4,6,0,232,0,0,0,0,0,0,0,0,<<0,0,0,0>>,<<"hello">>}

52> Data2 = <<4:4, 6:4, 0:8, 232:16, 0:16, 1:3, 2:13, 3:8, 4:8, 5:16, 6:32, 7:32, 8:32, "bye" >>.
<<70,0,0,232,0,0,32,2,3,4,0,5,0,0,0,6,0,0,0,7,0,0,0,8,98,
  121,101>>
53> protocol:ipv4(Data2).
Received data <<"bye">>
{ipv4,4,6,0,232,0,1,2,3,4,5,6,7,<<0,0,0,8>>,<<"bye">>}
```



1. Вызовите предыдущую функцию в новом процессе с валидными данными Data1 или Data2 (spawn/3)

2. Проверьте ваш pid (self/0).
      Вызовите предыдущую функцию protocol:ipv4(Data) в новом процессе (spawn/3) с DataWrongFormat или DataWrongVer.
      Проверьте ваш pid (self/0). 
      Изменился ли он после того как предыдущая строка завершилась ошибкой? Почему?

```erlang
60> self().
<0.148.0>
61> spawn(protocol, ipv4, [Data1]).
Received data <<"hello">>
<0.158.0>
62> self().
<0.148.0>
63> spawn(protocol, ipv4, [DataWrongFormat]).
=ERROR REPORT==== 31-Oct-2023::21:54:53.153000 ===
Error in process <0.161.0> with exit value:
{{nocatch,"Incorrect argument"},
 [{protocol,ipv4,1,[{file,"protocol.erl"},{line,51}]}]}
<0.161.0>
64> self().
<0.148.0>
65> throw("some error").
** exception throw: "some error"
66> self().
<0.164.0>
```

Исключения в Erlang не приводят к завершению всех процессов. Вместо этого они приводят только к завершению того процесса, в котором было выброшено исключение.

# 4. `ipv4_listener/0` 

Листинг `ipv4_listener/0`:
```erlang
ipv4_listener() ->
    receive 
    {ipv4, From, BinData} when is_bitstring(BinData) ->
        try ipv4(BinData) of
            Result -> From ! Result
        catch
            throw:Error -> From ! {error, Error},
            io:format("Error: ~p~n", [Error])
        end,
        ipv4_listener();
    Msg ->
        io:format("Received wrong format ~p~n", [Msg]),
        ipv4_listener()
    end.
```
Результаты:

```erlang
6> Pid = spawn(protocol, ipv4_listener, []).
<0.96.0>
7> Pid ! {ipv4, self(), DataWrongVer}.
Error: "Incorrect version of ip"
{ipv4,<0.85.0>,
      <<102,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        104,101,...>>}
8> Pid ! {ipv4, self(), DataWrongFormat}.
Error: "Incorrect argument"
{ipv4,<0.85.0>,<<70,0,0:3>>}
9> Pid ! {ipv4, self(), Data1}.
Received data <<"hello">>
{ipv4,<0.85.0>,
      <<70,0,0,232,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,104,
        101,...>>}
10> Pid ! {ipv4, self(), Data2}.
Received data <<"bye">>
{ipv4,<0.85.0>,
      <<70,0,0,232,0,0,32,2,3,4,0,5,0,0,0,6,0,0,0,7,0,0,0,8,98,
        121,...>>}
11> Pid ! "hello".
Received wrong format "hello"
"hello"

12> flush().
Shell got {error,"Incorrect version of ip"}
Shell got {error,"Incorrect argument"}
Shell got {ipv4,4,6,0,232,0,0,0,0,0,0,0,0,<<0,0,0,0>>,<<"hello">>}
Shell got {ipv4,4,6,0,232,0,1,2,3,4,5,6,7,<<0,0,0,8>>,<<"bye">>}
ok
```

Программа работает как и ожидалось, обрабатывая возникающие ошибки.