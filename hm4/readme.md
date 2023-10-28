# Часть 1

### to_rub2/1:

```erlang
to_rub2(Arg) ->
    Result = 
        case Arg of
            {usd, Amount} when is_integer(Amount), Amount > 0 ->
                io:format("Convert ~p to rub, amount ~p~n", [usd, Amount]),
                {ok, 75.5 * Amount};
            {euro, Amount} when is_integer(Amount), Amount > 0 ->
                io:format("Convert ~p to rub, amount ~p~n", [euro, Amount]),
                {ok, 80 * Amount};    
            {lari, Amount} when is_integer(Amount), Amount > 0 ->
                io:format("Convert ~p to rub, amount ~p~n", [lari, Amount]),
                {ok, 29 * Amount};
            {peso, Amount} when is_integer(Amount), Amount > 0 ->
                io:format("Convert ~p to rub, amount ~p~n", [peso, Amount]),
                {ok, 3 * Amount};
            {krone, Amount} when is_integer(Amount), Amount > 0 ->
                io:format("Convert ~p to rub, amount ~p~n", [krone, Amount]),
                {ok, 10 * Amount};
            Error ->
                io:format("Can’t convert to rub, error ~p~n", [Error]),
                {error, badarg}
        end,
    % Эта строчка кода была закомментирована, потому что из-за нее был бы вывод, где написано, что валюта "сконвертирована", даже если это не так
    % io:format("Converted ~p to rub, amount ~p, Result ~p~n", [Type, Amount, Result]),
    Result.
            
```

Проверка работы функции to_rub2/1:
```erlang
18> c("new_converter").
{ok,new_converter}
19> new_converter:to_rub2({usd, 100}).
Convert usd to rub, amount 100
{ok,7550.0}
20> new_converter:to_rub2({peso, 12}).
Convert peso to rub, amount 12
{ok,36}
21> new_converter:to_rub2({yene, 30}).
Can’t convert to rub, error {yene,30}
{error,badarg}
22> new_converter:to_rub2({euro, -15}).
Can’t convert to rub, error {euro,-15}
{error,badarg}
```
Функция работает корректно.

### to_rub3/1:
```erlang
to_rub3(Arg) ->
    case Arg of
        {usd, Amount} when is_integer(Amount), Amount > 0 ->
            io:format("Convert ~p to rub, amount ~p~n", [usd, Amount]),
            {ok, 75.5 * Amount};
        {euro, Amount} when is_integer(Amount), Amount > 0 ->
            io:format("Convert ~p to rub, amount ~p~n", [euro, Amount]),
            {ok, 80 * Amount};    
        {lari, Amount} when is_integer(Amount), Amount > 0 ->
            io:format("Convert ~p to rub, amount ~p~n", [lari, Amount]),
            {ok, 29 * Amount};
        {peso, Amount} when is_integer(Amount), Amount > 0 ->
            io:format("Convert ~p to rub, amount ~p~n", [peso, Amount]),
            {ok, 3 * Amount};
        {krone, Amount} when is_integer(Amount), Amount > 0 ->
            io:format("Convert ~p to rub, amount ~p~n", [krone, Amount]),
            {ok, 10 * Amount};
        Error ->
            io:format("Can’t convert to rub, error ~p~n", [Error]),
            {error, badarg}
    end.
```


Проверка работы функции to_rub3/1:
```erlang
23> new_converter:to_rub3({usd, 100}).
Convert usd to rub, amount 100
{ok,7550.0}
24> new_converter:to_rub3({peso, 12}).
Convert peso to rub, amount 12
{ok,36}
25> new_converter:to_rub3({yene, 30}).
Can’t convert to rub, error {yene,30}
{error,badarg}
26> new_converter:to_rub3({euro, -15}).
Can’t convert to rub, error {euro,-15}
{error,badarg}
```
Функция работает корректно.

Как видно, результат работы функций идентичен. Разница заключается в том что одна функция возвращает результат из case, а другая возвращает значение переменной Result  

## Часть 2

### rec_to_rub:

```erlang
rec_to_rub(#conv_info{type = usd, amount = Amount, commission = Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 75.5,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
rec_to_rub(#conv_info{type = euro, amount = Amount, commission = Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 80,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
rec_to_rub(#conv_info{type = lari, amount = Amount, commission = Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 29,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
rec_to_rub(#conv_info{type = peso, amount = Amount, commission = Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 3,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
rec_to_rub(#conv_info{type = krone, amount = Amount, commission = Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 10,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };

rec_to_rub(BadArgs) ->
    io:format("Can’t convert ~n~p~n", [BadArgs]),
    {error, badarg}.
```

Результаты работы функции:
```erlang
26> converter:rec_to_rub(#conv_info{type = usd, amount = 100, commission = 0.01}).
{ok,7474.5}
27> converter:rec_to_rub(#conv_info{type = peso, amount = 12, commission = 0.02}).
{ok,35.28}
28> converter:rec_to_rub(#conv_info{type = yene, amount = 30, commission = 0.02}).
Can’t convert
{conv_info,yene,30,0.02}
{error,badarg}
29> converter:rec_to_rub(#conv_info{type = euro, amount = -15, commission = 0.02}).
Can’t convert
{conv_info,euro,-15,0.02}
{error,badarg}
```
Видно, что функция работает корректно и выводит ошибки, в случае если такие пользователь допустил при вызове функции.

### map_to_rub:
```erlang
map_to_rub(#{type := usd, amount := Amount, commission := Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 75.5,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
map_to_rub(#{type := euro, amount := Amount, commission := Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 80,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
map_to_rub(#{type := lari, amount := Amount, commission := Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 29,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
map_to_rub(#{type := peso, amount := Amount, commission := Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 3,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };
map_to_rub(#{type := krone, amount := Amount, commission := Commission}) when Amount > 0, is_integer(Amount), Commission =< 1, Commission >= 0 ->
    ConvAmount = Amount * 10,
    CommissionResult = ConvAmount * Commission,
    {ok, ConvAmount - CommissionResult };

map_to_rub(BadArgs) ->
    io:format("Can’t convert ~n~p~n", [BadArgs]),
    {error, badarg}.
```

Результаты работы функции:
```erlang
32> converter:map_to_rub(#{type => usd, amount => 100, commission => 0.01}).
{ok,7474.5}
33> converter:map_to_rub(#{type => peso, amount => 12, commission => 0.02}).
{ok,35.28}
34> converter:map_to_rub(#{type => yene, amount => 30, commission => 0.02}).
Can’t convert
#{type => yene,amount => 30,commission => 0.02}
{error,badarg}
37> converter:map_to_rub(#{type => euro, amount => -15, commission => 0.02}).
Can’t convert
#{type => euro,amount => -15,commission => 0.02}
{error,badarg}
```

## Часть 3
### 3.1 fac/1 и tail_fac/1

```erlang
fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N*fac(N-1).


tail_fac(N) when N >= 0, is_integer(N) ->  tail_fac(N, 1).
tail_fac(0, Acc) -> Acc;
tail_fac(N, Acc) -> 
    tail_fac(N - 1, Acc * N).
```

Результат работы функции:
```erlang
23> recursion:tail_fac(4).
24
24> recursion:tail_fac(0).
1
```

### 3.2 duplicate/1 и tail_duplicate/1
```erlang
duplicate([]) -> [];
duplicate([H|T]) -> [H, H | duplicate(T) ].

tail_duplicate(List) when is_list(List) -> tail_duplicate(List, []).
tail_duplicate([], Acc) -> lists:reverse(Acc);
tail_duplicate([H|T], Acc) -> tail_duplicate(T, [H, H|Acc]).
```

Результат выполнения функций:
```erlang
25> recursion:tail_duplicate([]).
[]
26> recursion:tail_duplicate([a, b, c, d]).
[a,a,b,b,c,c,d,d]
27> recursion:duplicate([]).
[]
28> recursion:duplicate([a, b, c, d]).
[a,a,b,b,c,c,d,d]
```