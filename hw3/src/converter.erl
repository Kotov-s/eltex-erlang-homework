%%% Конвертер валют

-module(converter).

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

%% Вывод ошибки
to_rub(_) ->
    io:format("Bad arguments~n"),
    {error, badarg}.