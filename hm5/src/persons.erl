-module(persons).

-export([my_get_average_age/1, my_filter/2, my_all/2, my_any/2, my_update/2]).
-export([filter/2, all/2, any/2, update/2, get_average_age/1]).

-export([is_female/1, is_male/1]).

-include("person.hrl").


is_female(#person{gender = Gender}) -> Gender =:= ?FEMALE.
is_male(#person{gender = Gender}) -> Gender =:= ?MALE.


%% Функция фильтрации
filter(Fun, Persons) -> lists:filter(Fun, Persons).

my_filter(Fun, Persons) -> my_filter(Fun, Persons, []).

my_filter(_Fun, [], Acc) -> lists:reverse(Acc);
my_filter(Fun, [Person|Persons], Acc) -> 
    case Fun(Person) of 
        true -> my_filter(Fun, Persons, [Person|Acc]);
        false -> my_filter(Fun, Persons, Acc)
    end.


%% Функцию проверки, что все персоны подходят под условие (lists:all/2)
all(Fun, Persons) -> lists:all(Fun, Persons).

my_all(_Fun, []) -> true;
my_all(Fun, [Person|Persons]) -> 
    case Fun(Person) of
        true -> my_all(Fun, Persons);
        false -> false
    end.


%% Функцию проверки, что хотя бы одна персона подходит под условие (lists:any/2)
any(Fun, Persons) -> lists:any(Fun, Persons).

my_any(_Fun, []) -> false;
my_any(Fun, [Person|Persons]) -> 
    case Fun(Person) of
        false -> my_any(Fun, Persons);
        true -> true
    end.


%% Функция обновления данных персон
update(Fun, Persons) -> lists:map(Fun, Persons).

my_update(Fun, Persons) -> my_update(Fun, Persons, []).

my_update(_Fun, [], Acc) -> lists:reverse(Acc);
my_update(Fun, [Person|Persons], Acc) -> my_update(Fun, Persons, [Fun(Person)|Acc]).


%% Функция подсчета среднего возраста персон из списка
my_get_average_age([]) -> {error, badarg};
my_get_average_age(Persons) when is_list(Persons) -> my_get_average_age(Persons, {0, 0}).

my_get_average_age([], {Age, Persons}) when Persons /= 0 -> Age / Persons;
my_get_average_age([#person{age = Age}|Persons], {AgeCount, PersonsCount}) ->
    my_get_average_age(Persons, {AgeCount + Age, PersonsCount + 1});

my_get_average_age(_, _) -> {error, badarg}.

get_average_age([]) -> {error, badarg};
get_average_age(Persons) when is_list(Persons) ->
    {AgeCount, PersonCount} = lists:foldl(fun(#person{age = Age}, {AgeCount, PersonCount}) -> {AgeCount + Age, PersonCount + 1} end, {0, 0}, Persons),
    case PersonCount of
        0 -> {error, badarg};
        _ -> AgeCount/PersonCount
    end.
