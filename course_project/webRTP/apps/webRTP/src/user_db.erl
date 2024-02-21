%% @doc This module is responsible for interacting with the database

-module(user_db).

-export([create_and_start/0, add_value/2, delete_value/1, show_value/1, show_values/0]).

-record(abonents, 
                {num,   % Number of abonent
                name}). % Name of abonent

%%% API  

%% @doc Creates mnesia schema, abonent table (if don't exists) and start mnesia
-spec create_and_start() -> {ok, already_exists | created_and_started}.
create_and_start() ->
    mnesia:start(),
    case mnesia:create_table(abonents, [{disc_only_copies, [node()]}, {attributes, record_info(fields, abonents)}]) of
        {aborted,{already_exists,abonents}} -> {ok, already_exists};
        _ -> 
            mnesia:stop(), 
            mnesia:create_schema([node()]),
            mnesia:start(),
            mnesia:create_table(abonents, [{disc_only_copies, [node()]}, {attributes, record_info(fields, abonents)}]),
            fill_db_if_json_exists(),
            {ok, created_and_started}
    end.
    

%% @doc Adds given data to abonents table
-spec add_value(Num::integer(), Name::string()) -> {error, already_exists} | ok | aborted.
add_value(Num, Name) ->
    T_res =
        mnesia:transaction(
            fun() ->
                case mnesia:read({abonents, Num}) of
                    [] -> mnesia:write(#abonents{num = Num, name = Name});
                    _ -> {error, already_exists}
                end
            end
        ),
    t_res_to_res(T_res).

%% @doc Deletes data from abonents table by given abonent num
-spec delete_value(Num::integer()) -> {error, not_found} | ok | aborted.
delete_value(Num) ->
    T_res = 
        mnesia:transaction(
            fun() ->
                case mnesia:read({abonents, Num}) of
                    [#abonents{num = Num, name = _Name}] -> mnesia:delete({abonents, Num});
                    _ -> {error, not_found}
                end
            end
        ),
    t_res_to_res(T_res).

%% @doc Shows abonent's table data by given abonent num
-spec show_value(Num::integer()) -> [tuple()] | [] | aborted.
show_value(Num) ->
    T_res = 
        mnesia:transaction(
            fun() ->
                mnesia:read({abonents, Num})
            end
        ),
    t_res_to_res(T_res).

%% @doc Shows all abonent's table data
-spec show_values() -> [tuple()] | [] | aborted.
show_values() ->
    T_res = 
        mnesia:transaction(
            fun() ->
                mnesia:match_object(#abonents{_ = '_'})
            end
        ),
    t_res_to_res(T_res).

%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%% 

%% @hidden
fill_db_if_json_exists() ->
    PathToJson = "priv/db/abonents.json",
    case filelib:is_file(PathToJson) of
        true -> 
            {ok, JsonData} = file:read_file(PathToJson),
            ParsedData = jsone:decode(JsonData),
            lists:foreach(fun(#{<<"name">> := BinName, <<"num">> := Number}) -> add_value(Number, binary_to_list(BinName)) end, ParsedData);
        false ->
            logger:warning("There is no '~p' file to fill database", [PathToJson])
    end.

%% @hidden
t_res_to_res(T_res) ->
    case T_res of
        {'atomic', Res} -> Res;
        {'aborted', _Reason} -> aborted
    end.