-module(user_db).

-export([create_and_start/0, add_value/2, delete_value/1, show_value/1, show_values/0]).
-record(abonents, 
                {num,   % Num of abonent
                name}). % Name of abonent

%%% API  

%% @doc Creates mnesia schema, abonent table and start mnesia
-spec create_and_start() -> mnesia:t_result(ok).
create_and_start() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(abonents, [{attributes, record_info(fields, abonents)}]).

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
-spec show_value(Num::integer()) -> [tuple()] | []  | aborted.
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
t_res_to_res(T_res) ->
    case T_res of
        {'atomic', Res} -> Res;
        {'aborted', _Reason} -> aborted
    end.