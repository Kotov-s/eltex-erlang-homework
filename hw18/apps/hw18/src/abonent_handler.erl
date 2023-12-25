-module(abonent_handler).

-export([init/2]).

init(Req, State) ->
    #{method := Method} = Req,
    Resp = 
        case Method of
            <<"GET">> -> 
                case cowboy_req:binding(abonent_num, Req, undefined)  of
                    undefined ->  
                        cowboy_req:reply(404, Req);
                    Number ->            
                        Res = user_db:show_value(binary_to_integer(Number)),
                        cowboy_req:reply(200, #{
                            <<"content-type">> => <<"text/plain">>
                        }, io_lib:format("~p", [Res]), Req)
                end;
            <<"DELETE">> -> 
                case cowboy_req:binding(abonent_num, Req, undefined)  of
                    undefined ->  
                        cowboy_req:reply(404, Req);   
                    Number ->    
                        Res = user_db:delete_value(binary_to_integer(Number)),
                        cowboy_req:reply(200, #{
                            <<"content-type">> => <<"text/plain">>
                        }, io_lib:format("~p", [Res]), Req)
                end;
            <<"POST">> ->
                {ok, Body, _Req2} = cowboy_req:read_body(Req),
                case parse_body(Body) of
                    {error, wrong_data} -> cowboy_req:reply(400, Req); 
                    {Num, Name} -> 
                        Res = user_db:add_value(Num, Name),
                        cowboy_req:reply(201, #{
                            <<"content-type">> => <<"text/plain">>
                        }, io_lib:format("~p", [Res]), Req)
                end
        end,
        {ok, Resp, State}.

%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%% 

%% @hidden
parse_body(Body) ->
    case cow_qs:parse_qs(Body) of
        [{<<"name">>, BinaryName}, {<<"num">>, BinaryNum}] ->
            {binary_to_integer(BinaryNum), binary_to_list(BinaryName)};
        _ ->
            {error, wrong_data}     
    end.      