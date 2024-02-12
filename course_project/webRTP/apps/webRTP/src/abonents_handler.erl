-module(abonents_handler).

-export([init/2]).

init(Req, State) ->
    #{method := Method} = Req,     
    {ok, handle_request(Method, Req), State}.

%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%% 

handle_request(<<"GET">>, Req) ->    
    Res = sip_calls_api:call_all(user_db:show_values()), 
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, io_lib:format("~p", [Res]), Req);

handle_request(_Other, Req) ->  
    cowboy_req:reply(405, Req).