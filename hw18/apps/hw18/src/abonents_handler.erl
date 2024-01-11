-module(abonents_handler).

-export([init/2]).
init(Req, State) ->
    Res = user_db:show_values(),
    Resp = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, io_lib:format("~p", [Res]), Req),
    {ok, Resp, State}.