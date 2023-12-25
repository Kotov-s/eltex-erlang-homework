%%%-------------------------------------------------------------------
%% @doc hw18 public API
%% @end
%%%-------------------------------------------------------------------

-module(hw18_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/abonent/:abonent_num", abonent_handler, []},
            {"/abonent/", abonent_handler, []},
            {"/abonents/", abonents_handler, []}
        ]
        }]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8011}],
        #{env => #{dispatch => Dispatch}}
    ),
    user_db:create_and_start(),
    hw18_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
