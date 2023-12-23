-module(keylist_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    io:format("Starting supervisor~n"),

    SupervisorSpecification = #{
        strategy => simple_one_for_one
    },

    ChildSpecifications = [
        #{
            id => keylist,
            start => {keylist, start_link, []},
            restart => temporary
        }
    ],

    {ok, {SupervisorSpecification, ChildSpecifications}}.
