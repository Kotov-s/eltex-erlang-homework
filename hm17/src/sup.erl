-module(sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
      strategy => one_for_all
    },

    ChildSpecs =
        [#{id => keylist_mgr,
           start => {keylist_mgr, start, []},
           restart => permanent,
           type => worker},
         #{id => keylist_sup,
           start => {keylist_sup, start_link, []},
           restart => permanent,
           type => supervisor}],
           
    {ok, {SupFlags, ChildSpecs}}.
