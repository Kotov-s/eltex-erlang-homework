-module(door_test).

-include_lib("eunit/include/eunit.hrl").

calc_add_test_() ->
    {
      foreach, 
      fun setup/0, 
      fun teardown/1,
      [
        fun test_open_state/1,
        fun test_set_new_code/1,
        fun test_suspended/1,
        fun test_open_timeout/1
      ]
    }.

setup() ->
    {ok, Pid} = door:start_link([1, 2, 3, 4]),
    Pid.

teardown(Pid) ->
    door:stop(Pid),
    Pid.

test_open_state(Pid) ->
    [ 
        fun() ->
            ?debugMsg("Testing locked state..."),
            Code1 = 1,    
            Code2 = 2,
            Code3 = 3, 
            Code4 = 4, 

            ?assertEqual({error, bad_command}, door:set_new_code(Pid, Code1)),

            ?assertEqual({ok,next}, door:enter(Pid, Code1)),
            ?assertEqual({ok,next}, door:enter(Pid, Code2)),
            ?assertEqual({ok,next}, door:enter(Pid, Code3)),
            ?assertEqual({ok,open}, door:enter(Pid, Code4))
        end
    ].

test_set_new_code(Pid) ->
    [
        fun() ->
            ?debugMsg("Testing open state..."),
            Code1 = 1,    
            Code2 = 2,
            Code3 = 3, 
            Code4 = 4, 
            
            %% locked state
            ?assertEqual({ok, next}, door:enter(Pid, Code1)),
            ?assertEqual({ok, next}, door:enter(Pid, Code2)),
            ?assertEqual({ok, next}, door:enter(Pid, Code3)),
            ?assertEqual({ok, open}, door:enter(Pid, Code4)),
            
            %% open state
            ?assertEqual({error, bad_command}, door:enter(Pid, Code4)),
            ?assertEqual({ok, next}, door:set_new_code(Pid, Code4)),
            ?assertEqual({ok, next}, door:set_new_code(Pid, Code4)),
            ?assertEqual({ok, locked}, door:approve_code(Pid)),

            %% locked state
            ?assertEqual({ok, next}, door:enter(Pid, Code4)),
            ?assertEqual({ok, open}, door:enter(Pid, Code4))
        end
    ].

test_suspended(Pid) ->
    {   
        %% Setting a timeout for the test is necessary because we need to wait more than default 5s due to the suspension timeout being 10s
        timeout, 60,
        fun() ->
            ?debugMsg("Testing suspended state..."),
            Code = 1,    
            
            %% locked state (1st try)
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({error, wrong_code}, door:enter(Pid, Code)),
            %% (2st try)
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({error, wrong_code}, door:enter(Pid, Code)),
            %% (3rd try)
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({ok,next}, door:enter(Pid, Code)),
            ?assertEqual({error, wrong_code}, door:enter(Pid, Code)), 
            %% (4th try)
            ?assertEqual(suspended, door:enter(Pid, Code)),

            %% suspended state
            ?assertEqual({error, bad_command}, door:enter(Pid, Code)),
            %% The state is expected to return to locked in 10s
            timer:sleep(10000),
            ?assertEqual({ok,next}, door:enter(Pid, Code))
        end
    }.

test_open_timeout(Pid) ->
    {   
        timeout, 60,
        fun() ->
            ?debugMsg("Testing open state timeout..."),
            Code1 = 1,    
            Code2 = 2,
            Code3 = 3, 
            Code4 = 4, 
            
            %% locked state
            ?assertEqual({ok, next}, door:enter(Pid, Code1)),
            ?assertEqual({ok, next}, door:enter(Pid, Code2)),
            ?assertEqual({ok, next}, door:enter(Pid, Code3)),
            ?assertEqual({ok, open}, door:enter(Pid, Code4)),
            
            %% open state
            timer:sleep(10000),

            %% locked state
            ?assertEqual({error, bad_command}, door:set_new_code(Pid, Code4))
        end
    }.