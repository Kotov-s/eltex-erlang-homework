-module(keylist_test).

-include_lib("eunit/include/eunit.hrl").

-define(KEYNAME1, keylist1).
-define(KEYNAME2, keylist2).

calc_add_test_() ->
    {
      foreach, 
      fun setup/0, 
      fun teardown/1,
      [
        fun test_keylist/0,
        fun test_keylist_mgr/0
      ]
    }.

setup() ->
    keylist_mgr:start(),
    {ok, Pid1} = keylist_mgr:start_child(#{name => ?KEYNAME1, restart => permanent}),
    {ok, Pid2} = keylist_mgr:start_child(#{name => ?KEYNAME2, restart => temporary}),
    #{childs => [{?KEYNAME1, Pid1}, {?KEYNAME2, Pid2}]}.

teardown(State) ->
    keylist_mgr:stop(),
    State.

test_keylist() ->
    ?debugMsg("keylist module testing"),
    Key1 = 1,    
    Key2 = 2,
    Key3 = 3, 
    Non_existent_key = 4, 

    Value1 = "one",
    Value2 = "two",
    Value3 = "three",
    Comment = "number",

    ?assertEqual({ok, 1}, keylist:add(?KEYNAME1, Key1, Value1, Comment)),
    ?assertEqual({ok, 2}, keylist:add(?KEYNAME1, Key2, Value2, Comment)),
    ?assertEqual({ok, 3}, keylist:add(?KEYNAME1, Key3, Value3, Comment)),

    ?assertEqual({ok, true, 4}, keylist:is_member(?KEYNAME1, Key3)),
    ?assertEqual({ok, false, 5}, keylist:is_member(?KEYNAME1, Non_existent_key)),
    
    ?assertEqual({ok, {Key3,Value3,Comment}, 6}, keylist:take(?KEYNAME1, Key3)),
    ?assertEqual({ok, not_found, 7}, keylist:take(?KEYNAME1, Non_existent_key)),

    ?assertEqual({ok, {Key2,Value2,Comment}, 8}, keylist:find(?KEYNAME1, Key2)),
    ?assertEqual({ok, not_found, 9}, keylist:find(?KEYNAME1, Non_existent_key)), 
    
    ?assertEqual({ok, [{Key1,Value1,Comment}], 10}, keylist:delete(?KEYNAME1, Key2)).

test_keylist_mgr() ->
    ?debugMsg("keylist_mgr module testing"),
    exit(whereis(?KEYNAME1), kill),
    %% Delay to wait for the process to restart and update the list
    timer:sleep(10),
    exit(whereis(?KEYNAME2), kill),
    timer:sleep(10),
    ?assertEqual(undefined, whereis(?KEYNAME2)),
    ?assertMatch([{?KEYNAME1, _Pid1}], keylist_mgr:get_names()).
    