-module(keylist).

-export([loop/1, start_monitor/1, start_link/1]).
-export([init/1]).

-record(state, {list = [], counter = 0}).

loop(#state{list = List, counter = Counter} = State) ->
    receive
        {From, add, Key, Value, Comment} ->
            NewState = State#state{list = [{Key, Value, Comment} | List], counter = Counter + 1},
            From ! {ok, NewState#state.counter},
            loop(NewState);
        {From, is_member, Key} ->
            Res = lists:keymember(Key, 1, List),
            NewState = State#state{counter = Counter + 1},
            From ! {ok, Res, NewState#state.counter},
            loop(NewState);
        {From, take, Key} ->
            Res = lists:keytake(Key, 1, List),
            case Res of
                false ->
                    NewState = State#state{counter = Counter + 1},
                    From ! {ok, not_found, NewState#state.counter},
                    loop(NewState);
                _ ->
                    {_, Found, NewList} = Res,
                    NewState = State#state{list = NewList, counter = Counter + 1},
                    From ! {ok, Found, NewState#state.counter},
                    loop(NewState)
            end;
        {From, find, Key} ->
            Res = lists:keyfind(Key, 1, List),
            NewState = State#state{counter = Counter + 1},
            case Res of
                false ->
                    From ! {ok, not_found, NewState#state.counter},
                    loop(NewState);
                _ ->
                    From ! {ok, Res, NewState#state.counter},
                    loop(NewState)
            end;
        {From, delete, Key} ->
            Res = lists:keydelete(Key, 1, List),
            NewState = State#state{counter = Counter + 1, list = Res},
            From ! {ok, Res, NewState#state.counter},
            loop(NewState);
        {From, show_list} ->
            NewState = State#state{counter = Counter + 1},
            From ! {ok, List, NewState#state.counter},
            loop(NewState);
        {stop, Name} ->
            exit(stop_proc),
            {ok, Name, stopped}
    end.

start_monitor(Name) ->
    {Pid, MonitorRef} = spawn_monitor(?MODULE, init, [Name]),
    {ok, Pid, MonitorRef}.

start_link(Name) ->
    Pid = spawn_link(?MODULE, init, [Name]),
    {ok, Pid}.

init(Name) ->
    register(Name, self()),
    loop(#state{}).
