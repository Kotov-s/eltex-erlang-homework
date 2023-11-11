-module(keylist_mgr).

-export([loop/1, init/1, start/0]).

-record(state, {children = []}).

init(Name) ->
    process_flag(trap_exit, true),
    register(Name, self()),
    loop(#state{}).

start() ->
    case whereis(keylist_mgr) of
        undefined -> 
            {Pid, MonitorRef} = spawn_monitor(?MODULE, init, [?MODULE]),
            {ok, Pid, MonitorRef};
        _ ->
            {already_exists}
    end.


loop(#state{children = Childrens} = State) ->
    receive
        {From, start_child, Name} ->
            Res = proplists:lookup(Name, Childrens),
            case Res of
                none ->
                    {ok, Pid} = keylist:start_link(Name),
                    NewState = State#state{children = [{Name, Pid} | Childrens]},
                    From ! {ok, Pid},
                    loop(NewState);
                _ ->
                    From ! {already_exists},
                    loop(State)
            end;
        {From, stop_child, Name} ->
            Res = proplists:lookup(Name, Childrens),
            case Res of
                none ->
                    From ! {not_found},
                    loop(State);                
                _ ->
                    Name ! {stop, Name},
                    NewState = State#state{children = proplists:delete(Name, Childrens)},
                    From ! {ok, stop_child},
                    loop(NewState)

            end;
        {From, get_names} ->
            From ! Childrens,
            loop(State);
        stop ->
            exit(stop_proc);
        {'EXIT', Pid, Reason} -> 
            case lists:keyfind(Pid, 2, Childrens) of
                false -> 
                    io:format("Received an exit signal from an unknown process with pid = ~p and reason = ~p~n", [Pid, Reason]),
                    loop(State);
                {Name, Pid} ->
                    io:format("Process with name = ~p, pid = ~p exited with reason ~p~n", [Name, Pid, Reason]),
                    NewState = State#state{children = proplists:delete(Name, Childrens)},
                    loop(NewState)
            end            
    end.
