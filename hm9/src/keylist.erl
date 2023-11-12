%%% @author Andrey Kotov
%%% @since 12.11.2023
%%% @doc This module manages keylists.
-module(keylist).

-export([loop/1, start_monitor/1, start_link/1]).
-export([init/1]).
-export([add/4, is_member/2, take/2, find/2, delete/2, show_list/1, stop/1]).

-record(state, {
    list = []   :: list(#state{}) | [], 
    counter = 0 :: integer()
}).


-spec add(atom(), any(), any(), any()) -> no_return().
%% @doc Adds a key-value pair to the keylist.
add(Name, Key, Value, Comment) ->
    Name ! {self(), add, Key, Value, Comment}.

-spec is_member(atom(), any()) -> no_return().  
%% @doc Checks if a key is a member of the keylist.  
is_member(Name, Key) ->
    Name ! {self(), is_member, Key}. 

-spec take(atom(), any()) -> no_return(). 
%% @doc Takes a key-value pair from the keylist.
take(Name, Key) ->
    Name ! {self(), take, Key}. 

-spec find(atom(), any()) -> no_return().
%% @doc Finds a key-value pair in the keylist. 
find(Name, Key) ->
    Name ! {self(), find, Key}. 

-spec delete(atom(), any()) -> no_return(). 
%% @doc Deletes a key-value pair from the keylist.
delete(Name, Key) ->
    Name ! {self(), delete, Key}. 

-spec show_list(atom()) -> no_return(). 
%% @doc Shows the list of key-value pairs in the keylist.
show_list(Name) ->
    Name ! {self(), show_list}. 

-spec stop(atom()) -> no_return(). 
%% @doc Stops the keylist.
stop(Name) ->
    Name ! {stop, Name}. 

%% @doc The main loop of the keylist.
%% This function receives and handles messages to add, check, take, find, and delete key-value pairs, show the list of all key-value pairs, and stop the keylist.
%% @end
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

-spec start_monitor(atom()) -> {ok, pid(), reference()}.
%% @doc Starts a monitor for the keylist.
start_monitor(Name) ->
    {Pid, MonitorRef} = spawn_monitor(?MODULE, init, [Name]),
    {ok, Pid, MonitorRef}.

-spec start_link(atom()) -> {ok, pid()}.
%% @doc Starts a link for the keylist.
start_link(Name) ->
    Pid = spawn_link(?MODULE, init, [Name]),
    {ok, Pid}.

%% @doc Initializes the keylist.
%% This function registers the process with the given name and starts the loop.
init(Name) ->
    register(Name, self()),
    loop(#state{}).
