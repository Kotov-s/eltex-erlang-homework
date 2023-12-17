-module(door).

-behaviour(gen_statem).

-export([stop/1, start_link/1, enter/2, set_new_code/2, approve_code/1]).
-export([init/1, callback_mode/0, locked/3, open/3, suspended/3]).

-record(door, {
            code            :: list(), 
            entered = []    :: list(), 
            attempts = 0    :: integer()
        }).

-type bad_command() :: {error, bad_command}.

%% API

%% @doc Stops the process.
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_statem:stop(Pid).

%% @doc Starts and links the gen_statem process with the given code.
-spec start_link(list(integer())) -> gen_statem:start_ret().
start_link(Code) when length(Code) /= 0->
    gen_statem:start_link(?MODULE, Code, []).

%% @doc Is used to input a single digit of the code..
%% After all digits of the code have been entered, the state of the changes to open. 
%% However, if not all digits of the code have been entered or if the entered code is incorrect, the state remains locked.
%% @end
-spec enter(pid(), integer()) -> suspended | {ok, next} | {error, wrong_code} | {ok, open} | bad_command().
enter(Pid, Code) ->
    gen_statem:call(Pid, {enter, Code}).

%% @doc This function is used to set a new code. 
%% It takes a single digit of the new code to be set.
%% After the first digit of the new code is entered, the state timeout is reset. 
%% The open state will remain open until approved using the door:approve_code/1 function.
%% @end
-spec set_new_code(pid(), integer()) -> {ok, next} | bad_command().
set_new_code(Pid, NewCode) ->
    gen_statem:call(Pid, {set_new_code, NewCode}).

%% @doc Approves the new code.
-spec approve_code(pid()) -> {ok, locked} | bad_command().
approve_code(Pid) ->
    gen_statem:call(Pid, approve_code).

%% Callbacks

init(Code) ->
    process_flag(trap_exit, true),
    {ok, locked, #door{code = Code}}.

callback_mode() -> 
    state_functions.

locked({call, From}, {enter, Num}, DoorData = #door{code = Code, entered = Entered, attempts = Attempts}) ->
    EnteredCode = [Num | Entered],
    case Attempts =:= 3 of
        false ->
            case length(EnteredCode) =:= length(Code) of
                true -> 
                    case lists:reverse(EnteredCode) =:= Code of
                        true -> {next_state, open, DoorData#door{entered = [], attempts = 0}, [{reply, From, {ok, open}}, {state_timeout, 10000, open_state_timeout}]};
                        false -> {keep_state, DoorData#door{entered = [], attempts = Attempts + 1}, {reply, From, {error, wrong_code}}}
                    end;
                false ->
                    {keep_state,  DoorData#door{entered = EnteredCode}, {reply, From, {ok, next}}}    
            end;
        true -> {next_state, suspended, DoorData#door{entered = [], attempts = 0}, [{reply, From, suspended}, {state_timeout, 10000, suspended_state_timeout}]}
    end;
    
locked({call, From}, _Msg, _DoorData) ->
    io:format("An incorrect command has been entered for locked state.~n"),
    {keep_state_and_data, {reply, From, {error, bad_command}}}. 


open({call, From}, {set_new_code, NewCode}, #door{entered = Entered} = DoorData) ->
    {keep_state, DoorData#door{entered = [NewCode | Entered]}, [{reply, From, {ok, next}}, {state_timeout, infinity, open_door_timeout}]};

open({call, From}, approve_code, #door{entered = Entered} = DoorData) when length(Entered) /= 0->
    io:format("A new code ~p has been set~n", [lists:reverse(Entered)]),
    {next_state, locked, DoorData#door{code = lists:reverse(Entered), entered = []}, {reply, From, {ok, locked}}};

open({call, From}, _Msg, _DoorData) ->
    io:format("An incorrect command has been entered for open state.~n"),
    {keep_state_and_data, {reply, From, {error, bad_command}}};

open(state_timeout, open_state_timeout, DoorData) ->
    io:format("Timeout~n"),
    {next_state, locked, DoorData}.


suspended({call, From}, _Msg, _DoorData) ->
    io:format("You can't do anything since you tried to enter code too many times~n"),
    {keep_state_and_data, {reply, From, {error, bad_command}}};

suspended(state_timeout, suspended_state_timeout, DoorData) ->
    io:format("Try again~n"),
    {next_state, locked, DoorData}.
