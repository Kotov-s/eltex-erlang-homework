%% @doc This module implements functionality for making calls using the SIP protocol

-module(sip_calls_api).

-export([call/1, call_all/1, start/0]).

-define(PASSWORD, "1234"). 

-type call_answer() :: {integer(), {ok, voice_sent} | {error, term()} | {wrong_get_meta_data, term()}}.

%% @doc Starts nksip service
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    nksip:start_link(nksip_curse, #{ 
        sip_local_host  => "localhost", 
        sip_from => "sip:101@test.group", 
        plugins => [nksip_uac_auto_auth], 
        sip_listen => "sip:all:5060"
    }).        

%% @doc Function that initiates a call with specified UserNumber  
-spec call(integer()) -> call_answer().
call(UserNumber) ->
    nksip_uac:register(nksip_curse, "<sip:"++ integer_to_list(UserNumber) ++"@10.0.20.11:5060;transport=tcp>", 
        [{sip_pass, ?PASSWORD}, contact, {get_meta, [<<"contact">>]}]),        
    Medias = [{<<"audio">>, 1080, [{rtpmap, 0, <<"PCMU/8000">>}, <<"sendrecv">>]}],
    InviteResult = nksip_uac:invite(nksip_curse, "<sip:"++ integer_to_list(UserNumber) ++"@10.0.20.11:5060;transport=tcp>",
        [{sip_pass, ?PASSWORD}, {body, nksip_sdp:new("auto.nksip", Medias)}, auto_2xx_ack]),
    FinalResult = 
        case InviteResult of
            {ok, 200, [{dialog, DialogID}]} ->
                Res = nksip_dialog:get_meta(invite_remote_sdp, DialogID),
                case get_address_and_port(Res) of
                    {PBX_IP, Port} ->
                        VoicePath = voice_generation(UserNumber),
                        StartVoice = "./voice_client " ++ VoicePath ++ " " ++ PBX_IP ++ " " ++ erlang:integer_to_list(Port),
                        os:cmd(StartVoice),
                        {ok, voice_sent};
                    Other ->
                        logger:warning("Failed to get address and port, because the value of Res (~p) does not match the expected format: {ok, Info}", [Res]), 
                        Other
                end;
            _ ->
                logger:warning("Unexpected invite result: ~p~n", [InviteResult]), 
                InviteResult  
        end,
    {UserNumber, FinalResult}.

% Function that initiates calls to all subscribers in the list.
-spec call_all([{atom(), integer(), list()}]) -> [call_answer()] | [].
call_all(List) ->
    lists:foldl(fun({_RecordName, UserNumber, _Username}, ResList) -> [call(UserNumber) | ResList]  end, [], List).

%%%%%%%%%%%%%%%%%%%% PRIVATE FUNCTIONS %%%%%%%%%%%%%%%%%%%%

%% @hidden
voice_generation(UserNumber) ->
    [{abonents, UserNumber, Username}] = user_db:show_value(UserNumber),
    CreateVoiceFile = "gtts-cli 'Hello, " ++ Username ++ "' --output priv/voice/generate.wav" ,
    ConvertVoice = "ffmpeg -i priv/voice/generate.wav -codec:a pcm_mulaw -ar 8000 -ac 1 -af \"volume=20dB\" priv/voice/output.wav -y",
    Cmd = CreateVoiceFile ++ " && " ++ ConvertVoice,
    os:cmd(Cmd),
    "priv/voice/output.wav".

%% @hidden
get_address_and_port({ok, Info}) ->
    [PortAndAddressInfo] = element(tuple_size(Info), Info),
    Port = element(3, PortAndAddressInfo),
    {_, _, BinIP} = element(8, PortAndAddressInfo),
    PBX_IP = binary:bin_to_list(BinIP),
    {PBX_IP, Port};
    
get_address_and_port({error, Error}) ->
    {wrong_get_meta_data, Error}.    