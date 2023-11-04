-module(protocol).

-export([ipv4/1, ipv4_listener/0]).

-record(ipv4, {
	version,
	ihl,
	tos,
	total_length,
	identification,
	flags,
	frag_offset,
	time_to_live,
	protocol,
	checksum,
	source_address,
	destination_address,
	options_and_padding,
	remaining_data
}).

ipv4(<<Version:4, _Rest/bitstring >>) when Version =/= 4 ->
    throw("Incorrect version of ip");
ipv4(<<Version:4, IHL:4, ToS:8, TotalLength:16,
 	Identification:16, Flags:3, FragOffset:13,
 	TimeToLive:8, Protocol:8, Checksum:16,
 	SourceAddress:32, DestinationAddress:32,
 	OptionsAndPadding:((IHL-5)*32)/bits,
 	RemainingData/bytes >>
) when Version =:= 4 ->
	io:format("Received data ~p ~n", [RemainingData]),
	#ipv4{
		version = Version,
        ihl = IHL,
        tos = ToS,
        total_length = TotalLength,
        identification = Identification,
        flags = Flags,
        frag_offset = FragOffset,
        time_to_live = TimeToLive,
        protocol = Protocol,
        checksum = Checksum,
        source_address = SourceAddress,
        destination_address = DestinationAddress,
        options_and_padding = OptionsAndPadding,
		remaining_data = RemainingData
    };
ipv4(_) ->
    throw("Incorrect argument").

ipv4_listener() ->
    receive 
    {ipv4, From, BinData} when is_bitstring(BinData) ->
        try ipv4(BinData) of
            Result -> From ! Result
        catch
            throw:Error -> 
			From ! {error, Error},
            		io:format("Error: ~p~n", [Error])
        end,
        ipv4_listener();
    Msg ->
        io:format("Received wrong format ~p~n", [Msg]),
        ipv4_listener()
    end.
