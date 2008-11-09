-module (zigbee).

-export ([start/0, start/1]).
-export ([init/2]).
-export ([at_command/2]).

start() ->
	start("/dev/tty.usbserial-A6005vO3").

start(File) ->
	spawn_link(?MODULE, init, [self(), File]).

init(Pid, File) ->
	process_flag(trap_exit, true),
	Serial = serial:start([{speed, 9600},{open, File}]),
	loop(Pid, Serial, <<>>).

loop(Pid, Serial, PendingData) ->
	receive
		{'EXIT', Pid, Reason} ->
			io:format("Client died!"),
			exit(Reason);
		{'EXIT', Serial, Reason} ->
			io:format("Serial port died!"),
			exit(Reason);
		{data, Bytes} ->
			io:format("Got data: ~w~n", [Bytes]),
			loop(Pid, Serial, interpret_packet(<<PendingData/binary, Bytes/binary>>));
		{send, Bytes} ->
			io:format("Sending Bytes: ~w~n", [Bytes]),
			Serial ! {send, Bytes},
			loop(Pid, Serial, PendingData);
		{at_command, AT} ->
			[A, T] = AT,
			write_packet(Serial, <<16#08,16#01, A, T>>),
			loop(Pid, Serial, PendingData);
		Other ->
			io:format("Got other message: ~w~n", [Other])
	end.

write_packet(Serial, Contents) ->
	Size = size(Contents),
	Checksum = calc_checksum(Contents),
	Serial ! {send, <<16#7e, Size:16, Contents/binary, Checksum:8>>}.

calc_checksum(Checksum, Rest) when is_binary(Rest), size(Rest) == 0 ->
	Checksum;
calc_checksum(<<Checksum:8>>, <<A:8/unsigned-little-integer,Rest/binary>>) ->
	Val = Checksum + A,
	calc_checksum(<<Val:8>>, Rest).

calc_checksum(Bin) ->
	<<Res:8>> = calc_checksum(<<0>>, Bin),
	16#ff - Res.

interpret_packet(Bin) when size(Bin) == 0 ->
	<<>>;
interpret_packet(<<16#7e, Length:16, Contents:Length/binary, Checksum:8, Rest/binary>>) ->
	case calc_checksum(Contents) of
		Checksum ->
			interpret_api_packet(Contents);
		Else ->
			io:format("Got Packet with invalid checksum! (~w != ~w)~n", [Else, Checksum])
	end,
	Rest;
interpret_packet(Unknown) ->
	Unknown.

interpret_api_packet(Contents) ->
	io:format("Got ~w packet: ~w~n", [api_packet_type(Contents), Contents]).

api_packet_type(<<16#8a, _Status:8>>) ->
	modem_status;
api_packet_type(<<16#88, FrameID:8, Command1:8, Command2:8, Status:8, Data/binary>>) ->
	{at_command_response, {frameid, FrameID}, {at_command, [Command1,Command2]}, {status, Status}, {data,Data}};
api_packet_type(<<16#97, _FrameID:8, _RemoteAddr:64, _NetAddr:16, _Command:16/binary, _Status:8, _Data/binary>>) ->
	remote_at_command_response;
api_packet_type(<<16#8b, _FrameID:8, _NetAddr:16, _RetryCount:8, _DeliveryStatus:8, _DiscoveryStatus:8>>) ->
	transmit_status;
api_packet_type(<<16#90, _Address:64, _NetAddr:16, _Options:8, _Data/binary>>) ->
	rx_packet;
api_packet_type(<<16#91, _SourceAddr:64, _NetAddr:16, _SourceEndpoint:8, _DestEndpoint:8, _ClusterID:16,
					_ProfileID:16, _Options:8, _Data/binary>>) ->
	explicit_rx;
api_packet_type(<<16#92, _Address:64, _NetAddr:16, _Options:8, _NumSamples:8, _DigitalChannelMask:16,
					_AnalogChannelMask:8, _DigitalSamples:16, _AnalogSamples/binary>>) ->
	sample_rx;
api_packet_type(<<16#95, _Rest/binary>>) ->
	node_id;
api_packet_type(<<PacketType:8, _Rest/binary>>) ->
	PacketType.

%%% External API
at_command(Pid, AT) when is_list(AT), length(AT) == 2 ->
	Pid ! {at_command, AT}.