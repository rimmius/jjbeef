-module(connect_and_handshake).
-export([start/5]).

start(Host, Port, Info, Peer_id, From) ->
    case gen_tcp:connect(Host, Port, [binary, {active, false},
				      {packet, 0}], 1000) of
	{ok, Sock} ->
	    Msg = list_to_binary([<<19>>,<<"BitTorrent protocol">>,
				  <<3,2,1,3,2,1,2,3>>,Info,
				  list_to_binary(Peer_id)]),
	    ok = gen_tcp:send(Sock, Msg),
	    io:format("Sent handshake to peer from tracker~n"),
	    From ! {reply, ok, Sock};
	{error, Reason} ->
	    io:format(Reason),
	    From ! {error, Reason}
    end.
