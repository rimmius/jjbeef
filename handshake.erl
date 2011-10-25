-module(handshake).
-export([start/0, loop/0, send_handshake/5]).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
	{handshake, From, Pstrlen, Pstr, Reserved, Info_hash, Peer_id} ->
	    case {is_info_hash(Info_hash), is_valid_peer_id(Peer_id)} of
		{true, true} ->
		    From ! {reply, ok};
		{_, _} ->
		    From ! {reply, drop_connection}
	    end,
	    loop();
	{send_handshake, From, {Host, Port, Info, Peer_id}} ->
	    case gen_tcp:connect(Host, Port, [binary, {packet, 0}], 1000) of
		{ok, Sock} ->
		    Info_hashed = list_to_binary(sha:sha1hash(Info)),
		    Msg = list_to_binary([<<19>>,<<"BitTorrent Protocol">>, <<32132123>>, Info_hashed,<<"12345678912345678911">>]),
		    io:format("bababa~n"),
		    ok = gen_tcp:send(Sock, Msg),
		    From ! {reply, ok, Sock},
		    loop();
		{error, Reason} ->
		    io:format(Reason),
		    From ! {error, Reason},
		    loop()
	    end
    end.
is_info_hash(Info_hash) ->
    true.
is_valid_peer_id(Peer_id) ->
    true.

send_handshake(Host, Port, Info, Peer_id, Pid) ->
    Pid ! {send_handshake, self(), {Host, Port, Info, Peer_id}},
    receive
	{reply, ok, Sock} ->
	    Sock;
	{error, _} ->
	    error
    end.
