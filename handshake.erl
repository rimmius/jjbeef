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
	    case gen_tcp:connect(Host, Port, [binary, {packet, 0}], 5000) of
		{ok, Sock} ->
		    io:format(list_to_binary(Peer_id)),
		    %Msg = list_to_binary([<<19>>,<<"BitTorrent Protocol">>, <<32132123:64>>, <<Info:160>>,binary_to_list(list_to_binary(Peer_id))]),
		    %ok = gen_tcp:send(Sock, Msg),
		    From ! {reply, ok},
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
	{reply, ok} ->
	    ok;
	{error, _} ->
	    error
    end.
