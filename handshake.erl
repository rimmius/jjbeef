-module(handshake).
-export([start/0, loop/0]).

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
	    end
    end.
is_info_hash(Info_hash) ->
    true.
is_valid_peer_id(Peer_id) ->
    true.

