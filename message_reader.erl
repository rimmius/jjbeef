-module(message_reader).
-export([start/2]).
-export([loop/2]).

start(Peer_mutex_pid, Peer_id) ->
    spawn(?MODULE, loop, [Peer_mutex_pid, Peer_id]).

loop(Peer_mutex_pid, Peer_id) ->
    receive
	{bitfield, Bitfield, Bitfield_len} ->
	    %%io:format("~nBitfiled: ~w~n", [lol(<<Bitfield:Bitfield_len>>, 0)]),
	    mutex:request(Peer_mutex_pid, insert_bitfield, [Peer_id, lol(<<Bitfield:Bitfield_len>>, 0)]),
	    mutex:received(Peer_mutex_pid),
	    loop(Peer_mutex_pid, Peer_id);
	{have, _Piece_index} ->
%%	    mutex:request(Peer_mutex_pid, update_bitfield, [Peer_id, Piece_index]),
%%	    mutex:received(Peer_mutex_pid),
	    loop(Peer_mutex_pid, Peer_id);
	{choked, 1} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, choke, 1]),
	    mutex:received(Peer_mutex_pid),
	    loop(Peer_mutex_pid, Peer_id);
	{choked, 0} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, choke, 0]),
	    mutex:received(Peer_mutex_pid),
	    loop(Peer_mutex_pid, Peer_id);
	{interested, 1} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, interested, 1]),
	    mutex:received(Peer_mutex_pid),
	    loop(Peer_mutex_pid, Peer_id);
	{interested, 0} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, interested, 0]),
	    mutex:received(Peer_mutex_pid),
	    loop(Peer_mutex_pid, Peer_id);
	{port, Listen_port} ->
	    mutex:update_pee(Peer_mutex_pid, update_peer, [Peer_id, port, Listen_port]),
	    mutex:received(Peer_mutex_pid),
	    loop(Peer_mutex_pid, Peer_id)
    end.

lol(<<P:1>>, Index) ->
    [{P, Index}];
lol(<<P:1, Rest/bits>>, Index) ->
    [{P, Index} | lol(Rest, Index+1)].
     
