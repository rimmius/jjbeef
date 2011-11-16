-module(message_reader).
-export([start/3]).
-export([loop/3]).

start(Parent, Peer_id) ->
    spawn(?MODULE, loop, [Parent, Peer_id]).

loop(Parent, Peer_id) ->
    receive
	{bitfield, Bitfield} ->
	    mutex:update_peer(Parent, Peer_id, bitfield, lol(Bitfield, 0)),
	    loop(Parent, Peer_id);
	{have, Piece_index} ->
	    Old_bitfield = mutex:get_field(Parent, Peer_id, bitfield),
	    New_bitfield = lists:keyreplace(Piece_index, 2, Old_bitfield, {1, Piece_index}),
	    mutex:update_peer(Parent, Peer_id, bitfield, New_bitfield),
	    loop(Parent, Peer_id);
	{choke} ->
	    mutex:update_peer(Parent, Peer_id, choke, 1),
	    loop(Parent, Peer_id);
	{unchoke} ->
	    mutex:update_peer(Parent, Peer_id, choke, 0),
	    loop(Parent, Peer_id)
		
		
    end.

lol(<<P:1>>, Index) ->
    [{P, Index}];
lol(<<P:1, Rest/bits>>, Index) ->
    [{P, Index} | lol(Rest, Index+1)].
     
