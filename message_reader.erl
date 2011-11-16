-module(message_reader).
-export([start/2]).
-export([loop/2]).

start(Parent, Peer_id) ->
    spawn(?MODULE, loop, [Parent, Peer_id]).

loop(Parent, Peer_id) ->
    receive
	{bitfield, Bitfield} ->
	    mutex:update_peer(Parent, Peer_id, bitfield, lol(Bitfield, 0)),
	    mutex:received(Parent),
	    loop(Parent, Peer_id);
	{have, Piece_index} ->
	    Old_bitfield = mutex:get_field(Parent, Peer_id, bitfield),
	    mutex:received(Parent),
	    New_bitfield = lists:keyreplace(Piece_index, 2, Old_bitfield, {1, Piece_index}),
	    mutex:update_peer(Parent, Peer_id, bitfield, New_bitfield),
	    mutex:received(Parent),
	    loop(Parent, Peer_id);
	{choked, 1} ->
	    mutex:update_peer(Parent, Peer_id, choke, 1),
	    mutex:received(Parent),
	    loop(Parent, Peer_id);
	{choked, 0} ->
	    mutex:update_peer(Parent, Peer_id, choke, 0),
	    mutex:received(Parent),
	    loop(Parent, Peer_id);
	{interested, 1} ->
	    mutex:update_peer(Parent, Peer_id, interested, 1),
	    mutex:received(Parent),
	    loop(Parent, Peer_id);
	{interested, 0} ->
	    mutex:update_peer(Parent, Peer_id, interested, 0),
	    mutex:received(Parent),
	    loop(Parent, Peer_id)
    end.

lol(<<P:1>>, Index) ->
    [{P, Index}];
lol(<<P:1, Rest/bits>>, Index) ->
    [{P, Index} | lol(Rest, Index+1)].
     
