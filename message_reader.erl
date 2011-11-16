-module(message_reader).
-export([start/2]).
-export([loop/2]).

start(Parent, Peer_id) ->
    spawn(?MODULE, loop, [Parent, Peer_id]).

loop(Parent, Peer_id) ->
    receive
	{bitfield, Bitfield, Bitfield_len} ->
	    io:format("~nBitfiled: ~w~n", [lol(<<Bitfield:Bitfield_len>>, 0)]),
 	    peer_storage:update_peer(Parent, Peer_id, bitfield, lol(<<Bitfield:Bitfield_len>>, 0)),
 	    peer_storage:received(Parent),
	    loop(Parent, Peer_id);
	{have, Piece_index} ->
	    Old_bitfield = peer_storage:read_field(Parent, Peer_id, bitfield),
	    peer_storage:received(Parent),
	    New_bitfield = lists:keyreplace(Piece_index, 2, Old_bitfield, {1, Piece_index}),
	    peer_storage:update_peer(Parent, Peer_id, bitfield, New_bitfield),
	    peer_storage:received(Parent),
	    loop(Parent, Peer_id);
	{choked, 1} ->
	    peer_storage:update_peer(Parent, Peer_id, choke, 1),
	    peer_storage:received(Parent),
	    loop(Parent, Peer_id);
	{choked, 0} ->
	    peer_storage:update_peer(Parent, Peer_id, choke, 0),
	    peer_storage:received(Parent),
	    loop(Parent, Peer_id);
	{interested, 1} ->
	    peer_storage:update_peer(Parent, Peer_id, interested, 1),
	    peer_storage:received(Parent),
	    loop(Parent, Peer_id);
	{interested, 0} ->
	    peer_storage:update_peer(Parent, Peer_id, interested, 0),
	    peer_storage:received(Parent),
	    loop(Parent, Peer_id);
	{port, Listen_port} ->
	    peer_storage:update_peer(Parent, Peer_id, port, Listen_port),
	    peer_storage:received(Parent),
	    loop(Parent, Peer_id)
    end.

lol(<<P:1>>, Index) ->
    [{P, Index}];
lol(<<P:1, Rest/bits>>, Index) ->
    [{P, Index} | lol(Rest, Index+1)].
     
