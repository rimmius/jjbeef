-module(message_reader).
-export([start/2]).
-export([loop/2]).

start(Piece_mutex_pid, Peer_id) ->
    spawn(?MODULE, loop, [Piece_mutex_pid, Peer_id]).

loop(Piece_mutex_pid, Peer_id) ->
    receive
	{bitfield, Bitfield, Bitfield_len} ->
	    %%io:format("~nBitfiled: ~w~n", [lol(<<Bitfield:Bitfield_len>>, 0)]),
	    mutex:request(Piece_mutex_pid, insert_bitfield, [Peer_id, lol(<<Bitfield:Bitfield_len>>, 0)]),
	    mutex:received(Piece_mutex_pid),
	    loop(Piece_mutex_pid, Peer_id);
	{have, _Piece_index} ->
%%	    mutex:request(Piece_mutex_pid, update_bitfield, [Peer_id, Piece_index]),
%%	    mutex:received(Piece_mutex_pid),
	    loop(Piece_mutex_pid, Peer_id);
	{choked, 1} ->
	    mutex:request(Piece_mutex_pid, request, [Peer_id, choke, 1]),
	    mutex:received(Piece_mutex_pid),
	    loop(Piece_mutex_pid, Peer_id);
	{choked, 0} ->
	    mutex:request(Piece_mutex_pid, request, [Peer_id, choke, 0]),
	    mutex:received(Piece_mutex_pid),
	    loop(Piece_mutex_pid, Peer_id);
	{interested, 1} ->
	    mutex:request(Piece_mutex_pid, request, [Peer_id, interested, 1]),
	    mutex:received(Piece_mutex_pid),
	    loop(Piece_mutex_pid, Peer_id);
	{interested, 0} ->
	    mutex:request(Piece_mutex_pid, request, [Peer_id, interested, 0]),
	    mutex:received(Piece_mutex_pid),
	    loop(Piece_mutex_pid, Peer_id);
	{port, Listen_port} ->
	    mutex:request(Piece_mutex_pid, request, [Peer_id, port, Listen_port]),
	    mutex:received(Piece_mutex_pid),
	    loop(Piece_mutex_pid, Peer_id)
    end.

lol(<<P:1>>, Index) ->
    [{P, Index}];
lol(<<P:1, Rest/bits>>, Index) ->
    [{P, Index} | lol(Rest, Index+1)].
     
