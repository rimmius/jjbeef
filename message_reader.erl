-module(message_reader).
-export([start/5]).
-export([loop/5]).

start(Grandparent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id) ->
    spawn(?MODULE, loop, [Grandparent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id]).

loop(Grandparent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id) ->
    receive
	{bitfield, Bitfield, Bitfield_len} ->
	    %%io:format("~nBitfiled: ~w~n", [lol(<<Bitfield:Bitfield_len>>, 0)]),
	    Bitfield_in_list = make_bitfield_with_index(<<Bitfield:Bitfield_len>>, 0),
	    Am_interested = mutex:request(Piece_mutex_pid, insert_bitfield, [Peer_id, Bitfield_in_list]),
	    mutex:received(Piece_mutex_pid),
	    %% TODO change interest
	    piece_requester:send_event(Grandparent, bitfield, Bitfield_in_list),
	    piece_requester:send_event(Grandparent, am_interested, Am_interested);
	{have, Piece_index} ->
	    Am_interested = mutex:request(Piece_mutex_pid, update_bitfield, [Peer_id, Piece_index]),
	    mutex:received(Piece_mutex_pid),
	    case Am_interested of
		true -> piece_requester:send_event(Grandparent, am_interested, Am_interested);
		false -> ok %% TODO!!!! or not.
	    end;
	{am_choked, 1} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, choke, 1]),
	    mutex:received(Peer_mutex_pid),
	    piece_requester:send_event(Grandparent, am_choked, 1);
	{am_choked, 0} ->
	    io:format("msg_reader got unchoke~n"),
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, choke, 0]),
	    mutex:received(Peer_mutex_pid),
	    piece_requester:send_event(Grandparent, am_choked, 0),
	    io:format("unchoke sent to piece_requester~n");
	
	{is_interested, 1} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, interested, 1]),
	    mutex:received(Peer_mutex_pid);
	{is_interested, 0} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, interested, 0]),
	    mutex:received(Peer_mutex_pid);
	{port, Listen_port} ->
	    mutex:request(Peer_mutex_pid, update_peer, [Peer_id, port, Listen_port]),
	    mutex:received(Peer_mutex_pid);
	{piece, Index, Begin, Block, Block_len} ->
	    Is_complete = mutex:request(File_storage_pid, insert_chunk, [Index, Begin, Block, Block_len]),
	    mutex:received(File_storage_pid),
	    piece_requester:send_event(Grandparent, piece, {Is_complete, Index})
    end,
    loop(Grandparent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id).

make_bitfield_with_index(<<P:1>>, Index) ->
    [{P, Index}];
make_bitfield_with_index(<<P:1, Rest/bits>>, Index) ->
    [{P, Index} | make_bitfield_with_index(Rest, Index+1)].
     
