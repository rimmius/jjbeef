%%%Created by: Fredrik Gustafsson
%%%Date: 16-11-2011
-module(file_storage).
-export([start/4, init/4]).

start(Dl_storage_pid, Files, Length, Piece_length) ->
    spawn(?MODULE, init, [Dl_storage_pid, Files, Length-1, Piece_length]).

init(Dl_storage_pid, Files, Length, Piece_length) ->
    Data = initiate_data(0, Length),
    Table_id = ets:new(torrent, [ordered_set]),
    loop(Dl_storage_pid, Files, Data, Table_id, Length, Piece_length).

initiate_data(Nr, Length) when Nr =< Length ->
    [{0, Nr}|initiate_data(Nr+1, Length)];
initiate_data(_Nr, _Length) ->
    [].

loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length) ->
    receive
	{request, get_bitfield, [], From} ->
	    Reply = strip_bitfield(Bitfield, 0, length(Bitfield)-1),
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length);
	{request, insert_chunk, [Index, Begin, Block, Block_length], From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Chunk_table_id, {Begin, Block, Block_length}),
		    ets:insert(Table_id, {Index, Chunk_table_id});
		[{Index, Chunk_table_id}] ->
		    ets:insert(Chunk_table_id, {Begin, Block, Block_length})
	    end,
	    {ok , Io} = file:open(H, [write]),
	    ok =  write_to_file(Table_id, 0, Length, Io, Piece_length),
	    ok = file:close(Io),
	    New_bitfield = generate_bitfield(0, Length, Table_id, Piece_length, Dl_storage_pid),
	    From ! {reply, check_piece(0, Index, Chunk_table_id, Piece_length, Dl_storage_pid)},
	    loop(Dl_storage_pid, [H|T], New_bitfield, Table_id, Length, Piece_length);
	{request, compare_bitfield, [Peer_bitfield], From} ->
	    From ! {reply, am_interested(Bitfield, Peer_bitfield, 0, length(Bitfield)-1)},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length);
	{request, have, [Index], From} ->
	    From ! {reply, have(Index, Bitfield)},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length);
	{request, what_chunk, [Index], From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Table_id, {Index, Chunk_table_id}),
		    Reply =  what_chunk(0, Index, Chunk_table_id, Piece_length);
		[{Index, Chunk_table_id}] ->
		    Reply =  what_chunk(0, Index, Chunk_table_id, Piece_length)
	    end,
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length)
    end.

generate_bitfield(Acc, Length, Table_id, Piece_length, Dl_storage_pid) when Acc =< Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, Dl_storage_pid)];
	[{_, Chunk_table_id}]  ->
	    [{check_piece(0, Acc, Chunk_table_id, Piece_length, Dl_storage_pid), Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, Dl_storage_pid)]
    end;

generate_bitfield(_Acc, _Length, _Table_id, _Piece_length, _Dl_storage_pid) ->
    [].

check_piece(Acc, Index, Chunk_table_id, Piece_length, Dl_storage_pid) when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    0;
	[{_Begin, _Block, Length_of_block}] ->
	    check_piece(Acc+Length_of_block, Index, Chunk_table_id, Piece_length, Dl_storage_pid)
    end;
check_piece(_Acc, Index, _Chunk_table_id, _Piece_length, Dl_storage_pid) ->
    mutex:request(Dl_storage_pid, delete_piece, [Index]),
    mutex:received(Dl_storage_pid),
    1.

strip_bitfield(Bitfield, Acc, Max) when Acc =< Max ->
    {value, {Have, _Index}} = lists:keysearch(Acc, 2, Bitfield),
    [Have|strip_bitfield(Bitfield, Acc+1, Max)];
strip_bitfield(_Bitfield, _Acc, _Max) ->
    [].

write_to_file(Table_id, Acc, Length, Io, Piece_length) when Acc =< Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    write_to_file(Table_id, Acc+1, Length, Io, Piece_length);
	[{Acc, Chunk_table_id}]  ->
	    write_out_chunks(Chunk_table_id, 0, Piece_length, Io),
	    write_to_file(Table_id, Acc+1, Length, Io, Piece_length)
    end;
write_to_file(_Table_id, _Acc, _Length, _Io, _Piece_length) ->
    ok.

write_out_chunks(Chunk_table_id, Acc, Piece_length, Io) when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    ok;
	[{Acc, Chunk, Block_length}] ->
	    file:write(Io, <<Chunk:Block_length>>),
	    write_out_chunks(Chunk_table_id, Acc+Block_length, Piece_length, Io)
    end;
write_out_chunks(_Chunk_table_id, _Acc, _Piece_length, _Io) ->
    ok.

am_interested(Our_bitfield, Peer_bitfield, Acc, Max) when Acc =< Max ->
    {value, {Have, Acc}} = lists:keysearch(Acc, 2, Peer_bitfield),
    {value, {Our_have, Acc}} = lists:keysearch(Acc, 2, Our_bitfield),
    case Have of
	1 ->
	    case Our_have of
		0 ->
		    true;
		1 ->
		    am_interested(Our_bitfield, Peer_bitfield, Acc+1, Max)
	    end;
	0  ->
	    am_interested(Our_bitfield, Peer_bitfield, Acc+1, Max)
    end;
am_interested(_Our_bitfield, _Peer_bitfield, _Acc, _Max) ->
    false.
	
have(Index, Bitfield) ->
    {value, {Have, Index}} = lists:keysearch(Index, 2, Bitfield),
    case Have of
	0 ->
	    true;
	1  ->
	    false
    end.

what_chunk(Acc, Index, Chunk_table_id, Piece_length) when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    {Acc, 16384};
	[{_Begin, _Block, Block_length}] ->
	    what_chunk(Acc+Block_length, Index, Chunk_table_id, Piece_length)
    end;
what_chunk(_Acc, _Index, _Chunk_table_id, _Piece_length) ->
    access_denied.
