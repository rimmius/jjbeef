%%%Created by: Fredrik Gustafsson
%%%Date: 16-11-2011
-module(file_storage).
-export([start/4, init/4, get_bitfield/1, insert_chunk/5, compare_bitfield/2, have/2, what_chunk/2]).

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
	{bitfield, From} ->
	    From ! {reply, Bitfield},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length);
	{insert_chunk, From, Index, Begin, Block, Length_of_block} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Chunk_table_id, {Begin, Block, Length_of_block}),
		    ets:insert(Table_id, {Index, Chunk_table_id});
		[{Index, Chunk_table_id}] ->
		    ets:insert(Chunk_table_id, {Begin, Block, Length_of_block})
	    end,
	    From ! {reply, {Index, Chunk_table_id, Piece_length, Dl_storage_pid}},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length);
	{write_to_file, From} ->
	    {ok , Io} = file:open(H, [write]),
	    ok =  write_to_file(Table_id, 1, Length, Io, Piece_length),
	    ok = file:close(Io),
	    New_bitfield = generate_bitfield(0, Length, Table_id, Piece_length, Dl_storage_pid),
	    From ! {reply, ok},
	    loop(Dl_storage_pid, [H|T], New_bitfield, Table_id, Length, Piece_length);
	{chunk_table, From, Index} ->
	    [{Index, Chunk_table, _Length_of_block}] = ets:lookup(Table_id, Index),
	    From ! {reply, Chunk_table, Piece_length},
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
	{_Begin, _Block, Length_of_block} ->
	    check_piece(Acc+Length_of_block, Index, Chunk_table_id, Piece_length, Dl_storage_pid)
    end;
check_piece(_Acc, Index, _Chunk_table_id, _Piece_length, Dl_storage_pid) ->
    mutex:request(Dl_storage_pid, delete_piece, [Index]),
    1.

get_bitfield(File_storage_pid) ->
    File_storage_pid ! {bitfield, self()},
    receive
	{reply, Reply} ->
	    strip_bitfield(Reply, 0, length(Reply)-1)
    end.

strip_bitfield(Bitfield, Acc, Max) when Acc =< Max ->
    {value, {Have, _Index}} = lists:keysearch(Acc, 2, Bitfield),
    [Have|strip_bitfield(Bitfield, Acc+1, Max)];
strip_bitfield(_Bitfield, _Acc, _Max) ->
    [].

insert_chunk(File_storage_pid, Index, Begin, Block, Block_length) ->
    File_storage_pid ! {insert_chunk, self(), Index, Begin, Block, Block_length},
    receive
	{reply, Reply} ->
	    Reply
    end,
    File_storage_pid ! {write_to_file, self()},
    receive
	{reply, Reply2} ->
	    Reply2
    end,
    {Index, Chunk_table_id, Piece_length, Dl_storage_pid} = Reply,
    check_piece(0, Index, Chunk_table_id, Piece_length, Dl_storage_pid).

write_to_file(Table_id, Acc, Length, Io, Piece_length) when Acc =< Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    write_to_file(Table_id, Acc+1, Length, Io, Piece_length);
	[{Acc, Chunk_table_id}]  ->
	    write_out_chunks(Chunk_table_id, 1, Piece_length, Io),
	    write_to_file(Table_id, Acc+1, Length, Io, Piece_length)
    end;
write_to_file(_Table_id, _Acc, _Length, _Io, _Piece_length) ->
    ok.

write_out_chunks(Chunk_table_id, Acc, Piece_length, Io) when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    write_out_chunks(Chunk_table_id, Acc+16384, Piece_length, Io);
	[{Acc, Chunk, Block_length}] ->
	    file:write(Io, Chunk),
	    write_out_chunks(Chunk_table_id, Acc+Block_length, Piece_length, Io)
    end;
write_out_chunks(_Chunk_table_id, _Acc, _Piece_length, _Io) ->
    ok.

compare_bitfield(File_storage_pid, Peer_bitfield) ->
    File_storage_pid ! {bitfield, self()},
    receive
	{reply, Our_bitfield} ->
	    am_interested(Our_bitfield, Peer_bitfield, 0, length(Our_bitfield)-1)
    end.

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
	
have(File_storage_pid, Index) ->
    File_storage_pid ! {bitfield, self()},
    receive
	{reply, Bitfield} ->
	    {value, {Have, Index}} = lists:keysearch(Index, 2, Bitfield),
	    case Have of
		0 ->
		    true;
		1  ->
		    false
	    end
    end.  

what_chunk(File_storage_pid, Index) ->
    File_storage_pid ! {chunk_table, self(), Index},
    receive
	{reply, Chunk_table_id, Piece_length} ->
	    what_chunk(0, Index, Chunk_table_id, Piece_length)
    end.

what_chunk(Acc, Index, Chunk_table_id, Piece_length) when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    case (Piece_length - Acc) < Piece_length of
		true ->
		    {Acc, (Piece_length - Acc)};
		_ ->
		    {Acc, 16384}
	    end;
	{_Begin, _Block, Block_length} ->
	    what_chunk(Acc+Block_length, Index, Chunk_table_id, Piece_length)
    end;
what_chunk(_Acc, _Index, _Chunk_table_id, _Piece_length) ->
    access_denied.
