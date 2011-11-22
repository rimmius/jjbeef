%%%Created by: Fredrik Gustafsson
%%%Date: 16-11-2011
-module(file_storage).
-export([start/4, init/4, get_bitfield/1, insert_chunk/4]).

start(Dl_storage_pid, Files, Length, Piece_length) ->
    spawn(?MODULE, init, [Dl_storage_pid, Files, Length, Piece_length]).

init(Dl_storage_pid, Files, Length, Piece_length) ->
    Data = initiate_data(1, Length),
    Table_id = ets:new(torrent, [ordered_set]),
    loop(Dl_storage_pid, Files, Data, Table_id, Length, Piece_length).

initiate_data(Nr, Length) when Nr =< Length ->
    [0|initiate_data(Nr+1, Length)];
initiate_data(_Nr, _Length) ->
    [].

loop(Dl_storage_pid, [H|T], Data, Table_id, Length, Piece_length) ->
    receive
	{bitfield, From} ->
	    Bitfield = generate_bitfield(1, Length, Table_id, Piece_length),
	    From ! {reply, Bitfield},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length);
	{insert_chunk, From, Index, Begin, Block} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    New_chunk_table = ets:new(piece, [ordered_set]),
		    ets:insert(New_chunk_table, {Begin, Block}),
		    ets:insert(Table_id, {Index, New_chunk_table});
		[{Index, Chunk_table_id}] ->
		    ets:insert(Chunk_table_id, {Begin, Block})
	    end,
	    From ! {reply, {Table_id, Piece_length}},
	    loop(Dl_storage_pid, [H|T], Data, Table_id, Length, Piece_length);
	{write_to_file, From} ->
	    {ok , Io} = file:open(H, [write]),
	    ok =  write_to_file(Table_id, 1, Length, Io, Piece_length),
	    ok = file:close(Io),
	    From ! {reply, ok},
	    loop(Dl_storage_pid, [H|T], Data, Table_id, Length, Piece_length)
    end.

generate_bitfield(Acc, Length, Table_id, Piece_length) when Acc =< Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    [0|generate_bitfield(Acc+1, Length, Table_id, Piece_length)];
	[{_, Chunk_table_id}]  ->
	    [check_piece(1, Acc, Chunk_table_id, Piece_length)|generate_bitfield(Acc+1, Length, Table_id, Piece_length)]
    end;

generate_bitfield(_Acc, _Length, _Table_id, _Piece_length) ->
    [].

check_piece(Acc, Index, Chunk_table_id, Piece_length) when Acc =< Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    0;
	_ ->
	    check_piece(Acc+16384, Index, Chunk_table_id, Piece_length)
    end;
check_piece(_Acc, _Index, _Chunk_table_id, _Piece_length) ->
    1.

get_bitfield(File_storage_pid) ->
    File_storage_pid ! {bitfield, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.

insert_chunk(File_storage_pid, Index, Begin, Block) ->
    File_storage_pid ! {insert_chunk, self(), Index, Begin, Block},
    receive
	{reply, Reply} ->
	    Reply
    end,
    File_storage_pid ! {write_to_file, self()},
    receive
	{reply, Reply2} ->
	    Reply2
    end.

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

write_out_chunks(Chunk_table_id, Acc, Piece_length, Io) when Acc =< Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    write_out_chunks(Chunk_table_id, Acc+16384, Piece_length, Io);
	[{Acc, Chunk}] ->
	    file:write(Io, Chunk),
	    write_out_chunks(Chunk_table_id, Acc+16384, Piece_length, Io)
    end;
write_out_chunks(_Chunk_table_id, _Acc, _Piece_length, _Io) ->
    ok.
