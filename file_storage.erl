%%%Created by: Fredrik Gustafsson
%%%Date: 16-11-2011
-module(file_storage).
-export([start/6, init/6, full_length/1]).

start(Dl_storage_pid, Files, Length, Piece_length, Length_in_list, Piece_storage_pid) ->
    spawn(?MODULE, init, [Dl_storage_pid, Files, Length, Piece_length, Length_in_list, Piece_storage_pid]).

init(Dl_storage_pid, Files, Length, Piece_length, Length_in_list, Piece_storage_pid) ->
    Data = initiate_data(0, Length),
    Table_id = ets:new(torrent, [ordered_set]),
    loop(Dl_storage_pid, Files, Data, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid).

initiate_data(Nr, Length) when Nr =< Length ->
    [{0, Nr}|initiate_data(Nr+1, Length)];
initiate_data(_Nr, _Length) ->
    [].
loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid) ->
    receive
	{request, how_much, [], From} ->
	    Reply = Piece_length * get_amount_of_pieces(0, Length, Bitfield),
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid);
	{request, get_bitfield, [], From} ->
	    Reply = strip_bitfield(Bitfield, 0, Length),
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid);
	{request, insert_chunk, [Index, Begin, Block, Block_length], From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Chunk_table_id, {Begin, Block, Block_length}),
		    ets:insert(Table_id, {Index, Chunk_table_id});
		[{Index, Chunk_table_id}] ->
		    ets:insert(Chunk_table_id, {Begin, Block, Block_length})
	    end,
	    Check_piece = check_piece(0, Index, Table_id, Chunk_table_id, Piece_length, Dl_storage_pid, "", Piece_storage_pid),
	    From ! {reply, Check_piece},
	    case Check_piece of
		true ->
		    case length([H|T]) =:= 1 of
			true ->
			    {ok , Io} = file:open(H, [write]),
			    ok =  write_to_file(Table_id, 0, Length, Io, Piece_length),
			    ok = file:close(Io);
			_ ->
			    {ok, Io} = file:open(H, [write]),
			    write_to_files(Io,Table_id, 0,0, [H|T], Length, Length_in_list, Piece_length)
		    end,
		    New_bitfield = generate_bitfield(0, Length, Table_id, Piece_length, Dl_storage_pid, Piece_storage_pid),
		    io:format("~nLength=~wBITFIELD~w~n", [Length, New_bitfield]),
		    loop(Dl_storage_pid, [H|T], New_bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid);
		false ->
		    io:format("~nPiece not complete~n");
		error ->
		    io:format("~n~nPiece was corrupt~n")
	    end,
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid);
	{request, compare_bitfield, [Peer_bitfield], From} ->
	    From ! {reply, {ok, am_interested(Bitfield, Peer_bitfield, 0, length(Bitfield)-1)}},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid);
	{request, have, [Index], From} ->
	    From ! {reply, have(Index, Bitfield)},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid);
	{request, get_piece, [Index, Begin, _Length], From} ->
	    Chunk_table_id = ets:lookup(Table_id, Index),
	    From ! get_piece(Begin, Chunk_table_id),
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid);
	{request, what_chunk, [Index], From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Table_id, {Index, Chunk_table_id}),
		    
		    Reply =  what_chunk(0, Index, Chunk_table_id, Piece_length, Length-1, full_length(Length_in_list));
		[{Index, Chunk_table_id}] ->
		    Reply =  what_chunk(0, Index, Chunk_table_id, Piece_length, Length-1, full_length(Length_in_list))
	    end,
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, Length_in_list, Piece_storage_pid)
    end.
full_length([]) ->
    0;
full_length([H|T]) ->
    H + full_length(T).
write_to_files(Io, Table_id, Cur_index, Acc, [File_name|Rest], Length, [Size|Sizes], Piece_length) when Cur_index =< Length, Acc =< Size ->
    case ets:lookup(Table_id, Cur_index) of
	[] ->
	    write_to_files(Io, Table_id, Cur_index+1, Acc+Piece_length, [File_name|Rest], Length, [Size|Sizes], Piece_length);
	[{Cur_index, Chunk_table_id}] ->
	    write_out_chunks(Chunk_table_id, 0, Piece_length, Io),
	    write_to_files(Io, Table_id, Cur_index+1, Acc+Piece_length, [File_name|Rest], Length, [Size|Sizes], Piece_length)
    end;
write_to_files(Io, Table_id, Cur_index, Acc, [_File_name|Rest], Length, [_Size|Sizes], Piece_length) when Cur_index =< Length ->
    ok = file:close(Io),
    case Rest of
	[] ->
	    io:format("~n~n~nDONE!!!!"),
	    ok;
	_List ->
	    {ok, New_io} = file:open(hd(Rest), [write]),
	    write_to_files(New_io, Table_id, Cur_index, Acc, Rest, Length, Sizes, Piece_length)
    end.
get_amount_of_pieces(Acc, Length, Bitfield) when Acc < Length ->
    {value, {Have, Acc}} = lists:keysearch(Acc, 2, Bitfield),
    Have+get_amount_of_pieces(Acc+1, Length, Bitfield);
get_amount_of_pieces(_Acc, _Length, _Bitfield) ->
    0.
generate_bitfield(Acc, Length, Table_id, Piece_length, Dl_storage_pid, Piece_storage_pid) when Acc < Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, Dl_storage_pid, Piece_storage_pid)];
	[{_, Chunk_table_id}]  ->
	    [{check_piece_clean(0, Acc, Table_id, Chunk_table_id, Piece_length, Dl_storage_pid, "", Piece_storage_pid), Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, Dl_storage_pid, Piece_storage_pid)]
    end;

generate_bitfield(_Acc, _Length, _Table_id, _Piece_length, _Dl_storage_pid, _Piece_storage_pid) ->
    [].

check_piece_clean(Acc, Index, Table_id, Chunk_table_id, Piece_length, Dl_storage_pid, Blocks, Piece_storage_pid) when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    0;
	[{_Begin, Block, Length_of_block}] ->
	    check_piece_clean(Acc+Length_of_block, Index, Table_id, Chunk_table_id, Piece_length, Dl_storage_pid, Blocks ++ [<<Block:Length_of_block>>], Piece_storage_pid)
    end;
check_piece_clean(_Acc, _Index, _Table_id,  _Chunk_table_id, _Piece_length, _Dl_storage_pid, _Blocks, _Piece_storage_pid) ->
    1.

strip_bitfield(Bitfield, Acc, Max) when Acc < Max ->
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
	    write_out_chunks(Chunk_table_id, Acc+16384, Piece_length, Io);
	    %%ok;
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
		    [Acc|am_interested(Our_bitfield, Peer_bitfield, Acc+1, Max)];
		1 ->
		    am_interested(Our_bitfield, Peer_bitfield, Acc+1, Max)
	    end;
	0  ->
	    am_interested(Our_bitfield, Peer_bitfield, Acc+1, Max)
    end;
am_interested(_Our_bitfield, _Peer_bitfield, _Acc, _Max) ->
    [].
	
have(Index, Bitfield) ->
    {value, {Have, Index}} = lists:keysearch(Index, 2, Bitfield),
    case Have of
	0 ->
	    true;
	1  ->
	    false
    end.

what_chunk(Acc, Index, Chunk_table_id, Piece_length, Last_piece, Full_length) when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    io:format("~n~nIndex=~w~n~n", [Index]),
	    case Index =:= Last_piece of
		true ->
		    io:format("~nREQUESTING LAST PIECE~n~n"),
		    Last_piece_length = Full_length rem Piece_length,
		    Last_chunk = Last_piece_length rem 16384,
		    Req_last = Full_length - Last_chunk,
		    last_piece_req(Acc, Index, Req_last, Last_piece_length, Last_chunk);
		_ ->
		    {Acc, 16384}
	    end;
	[{_Begin, _Block, Block_length}] ->
	    what_chunk(Acc+Block_length, Index, Chunk_table_id, Piece_length, Last_piece, Full_length)
    end;
what_chunk(_Acc, _Index, _Chunk_table_id, _Piece_length, _Last_piece, _Full_length) ->
    access_denied.
last_piece_req(Acc, _Index, Req_last, Last_piece_length, Last_chunk) when Acc < Req_last  ->
    case Last_piece_length >= 16384 of
	true ->
	    {Acc, 16384};
	_ ->
	    {Acc, Last_chunk}
    end;
last_piece_req(Acc, _Index, _Req_last, _Last_piece_length, Last_chunk) ->
    {Acc, Last_chunk}.
get_piece(Begin, Chunk_table_id) ->
    case ets:lookup(Chunk_table_id, Begin) of
	[] ->
	    {error, false};
	[{_Begin, Block, _Length_of_block}] ->
	    {ok, Block}
    end.

check_piece(Acc, Index, Table_id, Chunk_table_id, Piece_length, Dl_storage_pid, Blocks, Piece_storage_pid) when Acc =< Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    false;
	[{_Begin, Block, Length_of_block}] ->
	    check_piece(Acc+Length_of_block, Index, Table_id, Chunk_table_id, Piece_length, Dl_storage_pid, Blocks ++ [<<Block:Length_of_block>>], Piece_storage_pid)
    end;
check_piece(_Acc, Index, Table_id,  _Chunk_table_id, _Piece_length, Dl_storage_pid, Blocks, Piece_storage_pid) ->
    Hash = sha:sha1raw(list_to_binary(Blocks)),
    case mutex:request(Dl_storage_pid, compare_hash, [Index, Hash]) of
	true ->
	    mutex:received(Dl_storage_pid),
	    true;
	What  ->
	    io:format("~n~nWHAT=~w~n~n", [What]),
	    mutex:received(Dl_storage_pid),
	    ets:delete(Table_id, Index),
	    {Index,{_Hash_correct,Peers}} = mutex:request(Dl_storage_pid, put_back_without_pid, [Index]),
	    mutex:received(Dl_storage_pid),
	    mutex:request(Piece_storage_pid, put_piece_back, [Index, Hash, Peers]),
	    mutex:received(Piece_storage_pid),
	    io:format("~n~nPIECE IS FAAAAAAAALSE~n~n"),
	    error
    end.
