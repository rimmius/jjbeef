%%% Created by: Fredrik Gustafsson
%%% Date: 16-11-2011

-module(file_storage).
-export([start/7, init/7, full_length/1]).

start(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
      Piece_storage_pid, Dets_name) ->
    spawn(?MODULE, init, [Dl_storage_pid, Files, List_of_pieces, Piece_length, 
			  Length_in_list, Piece_storage_pid, Dets_name]).

init(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
     Piece_storage_pid, Dets_name) ->
    %% io:format("~n~nPiece_length=~w~n~n", [Piece_length]),
    %% {ok, Dets_table} = dets:open_file(Dets_name, {file, Dets_name}),
    Table_id = ets:new(torrent, [ordered_set]),
    %% dets:close(Dets_table),
    Data = initiate_data(0, length(List_of_pieces)),
    loop(Dl_storage_pid, Files, Data, Table_id, length(List_of_pieces), 
	 Piece_length, Length_in_list, Piece_storage_pid, Dets_name).
%% check_dets(Table_id, Dets_table, Acc, Max, Piece_length) when Acc =< Max ->
%%     case dets:lookup(Dets_table, Acc) of
%% 	[] ->
%% 	    [{0, Acc}|check_dets(Table_id, Dets_table, Acc+1, Max, Piece_length)];
%% 	[{Acc, Piece}] ->
%% 	    Chunk_table_id = ets:new(piece, [ordered_set]),
%% 	    insert_to_tables(Piece, Chunk_table_id, 0, Piece_length),
%% 	    ets:insert(Table_id, {Acc, Chunk_table_id}),
%% 	    [{1, Acc}|check_dets(Table_id, Dets_table, Acc+1, Max, Piece_length)]
%%     end;
%% check_dets(_Table_id, _Dets_table, _Acc, _Max, _Piece_length) ->
%%     [].
%% insert_to_tables([], _, _, _) ->
%%     ok;
%% insert_to_tables([H|T], Chunk_table_id, Acc, Piece_length) ->
%%     ets:insert(Chunk_table_id, {(16384 * Acc), get_chunks(H, (16384 * Acc), ((16384* Acc) + 16384)), 16384}),
%%     insert_to_tables(T,Chunk_table_id,  Acc+1, Piece_length).
%% get_chunks(H, Acc, Max) ->
%%     A = binary:part(H, {Acc, Max}),
%%     <<R:(16384*8)>> = A,
%%     R.

initiate_data(Nr, Length) when Nr =< Length ->
    [{0, Nr}|initiate_data(Nr+1, Length)];
initiate_data(_Nr, _Length) ->
    [].
loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, 
     Length_in_list, Piece_storage_pid, Dets_table) ->
    receive
	stop -> ok;
	{request, how_much, [], From} ->
	    Reply = Piece_length * get_amount_of_pieces(0, Length, Bitfield),
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
	{request, get_bitfield, [], From} ->
	    Reply = strip_bitfield(Bitfield, 0, Length),
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
	{request, insert_chunk, [The_pid, Index, Begin, Block, Block_length], 
	 From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Chunk_table_id, {Begin, Block, Block_length}),
		    ets:insert(Table_id, {Index, Chunk_table_id});
		[{Index, Chunk_table_id}] ->
		    ets:insert(Chunk_table_id, {Begin, Block, Block_length})
	    end,
	     
	    Check_piece = check_piece(0, The_pid, Index, Table_id, 
				      Chunk_table_id, Piece_length, 
				      Dl_storage_pid, <<>>, Piece_storage_pid, Length-1),
	   
	    From ! {reply, Check_piece},
	    case Check_piece of
		true ->
		    New_bitfield = generate_bitfield(0, Length, Table_id, 
						     Piece_length, 
						     Dl_storage_pid, 
						     Piece_storage_pid),
		    case is_finished(New_bitfield, 0, Length-1) of
			true ->
			    case length([H|T]) =:= 1 of
				true ->
				    {ok , Io} = file:open(H, [write]),
				    ok =  write_to_file(Table_id, 0, Length, Io, 
						Piece_length),
				    ok = file:close(Io);
				_ ->
				    {ok, Io} = file:open(H, [write]),
				    write_to_files(Io,Table_id, 0,0, [H|T], Length, 
					   Length_in_list, Piece_length)
			    end;
			_ ->
			    ok
		    end,
		    io:format("~nLength=~wBITFIELD~w~n",[Length, New_bitfield]),
		    loop(Dl_storage_pid, [H|T], New_bitfield, Table_id, Length,
			 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
		false ->
		    ok;
		    %% io:format("~nPiece not complete~n");
		error ->
		    ok
		    %% io:format("~n~nPiece was corrupt~n")
	    end,
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
	{request, compare_bitfield, [Peer_bitfield], From} ->
	    From ! {reply, {ok, am_interested(Bitfield, Peer_bitfield, 0, 
					      length(Bitfield)-1)}},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
	{request, have, [Index], From} ->
	    From ! {reply, have(Index, Bitfield)},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
	{request, get_piece, [Index, Begin, Request_length], From} ->
	    io:format("~n~n~n~nGET_PIECE LENGTH=~w~n~n", [Request_length]),
	    case Request_length of
		16384 ->
		    [{Index, Chunk_table_id}] = ets:lookup(Table_id, Index),
		    From ! get_piece(Begin, Chunk_table_id);
		_ ->
		    From ! {error, false}
	    end,
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
	{request, check_piece, [List], From} ->
	    From ! {reply, pieces_to_remove(Table_id, List, Dl_storage_pid, 
					    Piece_length, Piece_storage_pid, Length)},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table);
	{request, what_chunk, [Index], From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Table_id, {Index, Chunk_table_id}),
		    
		    Reply =  what_chunk(0, Index, Chunk_table_id, Piece_length, Length-1, full_length(Length_in_list));
		[{Index, Chunk_table_id}] ->
		    Reply =  what_chunk(0, Index, Chunk_table_id, 
					Piece_length, Length-1, 
					full_length(Length_in_list))
	    end,
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table)
    end.

is_finished(Bitfield, Acc, Max) when Acc =< Max ->
    {value, {Have, _Index}} = lists:keysearch(Acc, 2, Bitfield),
    case Have of
	1 ->
	    is_finished(Bitfield, Acc+1, Max);
	_  ->
	    false
    end;
is_finished(_Bitfield, _Acc, _Max) ->
    true.
pieces_to_remove(_Table_id, [], _Dl_storage_pid, _Piece_length, 
		 _Piece_storage_pid, _Length) ->
    [];
pieces_to_remove(Table_id, [{Index, {Hash, Peers}}|T], Dl_storage_pid, 
		 Piece_length, Piece_storage_pid, Length) ->
    case ets:lookup(Table_id, Index) of
	[] ->
	    [{Index, {Hash, Peers}}|pieces_to_remove(Table_id, T, 
						     Dl_storage_pid, 
						     Piece_length, 
						     Piece_storage_pid, Length)];
	[{Index, Chunk_table_id}] ->
	    case check_piece_clean(0, Index, Table_id, Chunk_table_id, 
				   Piece_length, Dl_storage_pid, "", 
				   Piece_storage_pid, Length-1) of
		0 ->
		    [{Index, {Hash, Peers}}|pieces_to_remove(Table_id, T, 
							     Dl_storage_pid, 
							     Piece_length, 
							     Piece_storage_pid, Length)];
		{1, _Blocks}  ->
		    pieces_to_remove(Table_id, T, Dl_storage_pid, 
				     Piece_length, Piece_storage_pid, Length)
	    end
    end.
full_length([]) ->
    0;
full_length([H|T]) ->
    H + full_length(T).
write_to_files(Io, Table_id, Cur_index, Acc, [File_name|Rest], Length, 
	       [Size|Sizes], Piece_length) when Cur_index =< Length, 
						Acc =< Size ->
    case ets:lookup(Table_id, Cur_index) of
	[] ->
	    write_to_files(Io, Table_id, Cur_index+1, Acc+Piece_length, 
			   [File_name|Rest], Length, [Size|Sizes], 
			   Piece_length);
	[{Cur_index, Chunk_table_id}] ->
	    write_out_chunks(Chunk_table_id, 0, Piece_length, Io),
	    write_to_files(Io, Table_id, Cur_index+1, Acc+Piece_length, 
			   [File_name|Rest], Length, [Size|Sizes], Piece_length)
    end;
write_to_files(Io, Table_id, Cur_index, Acc, [_File_name|Rest], Length, 
	       [_Size|Sizes], Piece_length) when Cur_index =< Length ->
    ok = file:close(Io),
    case Rest of
	[] ->
	    %% io:format("~n~n~nDONE!!!!"),
	    ok;
	_List ->
	    {ok, New_io} = file:open(hd(Rest), [write]),
	    write_to_files(New_io, Table_id, Cur_index, Acc, Rest, Length, 
			   Sizes, Piece_length)
    end.
get_amount_of_pieces(Acc, Length, Bitfield) when Acc < Length ->
    {value, {Have, Acc}} = lists:keysearch(Acc, 2, Bitfield),
    Have+get_amount_of_pieces(Acc+1, Length, Bitfield);
get_amount_of_pieces(_Acc, _Length, _Bitfield) ->
    0.
generate_bitfield(Acc, Length, Table_id, Piece_length, Dl_storage_pid, 
		  Piece_storage_pid) when Acc < Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, 
					Dl_storage_pid, Piece_storage_pid)];
	[{_, Chunk_table_id}]  ->
	    case check_piece_clean(0, Acc, Table_id, Chunk_table_id, Piece_length,
				Dl_storage_pid, "", Piece_storage_pid, Length-1) of
		0 ->
		    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, 
				Dl_storage_pid, Piece_storage_pid)];
		{1, _Blocks} ->
		    [{1, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, 
				Dl_storage_pid, Piece_storage_pid)]
	    end
    end;

generate_bitfield(_Acc, _Length, _Table_id, _Piece_length, _Dl_storage_pid, 
		  _Piece_storage_pid) ->
    [].

check_piece_clean(Acc, Index, Table_id, Chunk_table_id, Piece_length, 
		  Dl_storage_pid, Blocks, Piece_storage_pid, Length) 
  when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    case Index =:= Length of
		true ->
		    Last_piece = Piece_length rem 16384,
		    case Acc >= Last_piece of
			true ->
			    {1, Blocks};
			_ ->
			    0
		    end;
		_ ->
		    0
	    end;
	[{_Begin, Block, Length_of_block}] ->
	    check_piece_clean(Acc+(Length_of_block div 8), Index, Table_id, 
			      Chunk_table_id, Piece_length, Dl_storage_pid, 
			      Blocks ++ [<<Block:Length_of_block>>], 
			      Piece_storage_pid, Length)
    end;
check_piece_clean(_Acc, _Index, _Table_id,  _Chunk_table_id, _Piece_length, 
		  _Dl_storage_pid, Blocks, _Piece_storage_pid, _Length) ->
    {1, Blocks}.

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

write_out_chunks(Chunk_table_id, Acc, Piece_length, Io) 
  when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    write_out_chunks(Chunk_table_id, Acc+16384, Piece_length, Io);
	    %%ok;
	[{Acc, Chunk, Block_length}] ->
	    file:write(Io, <<Chunk:Block_length>>),
	    write_out_chunks(Chunk_table_id, Acc+(Block_length div 8), Piece_length, Io)
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
		    [Acc|am_interested(Our_bitfield, Peer_bitfield, 
				       Acc+1, Max)];
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
what_chunk(Acc, Index, Chunk_table_id, Piece_length, Last_piece, Full_length) 
  when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    io:format("~n~nIndex=~w~n~n", [Index]),
	    case Index =:= Last_piece of
		true ->
		    io:format("~nREQUESTING ON LAST PIECE~n~n"),
		    Last_piece_length = Full_length rem Piece_length,
		    Last_chunk = Last_piece_length rem 16384,
		    Last_chunk_req = Last_piece_length - Last_chunk,
		    last_piece_req(Acc, Chunk_table_id, Index, 
				   Last_piece_length, Last_chunk, 
				   Last_chunk_req);
		_ ->
		    {Acc, 16384}
	    end;
	[{_Begin, _Block, Length_of_block}] ->
	    what_chunk(Acc+(Length_of_block div 8), Index, Chunk_table_id, 
		       Piece_length, Last_piece, Full_length)
    end;
what_chunk(_Acc, _Index, _Chunk_table_id, _Piece_length, _Last_piece, 
	   _Full_length) ->
    access_denied.
last_piece_req(Acc, Chunk_table_id,  Index, Last_piece_length, Last_chunk, 
	       Last_chunk_req) when Acc < Last_piece_length  ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    case Acc =:= Last_chunk_req of
		true ->
		    {Acc, Last_chunk};
		_ ->
		    {Acc, 16384}
	    end;
	[{_Begin, _Block, Length_of_block}] ->
	    last_piece_req(Acc+(Length_of_block div 8), Chunk_table_id, 
			   Index, Last_piece_length, Last_chunk, Last_chunk_req)
    end;
last_piece_req(_Acc, _Chunk_Table_id, _Index, _Last_piece_length, 
	       _Last_chunk, _Last_chunk_req) ->
    access_denied.
get_piece(Begin, Chunk_table_id) ->
    case ets:lookup(Chunk_table_id, Begin) of
	[] ->
	    {error, false};
	[{_Begin, Block, _Length_of_block}] ->
	    {ok, Block}
    end.

check_piece(Acc, The_pid, Index, Table_id, Chunk_table_id, Piece_length, 
	    Dl_storage_pid, Blocks, Piece_storage_pid, Length) 
  when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    case Index =:= (Length) of
		true ->
		    Last_piece = Piece_length rem 16384,
		    case Acc > Last_piece of
			true ->
			    check_piece(Piece_length+1, The_pid, Index, Table_id, Chunk_table_id, Piece_length, 
					Dl_storage_pid, Blocks, Piece_storage_pid, Length);
			_ ->
			    false
		    end;
		_ ->
		    false
	    end;
	[{_Begin, Block, Length_of_block}] ->
	    check_piece(Acc+(Length_of_block div 8), The_pid, Index, Table_id, 
			Chunk_table_id, Piece_length, 
			Dl_storage_pid, 
			list_to_binary([Blocks, <<Block:Length_of_block>>]), 
			Piece_storage_pid, Length)
    end;
check_piece(_Acc, The_pid, Index, Table_id,  _Chunk_table_id, _Piece_length, 
	    Dl_storage_pid, Blocks, Piece_storage_pid, _Length) ->
    Hash = sha:sha1raw(Blocks),
    case mutex:request(Dl_storage_pid, compare_hash, [The_pid, Index, Hash]) of
	true ->
	    mutex:received(Dl_storage_pid),
	    true;
	What  ->
	    io:format("~n~nWHAT=~w~n~n", [What]),
	    mutex:received(Dl_storage_pid),
	    ets:delete(Table_id, Index),
	    {Index,{_Hash_correct,Peers}} = mutex:request(Dl_storage_pid, 
							  put_back, 
							  [The_pid, Index]),
	    mutex:received(Dl_storage_pid),
	    mutex:request(Piece_storage_pid, put_piece_back, [Index, Hash, 
							      Peers]),
	    mutex:received(Piece_storage_pid),
	    io:format("~n~nPIECE IS FAAAAAAAALSE~n~n"),
	    error
    end.
