%%% Created by: Fredrik Gustafsson
%%% Date: 16-11-2011

-module(file_storage).
-export([start/8, init/8, full_length/1]).

start(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
      Piece_storage_pid, Dets_name, Path) ->
    spawn(?MODULE, init, [Dl_storage_pid, Files, List_of_pieces, Piece_length, 
			  Length_in_list, Piece_storage_pid, Dets_name, Path]).

init(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
     Piece_storage_pid, Dets_name, Path) ->
    %% io:format("~n~nPiece_length=~w~n~n", [Piece_length]),
    
    case Path of
	[] ->
	    New_path = "./";
	_ ->
	    case file:make_dir(binary_to_list(Path)) of
		{error, eexist} ->
		    New_path = binary_to_list(Path) ++ "/";
		ok ->
		    New_path = binary_to_list(Path) ++ "/"
	    end
    end,
    Table_id = ets:new(torrent, [ordered_set]),
    {ok, Dets_table} = dets:open_file(Dets_name, [{file, Dets_name}, {type, set}]),
    New_bitfield = check_dets(Dets_table, 0, length(List_of_pieces)-1, Table_id, full_length(Length_in_list), Piece_length),
    %% New_bitfield = generate_bitfield(0, length(List_of_pieces), Table_id, 
    %% 						     Piece_length, 
    %% 						     Dl_storage_pid, 
    %% 						     Piece_storage_pid, full_length(Length_in_list)),
    io:format("~w~n", [New_bitfield]),
    dets:close(Dets_table),
    %% Data = initiate_data(0, length(List_of_pieces)),
    loop(Dl_storage_pid, Files, New_bitfield, Table_id, length(List_of_pieces), 
	 Piece_length, Length_in_list, Piece_storage_pid, Dets_name, New_path, 0).
check_dets(Dets_name, Acc, Length, Table_id, Full_length, Piece_length) when Acc =< Length ->
    case dets:lookup(Dets_name, Acc) of
	[] ->
	    [{0, Acc}|check_dets(Dets_name, Acc+1, Length, Table_id, Full_length, Piece_length)];
	[{Acc, _Blocks}] ->
	    %% List = get_blocks(list_to_binary(Blocks), 0),
	    ets:insert(Table_id, {Acc, 1}),
	    [{1, Acc}|check_dets(Dets_name, Acc+1, Length, Table_id, Full_length, Piece_length)]
    end;
check_dets(_Dets, _Acc, _Length, _Table, _Full_length, _Piece_length) ->
    [].
get_blocks(<<P:(8*16384)>>, Index) ->
    [{<<P:(8*16384)>>, Index}];
get_blocks(<<P:(8*16384), Rest/bitstring>>, Index) ->
    [{<<P:(8*16384)>>, Index} | get_blocks(Rest, Index+16384)];
get_blocks(Other, Index) ->
    [{Other, Index}].

%% count_bin(<<>>, Count) ->
%%     Count;
%% count_bin(<<_First:8, Rest/binary>>, Count) ->
%%     count_bin(Rest, Count+1).

%% initiate_data(Nr, Length) when Nr =< Length ->
%%     [{0, Nr}|initiate_data(Nr+1, Length)];
%% initiate_data(_Nr, _Length) ->
%%     [].
loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, Piece_length, 
     Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded) ->
    receive
	stop -> ok;
	{request, how_much, [], From} ->
	    Reply = Piece_length * get_amount_of_pieces(0, Length, Bitfield),
	    case Reply > full_length(Length_in_list) of
		true ->
		    From ! {reply, {full_length(Length_in_list), Uploaded}};
		_ ->
		    From ! {reply, {Reply, Uploaded}}
	    end,
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded);
	{request, get_bitfield, [], From} ->
	    Reply = strip_bitfield(Bitfield, 0, Length),
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded);
	{request, insert_chunk, [The_pid, Index, Begin, Block, Block_length], 
	 From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Chunk_table_id, {Begin, Block, Block_length}),
		    ets:insert(Table_id, {Index, Chunk_table_id});
		[{Index, Chunk_table_id}] ->
		    case Chunk_table_id of
			1 ->
			    From ! {reply, true};
			_ ->
			    ets:insert(Chunk_table_id, {Begin, Block, Block_length})
		    end
	    end,
	    case Chunk_table_id /= 1 of
		true ->
		    case check_piece(0, The_pid, Index, Table_id, 
				     Chunk_table_id, Piece_length, 
				     Dl_storage_pid, <<>>, Piece_storage_pid, Length-1, full_length(Length_in_list)) of
			false ->
			    From ! {reply, false};
			error ->
			    From ! {reply, error};
			{true, Blocks} ->
			    From ! {reply, true},
			    {ok, Dets_name} = dets:open_file(Dets_table, [{file, Dets_table}, {type, set}]),
			    dets:insert(Dets_name, {Index, Blocks}),
			    dets:close(Dets_name),
			    ets:delete_all_objects(Chunk_table_id),
			    ets:insert(Table_id, {Index, 1}),
			    New_bitfield = generate_bitfield(0, Length, Table_id, 
							     Piece_length, 
							     Dl_storage_pid, 
							     Piece_storage_pid, full_length(Length_in_list)),
			    case is_finished(New_bitfield, 0, Length-1) of
				true ->
				    case length([H|T]) =:= 1 of
					true ->
					    {ok, Dets_name_} = dets:open_file(Dets_table, [{file, Dets_table}, {type, set}]),
					    {ok , Io} = file:open(New_path ++ H, [write]),
					    ok =  write_to_file(Dets_name_, 0, Length, Io, 
								Piece_length),
					    ok = file:close(Io),
					    dets:close(Dets_name_);
					_ ->
					    {ok, Dets_name_} = dets:open_file(Dets_table, [{file, Dets_table}, {type, set}]),
					    {ok, Io} = file:open(New_path ++ H, [write]),
					    write_to_files(Io,Dets_name_, 0,0, [H|T], Length, 
							   Length_in_list, Piece_length, New_path),
					    dets:close(Dets_name_)
				    end;
				_ ->
				    ok
			    end,
			    io:format("~nLength=~wBITFIELD~w~n",[Length, New_bitfield]),
			    loop(Dl_storage_pid, [H|T], New_bitfield, Table_id, Length,
				 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded)
		    end;
		_  ->
		    ok
	    end,
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded);
	{request, compare_bitfield, [Peer_bitfield], From} ->
	    From ! {reply, {ok, am_interested(Bitfield, Peer_bitfield, 0, 
					      length(Bitfield)-1)}},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded);
	{request, have, [Index], From} ->
	    From ! {reply, have(Index, Bitfield)},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded);
	{request, get_piece, [Index, Begin, Request_length], From} ->
	    {ok, Dets_name} = dets:open_file(Dets_table, [{file, Dets_table}, {type, set}]),
	    case Request_length of
		16384 ->
		    case dets:lookup(Dets_name, Index) of
			[] ->
			    From ! {reply, {error, false}};
			[{_Acc, Blocks}] ->
			    List = get_blocks(Blocks, 0),
			    case lists:keysearch(Begin, 2, List) of
				false ->
				    From ! {reply, {error, false}};
				{value, {Block, Begin}} ->
				    From ! {reply, {ok, Block}}
			    end
		    end;
		_ ->
		    case Index =:= Length-1 of
			true ->
			    case dets:lookup(Dets_name, Index) of
				[] ->
				    From ! {reply, {error, false}};
				[{_Acc, Blocks}] ->
				    Full_length_ = full_length(Length_in_list),
				    Last_piece_l = Full_length_ rem Piece_length,
				    Last_chunk_l = Last_piece_l rem 16384,
				    case Begin =:= (Last_piece_l - Last_chunk_l) of
					true ->
					    List = get_blocks(Blocks, 0),
					    case lists:keysearch(Begin, 2, List) of
						[] ->
						    From ! {reply, {error, false}};
						 {value, {Block, Begin}} ->
						    From ! {reply, {ok, Block}}
					    end;
					_ ->
					    From ! {reply, {error, false}}
				    end
			    end;
			_ ->
			    From ! {reply, {error, false}}
		    end
	    end,
	    dets:close(Dets_name),
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded + Request_length);
	{request, check_piece, [List], From} ->
	    From ! {reply, pieces_to_remove(Table_id, List, Dl_storage_pid, 
					    Piece_length, Piece_storage_pid, Length, full_length(Length_in_list))},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded);
	{request, what_chunk, [Index], From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Table_id, {Index, Chunk_table_id}),
		    
		    Reply =  what_chunk(0, Index, Chunk_table_id, Piece_length, Length-1, full_length(Length_in_list));
		[{Index, Something}] ->
		    case Something of
			1 ->
			    Reply = access_denied;
			Chunk_table_id ->
			    Reply =  what_chunk(0, Index, Chunk_table_id, 
						Piece_length, Length-1, 
						full_length(Length_in_list))
		    end
	    end,
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, New_path, Uploaded)
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
		 _Piece_storage_pid, _Length, _Full_length) ->
    [];
pieces_to_remove(Table_id, [{Index, {Hash, Peers}}|T], Dl_storage_pid, 
		 Piece_length, Piece_storage_pid, Length, Full_length) ->
    case ets:lookup(Table_id, Index) of
	[] ->
	    [{Index, {Hash, Peers}}|pieces_to_remove(Table_id, T, 
						     Dl_storage_pid, 
						     Piece_length, 
						     Piece_storage_pid, Length, Full_length)];
	[{Index, Something}] ->
	    case Something of
		1 ->
		   pieces_to_remove(Table_id, T, Dl_storage_pid, 
				     Piece_length, Piece_storage_pid, Length, Full_length);
		_  ->
		    [{Index, {Hash, Peers}}|pieces_to_remove(Table_id, T, 
							     Dl_storage_pid, 
							     Piece_length, 
							     Piece_storage_pid, Length, Full_length)]
	    end
    end.
full_length([]) ->
    0;
full_length([H|T]) ->
    H + full_length(T).

write_to_files(Io, Dets_name, Cur_index, Acc, [File_name|Rest], Length, 
	       [Size|Sizes], Piece_length, Path) when Cur_index =< Length, 
						Acc =< Size ->
    case dets:lookup(Dets_name, Cur_index) of
	[] ->
	    write_to_files(Io, Dets_name, Cur_index+1, Acc+Piece_length, 
			   [File_name|Rest], Length, [Size|Sizes], 
			   Piece_length, Path);
	[{Cur_index, Blocks}] ->
	    write_out_bits(Io, get_blocks(Blocks,0), 0, Piece_length),
	    write_to_files(Io, Dets_name, Cur_index+1, Acc+Piece_length, 
			   [File_name|Rest], Length, [Size|Sizes], Piece_length, Path)
    end;
write_to_files(Io, Dets_name, Cur_index, Acc, [_File_name|Rest], Length, 
	       [_Size|Sizes], Piece_length, Path) when Cur_index =< Length ->
    ok = file:close(Io),
    case Rest of
	[] ->
	    %% io:format("~n~n~nDONE!!!!"),
	    ok;
	_List ->
	    {ok, New_io} = file:open(Path ++ hd(Rest), [write]),
	    write_to_files(New_io, Dets_name, Cur_index, Acc, Rest, Length, 
			   Sizes, Piece_length, Path)
    end.
write_out_bits(Io, List, Acc, Piece_length) when Acc < Piece_length ->
    {value, {Block, Acc}} = lists:keysearch(Acc, 2, List),
    file:write(Io, Block),
    write_out_bits(Io, List, Acc+(16384*8), Piece_length);
write_out_bits(_,_,_,_) ->
    ok.
get_amount_of_pieces(Acc, Length, Bitfield) when Acc < Length ->
    {value, {Have, Acc}} = lists:keysearch(Acc, 2, Bitfield),
    Have+get_amount_of_pieces(Acc+1, Length, Bitfield);
get_amount_of_pieces(_Acc, _Length, _Bitfield) ->
    0.

generate_bitfield(Acc, Length, Table_id, Piece_length, Dl_storage_pid, 
		  Piece_storage_pid, Full_length) when Acc < Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, 
					Dl_storage_pid, Piece_storage_pid, Full_length)];
	[{_, Something}]  ->
	    case Something of
		1 ->
		    [{1, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, 
				Dl_storage_pid, Piece_storage_pid, Full_length)];
		_ ->
		    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, 
				Dl_storage_pid, Piece_storage_pid, Full_length)]
	    end
    end;

generate_bitfield(_Acc, _Length, _Table_id, _Piece_length, _Dl_storage_pid, 
		  _Piece_storage_pid, _Full_length) ->
    [].

strip_bitfield(Bitfield, Acc, Max) when Acc < Max ->
    {value, {Have, _Index}} = lists:keysearch(Acc, 2, Bitfield),
    [Have|strip_bitfield(Bitfield, Acc+1, Max)];
strip_bitfield(_Bitfield, _Acc, _Max) ->
    [].

write_to_file(Dets_name, Acc, Length, Io, Piece_length) when Acc =< Length ->
    case dets:lookup(Dets_name, Acc) of
	[] ->
	    write_to_file(Dets_name, Acc+1, Length, Io, Piece_length);
	[{Acc, Blocks}]  ->
	    file:write(Io, Blocks),
	    write_to_file(Dets_name, Acc+1, Length, Io, Piece_length)
    end;
write_to_file(_Dets_name, _Acc, _Length, _Io, _Piece_length) ->
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

check_piece(Acc, The_pid, Index, Table_id, Chunk_table_id, Piece_length, 
	    Dl_storage_pid, Blocks, Piece_storage_pid, Length, Full_length) 
  when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    case Index =:= Length of
		true ->
		    Last_piece_length = Full_length rem Piece_length,
		    Last_chunk = Last_piece_length rem 16384,
		    Last_chunk_req = Last_piece_length - Last_chunk,
		    case Acc > Last_chunk_req of
			true ->
			    check_piece(Piece_length+1, The_pid, Index, Table_id, Chunk_table_id, Piece_length, 
					Dl_storage_pid, Blocks, Piece_storage_pid, Length, Full_length);
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
			Piece_storage_pid, Length, Full_length)
    end;
check_piece(_Acc, The_pid, Index, Table_id,  Chunk_table_id, _Piece_length, 
	    Dl_storage_pid, Blocks, Piece_storage_pid, _Length, _Full_length) ->
    Hash = sha:sha1raw(Blocks),
    case mutex:request(Dl_storage_pid, compare_hash, [The_pid, Index, Hash]) of
	true ->
	    mutex:received(Dl_storage_pid),
	    {true, Blocks};
	What  ->
	    io:format("~n~nWHAT=~w~n~n", [What]),
	    mutex:received(Dl_storage_pid),
	    ets:delete_all_objects(Chunk_table_id),
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
