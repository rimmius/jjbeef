%%%---------------------------------------------------------------------
%%% Created by: Fredrik Gustafsson
%%% Creation date: 2011-11-16
%%%--------------------------------------------------------------------- 
%%% Description module file_storage
%%%--------------------------------------------------------------------- 
%%% This module handles all the incoming blocks
%%% and pieces. Placing it in the right order when
%%% downloading and taking the right block when
%%% a peer requests. Also generating bitfield.
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
%%%       Piece_storage_pid, Dets_name, Path)
%%%     Start the file storage process
%%%--------------------------------------------------------------------- 
%%% init(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
%%%      Piece_storage_pid, Dets_name, Path)
%%%   Initiates the bitfield, path and reads the info from
%%% the temporary storage, dets table.
%%%---------------------------------------------------------------------
%%% full_length(List)
%%%  sums a list of lengths to one full length.
%%%---------------------------------------------------------------------

-module(file_storage).
-export([start/8, init/8, full_length/1]).

start(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
      Piece_storage_pid, Dets_name, Path) ->
    spawn(?MODULE, init, [Dl_storage_pid, Files, List_of_pieces, Piece_length, 
			  Length_in_list, Piece_storage_pid, Dets_name, Path]).

init(Dl_storage_pid, Files, List_of_pieces, Piece_length, Length_in_list, 
     Piece_storage_pid, Dets_name, Path) ->
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
    {ok, Dets_table} = dets:open_file(Dets_name, [{file, Dets_name},
						  {type, set}]),
    New_bitfield = check_dets(Dets_table, 0, length(List_of_pieces)-1,
			      Table_id, full_length(Length_in_list), 
			      Piece_length),
    io:format("~nGENERATING NEW BITFIELD~n"),
    dets:close(Dets_table),
    loop(Dl_storage_pid, Files, New_bitfield, Table_id, length(List_of_pieces), 
	 Piece_length, Length_in_list, Piece_storage_pid, Dets_name, New_path,
	 0).


%%--------------------------------------------------------------------
%% Function:check_dets/6
%% Purpose: checks the temporary storage which is
%% stored in a dets table on the computer. if its empty
%% and no have's stored
%% Args:Dets_name - the name of the dets table.
%% Acc: The index accumalated recursively
%% Length: the total amount of pieces.
%% Table_id: Id of the piece_table.
%% The ordinary length of the piece, except the last piece
%% Returns: A bitfield.
%%--------------------------------------------------------------------

check_dets(Dets_name, Acc, Length, Table_id, Full_length, Piece_length)
 when Acc =< Length ->
    case dets:lookup(Dets_name, Acc) of
	[] ->
	    [{0, Acc}|check_dets(Dets_name, Acc+1, Length, Table_id, 
				 Full_length, Piece_length)];
	[{Acc, _Blocks}] ->
	    ets:insert(Table_id, {Acc, 1}),
	    [{1, Acc}|check_dets(Dets_name, Acc+1, Length, Table_id, 
				 Full_length, Piece_length)]
    end;
check_dets(_Dets, _Acc, _Length, _Table, _Full_length, _Piece_length) ->
    [].

%%--------------------------------------------------------------------
%% Function:get_blocks/2
%% Purpose: Get the blocks of the piece
%% Args: P: The block.
%% Begin: Where in the piece the block begins.
%% Returns: tupled list with piece.
%%--------------------------------------------------------------------
get_blocks(<<P:(8*16384)>>, Begin) ->
    [{<<P:(8*16384)>>, Begin}];
get_blocks(<<P:(8*16384), Rest/bitstring>>, Begin) ->
    [{<<P:(8*16384)>>, Begin} | get_blocks(Rest, Begin+16384)];
get_blocks(Other, Begin) ->
    [{Other, Begin}].

%%--------------------------------------------------------------------
%% Function:loop/11
%% Purpose: Inserts chunks and sends them to peers.
%% Args: Dl_storage_pid: The pid of the Downloading_storage module
%% [H|T] : The list with the different actual files.
%% Bitfield: The actual bitfield
%% Table_id: The piece table
%% Length: The total amount of pieces. Zero-based.
%% Piece_length : The ordinary piece length except for the last piece.
%% Length_in_list: The different lengths for the files.
%% Piece_storage_pid: The pid of Piece_storage module.
%% New_path: The actual path to where the file(s) is going to be stored.
%% Uploaded: The total amount we have seeded to different peers.
%% Returns:
%%--------------------------------------------------------------------
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
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, 
		 New_path, Uploaded);
	{request, c_downloaded_pieces, [], From} ->
	    From ! {reply, (Length - get_amount_of_pieces(0, Length, 
							  Bitfield))},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, 
		 New_path, Uploaded);
	{request, get_bitfield, [], From} ->
	    Reply = strip_bitfield(Bitfield, 0, Length),
	    From ! {reply, Reply},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, 
		 New_path, Uploaded);
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
			    ets:insert(Chunk_table_id, {Begin, Block, 
							Block_length})
		    end
	    end,
	    case Chunk_table_id /= 1 of
		true ->
		    case check_piece(0, The_pid, Index, Table_id, 
				     Chunk_table_id, Piece_length, 
				     Dl_storage_pid, <<>>, Piece_storage_pid,
				     Length-1, full_length(Length_in_list)) of
			false ->
			    From ! {reply, false};
			error ->
			    From ! {reply, error};
			{true, Blocks} ->
			    From ! {reply, true},
			    {ok, Dets_name} = dets:open_file(Dets_table, 
							     [{file,
							       Dets_table},
							      {type, set}]),
			    dets:insert(Dets_name, {Index, Blocks}),
			    dets:close(Dets_name),
			    ets:delete_all_objects(Chunk_table_id),
			    ets:insert(Table_id, {Index, 1}),
			    io:format("~nGENERATING NEW BITFIELD~n"),
			    New_bitfield = generate_bitfield(0, Length,
							     Table_id, 
							     Piece_length, 
							     Dl_storage_pid, 
							     Piece_storage_pid,
							     full_length(
							       Length_in_list)),
			    case is_finished(New_bitfield, 0, Length-1) of
				true ->
				    case length([H|T]) =:= 1 of
					true ->
					    {ok, Dets_name_} = 
						dets:open_file(Dets_table,
							       [{file,
								 Dets_table},
								{type, set}]),
					    {ok , Io} = file:open(New_path ++ H,
								  [write]),
					    ok =  write_to_file(Dets_name_, 0,
								Length, Io, 
								Piece_length),
					    ok = file:close(Io),
					    dets:close(Dets_name_);
					_ ->
					    {ok, Dets_name_} = 
						dets:open_file(Dets_table,
							       [{file,
								 Dets_table}, 
								{type, set}]),
					    {ok, Io} = file:open(New_path ++ H,
								 [write]),
					    write_to_files(Io,Dets_name_, 0,0,
							   [H|T], Length, 
							   Length_in_list, 
							   Piece_length, 
							   New_path),
					    dets:close(Dets_name_)
				    end;
				_ ->
				    ok
			    end,
			    loop(Dl_storage_pid, [H|T], New_bitfield, Table_id,
				 Length,
				 Piece_length, Length_in_list, 
				 Piece_storage_pid, Dets_table, 
				 New_path, Uploaded)
		    end;
		_  ->
		    ok
	    end,
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid,
		 Dets_table, New_path, Uploaded);
	{request, compare_bitfield, [Peer_bitfield], From} ->
	    From ! {reply, {ok, am_interested(Bitfield, Peer_bitfield, 0, 
					      length(Bitfield)-1)}},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table,
		 New_path, Uploaded);
	{request, have, [Index], From} ->
	    From ! {reply, have(Index, Bitfield)},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, 
		 New_path, Uploaded);
	{request, get_piece, [Index, Begin, Request_length], From} ->
	    {ok, Dets_name} = dets:open_file(Dets_table, [{file, Dets_table}, 
							  {type, set}]),
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
				    Last_piece_l = Full_length_ 
					rem Piece_length,
				    Last_chunk_l = Last_piece_l rem 16384,
				    case Begin =:= (Last_piece_l
						    - Last_chunk_l) of
					true ->
					    List = get_blocks(Blocks, 0),
					    case lists:keysearch(Begin, 2,
								 List) of
						[] ->
						    From ! {reply, {error,
								    false}};
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
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table,
		 New_path, Uploaded + Request_length);
	{request, check_piece, [List], From} ->
	    From ! {reply, pieces_to_remove(Table_id, List, Dl_storage_pid, 
					    Piece_length, Piece_storage_pid,
					    Length, 
					    full_length(Length_in_list))},
	    loop(Dl_storage_pid, [H|T], Bitfield, Table_id, Length, 
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table,
		 New_path, Uploaded);
	{request, what_chunk, [Index], From} ->
	    case ets:lookup(Table_id, Index) of
		[] ->
		    Chunk_table_id = ets:new(piece, [ordered_set]),
		    ets:insert(Table_id, {Index, Chunk_table_id}),
		    
		    Reply =  what_chunk(0, Index, Chunk_table_id, Piece_length,
					Length-1, full_length(Length_in_list));
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
		 Piece_length, Length_in_list, Piece_storage_pid, Dets_table, 
		 New_path, Uploaded)
    end.

%%--------------------------------------------------------------------
%% Function:is_finished/3
%% Purpose: Check if the piece is complete
%% Args: 
%% Returns:
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% -Function:pieces_to_remove/7
%% -Purpose: checks which pieces which is finished from disconnected peers
%% -Args: Table_id: The piece_table
%% -([{Index, {Hash, Peers}}|T] / []): List of pieces combined with hash
%% and peers connected to that piece
%% -Dl_storage_pid: The pid of Downloading_storage module
%% -Piece_length: The ordinary length of the piece except for the last piece
%% -Piece_storage_pid : The pid of piece_storage module
%% -Length: The total amount of pieces.
%% -Full_length: The full length of the piece
%% -Returns: List of which pieces that should be moved back to
%% piece_storage.
%%--------------------------------------------------------------------
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
						     Piece_storage_pid, Length,
						     Full_length)];
	[{Index, Something}] ->
	    case Something of
		1 ->
		   pieces_to_remove(Table_id, T, Dl_storage_pid, 
				     Piece_length, Piece_storage_pid, Length,
				    Full_length);
		_  ->
		    [{Index, {Hash, Peers}}|pieces_to_remove(Table_id, T, 
							     Dl_storage_pid, 
							     Piece_length, 
							     Piece_storage_pid,
							     Length,
							     Full_length)]
	    end
    end.
%%--------------------------------------------------------------------
%% Function:full_length/1
%% Purpose: Returning the total length of the task
%% Args: [H|T]: a list containing the different lengths of the files
%% Returns: A total length of the task
%%--------------------------------------------------------------------
full_length([]) ->
    0;
full_length([H|T]) ->
    H + full_length(T).

%%--------------------------------------------------------------------
%% Function:write_to_files/9
%% Purpose: Writing to multiple files
%% Args: Io: The Io-device
%%
%% Returns:
%%--------------------------------------------------------------------
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
			   [File_name|Rest], Length, [Size|Sizes], Piece_length
			   ,Path)
    end;
write_to_files(Io, Dets_name, Cur_index, Acc, [_File_name|Rest], Length, 
	       [_Size|Sizes], Piece_length, Path) when Cur_index =< Length ->
    ok = file:close(Io),
    case Rest of
	[] ->
	    ok;
	_List ->
	    {ok, New_io} = file:open(Path ++ hd(Rest), [write]),
	    write_to_files(New_io, Dets_name, Cur_index, Acc, Rest, Length, 
			   Sizes, Piece_length, Path)
    end.

%%--------------------------------------------------------------------
%% Function:write_out_bits/4
%% Purpose:
%% Args:
%% Returns:
%%--------------------------------------------------------------------
write_out_bits(Io, List, Acc, Piece_length) when Acc < Piece_length ->
    {value, {Block, Acc}} = lists:keysearch(Acc, 2, List),
    file:write(Io, Block),
    write_out_bits(Io, List, Acc+(16384*8), Piece_length);
write_out_bits(_,_,_,_) ->
    ok.

%%--------------------------------------------------------------------
%% Function:get_amount_of_pieces/3
%% Purpose:
%% Args:
%% Returns:
%%--------------------------------------------------------------------
get_amount_of_pieces(Acc, Length, Bitfield) when Acc < Length ->
    {value, {Have, Acc}} = lists:keysearch(Acc, 2, Bitfield),
    Have+get_amount_of_pieces(Acc+1, Length, Bitfield);
get_amount_of_pieces(_Acc, _Length, _Bitfield) ->
    0.

%%--------------------------------------------------------------------
%% Function:generate_bitfield/7
%% Purpose: Generating a new bitfield
%% Args: Acc: The current index
%% Length: The total amount of pieces
%% Table_id: The piece - table
%% Piece_length: The ordinary piece length except for the last piece
%% Dl_storage_pid: The pid of the downloading_storage module
%% Piece_storage_pid : The pid of piece_storage module
%% Full_length: The full length of the task.
%% Returns:
%%--------------------------------------------------------------------
generate_bitfield(Acc, Length, Table_id, Piece_length, Dl_storage_pid, 
		  Piece_storage_pid, Full_length) when Acc < Length ->
    case ets:lookup(Table_id, Acc) of
	[] ->
	    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, Piece_length, 
					Dl_storage_pid, Piece_storage_pid, 
					Full_length)];
	[{_, Something}]  ->
	    case Something of
		1 ->
		    [{1, Acc}|generate_bitfield(Acc+1, Length, Table_id, 
						Piece_length, Dl_storage_pid, 
						Piece_storage_pid, 
						Full_length)];
		_ ->
		    [{0, Acc}|generate_bitfield(Acc+1, Length, Table_id, 
						Piece_length, Dl_storage_pid, 
						Piece_storage_pid, Full_length)]
	    end
    end;

generate_bitfield(_Acc, _Length, _Table_id, _Piece_length, _Dl_storage_pid, 
		  _Piece_storage_pid, _Full_length) ->
    [].

%%--------------------------------------------------------------------
%% Function:strip_bitfield/3
%% Purpose:
%% Args:
%% Returns:
%%--------------------------------------------------------------------
strip_bitfield(Bitfield, Acc, Max) when Acc < Max ->
    {value, {Have, _Index}} = lists:keysearch(Acc, 2, Bitfield),
    [Have|strip_bitfield(Bitfield, Acc+1, Max)];
strip_bitfield(_Bitfield, _Acc, _Max) ->
    [].

%%--------------------------------------------------------------------
%% Function:write_to_file/5
%% Purpose: Strip a bitfield to a list where 1 means that we have the
%% piece and 0 that we don't have the piece
%% Args: Dets_name: Name of the dets table - the temp-storage.
%% Acc: The current index.
%% Length: The total amount of pieces.
%% Io: The current io-device
%% Piece_length: The ordinary piece length except for the last piece
%% Returns: ok
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% Function:am_interested/4
%% Purpose: Returns a list of which pieces we are interested in
%% in the bitfield sent from peer.
%% Args: Our_bitfield: Our bitfield
%% Peer_bitfield: Peer's bitfield
%% Acc: The current index.
%% Max: The max amount of pieces of the task.
%% Returns: List with which indexes we are interested in.
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% Function:have/2
%% Purpose: Check if we are interested in the have message the
%% peer sent.
%% Args: Index: The index of the piece.
%% Bitfield: Our bitfield.
%% Returns: true or false, where true is means that we are 
%% interested in that piece
%% false, not interested.
%%--------------------------------------------------------------------	
have(Index, Bitfield) ->
    {value, {Have, Index}} = lists:keysearch(Index, 2, Bitfield),
    case Have of
	0 ->
	    true;
	1  ->
	    false
    end.

%%--------------------------------------------------------------------
%% Function:what_chunk/6
%% Purpose: Check which chunk in specified piece we are
%% interested in.
%% Args: Acc: The current Begin.
%% Index:  The current Index.
%% Chunk_table_id: The ets table which we are going to request from
%% Piece_length: The ordinary piece length except for the last piece
%% Last_piece: The zero-based nr of the last piece.
%% FUll_length: The full length of the task.
%% Returns:{Begin, Block length} which we are going to request
%% or access_denied.
%%--------------------------------------------------------------------
what_chunk(Acc, Index, Chunk_table_id, Piece_length, Last_piece, Full_length) 
  when Acc < Piece_length ->
    case ets:lookup(Chunk_table_id, Acc) of
	[] ->
	    case Index =:= Last_piece of
		true ->
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

%%--------------------------------------------------------------------
%% Function:last_piece_req
%% Purpose: Calculate the request of the last index.
%% Args: Acc: The current Begin
%% Chunk_table_id, The current chunk table where we request from.
%% Index: The current index.
%% Last_piece_length: The length of the last piece
%% Last_chunk: The length of the last block
%% Last_chunk_req : The begin where we are going to request the last chunk 
%% Returns: {Begin, Block length} where begin is the offset in the piece
%% and block length the length of the block request.
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% Function:check_piece/11
%% Purpose: Checks the piece after its been downloaded. 
%% Args: Acc: The current offset in the piece.
%% The_pid: The owner of the piece.
%% Index: The current piece
%% Table_id: The current piece table for the task.
%% Chunk_table_id: The current block table
%% Piece_length: The length of the piece except for the last.
%% Dl_storage_pid: Downloading_storage's pid
%% Blocks: Collecting all the blocks in the chunk table
%% Piece_storage_pid : The pid of piece_storage module
%% Length: The total amount of pieces in the task
%% Full_length: The total length for the file(s)
%% Returns: {true, Blocks} if the piece is correct, false if
%% it is not finished, error if its not finished and not correct.
%%--------------------------------------------------------------------
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
			    check_piece(Piece_length+1, The_pid, Index,
					Table_id, Chunk_table_id, Piece_length, 
					Dl_storage_pid, Blocks, 
					Piece_storage_pid, Length, 
					Full_length);
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
check_piece(_Acc, The_pid, Index, _Table_id,  Chunk_table_id, _Piece_length, 
	    Dl_storage_pid, Blocks, Piece_storage_pid, _Length, _Full_length) ->
    io:format("~nPIECE RECEIVED~n")
    Hash = sha:sha1raw(Blocks),
    case mutex:request(Dl_storage_pid, compare_hash, [The_pid, Index, Hash]) of
	true ->
	    mutex:received(Dl_storage_pid),
	    io:format("~nPIECE GOOD~n"),
	    {true, Blocks};
	What  ->
	    mutex:received(Dl_storage_pid),
	    ets:delete_all_objects(Chunk_table_id),
	    {Index,{_Hash_correct,Peers}} = mutex:request(Dl_storage_pid, 
							  put_back, 
							  [The_pid, Index]),
	    mutex:received(Dl_storage_pid),
	    mutex:request(Piece_storage_pid, put_piece_back, [Index, Hash, 
							      Peers]),
	    mutex:received(Piece_storage_pid),
	    io:format("~nPIECE IS CORRUPT~n"),
	    error
    end.
