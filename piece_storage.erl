%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Rarest piece algorithm: Fredrik Gustafsson
%%% Creation date: 2011-11-16

-module(piece_storage).
-export([start/2, init/2]).

start(List, File_name) ->
    spawn(?MODULE, init, [List, File_name]).

%%--------------------------------------------------------------------
%% Function: init/2
%% Purpose: Check if we have an existing dets table with pieces 
%%          previously downloaded.
%% Args: List of piece hashes, file name of dets table.  
%%--------------------------------------------------------------------

init(List, File_name) ->
    Piece_table = initiate_table(List),
    case dets:open_file(File_name) of
	{error, _Reason} -> 
	    io:format("No table exists~n"),
	    loop(Piece_table, length(List));
	{ok, Reference} -> 
	    Key = dets:first(Reference),
	    case Key of
		'$end_of_table' -> 
		    io:format("Table is empty~n"),
		    loop(Piece_table, length(List));
		_Key1 ->
		    remove_pieces_we_have(Reference, Key, Piece_table), 
		    dets:close(Reference),
		    Piece_table_size = ets:info(Piece_table, size),
		    loop(Piece_table, Piece_table_size)
            end
    end.

%%--------------------------------------------------------------------
%% Function: remove_pieces_we_have/2
%% Purpose: Delete the pieces we have already downloaded
%% Args: Reference to dets table where we store downloaded pieces,
%%       the key of the entry we want to lookup, piece table  
%%--------------------------------------------------------------------

remove_pieces_we_have(Reference, Key, Piece_table) ->
    case dets:next(Reference, Key) of
	'$end_of_table' -> 
	    [{Index, _Object}] = dets:lookup(Reference, Key),
	    ets:delete(Piece_table, Index);
	Next_key -> 
	    [{Index, _Object}] = dets:lookup(Reference, Key),
	    ets:delete(Piece_table, Index),
	    remove_pieces_we_have(Reference, Next_key, Piece_table)
	
    end.

%%--------------------------------------------------------------------
%% Function: initiate_table/1
%% Purpose: Create an ordered_set ets table and add the piece indexes 
%%          and hashes.
%% Args: A list of piece hashes.
%% Returns: Table ID of the ets table.
%%--------------------------------------------------------------------

initiate_table(List) ->
    Tid = ets:new(db, [ordered_set]),
    initiate_table(Tid, List, 0).
initiate_table(Tid, [H|T], Index) ->
    ets:insert(Tid, {Index, {H, []}}),
    initiate_table(Tid, T, Index + 1);
initiate_table(Tid, [], _Index) ->
    Tid.

%%--------------------------------------------------------------------
%% Function:loop/2
%% Purpose: receive requests from piece mutex about what functions to
%%          execute
%% Args:  TableID of piece table, the amount of pieces to be
%%        downloaded
%% Returns: the requested information
%%--------------------------------------------------------------------

loop(Tid, Nr_of_pieces)->
    receive
	{request, Function, Args, From} ->
	    case Function of
		insert_bitfield ->
		    [Peer_id, [H|T]] = Args,
		    Reply = insert_bitfield(Tid, Peer_id, [H|T]);
		read_piece ->
		    [Index] = Args,
		    Reply = read_piece(Tid, Index);
		update_bitfield ->
		    [Peer_id, Piece_index] = Args,
		    Reply = update_bitfield(Tid, Peer_id, Piece_index);
		get_piece_hash ->
		    [Index] = Args,
		    Reply = get_piece_hash(Tid, Index);
		delete_peer ->
		    [Peer_id] = Args,
		    Reply = delete_peer(Tid,Peer_id);
		delete_piece ->
		    [Index] = Args,
		    Reply = delete_piece(Tid, Index);
		put_piece_back ->
		    [Index, Hash, Peers] = Args,
		    Reply = put_piece_back(Tid, Index, Hash, Peers);
		put_pieces_back ->
		    [List] = Args,
		    Reply = put_pieces_back(Tid, List);
		get_rarest_index ->
		    [Peer_id] = Args,
		    Reply = get_rarest_index(Tid, Peer_id, Nr_of_pieces),
		    case Reply of
			{ok, Index, _Tuple} ->
			    delete_piece(Tid, Index),
			    Reply;
			{hold} ->
			    Reply
		    end;		   
		get_rarest_again ->
		    [Peer_id, Old_index] = Args,
		    Reply = get_rarest_again(Tid, Peer_id, Old_index,
					     Nr_of_pieces),
		    case Reply of
			{ok, Index, _Tuple} ->
			    delete_piece(Tid, Index),
			    Reply;
			{hold} ->
			    Reply
		    end	
	    end,
	    From ! {reply, Reply},
	    loop(Tid, Nr_of_pieces);
	{lookup, Data, From} -> 
	    Result = ets:lookup(Tid, Data),
	    From ! {reply, Result},
	    loop(Tid, Nr_of_pieces);
	stop -> ok;
	_Anything  ->
	    loop(piece_table, Nr_of_pieces)
    end.

%%--------------------------------------------------------------------
%% Function:delete_piece/2
%% Purpose: delete a piece from the piece table
%% Args: TableID of piece table, piece index
%%--------------------------------------------------------------------

delete_piece(Tid, Index) ->
    ets:delete(Tid, Index).

%%--------------------------------------------------------------------
%% Function:put_piece_back/4
%% Purpose: After a piece is downloaded, if its hash is incorrect, 
%%          this function would put this piece back to piece table 
%%          so it can be downloaded again.
%% Args: TableID of piece table, piece index,piece hash, a list of
%%       peers who have this piece.
%%--------------------------------------------------------------------

put_piece_back(Tid, Index, Hash, Peers)->
    ets:insert(Tid, {Index, {Hash, Peers}}).

%%--------------------------------------------------------------------
%% Function:put_pieces_back/2
%% Purpose: If a peer disconnects before we have received the entire 
%%          piece, this function would put a list of pieces downloaded
%%          by this peer back to piece  table so it can be downloaded 
%%          again.
%% Args: TableID of piece table,a list of pieces.
%% Returns: atom has_inserted_all.
%%--------------------------------------------------------------------

put_pieces_back(Tid, [{Index, {Hash, Peers}}|T])->
    ets:insert(Tid, {Index, {Hash, Peers}}),
    put_pieces_back(Tid, T);
put_pieces_back(_Tid, []) ->
    has_inserted_all.

%%--------------------------------------------------------------------
%% Function: get_rarest_again/4
%% Purpose: If the rarest piece was corrupted,this function returns 
%%          the second rarest piece the peer has.
%% Args: TableId of piece table,Peer_id,piece index,amount of pieces 
%%       in the piece table.
%% Returns: either {ok,Piece index}if the peer has it or {hold}if the
%%          peer hasn't
%%--------------------------------------------------------------------

get_rarest_again(Tid, Peer_id, Index, Nr_of_pieces)->
    L = get_rarest(Tid, 0, Nr_of_pieces, []),
    RarestList = kick_out(Index, L),
    get_rarest_index_inner(Tid, Peer_id, RarestList).

%%--------------------------------------------------------------------
%% Function: kick_out/2
%% Purpose: Skip the rarest piece that was corrupted.
%% Args: Piece index, a list of the pieces sorted by the rarest first
%% Returns: a list of pieces sorted by the rarest first excluding the 
%%          corruped one.
%%--------------------------------------------------------------------
    
kick_out(Index, [{Index_2, Peers}|T])->
    case Index =:= Index_2 of
	false->
	    [{Index_2, Peers}|kick_out(Index, T)];
	true ->
	    T
    end;
kick_out(_Index,[]) ->
    [].

%%--------------------------------------------------------------------
%% Function: get_rarest_index/3
%% Purpose: this function returns the rarest piece the peer has.
%% Args: TableID of piece table, Peer_id, amount of pieces in piece table.
%% Returns: either {ok,Piece index} when the peer has it or {hold}
%%          if it hasn't.
%%--------------------------------------------------------------------

get_rarest_index(Tid, Peer_id, Nr_of_pieces)->
    RarestList = get_rarest(Tid, 0, Nr_of_pieces, []),
    get_rarest_index_inner(Tid, Peer_id, RarestList).

%% inner function of insert_bitfield
get_rarest_index_inner(Tid, Peer_id, [H|T])->
    {Index, [P|Peers]} = H,
    Reply = compare(Peer_id, [P|Peers], Index),
    case Reply of
	{ok, Index} ->
	    Tuple = read_piece(Tid, Index),
	    {ok, Index, Tuple};
	{hold} ->
	    get_rarest_index_inner(Tid, Peer_id,T)
    end;
get_rarest_index_inner(_Tid, _Peer_id, [])->
    {hold}.

%%--------------------------------------------------------------------
%% Function:compare/3
%% Purpose: check if the peer exists in the peer list of a certain piece
%% Args: Peer_id,peer list,piece index
%% Returns: either {ok,Index} if the peer is found or {hold}if not 
%%--------------------------------------------------------------------

compare(Peer_id, [P|Peers], Index)->
    case Peer_id == P of
	true ->
	    {ok, Index};
	false ->
	    compare(Peer_id, Peers, Index)
    end;
compare(_Peer_id, [], _Index) ->
    {hold}.

%%--------------------------------------------------------------------
%% Function: get_rarest/4
%% Purpose: create list of pieces sorted by rarest first
%% Args: TableId of piece table, accumulator, the length of the table,
%%       the rarest list to be returned
%% Returns: the list of pieces sorted by rarest first
%%--------------------------------------------------------------------

get_rarest(Tid, Acc, Max, Rarest_list) when Acc =< Max ->
    case ets:lookup(Tid, Acc) of
	[] ->
	    get_rarest(Tid, Acc + 1, Max, Rarest_list);
	[{Index, {_Hash, Peers}}] ->
	    case length(Peers) of
		0 ->
		    get_rarest(Tid, Acc + 1, Max, Rarest_list);
		_Nr  ->
		    get_rarest(Tid, Acc + 1, Max, 
			       place_rarest(Index, Peers, Rarest_list, []))
	    end
    end;
get_rarest(_Tid, _Acc, _Max, Rarest_list) ->
    Rarest_list.

%% inner function of insert_bitfield
place_rarest(Index, Peers, [], New_list) ->
    New_list ++ [{Index, Peers}];
place_rarest(Index, Peers, [{Index2, Peers2}|T], New_list) 
  when length(Peers) /= 0 ->

    case length(Peers) < length(Peers2) of
	true ->
	    New_list ++ [{Index,Peers}] ++ [{Index2, Peers2}|T];
	_  ->
	    place_rarest(Index, Peers, T, New_list ++ [{Index2, Peers2}])
    end;
place_rarest(_Index, _Peers, [H|T], _New_list) ->
    [H|T].

%%--------------------------------------------------------------------
%% Function: insert_bitfield/3
%% Purpose: insert a new peer that has one of the pieces we want into the table
%% Args: TableId of piece table,Peer_id, the bitfield of the peer
%% Returns: atom has_inserted
%%--------------------------------------------------------------------

insert_bitfield(Tid, Peer_id, [H|T]) ->
    Has = [X || {1, X} <- [H|T]],
    insert_to_table(Tid, Has, Peer_id).

%% inner function of insert_bitfield
insert_to_table(Tid, [Has|T], Peer_id) ->
    Result = ets:lookup(Tid, Has),
    case Result of 
	[]->
	    insert_to_table(Tid, T, Peer_id);
	_found ->
	    [{Index, {Hash, Peers}}] = Result,
	    ets:insert(Tid, {Index, {Hash, [Peer_id|Peers]}}),
	    insert_to_table(Tid, T, Peer_id)
     end;
insert_to_table(_Tid, [], _Peer_id) ->
     has_inserted.

%%--------------------------------------------------------------------
%% Function:update_bitfield/3
%% Purpose: update piece storage when a have message is received from 
%%          a peer
%% Args: TableId of piece table, Peer_id, piece index
%% Returns: atom has_updated
%%--------------------------------------------------------------------

update_bitfield(Tid, Peer_id, Piece_index) ->
    Result = ets:lookup(Tid, Piece_index),
    case Result of
	[]->
	    non_existent;
	_found ->
	    [{Piece_index, {Hash, Peers}}] = Result,
	    ets:insert(Tid, {Piece_index, {Hash, [Peer_id|Peers]}}),
	    has_updated
    end.

%%--------------------------------------------------------------------
%% Function:read_piece/2
%% Purpose: read the list of peers that has a certain piece by providing 
%%          the piece index. 
%% Args: TableId of piece table,piece index
%% Returns: The piece
%%--------------------------------------------------------------------

read_piece(Tid, Index) ->
    [Content] = ets:lookup(Tid, Index),
    Content.

%%--------------------------------------------------------------------
%% Function: get_piece_hash/2
%% Purpose: get the Piece_index of a piece by providing the piece index
%% Args: TableId of piece table,piece index
%% Returns: the hash of the requested piece
%%--------------------------------------------------------------------

get_piece_hash(Tid, Index) ->
    [{Index, {Piece_index, _Peers}}] = ets:lookup(Tid, Index),
    Piece_index.

%%--------------------------------------------------------------------
%% Function: delete_peer/2
%% Purpose: when a peer disconnects, this peer is removed from the peer
%%          lists in piece table if this peer exists before
%% Args: TableId of piece table, Peer_id
%% Returns: atom has_deleted
%%--------------------------------------------------------------------

delete_peer(Tid, Peer_id)->
    delete_peer(Tid, Peer_id, 0).
delete_peer(Tid, Peer_id, Index) ->
    io:format("last: ~w~n",[ets:last(Tid)]),
    case Index > ets:last(Tid) of
	true ->
	    has_deleted;
	false ->
	    [{Index, {Piece_index, Peers}}] = ets:lookup(Tid, Index),
	    ets:insert(Tid, {Index, {Piece_index, Peers -- [Peer_id]}}),
	    delete_peer(Tid, Peer_id, Index + 1)
    end.

