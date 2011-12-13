%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Rarest piece algorithm: Fredrik Gustafsson
%%% Creation date: 2011-11-16

-module(piece_storage).
-export([start/1, init/1]).

start(List) ->
    spawn(?MODULE, init, [List]).

init(List) ->
    Tid = initiate_table(List),
    loop(Tid, length(List)).

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
		    [PeerId, [H|T]] = Args,
		    Reply = insert_bitfield(Tid, PeerId, [H|T]);
		read_piece ->
		    [Index] = Args,
		    Reply = read_piece(Tid, Index);
		update_bitfield ->
		    [PeerId, PieceIndex] = Args,
		    Reply = update_bitfield(Tid, PeerId, PieceIndex);
		get_piece_hash ->
		    [Index] = Args,
		    Reply = get_piece_hash(Tid, Index);
		delete_peer ->
		    [PeerId] = Args,
		    Reply = delete_peer(Tid,PeerId);
		delete_piece ->
		    [Index] = Args,
		    Reply = delete_piece(Tid, Index);
		put_piece_back ->
		    [Index, Hash, Peers]=Args,
		    Reply = put_piece_back(Tid, Index, Hash, Peers);
		put_pieces_back ->
		    [List] = Args,
		    Reply = put_pieces_back(Tid, List);
		get_rarest_index ->
		    [PeerId] = Args,
		    Reply = get_rarest_index(Tid, PeerId, Nr_of_pieces),
		    case Reply of
			{ok, Index, _Tuple} ->
			    delete_piece(Tid, Index),
			    Reply;
			{hold} ->
			    Reply
		    end;		   
		get_rarest_again ->
		    [PeerId, Old_index] = Args,
		    Reply = get_rarest_again(Tid, PeerId, Old_index,
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
%% Args: TableId of piece table,peerId,piece index,amount of pieces 
%%       in the piece table.
%% Returns: either {ok,Piece index}if the peer has it or {hold}if the
%%          peer hasn't
%%--------------------------------------------------------------------

get_rarest_again(Tid, PeerId, Index, Nr_of_pieces)->
    L = get_rarest(Tid, 0, Nr_of_pieces, []),
    RarestList = kick_out(Index, L),
    get_rarest_index_inner(Tid, PeerId, RarestList).

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
%% Args: TableID of piece table, peerID, amount of pieces in piece table.
%% Returns: either {ok,Piece index} when the peer has it or {hold}
%%          if it hasn't.
%%--------------------------------------------------------------------

get_rarest_index(Tid, PeerId, Nr_of_pieces)->
    RarestList = get_rarest(Tid, 0, Nr_of_pieces, []),
    get_rarest_index_inner(Tid, PeerId, RarestList).

%% inner function of insert_bitfield
get_rarest_index_inner(Tid, PeerId, [H|T])->
    {Index, [P|Peers]} = H,
    Reply = compare(PeerId, [P|Peers], Index),
    case Reply of
	{ok, Index} ->
	    Tuple = read_piece(Tid, Index),
	    {ok, Index, Tuple};
	{hold} ->
	    get_rarest_index_inner(Tid, PeerId,T)
    end;
get_rarest_index_inner(_Tid, _PeerId, [])->
    {hold}.

%%--------------------------------------------------------------------
%% Function:compare/3
%% Purpose: check if the peer exists in the peer list of a certain piece
%% Args: PeerId,peer list,piece index
%% Returns: either {ok,Index} if the peer is found or {hold}if not 
%%--------------------------------------------------------------------

compare(PeerId, [P|Peers], Index)->
    case PeerId == P of
	true ->
	    {ok, Index};
	false ->
	    compare(PeerId, Peers, Index)
    end;
compare(_PeerId, [], _Index) ->
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
%% Args: TableId of piece table,PeerId, the bitfield of the peer
%% Returns: atom has_inserted
%%--------------------------------------------------------------------

insert_bitfield(Tid, PeerId, [H|T]) ->
    Has = [X || {1, X} <- [H|T]],
    insert_to_table(Tid, Has, PeerId).

%% inner function of insert_bitfield
insert_to_table(Tid, [Has|T], PeerId) ->
    Result = ets:lookup(Tid, Has),
    case Result of 
	[]->
	    insert_to_table(Tid, T, PeerId);
	_found ->
	    [{Index, {Hash, Peers}}] = Result,
	    ets:insert(Tid, {Index, {Hash, [PeerId|Peers]}}),
	    insert_to_table(Tid, T, PeerId)
     end;
insert_to_table(_Tid, [], _PeerId) ->
     has_inserted.

%%--------------------------------------------------------------------
%% Function:update_bitfield/3
%% Purpose: update piece storage when a have message is received from 
%%          a peer
%% Args: TableId of piece table, peerId, piece index
%% Returns: atom has_updated
%%--------------------------------------------------------------------

update_bitfield(Tid, PeerId, PieceIndex) ->
    Result = ets:lookup(Tid, PieceIndex),
    case Result of
	[]->
	    non_existent;
	_found ->
	    [{PieceIndex, {Hash, Peers}}] = Result,
	    ets:insert(Tid, {PieceIndex, {Hash, [PeerId|Peers]}}),
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
%% Purpose: get the piecehash of a piece by providing the piece index
%% Args: TableId of piece table,piece index
%% Returns: the hash of the requested piece
%%--------------------------------------------------------------------

get_piece_hash(Tid, Index) ->
    [{Index, {Piecehash, _Peers}}] = ets:lookup(Tid, Index),
    Piecehash.

%%--------------------------------------------------------------------
%% Function: delete_peer/2
%% Purpose: when a peer disconnects, this peer is removed from the peer
%%          lists in piece table if this peer exists before
%% Args: TableId of piece table, PeerId
%% Returns: atom has_deleted
%%--------------------------------------------------------------------

delete_peer(Tid, PeerId)->
    delete_peer(Tid, PeerId, 0).
delete_peer(Tid, PeerId, Index) ->
    io:format("last: ~w~n",[ets:last(Tid)]),
    case Index > ets:last(Tid) of
	true ->
	    has_deleted;
	false ->
	    [{Index, {Piecehash, Peers}}] = ets:lookup(Tid, Index),
	    ets:insert(Tid, {Index, {Piecehash, Peers -- [PeerId]}}),
	    delete_peer(Tid, PeerId, Index + 1)
    end.

