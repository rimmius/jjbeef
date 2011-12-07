%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%%Rarest piece algorithm: Fredrik Gustafsson
%%% Creation date: 2011-11-16

-module(piece_storage).
-export([start/1, init/1, initiate_table/1, place_rarest/4]).


start(List) ->
    spawn(?MODULE, init, [List]).

init(List) ->
    initiate_table(List),
    loop(piece_table, length(List)-1).


initiate_table(List) ->
    ets:new(piece_table,[named_table, ordered_set]),
    initiate_table(piece_table, List, 0).
initiate_table(piece_table, [H|T], Index) ->
    ets:insert(piece_table, {Index, {H, []}}),
    initiate_table(piece_table, T, Index + 1);
initiate_table(piece_table, [], _Index) ->
    piece_table.

loop(piece_table, Nr_of_pieces)->
    receive
	{request, Function, Args, From} ->
	    case Function of
		insert_bitfield ->
		    [PeerId, [H|T]] = Args,
		    Reply = insert_bitfield(piece_table, PeerId, [H|T]);
		read_piece ->
		    [Index] = Args,
		    Reply = read_piece(piece_table, Index);
		update_bitfield ->
		    [PeerId, PieceIndex] = Args,
		    Reply = update_bitfield(piece_table, PeerId, PieceIndex);
		get_piece_hash ->
		    [Index] = Args,
		    Reply = get_piece_hash(piece_table, Index);
		delete_peer ->
		    [PeerId] = Args,
		    Reply = delete_peer(piece_table,PeerId);
		delete_piece ->
		    [Index] = Args,
		    Reply = delete_piece(piece_table, Index);
		put_piece_back ->
		    [Index,Hash,Peers]=Args,
		    Reply = put_piece_back(piece_table,Index,Hash,Peers);
		get_rarest ->
		    Reply = get_rarest(piece_table, 0, Nr_of_pieces, []);
		get_rarest_index ->
		    [PeerId] = Args,
		    Reply = get_rarest_index(piece_table,PeerId,Nr_of_pieces),
		    case Reply of
			{ok, Index, _Tuple} ->
			    delete_piece(piece_table, Index),
			    Reply;
			{hold} ->
			    Reply
		    end		   
	    end,
	    From ! {reply, Reply},
	    loop(piece_table, Nr_of_pieces);
	stop -> ok;
	_Anything  ->
	    loop(piece_table, Nr_of_pieces)
    end.

delete_piece(piece_table, Index) ->
    ets:delete(piece_table, Index).

put_piece_back(piece_table,Index,Hash,Peers)->
    ets:insert(piece_table,{Index,{Hash,Peers}}).

get_rarest_index(piece_table,PeerId,Nr_of_pieces)->
    RarestList = get_rarest(piece_table,0,Nr_of_pieces,[]),
    get_rarest_index_inner(piece_table,PeerId,RarestList).
get_rarest_index_inner(piece_table,PeerId,[H|T])->
    {Index,[P|Peers]}=H,
    Reply = compare(PeerId,[P|Peers],Index),
    case Reply of
	{ok,Index}->
	    Tuple = read_piece(piece_table,Index),
	    {ok,Index,Tuple};
	{hold} ->
	    get_rarest_index_inner(piece_table,PeerId,T)
    end;
get_rarest_index_inner(piece_table,_PeerId,[])->
    {hold}.

compare(PeerId,[P|Peers],Index)->
    case PeerId == P of
	true ->
	    {ok,Index};
	false ->
	    compare(PeerId,Peers,Index)
    end;
compare(_PeerId,[],_Index) ->
    {hold}.


get_rarest(piece_table, Acc, Max, Rarest_list) when Acc =< Max ->
    case ets:lookup(piece_table, Acc) of
	[] ->
	    get_rarest(piece_table, Acc+1, Max, Rarest_list);
	[{Index, {_Hash, Peers}}] ->
	    case length(Peers) of
		0 ->
		    get_rarest(piece_table, Acc+1, Max, Rarest_list);
		_Nr  ->
		    get_rarest(piece_table, Acc+1, Max, place_rarest(Index, Peers, Rarest_list, []))
	    end
    end;
get_rarest(piece_table, _Acc, _Max, Rarest_list) ->
    Rarest_list.

place_rarest(Index, Peers, [], New_list) ->
    New_list ++ [{Index, Peers}];
place_rarest(Index, Peers, [{Index2, Peers2}|T], New_list) when length(Peers) /= 0 ->
    case length(Peers) < length(Peers2) of
	true ->
	    New_list ++ [{Index,Peers}] ++ [{Index2, Peers2}|T];
	_  ->
	    place_rarest(Index, Peers, T, New_list ++ [{Index2, Peers2}])
    end;
place_rarest(_Index, _Peers, [H|T], _New_list) ->
    [H|T].
%% insert a new peer that has one of the pieces we want into the table
insert_bitfield(piece_table, PeerId, [H|T]) ->
    Has = [X || {1, X} <- [H|T]],
    insert_to_table(piece_table, Has, PeerId).

%% inner function of insert_bitfield
insert_to_table(piece_table, [Has|T], PeerId) ->
    Result = ets:lookup(piece_table,Has),
    case Result of 
	[]->
	    insert_to_table(piece_table,T,PeerId);
	_found ->
	    [{Index, {Hash, Peers}}] = Result,
	    ets:insert(piece_table, {Index, {Hash, [PeerId|Peers]}}),
	    insert_to_table(piece_table, T, PeerId)
     end;
insert_to_table(piece_table, [], _PeerId) ->
     has_inserted.

%% update piece storage when a have message is received
update_bitfield(piece_table, PeerId, PieceIndex) ->
    Result = ets:lookup(piece_table,PieceIndex),
    case Result of
	[]->
	    non_existent;
	_found ->
	    [{PieceIndex, {Hash, Peers}}] = Result,
	    ets:insert(piece_table, {PieceIndex, {Hash, [PeerId|Peers]}}),
	    has_updated
    end.
    
%% read the list of peers that has a certain piece by 
%% providing the piece index. 
read_piece(piece_table, Index) ->
    [Content] = ets:lookup(piece_table, Index),
    Content.

%% get the piecehash of a piece by providing the piece index
get_piece_hash(piece_table, Index) ->
    [{Index, {Piecehash, _Peers}}] = ets:lookup(piece_table, Index),
    Piecehash.

delete_peer(piece_table,PeerId)->
    delete_peer(piece_table,PeerId,0).
delete_peer(piece_table,PeerId,Index) ->
    io:format("last: ~w~n",[ets:last(piece_table)]),
    case Index > ets:last(piece_table) of
	true ->
	    has_deleted;
	false ->
	    [{Index,{Piecehash,Peers}}] = ets:lookup(piece_table,Index),
	    ets:insert(piece_table, {Index, {Piecehash, Peers--[PeerId]}}),
	    delete_peer(piece_table,PeerId,Index+1)
    end.

