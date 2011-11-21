%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-16

-module(piece_storage).
-export([start/1, init/1, stop/1, loop/1]).

start(List)->
    spawn(?MODULE, init, [List]).

init(List)->
    Tid = ets:new(db,[ordered_set]),
    initiate_table(List, Tid),
    loop(Tid).

stop(Pid)->
    Pid ! stop.

initiate_table(List,Tid)->
    initiate_table(List, Tid, 0).
initiate_table([H|T], Tid, Index)->
    ets:insert(Tid, {Index, {H, []}}),
    initiate_table(T, Tid, Index + 1);
initiate_table([], _Tid, _Index)->
    done.

loop(Tid)->
    receive
	{request, Function, Args, From} ->
	    case Function of
		insert_bitfield ->
		    [PeerId, [H|T]] = Args,
		    Reply = insert_bitfield(Tid, PeerId, [H|T]);
		read_piece ->
		    Index = Args,
		    Reply = read_piece(Tid, Index);
		update_bitfield ->
		    [PeerId,PieceIndex] = Args,
		    Reply = update_bitfield(Tid,PeerId,PieceIndex);
		get_piece_hash ->
		    [Index]=Args,
		    Reply = get_piece_hash(Tid,Index);
		putback ->
		    Piece = Args,
		    Reply = putback(Tid, Piece)
	    end,
	    From!{reply, Reply},
	    loop(Tid);
	stop -> ok
    end.

%%insert a new bitfield into the table
insert_bitfield(Tid, PeerId, [H|T])->
    Has = [X || {1, X} <- [H|T]],
    insert_to_table(Has, PeerId, Tid).

%%inner function of insert_birfield
insert_to_table([Has|T], PeerId, Tid)->
    [{Index, {Hash, Peers}}] = ets:lookup(Tid, Has),
     ets:insert(Tid, {Index, {Hash, [PeerId|Peers]}}),
     insert_to_table(T, PeerId, Tid);
insert_to_table([], _PeerId, _Tid) ->
     has_inserted.

%%update piece storage when a have message is received
update_bitfield(Tid,PeerId,PieceIndex)->
    [{PieceIndex,{Hash,Peers}}]=ets:lookup(Tid,PieceIndex),
    ets:insert(Tid,{PieceIndex,{Hash,[PeerId|Peers]}}),
    has_updated.

%%read the list of peers that has a certain piece by 
%%providing the piece index . 
read_piece(Tid, Index)->
    [Content] = ets:lookup(Tid, Index),
    Content.

%%get the piecehash of a piece by providing the piece index
get_piece_hash(Tid,Index)->
    [{Index,{Piecehash,_Peers}}] = ets:lookup(Tid,Index),
    Piecehash.

%% insert the piece returned from downloading_storage
putback(Tid, Piece)->
    {PieceIndex, {Piecehash, AllPeersList}} = Piece,
    ets:insert(Tid, {PieceIndex, {Piecehash, AllPeersList}}).
