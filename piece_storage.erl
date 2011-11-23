%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-16

-module(piece_storage).
-export([start/1, init/1, initiate_table/1]).

-include_lib("eunit/include/eunit.hrl").

start(List) ->
    spawn(?MODULE, init, [List]).

init(List) ->
    initiate_table(List),
    loop(piece_table).

initiate_table(List) ->
    ets:new(piece_table,[named_table, ordered_set]),
    initiate_table(piece_table, List, 0).
initiate_table(piece_table, [H|T], Index) ->
    ets:insert(piece_table, {Index, {H, []}}),
    initiate_table(piece_table, T, Index + 1);
initiate_table(piece_table, [], _Index) ->
    piece_table.

loop(piece_table)->
    receive
	{request, Function, Args, From} ->
	    case Function of
		insert_bitfield ->
		    [PeerId, [H|T]] = Args,
		    Reply = insert_bitfield(piece_table, PeerId, [H|T]);
		read_piece ->
		    Index = Args,
		    Reply = read_piece(piece_table, Index);
		update_bitfield ->
		    [PeerId, PieceIndex] = Args,
		    Reply = update_bitfield(piece_table, PeerId, PieceIndex);
		get_piece_hash ->
		    [Index] = Args,
		    Reply = get_piece_hash(piece_table, Index);
		putback ->
		    Piece = Args,
		    Reply = putback(piece_table, Piece)
	    end,
	    From ! {reply, Reply},
	    loop(piece_table);
	stop -> ok
    end.

%% insert a new bitfield into the table
insert_bitfield(piece_table, PeerId, [H|T]) ->
    Has = [X || {1, X} <- [H|T]],
    insert_to_table(Has, PeerId, piece_table).

%% inner function of insert_bitfield
insert_to_table([Has|T], PeerId, piece_table) ->
    [{Index, {Hash, Peers}}] = ets:lookup(piece_table, Has),
     ets:insert(piece_table, {Index, {Hash, [PeerId|Peers]}}),
     insert_to_table(piece_table, T, PeerId);
insert_to_table(piece_table, [], _PeerId) ->
     has_inserted.

%% update piece storage when a have message is received
update_bitfield(piece_table, PeerId, PieceIndex) ->
    [{PieceIndex, {Hash, Peers}}] = ets:lookup(piece_table, PieceIndex),
    Is_updated = ets:insert(piece_table, {PieceIndex, {Hash, [PeerId|Peers]}}),
    Is_updated.

%% read the list of peers that has a certain piece by 
%% providing the piece index. 
read_piece(piece_table, Index) ->
    [Content] = ets:lookup(piece_table, Index),
    Content.

%% get the piecehash of a piece by providing the piece index
get_piece_hash(piece_table, Index) ->
    [{Index, {Piecehash, _Peers}}] = ets:lookup(piece_table, Index),
    Piecehash.

%% insert the piece returned from downloading_storage
putback(piece_table, Piece)->
    {PieceIndex, {Piecehash, AllPeersList}} = Piece,
    ets:insert(piece_table, {PieceIndex, {Piecehash, AllPeersList}}).



%% TEST CASES %%

%% Id:                 1 
%% Title:              Initiate piece table
%% Purpose:            Ensure correct creation of table and first insertion of 
%%                     pieces and their hashes
%% Prerequisites:      None
%% Expected result:    The table has been created and pieces correctly inserted
%% Pass/Fail criteria: When run response is "All 2 tests passed"/When run 
%%                     response is error

initiate_table_test_() ->
    {spawn,
     {setup,
      fun() ->
	      initiate_table([a,b,c])
      end,
      fun(_) ->
	      ets:delete(piece_table) 
      end,
      [?_assertMatch([], ets:lookup(test, 3)),
       ?_assertMatch([{0, {a, []}}], ets:lookup(test, 0))]
     }
    }.    

	      
	      
	      

