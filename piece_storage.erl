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

%% insert a new peer that has one of the pieces we want into the table
insert_bitfield(piece_table, PeerId, [H|T]) ->
    Has = [X || {1, X} <- [H|T]],
    insert_to_table(piece_table, Has, PeerId).

%% inner function of insert_bitfield
insert_to_table(piece_table, [Has|T], PeerId) ->
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
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error

initiate_table_test_() ->
    {spawn,
     {setup,
      fun() ->
	      initiate_table([hash0, hash1, hash2])
      end,
      [?_assertMatch([], ets:lookup(piece_table, 3)),
       ?_assertMatch([{0, {hash0, []}}], ets:lookup(piece_table, 0))]
     }
    }. 

%% Id:                 2 
%% Title:              Insert bitfield info
%% Purpose:            Ensure correct insert of bitfield, peer and piece info
%% Prerequisites:      Existing piece table with pieces
%% Expected result:    Info in bitfield has been inserted
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error 

insert_bitfield_test_() ->  
    {spawn,
     {setup,
      fun() ->
	      initiate_table([hash0, hash1, hash2]), 
	      insert_bitfield(piece_table, peer1, [{1,0}, {1,1}, {0,2}])
      end,
      [?_assertMatch([{0, {hash0, [peer1]}}], ets:lookup(piece_table, 0)),
       ?_assertMatch([{1, {hash1, [peer1]}}], ets:lookup(piece_table, 1)),
       ?_assertMatch([{2, {hash2, []}}], ets:lookup(piece_table, 2)),
       ?_assertError(badarg, ets:lookup(piece2_table, 2))]
     }
    }.

%% Id:                 3 
%% Title:              Update bitfield info
%% Purpose:            Ensure correct update of bitfield 
%% Prerequisites:      Existing table with pieces and initial bitfield info
%% Expected result:    Info in bitfield has been updated
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error

update_bitfield_test_() ->
    {spawn,
     {setup,
      fun() ->
	      initiate_table([hash0, hash1, hash2]), 
	      insert_bitfield(piece_table, peer1, [{1,0}, {1,1}, {0,2}]),
	      update_bitfield(piece_table, peer2, 2),
	      update_bitfield(piece_table, peer2, 0)
      end,
      [?_assertMatch([{2, {hash2, [peer2]}}], ets:lookup(piece_table, 2)),
       ?_assertMatch([{0, {hash0, [peer2, peer1]}}], 
		     ets:lookup(piece_table, 0))]
     }
    }.

%% Id:                 4 
%% Title:              Get piece info
%% Purpose:            Ensure correct extraction of piece info 
%% Prerequisites:      Existing table with pieces and bitfield info
%% Expected result:    All stored values of the requested piece is returned
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error

read_piece_test_() ->
    {spawn,
     {setup,
      fun() ->
	      initiate_table([hash0, hash1, hash2]), 
	      insert_bitfield(piece_table, peer1, [{1,0}, {1,1}, {0,2}])
      end,
      [?_assertEqual(read_piece(piece_table, 1), {1, {hash1, [peer1]}}),
       ?_assertError(function_clause, read_piece(piece2_table, 1))]
     }
    }.

%% Id:                 5 
%% Title:              Get piece hash
%% Purpose:            Ensure correct extraction of piece hash 
%% Prerequisites:      Existing table with pieces and their hash
%% Expected result:    The hash of the requested piece is returned
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error

get_piece_hash_test_() ->
    {spawn,
     {setup,
      fun() ->
	      initiate_table([hash0, hash1, hash2]), 
	      insert_bitfield(piece_table, peer1, [{1,0}, {1,1}, {0,2}])
      end,
      [?_assertEqual(get_piece_hash(piece_table, 1), hash1),
       ?_assertError(function_clause, get_piece_hash(piece2_table, 1))]
     }
    }.

%% Id:                 6 
%% Title:              Put back piece
%% Purpose:            Ensure correct insertion of piece and its info 
%% Prerequisites:      Existing piece table
%% Expected result:    The piece is correctly inserted with all its info
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error

putback_test_() ->
    {spawn,
     {setup,
      fun() ->
	      initiate_table([hash0, hash1, hash2]), 
	      insert_bitfield(piece_table, peer1, [{1,0}, {1,1}, {0,2}]),
	      putback(piece_table, {3, {hash3, [peer3]}}) 		  
      end,
      [?_assertMatch([{3, {hash3, [peer3]}}], ets:lookup(piece_table, 3)),
       ?_assertMatch([{2, {hash2, []}}], ets:lookup(piece_table, 2))]
     }
    }.
		     
    
	      
	      

	      
	      
	      

