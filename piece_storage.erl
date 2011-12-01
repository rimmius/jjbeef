%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%%Rarest piece algorithm: Fredrik Gustafsson
%%% Creation date: 2011-11-16

-module(piece_storage).
-export([start/1, init/1, initiate_table/1, place_rarest/4]).

-include_lib("eunit/include/eunit.hrl").

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
	Anything  ->
	    io:format("~n~n~n~w~n~n", [Anything])
    end.

delete_piece(piece_table, Index) ->
    ets:delete(piece_table, Index).

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
    io:format("~n~n~nHAAAAAAAAAAAAAAAER111111111111111~n~n"),
    case ets:lookup(piece_table, Acc) of
	[] ->
	    get_rarest(piece_table, Acc+1, Max, Rarest_list);
	[{Index, {_Hash, Peers}}] ->
	    io:format("~n~n~nHAAAAAAAAAAAAAAAER222222222222~n~n"),
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

%%%%%%%% TEST CASES %%%%%%%%

setup() ->
    Pid = piece_storage:start([hash0, hash1, hash2]),
    register(?MODULE, Pid),
    Pid.

cleanup(Pid) ->
    Pid ! stop.

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
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      [?assertMatch([], ets:lookup(piece_table, 3)),
	       ?assertMatch([{0, {hash0, []}}], ets:lookup(piece_table, 0))]
      end
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
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, insert_bitfield, 
			 [peer1, [{1,0}, {1,1}, {0,2}]], self()},
	      receive {reply, _Reply} -> ok end,

	      [?assertMatch([{0, {hash0, [peer1]}}], 
			    ets:lookup(piece_table, 0)),
	       ?assertMatch([{1, {hash1, [peer1]}}], 
			    ets:lookup(piece_table, 1)),
	       ?assertMatch([{2, {hash2, []}}], ets:lookup(piece_table, 2)),
	       ?assertError(badarg, ets:lookup(piece2_table, 2))]
      end
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
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, insert_bitfield, 
			 [peer1, [{1,0}, {1,1}, {0,2}]], self()},
	      receive {reply, _Reply} -> ok end,

	      ?MODULE ! {request, update_bitfield, [peer2, 2], self()},
	      receive {reply, _} -> ok end,

	      ?MODULE ! {request, update_bitfield, [peer2, 0], self()},
	      receive {reply, _} -> ok end,

	      [?assertMatch([{2, {hash2, [peer2]}}], 
			     ets:lookup(piece_table, 2)),
	       ?assertMatch([{0, {hash0, [peer2, peer1]}}], 
			    ets:lookup(piece_table, 0))]
      end
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
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, insert_bitfield, 
			 [peer1, [{1,0}, {1,1}, {0,2}]], self()},
	      receive {reply, _Reply} -> ok end,

	      ?MODULE ! {request, read_piece, [1], self()},
	      receive {reply, Reply} -> ok end,

	      [?assertEqual(Reply, {1, {hash1, [peer1]}}),
	       ?assertError(function_clause, read_piece(piece2_table, 1))]
      end
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
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, insert_bitfield, 
			 [peer1, [{1,0}, {1,1}, {0,2}]], self()},
	      receive {reply, _Reply} -> ok end,

	      ?MODULE ! {request, get_piece_hash, [1], self()},
	      receive {reply, Reply} -> ok end,

	      [?assertEqual(Reply, hash1),
	       ?assertError(function_clause, get_piece_hash(piece2_table, 1))]
      end
     }
    }.

%% Id:                 6 
%% Title:              Delete piece
%% Purpose:            Be able to delete a piece when it has been downloaded 
%% Prerequisites:      Existing piece table, with pieces
%% Expected result:    The piece is removed with all its info
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error

delete_piece_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, insert_bitfield, 
			 [peer1, [{1,0}, {1,1}, {0,2}]], self()},
	      receive {reply, _} -> ok end,

	      ?MODULE ! {request, delete_piece, [1], self()},
	      receive {reply, _} -> ok end,
   
	      [?assertMatch([], ets:lookup(piece_table, 1)),
	       ?assertMatch([{2, {hash2, []}}], ets:lookup(piece_table, 2))]
      end
     }
    }.

%% Id:                 7 
%% Title:              Delete peer
%% Purpose:            Be able to delete a peer that has disconnected 
%% Prerequisites:      Existing piece table with pieces
%% Expected result:    The peer is correctly removed from all pieces
%% Pass/Fail criteria: When run response is "All tests passed"/When run 
%%                     response is error

delete_peer_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, insert_bitfield, 
			 [peer1, [{1,0}, {1,1}, {0,2}]], self()},
	      receive {reply, _} -> ok end,

	      ?MODULE ! {request, insert_bitfield, 
			 [peer2, [{0,0}, {1,1}, {1,2}]], self()},
	      receive {reply, _} -> ok end,

	      ?MODULE ! {request, delete_peer, [peer1], self()},
	      receive {reply, _} -> ok end,
   
	      [?assertMatch([{0, {hash0, []}}], ets:lookup(piece_table, 0)),
	       ?assertMatch([{1, {hash1, [peer2]}}], 
			    ets:lookup(piece_table, 1)),
	       ?assertMatch([{2, {hash2, [peer2]}}], 
			    ets:lookup(piece_table, 2))]
      end
     }
    }.


		     
    
	      
	      

	      
	      
	      

