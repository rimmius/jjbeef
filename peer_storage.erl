%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-08
%%% This module creates a table which stores peer info.

-module(peer_storage).
-export([start/0, init/0]).

-record(peer, {peerid = undefined, interested = 0, choke = 1, 
	       ip = undefined, socket = undefined,
	       port = undefined, request = undefined}).

-include_lib("eunit/include/eunit.hrl").

start() ->
    spawn(?MODULE, init, []).
 
init() ->
    ets:new(peer_table, [named_table, {keypos, #peer.peerid}]),
    loop(peer_table).

loop(peer_table) ->
    receive
	{request, Function, Args, From}->
	    case Function of
		insert_new_peer ->
		    [Ip, PeerId, Socket, Port, Request] = Args,
		    Reply = insert_new_peer(Ip, PeerId, Socket,
					    Port, Request);
		update_peer ->
		    [PeerId, Field, Value] = Args,
		    Reply = update_peer(PeerId, Field, Value);
		delete_peer ->
		    [PeerId] = Args,
		    Reply = delete_peer(peer_table, PeerId);
		read_field ->
		    [PeerId, Field] = Args,
		    Reply = read_field(peer_table, PeerId, Field)
	    end,
	    From ! {reply, Reply},
	    loop(peer_table);
	stop -> ok
    end.

%% insert peer for the first time.
insert_new_peer(Ip, PeerId, Socket, Port, Request) -> 
    ets:insert(peer_table, #peer{peerid = PeerId, interested = 0, choke = 1,
			  ip = Ip, socket = Socket, port = Port,
			  request = Request}).

%% update certain peer fields
update_peer(PeerId, Field, Value) ->
    case Field of
	interested -> 
	    [#peer{peerid = PeerId, interested = _Int, choke = Ch,
		   ip = Ip, socket = Soc, port = Port, request = Req}] =
		  ets:lookup(peer_table, PeerId),
	    ets:insert(peer_table, #peer{peerid = PeerId, interested = Value,
					 choke = Ch, ip = Ip, socket = Soc,
					port = Port, request = Req});
	choke -> 
	    [#peer{peerid = PeerId, interested = Int, choke = _Ch,
		   ip = Ip, socket = Soc, port = Port, request = Req}] =
		  ets:lookup(peer_table, PeerId),
	    ets:insert(peer_table, #peer{peerid = PeerId, interested = Int,
					 choke = Value, ip = Ip, socket = Soc,
					port = Port, request = Req});
	socket -> 
	    [#peer{peerid = PeerId, interested = Int, choke = Ch,
		   ip = Ip, socket = _Soc, port = Port, request = Req}] =
		  ets:lookup(peer_table, PeerId),
	    ets:insert(peer_table, #peer{peerid = PeerId, interested = Int,
					 choke = Ch, ip = Ip, socket = Value,
					port = Port, request = Req});
	port -> 
	    [#peer{peerid = PeerId, interested = Int, choke = Ch,
		   ip = Ip, socket = Soc, port = _Port, request = Req}] =
		  ets:lookup(peer_table, PeerId),
	    ets:insert(peer_table, #peer{peerid = PeerId, interested = Int,
					 choke = Ch, ip = Ip, socket = Soc,
					port = Value, request = Req});
	request -> 
	    [#peer{peerid = PeerId, interested = Int, choke = Ch,
		   ip = Ip, socket = Soc, port = Port, request = _Req}] =
		  ets:lookup(peer_table, PeerId),
	    ets:insert(peer_table, #peer{peerid = PeerId, interested = Int,
					 choke = Ch, ip = Ip, socket = Soc,
					port = Port, request = Value})
    end.

%% delete a peer 
delete_peer(peer_table, PeerId)->
    ets:delete(peer_table, PeerId).

%% read certain peer fields and return their value
read_field(peer_table, PeerId, Field) ->
    [Peer] = ets:lookup(peer_table, PeerId),
    case Field of
	interested -> Peer#peer.interested;
	choke -> Peer#peer.choke;
	socket -> Peer#peer.socket;
	port -> Peer#peer.port;
	request -> Peer#peer.request
    end.


%%%%%%%% TEST CODE %%%%%%%

setup() ->
    Pid = peer_storage:start(),
    register(?MODULE, Pid),
    ?MODULE ! {request, insert_new_peer, 
	       [ip1, peer1, 1, 2, req1], self()},
    receive {reply, _Reply} -> ok end,
    Pid.

cleanup(Pid) ->
    Pid ! stop.

insert_new_peer_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun() ->
	     [?assertMatch([#peer{peerid = peer1, interested = 0, choke = 1, 
			   ip = ip1, socket = 1, port = 2, 
			   request = req1}], 
			  ets:lookup(peer_table, peer1)),
	     ?assertMatch([], ets:lookup(peer_table, peer2)),
	     ?assertError(badarg, ets:lookup(peer2_table, peer1))]
      end
     }
    }.

update_peer_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, update_peer, 
		      [peer1, socket, 3], self()},
	      receive {reply, _} -> ok end,
	      ?assertMatch([#peer{peerid = peer1, interested = 0, choke = 1, 
			   ip = ip1, socket = 3, port = 2, request = req1}], 
			    ets:lookup(peer_table, peer1)),

	      ?MODULE ! {request, update_peer, 
		      [peer1, interested, 1], self()},
	      receive {reply, _} -> ok end,
	      ?assertMatch([#peer{peerid = peer1, interested = 1, choke = 1, 
			   ip = ip1, socket = 3, port = 2, request = req1}], 
			    ets:lookup(peer_table, peer1)),

	      ?MODULE ! {request, update_peer, 
		      [peer1, choke, 0], self()},
	      receive {reply, _} -> ok end,
	      ?assertMatch([#peer{peerid = peer1, interested = 1, choke = 0, 
			   ip = ip1, socket = 3, port = 2, request = req1}], 
			    ets:lookup(peer_table, peer1)),

	      ?MODULE ! {request, update_peer, 
		      [peer1, port, 5], self()},
	      receive {reply, _} -> ok end,
	      ?assertMatch([#peer{peerid = peer1, interested = 1, choke = 0, 
			   ip = ip1, socket = 3, port = 5, request = req1}], 
			    ets:lookup(peer_table, peer1)),

	      ?MODULE ! {request, update_peer, 
		      [peer1, request, req3], self()},
	      receive {reply, _} -> ok end,
	      ?assertMatch([#peer{peerid = peer1, interested = 1, choke = 0, 
			   ip = ip1, socket = 3, port = 5, request = req3}], 
			    ets:lookup(peer_table, peer1))
      end
     }
    }.

delete_peer_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, delete_peer, 
		      [peer1], self()},
	      receive {reply, _Reply} -> ok end,
	      ?assertMatch([], ets:lookup(peer_table, peer1))
      end
     }
    }.

read_field_test_() ->
    {spawn,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun() ->
	      ?MODULE ! {request, read_field, [peer1, port], self()},
	      receive {reply, PortReply} -> ok end,
	      ?assertEqual(PortReply, 2),

	      ?MODULE ! {request, read_field, [peer1, interested], self()},
	      receive {reply, IntReply} -> ok end,
	      ?assertEqual(IntReply, 0),

	      ?MODULE ! {request, read_field, [peer1, choke], self()},
	      receive {reply, ChokeReply} -> ok end,
	      ?assertEqual(ChokeReply, 1),

	      ?MODULE ! {request, read_field, [peer1, socket], self()},
	      receive {reply, SockReply} -> ok end,
	      ?assertEqual(SockReply, 1),

	      ?MODULE ! {request, read_field, [peer1, request], self()},
	      receive {reply, ReqReply} -> ok end,
	      ?assertEqual(ReqReply, req1)
      end
     }
    }.
    
    

