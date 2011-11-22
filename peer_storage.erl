%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-08
%%% This module creates a table which stores peer info.

-module(peer_storage).
-export([start/0, init/0]).

-record(peer, {peerid = undefined, interested = 0, choke = 1, 
	       ip = undefined, socket = undefined,
	       port = undefined, request = undefined}).

start() ->
    spawn(?MODULE, init, []).

init() ->
    Tid = ets:new(db, [{keypos, #peer.peerid}]),
    loop(Tid).

loop(Tid) ->
    receive
	{request, Function, Args, From}->
	    case Function of
		insert_new_peer->
		    [Ip, PeerId, Socket, Port, Request] = Args,
		    Reply = insert_new_peer(Tid, Ip, PeerId, Socket,
					    Port, Request);
		update_peer->
		    [PeerId, Field, Value] = Args,
		    Reply = update_peer(Tid, PeerId, Field, Value);
		delete_peer->
		    PeerId=Args,
		    Reply = delete_peer(Tid,PeerId);
		read_field->
		    [PeerId,Field] = Args,
		    Reply = read_field(Tid, PeerId, Field)
	    end,
	    From ! {reply, Reply},
	    loop(Tid);
	stop -> ok
    end.

%% insert peer for the first time.
insert_new_peer(Tid, Ip, PeerId, Socket, Port, Request) -> 
    ets:insert(Tid, #peer{peerid = PeerId, interested = 0, choke = 1,
			  ip = Ip, socket = Socket, port = Port,
			  request = Request}).

%% update certain peer fields
update_peer(Tid, PeerId, Field, Value) ->
    case Field of
	interested -> ets:insert(Tid, #peer{peerid = PeerId, 
					   interested = Value});
	choke -> ets:insert(Tid, #peer{peerid = PeerId, choke = Value});
	socket -> ets:insert(Tid, #peer{peerid = PeerId, socket = Value});
	port -> ets:insert(Tid, #peer{peerid = PeerId, port = Value});
	request -> ets: insert(Tid, #peer{peerid = PeerId, request = Value})
    end.

%% delete a peer 
delete_peer(Tid,PeerId)->
    ets:delete(Tid,PeerId).

%% read certain peer fields and return their value
read_field(Tid, PeerId, Field) ->
    [Peer] = ets:lookup(Tid, PeerId),
    case Field of
	interested -> Peer#peer.interested;
	choke -> Peer#peer.choke;
	socket -> Peer#peer.socket;
	port -> Peer#peer.port;
	request -> Peer#peer.request
    end.	    

