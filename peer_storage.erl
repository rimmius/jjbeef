%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-08
%%% This module creates a table which stores peer info.

-module(peer_storage).
-export([start/0, init/0]).

%%---------------------------------------------------------------------
%% Record definition
%% Data Type: peer
%% where:
%%    peerid: A process id  (default is undefined).
%%    interested: An integer (default is 0).
%%    choke: An integer (default is 1). 
%%    ip: An ip address (default is undefined).
%%    socket: An integer (default is undefined).
%%    port: An integer (default is undefined).    
%%    request: A string (default is undefined).
%%----------------------------------------------------------------------


-record(peer, {peerid = undefined, interested = 0, choke = 1, 
	       ip = undefined, socket = undefined,
	       port = undefined, request = undefined}).


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


