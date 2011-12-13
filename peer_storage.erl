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

%%--------------------------------------------------------------------
%% Function:init/0
%% Purpose: create an ets table for peer info storage
%% Args:empty
%% Returns: TableId of peer table
%%-------------------------------------------------------------------- 

init() ->
    Tid = ets:new(peer_table, [{keypos, #peer.peerid}]),
    loop(Tid).

%%--------------------------------------------------------------------
%% Function:loop/1
%% Purpose: receive requests from peer mutex about what functions to
%%          execute
%% Args:  TableID of peer table
%% Returns: the requested information
%%--------------------------------------------------------------------

loop(Tid) ->
    receive
	{request, Function, Args, From}->
	    case Function of
		insert_new_peer ->
		    [Ip, Peer_id, Socket, Port, Request] = Args,
		    Reply = insert_new_peer(Tid, Ip, Peer_id, Socket,
					    Port, Request);
		update_peer ->
		    [Peer_id, Field, Value] = Args,
		    Reply = update_peer(Tid, Peer_id, Field, Value);
		delete_peer ->
		    [Peer_id] = Args,
		    Reply = delete_peer(Tid, Peer_id);
		read_field ->
		    [Peer_id, Field] = Args,
		    Reply = read_field(Tid, Peer_id, Field)
	    end,
	    From ! {reply, Reply},
	    loop(Tid);
	{lookup, Data, From} -> 
	    Result = ets:lookup(Tid, Data),
	    From ! {reply, Result},
	    loop(Tid);
	stop -> ok
    end.
 
%%--------------------------------------------------------------------
%% Function: insert_new_peer/6
%% Purpose: insert peer for the first time.
%% Args: TableId of peer table, Ip, PeerId, Socket, Port and Request
%%--------------------------------------------------------------------

insert_new_peer(Tid, Ip, Peer_id, Socket, Port, Request) -> 
    ets:insert(Tid, #peer{peerid = Peer_id, interested = 0, choke = 1,
			  ip = Ip, socket = Socket, port = Port,
			  request = Request}).

%%--------------------------------------------------------------------
%% Function: update_peer/4
%% Purpose: update certain peer fields
%% Args: TableId of peer table, PeerId,the field to be updated, the new value
%%--------------------------------------------------------------------

update_peer(Tid, Peer_id, Field, Value) ->
    case Field of
	interested -> 
	    [#peer{peerid = Peer_id, interested = _Int, choke = Ch,
		   ip = Ip, socket = Soc, port = Port, request = Req}] =
		  ets:lookup(Tid, Peer_id),
	    ets:insert(Tid, #peer{peerid = Peer_id, interested = Value,
					 choke = Ch, ip = Ip, socket = Soc,
					port = Port, request = Req});
	choke -> 
	    [#peer{peerid = Peer_id, interested = Int, choke = _Ch,
		   ip = Ip, socket = Soc, port = Port, request = Req}] =
		  ets:lookup(Tid, Peer_id),
	    ets:insert(Tid, #peer{peerid = Peer_id, interested = Int,
					 choke = Value, ip = Ip, socket = Soc,
					port = Port, request = Req});
	socket -> 
	    [#peer{peerid = Peer_id, interested = Int, choke = Ch,
		   ip = Ip, socket = _Soc, port = Port, request = Req}] =
		  ets:lookup(Tid, Peer_id),
	    ets:insert(Tid, #peer{peerid = Peer_id, interested = Int,
					 choke = Ch, ip = Ip, socket = Value,
					port = Port, request = Req});
	port -> 
	    [#peer{peerid = Peer_id, interested = Int, choke = Ch,
		   ip = Ip, socket = Soc, port = _Port, request = Req}] =
		  ets:lookup(Tid, Peer_id),
	    ets:insert(Tid, #peer{peerid = Peer_id, interested = Int,
					 choke = Ch, ip = Ip, socket = Soc,
					port = Value, request = Req});
	request -> 
	    [#peer{peerid = Peer_id, interested = Int, choke = Ch,
		   ip = Ip, socket = Soc, port = Port, request = _Req}] =
		  ets:lookup(Tid, Peer_id),
	    ets:insert(Tid, #peer{peerid = Peer_id, interested = Int,
					 choke = Ch, ip = Ip, socket = Soc,
					port = Port, request = Value})
    end.

%%--------------------------------------------------------------------
%% Function: delete_peer/2
%% Purpose: delete a peer from the peer table
%% Args: tableId of the peer table, PeerId
%%--------------------------------------------------------------------

delete_peer(Tid, Peer_id)->
    ets:delete(Tid, Peer_id).

%%--------------------------------------------------------------------
%% Function: read_field/3
%% Purpose: read certain peer fields and return their value
%% Args: tableId of the peer table, PeerId,the field to be read
%% Returns: the value of the requested field
%%--------------------------------------------------------------------

read_field(Tid, Peer_id, Field) ->
    [Peer] = ets:lookup(Tid, Peer_id),
    case Field of
	interested -> Peer#peer.interested;
	choke -> Peer#peer.choke;
	socket -> Peer#peer.socket;
	port -> Peer#peer.port;
	request -> Peer#peer.request
    end.


