%%% Created by: Francesco Cesarini,Simon Thompson 
%%% from the book Erlang Programming page 130 
%%% Modified by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-08

-module(peer_storage).
-export([start/0,init/0,stop/1]).
-export([received/1, insert_new_peer/6, update_peer/4,read_field/3]).

-record(peer, {peerid = undefined, interested = 0, choke = 1, 
	       ip = undefined, socket = undefined,
	       port = undefined, request = undefined}).

start() ->
    spawn(?MODULE, init, []).

init() ->
    Tid = ets:new(db, [{keypos, #peer.peerid}]),
    free(Tid).

stop(Pid) ->
    Pid ! stop.

%% once client received reply the mutex should be set to free
received(Pid) ->
    Pid ! {received, self()}, ok.

%% send peer info to mutex for storage in db
insert_new_peer(Pid, Ip, PeerId, Socket, Port,Request) ->
    Pid ! {insert_new_peer, Ip, PeerId, Socket, Port, Request,self()},
    receive {reply, Reply} -> 
	    Reply
    end.  

%% send updated peer info to mutex for storage in db
update_peer(Pid, PeerId, Field, Value) ->  
    Pid ! {update_peer, PeerId, Field, Value, self()},
    receive {reply, Reply} -> 
	    Reply
    end.

read_field(Pid,PeerId,Field)->
    Pid ! {read_field,PeerId,Field,self()},
    receive
	{reply,Value} ->
	    Value
    end.

free(Tid) ->
    receive
	{update_peer, PeerId, Field, Value, ClientPid} ->
	    %% updated peer info in db. if successful atom true is returned
	    Reply = update_peer_local(Tid,PeerId,Field,Value),
	    case Reply of 
		true->io:format("we have updated ~w, ~w, ~w, ~w~n",
				[Tid,PeerId,Field,Value])
	    end,
	    ClientPid!{reply,Reply},
	    busy(ClientPid,Tid);
	{insert_new_peer, Ip, PeerId, Socket, Port, Request, ClientPid} ->
	    %% store peer info in db. if successful atom true is returned
	    Reply = insert_new_peer_local(Tid,Ip,PeerId, Socket, Port,Request),
	    case Reply of 
		true->io:format("we have inserted ~w, ~w, ~w, ~w, ~w, ~w~n",
				[Tid,Ip,PeerId,Socket,Port,Request])
	    end,
	    ClientPid!{reply,Reply},
	    busy(ClientPid,Tid);
	{read_field,PeerId,Field,ClientPid}->
	    Reply = read_field_local(Tid,PeerId,Field),
	    io:format("we have read ~w, ~w, ~w~n",[Tid,PeerId,Field]),
	    ClientPid!{reply,Reply},
	    free(Tid);
	stop -> ok
    end.

busy(ClientPid,Tid) ->
    receive
	{received, ClientPid} ->
	    free(Tid)
    end.

%% insert peer for the first time.
insert_new_peer_local(Tid, Ip, PeerId, Socket, Port,Request) -> 
    ets:insert(Tid, #peer{peerid = PeerId, interested = 0, choke = 1,
			  ip = Ip, socket = Socket, port = Port,request = Request}).

%% update certain peer fields
update_peer_local(Tid,PeerId, Field, Value) ->
    case Field of
	interested -> ets:insert(Tid, #peer{peerid = PeerId, 
					   interested = Value});
	choke -> ets:insert(Tid, #peer{peerid = PeerId, choke = Value});
	socket -> ets:insert(Tid, #peer{peerid = PeerId, socket = Value});
	port -> ets:insert(Tid, #peer{peerid = PeerId, port = Value});
	request -> ets: insert(Tid, #peer{peerid = PeerId,request = Value})
    end.

%% read certain peer fields and return their value
read_field_local(Tid, PeerId, Field) ->
    [Peer] = ets:lookup(Tid, PeerId),
    case Field of
	interested -> Peer#peer.interested;
	choke -> Peer#peer.choke;
	socket -> Peer#peer.socket;
	port -> Peer#peer.port;
	request -> Peer#peer.request
    end.	    

