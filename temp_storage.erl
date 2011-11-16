%%%Created by: Eva-Lisa Kedborn, Jing Liu
%%%Creation date: 2011-11-08

-module(temp_storage).
-export([start_ets/0,init/0,insert/4,retrieve_all_data/0, insert_new_peer/5,
	update_peer/4, read_field/3]).

-record(peer, {peerid = undefined, interested = 0, choke = 1, 
	       bitfield = undefined, ip = undefined, socket = undefined,
	       port = undefined}).

%% create table to store peer info. set peerid as key.


start_ets() ->
    spawn(?MODULE,init,[]).

init()->
    Tid = ets:new(db, [{keypos, #peer.peerid}]),
    loop(Tid).

%% this will be used later for file storage
insert(Tid, Index, Hash, Data) ->
    ets:insert(Tid, {Index, {Hash, Data}}).

%% insert peer for the first time.
insert_new_peer(Tid, Ip, PeerId, Socket, Port) -> 
    ets:insert(Tid, #peer{peerid = PeerId, interested = 0, choke = 1,
			 bitfield = haha, ip = Ip, socket = Socket, 
			 port = Port}).

%% update certain peer fields
update_peer(Tid,PeerId, Field, Value) ->
    case Field of
	interested -> ets:insert(Tid, #peer{peerid = PeerId, 
					   interested = Value});
	choke -> ets:insert(Tid, #peer{peerid = PeerId, choke = Value});
	bitfield -> ets:insert(Tid, #peer{peerid = PeerId, bitfield = Value});
	socket -> ets:insert(Tid, #peer{peerid = PeerId, socket = Value});
	port -> ets:insert(Tid, #peer{peerid = PeerId, port = Value})
    end.

%% read certain peer fields and return their value
read_field(Tid, PeerId, Field) ->
    [Peer] = ets:lookup(Tid, PeerId),
    case Field of
	interested -> Peer#peer.interested;
	choke -> Peer#peer.choke;
	bitfield -> Peer#peer.bitfield;
	socket -> Peer#peer.socket;
	port -> Peer#peer.port
    end.	    

%% this will be used later for file storage
retrieve_all_data() ->
    Length = ets:last(downloadedPieces),
    retrieve_all_data(1,Length).

retrieve_all_data(N,Length)when N=<Length ->
    {_Index,{_Hash,Data}} = ets:lookup(downloadedPieces,N),
    [Data|retrieve_all_data(N+1,Length)];

retrieve_all_data(_N,_Length) ->
    [].

loop(Tid)->
    receive
	{update_peer,PeerId,Field,Value,From}->
		 Has_updated =  update_peer(Tid,PeerId,Field,Value),
		 From!{reply,Has_updated},
		 loop(Tid);
	{write_new_peer, Ip, PeerId, Socket, Port, From}->
		 Has_inserted = insert_new_peer(Tid,Ip, PeerId, Socket, Port),
		 From!{reply,Has_inserted},
		 loop(Tid)
    end.
   
    

    
    
	   
    


    
    
