%%%Created by: Eva-Lisa Kedborn, Jing Liu
%%%Creation date: 2011-11-08

-module(temp_storage).
-export([create_ets/0,insert/4,retrieve_all_data/0, insert_new_peer/5,
	update_peer/4, read_bitfield/2]).

-record(peer, {peerid = undefined, interested = 0, choke = 1, 
	       bitfield = undefined, ip = undefined, socket = undefined,
	       port = undefined}).

%% create table to store peer info. set peerid as key.
create_peer_ets() ->
    ets:new(db, [{keypos, #peer.peerid}]).

%% this will be used later for file storage
insert(Db, Index, Hash, Data) ->
    ets:insert(Db, {Index, {Hash, Data}}).

%% insert peer for the first time.
insert_new_peer(Db, Ip, PeerId, Socket, Port) -> 
    ets:insert(Db, #peer{peerid = PeerId, interested = 0, choke = 1,
			 bitfield = haha, ip = Ip, socket = Socket, 
			 port = Port}).

%% update certain peer fields
update_peer(PeerId, Db, Field, Value) ->
    case Field of
	interested -> ets:insert(Db, #peer{peerid = PeerId, 
					   interested = Value});
	choke -> ets:insert(Db, #peer{peerid = PeerId, choke = Value});
	bitfield -> ets:insert(Db, #peer{peerid = PeerId, bitfield = Value});
	socket -> ets:insert(Db, #peer{peerid = PeerId, socket = Value});
	port -> ets:insert(Db, #peer{peerid = PeerId, port = Value})
    end.

%% read certain peer fields and return their value
read_field(Db, PeerId, Field) ->
    [Peer] = ets:lookup(Db, PeerId),    
    FieldValue = Peer#peer.Field,
    FieldValue.

%% this will be used later for file storage
retrieve_all_data() ->
    Length = ets:last(downloadedPieces),
    retrieve_all_data(1,Length).

retrieve_all_data(N,Length)when N=<Length ->
    {_Index,{_Hash,Data}} = ets:lookup(downloadedPieces,N),
    [Data|retrieve_all_data(N+1,Length)];

retrieve_all_data(_N,_Length) ->
    [].

    
    
	   
    


    
    
