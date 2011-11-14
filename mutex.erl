%%% Created by: Francesco Cesarini,Simon Thompson 
%%% from the book Erlang Programming page 130 
%%% Modified by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-08

-module(mutex).
-export([start/0, stop/1]).
-export([write_file/4, write_new_peer/6, update_peer/5, received/1]).
-export([init/0]).

start() ->
    spawn(?MODULE, init, []).

stop(MutexPid) ->
    MutexPid ! stop.

%% this will be used later for file storage
write_file(MutexPid, Index, Hash, Data)->
    MutexPid ! {write_file, Index, Hash, Data, self()},
    receive {reply, Reply} -> 
	    Reply
    end.

%% send peer info to mutex for storage in db
write_new_peer(MutexPid, Db, Ip, PeerId, Socket, Port) ->
    MutexPid ! {write_new_peer, Db, Ip, PeerId, Socket, Port, self()},
    receive {reply, Reply} -> 
	    Reply
    end.  

%% send updated peer info to mutex for storage in db
update_peer(MutexPid, PeerId, Db, Elem, Value) ->  
    MutexPid ! {update_peer, PeerId, Db, Elem, Value, self()},
    receive {reply, Reply} -> 
	    Reply
    end.

%% once client received reply the mutex should be set to free
received(MutexPid) ->
    MutexPid ! {received, self()}, ok.

init() ->
    free().

free() ->
    receive
	{update_peer, PeerId, Db, Elem, Value, ClientPid} ->
	    %% updated peer info in db. if successful atom true is returned
	    Has_updated = temp_storage:update_peer(PeerId, Db, Elem, Value),
	    ClientPid ! {reply, Has_updated},
	    busy(ClientPid);
	{write_new_peer, Db, Ip, PeerId, Socket, Port, ClientPid} ->
	    %% store peer info in db. if successful atom true is returned
	    Has_added = temp_storage:insert_new_peer(Db, Ip, PeerId, Socket, 
						     Port),
	    ClientPid ! {reply, Has_added},
	    busy(ClientPid);
	{write, Index, Hash, Data, ClientPid}->
	    %% this will be used later for file storage
	    Has_inserted = temp_storage:insert(Index, Hash, Data),
	    ClientPid ! {reply, Has_inserted},	    
	    busy(ClientPid);
	stop ->
	    terminate()
    end.

busy(ClientPid) ->
    receive
	{received, ClientPid} ->
	    free()
    end.

terminate() ->
    receive
	{_} ->
	    exit(ClientPid, kill),
	    terminate()
    after
	0 -> ok
    end.

