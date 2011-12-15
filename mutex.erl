%%%---------------------------------------------------------------------
%%% Created by: Francesco Cesarini, Simon Thompson
%%% from the book Erlang Programming page 130
%%% Modified by: Eva-Lisa Kedborn,Jing Liu
%%% Creation date: 2011-11-08
%%%--------------------------------------------------------------------- 
%%% Description module mutex
%%%--------------------------------------------------------------------- 
%%% The mutex is a finite state machine that has two states:free and busy.
%%% If the client wants to have access and change the content in the storage
%%% tables, it must go through the mutex. It prevents two processes modifying
%%% the table content at the same time.Isolating the process access to db
%%% storages ensures data safety.
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start(Module,Args)
%%%   spawns a new process for one of the storages and returns its PID
%%%--------------------------------------------------------------------- 
%%% stop()
%%%   stops the mutex process 
%%%--------------------------------------------------------------------- 
%%% init(Module,Args)
%%%   passes the PID of the spawned storage process into loop
%%%---------------------------------------------------------------------  
%%% received(MutexPid)
%%%   After the client receives replies for what it has requested,this
%%%   function is called to set the state of the mutex from busy to free
%%%---------------------------------------------------------------------
%%% request(MutexPid,Function,Args)
%%%   Client uses this function to send requests to the database.MutexPid
%%%   shows which storage the client wants to access. Function and Args are
%%%   the specific requests. 
%%%---------------------------------------------------------------------  

-module(mutex).
-export([start/2, stop/1]).
-export([init/2, received/1, request/3]).

start(Module, Args) ->
    spawn(?MODULE, init, [Module, Args]).

init(Module, Args) ->
    StoragePid = apply(Module, start, Args),
    link(StoragePid),
    free(StoragePid).

stop(MutexPid) ->
    MutexPid ! stop.

received(MutexPid)->
    MutexPid ! {received, self()},
    ok.

request(MutexPid, Function, Args) ->
    MutexPid ! {Function, Args, self()},
    receive 
	{reply, Reply} -> 
	    Reply 
    end.

%%--------------------------------------------------------------------
%% Function:free/1
%% Purpose: One of the states.It receives requests from the client,
%%          passes the requests to the storage process, and receives reply.
%% Args: processID of the storage process
%% Returns: what is being requested
%%--------------------------------------------------------------------

free(StoragePid) ->
    receive
	{Function, Args, ClientPid} ->
	    StoragePid ! {request, Function, Args, self()},
	    receive 
		{reply, Reply} -> 
		    ClientPid ! {reply, Reply} 
	    end,
	    busy(ClientPid, StoragePid);
	stop -> terminate(StoragePid)
    end.

%%--------------------------------------------------------------------
%% Function:busy/2
%% Purpose: One of the states. 
%% Args: processID of the client,processID of the storage
%%--------------------------------------------------------------------

busy(ClientPid, StoragePid) ->
    receive
	{received, ClientPid} ->
	    %%io:format("~n~n~nGOING FROM BUSY TO FREE~n~n~n"),
	    free(StoragePid)
    end.

%%--------------------------------------------------------------------
%% Function:terminate/1
%% Purpose: kill the client when the mutex shuts down
%% Args: processID of the storage
%%--------------------------------------------------------------------
terminate(StoragePid) ->
    receive
	{_Function, [_Args], ClientPid } ->
	    exit(ClientPid, kill),
	    terminate(StoragePid)
    after
	0 -> StoragePid ! stop,
	     ok
    end.

