%%% Created by: Francesco Cesarini, Simon Thompson 
%%% from the book Erlang Programming page 130 
%%% Modified by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-08
%%% This module isolates process access to db storage to ensure data safety

-module(mutex).
-export([start/2, stop/1]).
-export([init/2, received/1, request/3]).

%%--------------------------------------------------------------------
%% Function: start/2
%% Purpose: spawns the storage modules
%% Args: the specific storage module, arguments if necessary
%%--------------------------------------------------------------------

start(Module, Args) ->
    spawn(?MODULE, init, [Module, Args]).


%%--------------------------------------------------------------------
%% Function:init/2
%% Purpose: returns the pid of the storage being spawned
%% Args:the specific storage module, arguments if necessary
%% Returns: The process id of the storage module being spawned
%%--------------------------------------------------------------------

init(Module, Args) ->
    StoragePid = apply(Module, start, Args), 
%% changed this to apply to make it easier to apply the arguments of the
%% start function.
    link(StoragePid),
    free(StoragePid).


%%--------------------------------------------------------------------
%% Function:stop/1
%% Purpose: to stop this mutex process
%% Args: process id of the mutex
%%--------------------------------------------------------------------

stop(MutexPid) ->
    MutexPid ! stop.

%%--------------------------------------------------------------------
%% Function:received/1
%% Purpose: set the mutex state to free again when the client has received
%%          reply for the request
%% Args: process id of the mutex
%%--------------------------------------------------------------------

received(MutexPid)->
    MutexPid ! {received, self()},
    ok.


%%--------------------------------------------------------------------
%% Function: request/3
%% Purpose: Client uses this function to send request and gain access to
%%           the storages.
%% Args: process id of the mutex, what function to execute, arguments of
%%       this function
%% Returns: the requested infomation
%%--------------------------------------------------------------------

request(MutexPid, Function, Args) ->
    MutexPid ! {Function, Args, self()},
    receive 
	{reply, Reply} -> 
	    Reply 
    end.


%%--------------------------------------------------------------------
%% Function: free/1
%% Purpose: receive requests and send the requests to the storage that
%%          the client intends to have access to.state converts from free
%%          to busy.
%% Agrs: the process id of the storage being accessed
%% returns: the requested infomation
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
%% Function: busy/2
%% Purpose: set the state of the mutex from free to busy
%% Args: process id of the client, process id of the storage being accessed
%%--------------------------------------------------------------------

busy(ClientPid, StoragePid) ->
    receive
	{received, ClientPid} ->
	    %%io:format("~n~n~nGOING FROM BUSY TO FREE~n~n~n"),
	    free(StoragePid)
    end.


%%--------------------------------------------------------------------
%% Function: terminate/1
%% Purpose: terminates the client when a stop message is received
%% Args: the process id of the storage being accessed
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

