%%% Created by: Francesco Cesarini, Simon Thompson 
%%% from the book Erlang Programming page 130 
%%% Modified by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-08
%%% This module isolates process access to db storage to ensure data safety

-module(mutex).
-export([start/2, stop/1]).
-export([init/2, received/1, request/3]).
%%changed this to two-arguments, if the start function takes arguments
start(Module, Args) ->
    spawn(?MODULE, init, [Module, Args]).

init(Module, Args) ->
    StoragePid = apply(Module, start, Args), %%changed this to apply to make it easier to apply the arguments of the start function.
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

busy(ClientPid, StoragePid) ->
    receive
	{received, ClientPid} ->
	    %%io:format("~n~n~nGOING FROM BUSY TO FREE~n~n~n"),
	    free(StoragePid)
    end.

terminate(StoragePid) ->
    receive
	{_Function, [_Args], ClientPid } ->
	    exit(ClientPid, kill),
	    terminate(StoragePid)
    after
	0 -> StoragePid ! stop,
	     ok
    end.

