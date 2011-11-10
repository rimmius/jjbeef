%%%Created by: Francesco Cesarini,Simon Thompson 
%%%from the book Erlang Programming page 130 
%%%Modified by: Eva-Lisa Kedborn, Jing Liu
%%%Creation date: 2011-11-08
%%%This module supervises the torrent parser module

-module(mutex).
-export([start/0, stop/0]).
-export([write/3, received/0]).
-export([init/0]).

start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! stop.

write(Index,Hash,Data)->
    mutex ! {write,Index,Hash,Data,self()},
    receive {reply,Reply} -> 
	    Reply
    end.

received() ->
    mutex ! {received, self()}, ok.

init() ->
    free().

free() ->
    receive
	{write,Index,Hash,Data,Pid}->
	    Has_inserted = temp_storage:insert(Index,Hash,Data),
	    Pid!{reply,Has_inserted},	    
	    busy(Pid);
	stop ->
	    terminate()
    end.

busy(Pid) ->
    receive
	{received, Pid} ->
	    free()
    end.

terminate() ->
    receive
	{request, Pid} ->
	    exit(Pid, kill),
	    terminate()
    after
	0 -> ok
    end.
