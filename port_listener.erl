%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(port_listener).
-export([listen/2, start/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port, Dl_pid) ->
    spawn(?MODULE, listen, [Port, Dl_pid]).

listen(Port, Dl_pid) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket, Dl_pid).

accept(LSocket, Dl_pid) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> recv_loop(Socket, Dl_pid) end),
    accept(LSocket, Dl_pid).

recv_loop(Socket, Dl_pid) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, <<Pstrlen:8/integer, 
	       Pstr:(19*8), 
	       Reserved:64, 
	       Info_hash:160,
	       Peer_id:160>>} ->
	    %% Handshake reader
	    %% --------------------------	
	    
	    Pid_h = handshake_handler:start(Dl_pid),
	    Pid_m = message_handler:start(),
	    Pid_h ! {handshake, self(), Pstrlen, Pstr, Reserved, <<Info_hash:160>>, Peer_id},
	    receive 
		{reply, Pid_h, ok} ->
		    Pid_m ! {message, self(), Socket};	   
		{reply, Pid_h, drop_connection} ->
		    gen_tcp:close(Socket)
	    end,	
	    io:format("Handshaken, I'm waiting for message~n"),
	    receive
		{reply, Pid_m, Reply} ->
		    io:format("~w~n", [Reply])
	    end,
	    %% -------------------------
	    recv_loop(Socket, Dl_pid);
	{ok, Data} ->	
	    io:format(binary_to_list(Data) ++ "~n"),
	    recv_loop(Socket, Dl_pid);
	{error, closed} ->
	    ok
    end.

