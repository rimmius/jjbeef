%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(plistener).
-export([listen/1, start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    spawn(?MODULE, listen, [Port]).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> recv_loop(Socket) end),
    accept(LSocket).

recv_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, <<Pstrlen:8/integer, 
	       Pstr:(19*8), 
	       Reserved:64, 
	       Info_hash:160,
	       Peer_id:160>>} ->
	    %% Handshake reader
	    %% --------------------------	
	    io:format("ptrlen: ~w~npstr: ~w~nreserved: ~w~ninfo_hash: ~w~npeer_id: ~w~n",
		      [Pstrlen, Pstr, Reserved, Info_hash, Peer_id]),

	    Pid_h = hshandler:start(),
	    Pid_m = msghandler:start(),

	    Pid_h ! {handshake, self(), Pstrlen, Pstr, Reserved, Info_hash, Peer_id},
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
	    recv_loop(Socket);
	{ok, _Data} ->	
	    io:format("wrong format!~n"),
	    recv_loop(Socket);
	{error, closed} ->
	    ok
    end.

