-module(server).
-export([start/1]).
-export([listen/1]).
-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
%%Starts the server by register a new process and initiates the peer_id. Takes the port that is going to be used as argument.
start(Port) ->
    register(server, spawn(server, listen, [Port])).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> recv_loop(Socket) end),
    accept(LSocket).

recv_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, <<Pstrlen:8/integer, Pstr:(19*8), Reserved:64, Info_hash:160, Peer_id:160>>} ->
	    %% Handshake reader
	    %% --------------------------	
	    io:format("ptrlen: ~w~npstr: ~w~nreserved: ~w~ninfo_hash: ~w~npeer_id: ~w~n",[Pstrlen, Pstr, Reserved, Info_hash, Peer_id]),
	    Pid = handshake:start(),
	    Pid ! {handshake, self(), Pstrlen, Pstr, Reserved, Info_hash, Peer_id},
	    receive 
		{reply, ok} ->
		    spawn(message, recv, [Socket]);
		{reply, drop_connection} ->
		    gen_tcp:close(Socket)
	    end,	
	    %% -------------------------
	    recv_loop(Socket);
	{ok, Data} ->	
	    io:format(Data),
	    recv_loop(Socket);
	{error, closed} ->
	    ok
    end.
