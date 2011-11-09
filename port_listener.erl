%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(port_listener).
-export([listen/3, start/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port, Dl_pid, Parent) ->
    spawn(?MODULE, listen, [Port, Dl_pid, Parent]).

listen(Port, Dl_pid, Parent) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket, Dl_pid, Parent).

accept(LSocket, Dl_pid, Parent) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> recv(Socket, Dl_pid, Parent) end),
    accept(LSocket, Dl_pid, Parent).

recv(Socket, Dl_pid, Parent) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, <<Pstrlen:8/integer, 
	       Pstr:(19*8), 
	       Reserved:64, 
	       Info_hash:160,
	       Peer_id:160>>} ->
	    %% Handshake reader
	    %% --------------------------	
	    io:format("Got Handshake! Handshanke_handler started~n"),
	    Pid_h = handshake_handler:start(Dl_pid),
	    Pid_h ! {handshake, self(), Pstrlen, Pstr, Reserved, <<Info_hash:160>>, Peer_id},
	    io:format("Waiting for Handshake handler reply~n"),
	    receive 
		{reply, Pid_h, ok} ->
		    io:format("Handshake proved!!!!~n"),
		    My_peer_id = download_manager:get_my_id(Dl_pid),
		    My_info_hash = download_manager:get_my_info_hash(Dl_pid),
		    Msg = list_to_binary([<<19>>,<<"BitTorrent protocol">>, <<3,2,1,3,2,1,2,3>>, My_info_hash, list_to_binary(My_peer_id)]),
		    ok = gen_tcp:send(Socket, Msg),
		    io:format("Min handshake skickat TILLBAKA!~n"),
		    
		    %% ----------------------
		    
		    ok = peers:insert_valid_peer(Parent, Peer_id, Socket),
		    
		    message_handler:start(Dl_pid, Socket),

		    io:format("Handshaken, I'm waiting for message~n");
		    %%  receive
		    %%{reply, Pid_m, Reply} ->
		    %%io:format("MSG hdler reply: ~w~n", [Reply])
		    %%  end,
		    
		    %% -------------------------
		    %% recv_loop(Socket, Dl_pid);
		{reply, _Pid_h, drop_connection} ->
		    gen_tcp:close(Socket)
	    end;
	   
	{ok, Data} ->	
	    io:format("~nWTF IS THISSSS???????? " ++ binary_to_list(Data) ++ "~n");
	{error, Reason} ->
	    io:format("FYfAN: ~w~n", [Reason]),
	    ok
    end.

