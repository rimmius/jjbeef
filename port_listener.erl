%%%---------------------------------------------------------------------
%%% Created by: Bruce Yinhe, Fredrik Gustafsson
%%% Creation date: 2011-10-18
%%%--------------------------------------------------------------------- 
%%% Description module port_listener
%%%--------------------------------------------------------------------- 
%%% Opens a port and listens to the port and accepts a socket and
%%% receives a handshake from the socket.
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start(Port, Dl_pid, Parent)
%%%   starts the process
%%%   returns a Pid
%%%--------------------------------------------------------------------- 
%%% listen(Port, Dl_pid, Parent)
%%%   listens to a port, gets a listen socket and accept it
%%%---------------------------------------------------------------------

-module(port_listener).

%% API
-export([start/3]).
%%Internal function
-export([listen/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%%===================================================================
%%% API
%%%===================================================================

start(Port, Dl_pid, Parent) ->
    spawn(?MODULE, listen, [Port, Dl_pid, Parent]).

listen(Port, Dl_pid, Parent) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket, Dl_pid, Parent).

%%%===================================================================
%%% internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Function: accept/3
%% Purpose:  accepts a listen socket, receive from it, and continue to 
%%           accept the next listen socket
%% Args: LSocket, Dl_pid, Parent
%%--------------------------------------------------------------------

accept(LSocket, Dl_pid, Parent) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> recv(Socket, Dl_pid, Parent) end),
    accept(LSocket, Dl_pid, Parent).

%%--------------------------------------------------------------------
%% Function: recv/3
%% Purpose: receive the handshake from a socket, send a handshake back, 
%%          if it is approved, add the peer to the valid list
%% Args: Socket, Dl_pid, Parent
%% Returns: ok | {error, Reason}
%%--------------------------------------------------------------------

recv(Socket, Dl_pid, Parent) ->
    My_peer_id = download_manager:get_my_id(Dl_pid),
    My_info_hash = download_manager:get_my_info_hash(Dl_pid),

    case handshake_handler:recv_handshake(Socket, My_info_hash) of
	{ok, {Socket, Peer_id}} ->
	    case handshake_handler:send_handshake({socket, Socket}, 
						  My_info_hash, My_peer_id) of
		{ok, Socket} ->
		   case peers:insert_valid_peer(Parent, Peer_id, Socket) of
		       ok -> ok;
		       {error, Reason} -> {error, Reason}
		   end;
		{error, Reason} -> 
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.
