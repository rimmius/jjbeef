%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(port_listener).

%% API
-export([start/3]).
%%Internal function
-export([listen/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% starts the process
%%
%% @spec start(Port, Dl_pid, Parent) -> Pid                          
%% @end
%%--------------------------------------------------------------------
start(Port, Dl_pid, Parent) ->
    spawn(?MODULE, listen, [Port, Dl_pid, Parent]).

%%%===================================================================
%%% internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% listens to a port, gets a listen socket and accept it
%%
%% @spec listen(Port, Dl_pid, Parent) ->  void()
%% @end
%%--------------------------------------------------------------------
listen(Port, Dl_pid, Parent) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket, Dl_pid, Parent).

%%--------------------------------------------------------------------
%% @doc
%% accepts a listen socket, receive from it, and continue to accept the 
%% next listen socket
%%
%% @spec accept(LSocket, Dl_pid, Parent) -> void()
%% @end
%%--------------------------------------------------------------------
accept(LSocket, Dl_pid, Parent) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> recv(Socket, Dl_pid, Parent) end),
    accept(LSocket, Dl_pid, Parent).


%%--------------------------------------------------------------------
%% @doc
%% receive the handshake from a socket, send a handshake back, if it is 
%% approved, add the peer to the valid list
%%
%% @spec recv(Socket, Dl_pid, Parent) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
recv(Socket, Dl_pid, Parent) ->
    My_peer_id = download_manager:get_my_id(Dl_pid),
    My_info_hash = download_manager:get_my_info_hash(Dl_pid),

    case handshake_handler:recv_handshake(Socket, My_info_hash) of
	{ok, {Socket, Peer_id}} ->
	    case handshake_handler:send_handshake({socket, Socket}, My_info_hash, My_peer_id) of
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
