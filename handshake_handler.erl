%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%% This module takes a socket and send/receive handshake from it.
%%% The functions are called outside by other modules
%%% 
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(handshake_handler).

%% API
-export([send_handshake/3, recv_handshake/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% connects to a IP/Port, and send a handshake. If a socket is provided
%% rather than the ip/port, send a handshake directly
%%
%% @spec send_handshake(Option, My_info_hash, My_peer_id) -> 
%%                                   {ok, Socket} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
send_handshake({ip, Host, Port}, My_info_hash, My_peer_id) ->    
    case gen_tcp:connect(Host, Port, [binary, {active, false},
				      {packet, 0}], 1000) of
	{ok, Socket} ->	   	   
	    send_handshake({socket, Socket}, My_info_hash, My_peer_id);
	{error, Reason} ->
	    {error, Reason}
    end;
send_handshake({socket, Socket}, My_info_hash, My_peer_id) ->
    Msg = list_to_binary([<<19>>,<<"BitTorrent protocol">>,
			  <<3,2,1,3,2,1,2,3>>, My_info_hash,
			  list_to_binary(My_peer_id)]),
    case gen_tcp:send(Socket, Msg) of
	ok ->
	    {ok, Socket};	    
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc
%% receives a handshake from the socket
%%
%% @spec recv_handshake(Socket, My_info_hash) -> 
%%                 {ok, {Socket, Peer_id}} | {error, false_info_hash}
%% @end
%%--------------------------------------------------------------------
recv_handshake(Socket, My_info_hash) ->
    case gen_tcp:recv(Socket, 20) of
	{ok, <<19, "BitTorrent protocol">>} ->
	    case gen_tcp:recv(Socket, 48) of
		{ok, <<_Reserved:64,
		       Info_hash:160,
		       Peer_id:160>>} ->
		    case  binary_to_list(<<Info_hash:160>>) =:= binary_to_list(My_info_hash) of
			true ->
			    {ok, {Socket, Peer_id}};
			false ->
			    gen_tcp:close(Socket),
			    {error, false_info_hash}
		    end
	    end;
	{ok, Data} ->	
	    io:format("recv handshake unknown data: ~w~n", [Data]),
	    {error, unknown_data};
	{error, Reason} ->
	    io:format("recv handshake error: ~w~n", [Reason]),
	    {error, Reason}
    end.
