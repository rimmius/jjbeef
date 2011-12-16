%%%---------------------------------------------------------------------
%%% Created by: Bruce Yinhe, Fredrik Gustafsson
%%% Creation date: 2011-10-18
%%%--------------------------------------------------------------------- 
%%% Description module handshake_handler
%%%--------------------------------------------------------------------- 
%%% This module takes a socket and send/receive handshake from it.
%%% The functions are called outside by other modules
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% send_handshake({ip, Host, Port}, My_info_hash, My_peer_id)
%%%
%%%   connects to a IP/Port, and send a handshake. If a socket is provided
%%%   rather than the ip/port, send a handshake directly
%%%   returns  {ok, Socket} | {error, Reason}  
%%%--------------------------------------------------------------------- 
%%% recv_handshake(Socket, My_info_hash)
%%%
%%%   receives a handshake from the socket
%%%   returns  {ok, {Socket, Peer_id}} | {error, false_info_hash}
%%%--------------------------------------------------------------------
-module(handshake_handler).

%% API
-export([send_handshake/3, recv_handshake/2]).

%%%===================================================================
%%% API
%%%===================================================================

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
    io:format("~nHANDSHAKE SENT~n"),
    case gen_tcp:send(Socket, Msg) of
	ok ->
	    {ok, Socket};	    
	{error, Reason} ->
	    {error, Reason}
    end.

recv_handshake(Socket, My_info_hash) ->
    case gen_tcp:recv(Socket, 20) of
	{ok, <<19, "BitTorrent protocol">>} ->
	    case gen_tcp:recv(Socket, 48) of
		{ok, <<_Reserved:64,
		       Info_hash:160,
		       Peer_id:160>>} ->
		    case  binary_to_list(<<Info_hash:160>>) =:= 
			binary_to_list(My_info_hash) of
			true ->
			    io:format("~nHANDSHAKE RECEIVED~n"),
			    {ok, {Socket, Peer_id}};
			false ->
			    gen_tcp:close(Socket),
			    {error, false_info_hash}
		    end
	    end;
	{ok, _Data} ->	
	    {error, unknown_data};
	{error, Reason} ->
	    {error, Reason}
    end.
