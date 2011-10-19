%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(hshandler).
-export([start/0, loop/0]).

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    receive
	{handshake, From, Pstrlen, Pstr, Reserved, Info_hash, Peer_id} ->
	    case {is_valid_info_hash(Info_hash), is_valid_peer_id(Peer_id)} of
		{true, true} ->
		    From ! {reply, self(), ok};
		{_, _} ->
		    From ! {reply, self(), drop_connection}
	    end
    end.

is_valid_info_hash(Info_hash) ->
    true.

is_valid_peer_id(Peer_id) ->
    true.
