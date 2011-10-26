%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(hshandler).
-export([start/1]).
-export([loop/1]).

start(Dl_pid) ->
    spawn(?MODULE, loop, [Dl_pid]).

loop(Dl_pid) ->
    receive
	{handshake, From, Pstrlen, Pstr, Reserved, Info_hash, Peer_id} ->
	    case download_manager:is_valid_info_hash(binary_to_list(Info_hash), Dl_pid) of
		true ->
		    io:format("mama ~n"),
		    From ! {reply, self(), ok}
	    end
    end.
