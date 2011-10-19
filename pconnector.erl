%%%-------------------------------------------------------------------
%%% @author  Bruce Yinhe, Fredrik Gustafsson
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(pconnector).
-export([start/0, send_mess/1, send_mess/2]).
-export([loop/1]).

start() ->
    {ok, Name} = inet:gethostname(),
    Peer_id = check_peer_id("-JB-" ++ integer_to_list(erlang:phash2({Name}))),
    register(loop, spawn(?MODULE, loop, [Peer_id])).

send_mess(Host) ->
    loop ! {peer_id, self()},
    receive
	{reply, Reply} ->
	    Reply;
	{peer_id, Peer_id} ->
	    Msg = list_to_binary([<<19>>,
				  <<"BitTorrent Protocol">>, 
				  <<32132123:64>>, 
				  <<352352:160>>,
				  binary_to_list(list_to_binary(Peer_id))]),		
	    send_to_peer(Host, Msg)
    end.

send_mess(Host, Msg) ->
    Pid = spawn(?MODULE, loop, []),
    Pid ! {send, self(), {Host, Msg}},	
    receive
	{reply, Reply} ->
	    Reply
    after
	2000 ->
	    exit(Pid, kill),
	    timeout
    end.	

send_to_peer(Host, Msg) ->
    loop ! {send, self(), {Host, Msg}},
    receive
	{reply, Reply} ->
	    Reply
    after
	2000 ->
	    %% exit(whereis(loop), kill),
	    %% {ok, Name} = inet:gethostname(),
	    %% Peer_id = check_peer_id("-JB-" ++ integer_to_list(erlang:phash2({Name}))),
	    %% register(loop, spawn(?MODULE, loop, [Peer_id])),
	    timeout
    end.
	
check_peer_id(Peer_id) ->
    case {length(Peer_id) < 20, length(Peer_id) > 20} of
	{true,false} ->
	    Zeros = 20 - length(Peer_id),
	    New_peer_id = add_to_name(Peer_id, Zeros),
	    New_peer_id;
	{false,true} ->
	    New_peer_id = string:substr(Peer_id, 1, 20),
	    New_peer_id;
	{false, false} ->
	    Peer_id
    end.
	
add_to_name(Peer_id, 0) ->
    Peer_id;
add_to_name(Peer_id, Zeros) ->
    add_to_name(Peer_id ++ integer_to_list(0), Zeros-1).
		
loop(Peer_id) ->
    receive
	{send,From, {Host, Msg}} ->
	    {ok, Sock} = gen_tcp:connect(Host, 5678, [binary, {packet, 0}]),
	    ok = gen_tcp:send(Sock, Msg),
	    From ! {reply, ok},
	    loop(Peer_id);
	{peer_id, From} ->
	    From ! {peer_id, Peer_id},
	    loop(Peer_id)
    end.
