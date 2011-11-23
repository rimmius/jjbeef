%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_handler).
-export([start_link/3, send/3, done/1, error/2]).
-export([loop/5, init/3]).

start_link(Peer_mutex_pid, Socket, Peer_id) ->
    io:format("~nMsg_handler started. ~n"),
    spawn(?MODULE, init,
	  [Socket, Peer_id, Peer_mutex_pid]).

%%done
init(Socket, Peer_id, Peer_mutex_pid) ->
    io:format("~nmsg handler goes receiving!~n"),
    Msg_recver_pid = message_receiver:start(self(), Peer_mutex_pid, Socket, Peer_id),
    Msg_sender_pid = message_sender:start(self(), Socket),
    ok = message_receiver:start_receiving(Msg_recver_pid),   
    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, []).

%%%------------------------------------------------------------------
%%% Export functions
send(Pid, Type, Msg) ->
    Pid ! {start_sending, Type, Msg}.

done(Pid) ->
    Pid ! msg_done.

error(Pid, From) ->
    Pid ! {error, From}.
%%%------------------------------------------------------------------

loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Send_requests) ->
    receive
	{start_sending, Type, Msg} ->
	    loop(Socket, Peer_id, 
		 Msg_recver_pid, Msg_sender_pid,
		 [{Type, Msg} | Send_requests]);
	msg_done -> 
	    case Send_requests of 
		[] ->
		    ok = message_receiver:start_receiving(Msg_recver_pid),
		    loop(Socket, Peer_id, 
			 Msg_recver_pid, Msg_sender_pid, []);
	       [{Type, Msg} | Rest] ->
		    ok = message_sender:send(Msg_sender_pid, Type, Msg),
		    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Rest)
	    end;
	{error, Msg_recver_pid} ->
	    receive_error;
	{error, Msg_sender_pid} ->
	    send_error	
    end.
