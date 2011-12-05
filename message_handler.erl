%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_handler).
-export([start/6, send/3, done/1, error/2]).
-export([loop/6, init/6]).

start(Parent, Socket, Peer_id, 
      Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) ->
    io:format("~nMsg_handler started. ~n"),
    spawn(?MODULE, init,
	  [Parent, Socket, Peer_id, 
	   Peer_mutex_pid, Piece_mutex_pid, File_storage_pid]).

%%done
init(Parent, Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) ->
    io:format("~nmsg handler goes receiving!~n"),
    Msg_recver_pid = message_receiver:start(Parent, self(), 
					    Peer_mutex_pid,
					    Piece_mutex_pid, 
					    File_storage_pid,
					    Socket, Peer_id),
    Msg_sender_pid = message_sender:start(self(), Socket),
    ok = message_receiver:start_receiving(Msg_recver_pid),   
    link(Msg_recver_pid),
    link(Msg_sender_pid),
    loop(Parent, Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, []).

%%%------------------------------------------------------------------
%%% Export functions
send(Pid, Type, Msg) ->
    Pid ! {start_sending, Type, Msg}.

done(Pid) ->
    Pid ! msg_done.

error(Pid, From) ->
    io:format("~n~nMESSAGE IN MESSAGE_HANDLER~n~n"),
    Pid ! {error, From}.
%%%------------------------------------------------------------------

loop(Parent, Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Send_requests) ->
    receive
	{start_sending, Type, Msg} ->
	    loop(Parent, Socket, Peer_id, 
		 Msg_recver_pid, Msg_sender_pid,
		 [{Type, Msg} | Send_requests]);
	msg_done -> 
	    case Send_requests of 
		[] ->
		    ok = message_receiver:start_receiving(Msg_recver_pid),
		    loop(Parent, Socket, Peer_id, 
			 Msg_recver_pid, Msg_sender_pid, []);
	       [{Type, Msg} | Rest] ->
		    ok = message_sender:send(Msg_sender_pid, Type, Msg),
		    loop(Parent, Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Rest)
	    end;
	{error, Msg_recver_pid} ->
	    exit(Parent, kill);
	{error, Msg_sender_pid} ->
	    send_error
    end.
