%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_handler).
-export([start/6, send/3, done/1, error/2, close_socket/1]).
-export([loop/5, init/6]).

start(Requester_pid, Socket, Peer_id, 
      Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) ->
    spawn(?MODULE, init,
	  [Requester_pid, Socket, Peer_id, 
	   Peer_mutex_pid, Piece_mutex_pid, File_storage_pid]).

%%done
init(Requester_pid, Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) ->
    process_flag(trap_exit, true),
    receive
	{uploader, Uploader_pid} ->
	    ok
    end,
    Msg_recver_pid = message_receiver:start(Requester_pid, Uploader_pid, self(), 
					    Peer_mutex_pid,
					    Piece_mutex_pid, 
					    File_storage_pid,
					    Socket, Peer_id),
    link(Msg_recver_pid),

    Msg_sender_pid = message_sender:start(self(), Socket),
    link(Msg_sender_pid),

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

close_socket(Pid) ->
    Pid ! {close_socket, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.
%%%------------------------------------------------------------------

loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Send_requests) ->
    receive
	{start_sending, Type, Msg} ->
	    ok = message_sender:send(Msg_sender_pid, Type, Msg),
	    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Send_requests);
%% 	    loop(Socket, Peer_id, 
%% 		 Msg_recver_pid, Msg_sender_pid,
%% 		 [{Type, Msg} | Send_requests]);
	msg_done -> 
	    ok = message_receiver:start_receiving(Msg_recver_pid),
	    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Send_requests);
%% 	    case Send_requests of 
%% 		[] ->
%% 		    ok = message_receiver:start_receiving(Msg_recver_pid),
%% 		    loop(Socket, Peer_id, 
%% 			 Msg_recver_pid, Msg_sender_pid, []);
%% 	       [{Type, Msg} | Rest] ->
%% 		    ok = message_sender:send(Msg_sender_pid, Type, Msg),
%% 		    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, Rest)
%% 	    end;
	{close_socket, From} ->	    
	    From ! {reply, gen_tcp:close(Socket)};
	{error, Msg_recver_pid} ->
	    gen_tcp:close(Socket),
	    exit(self(), kill);
	{error, Msg_sender_pid} ->
	    gen_tcp:close(Socket),
	    exit(self(), kill);
	{'EXIT', _Pid, _Reason} ->
	    gen_tcp:close(Socket),
	    exit(self(), kill)
    end.
