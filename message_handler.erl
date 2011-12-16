%%%---------------------------------------------------------------------
%%% Created by: Bruce Yinhe
%%% Creation date: 2011-10-18
%%%--------------------------------------------------------------------- 
%%% Description module message_handler
%%%--------------------------------------------------------------------- 
%%% This is the supervisor for message_receiver and message_sender
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start(Requester_pid, Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, 
%%%       File_storage_pid)
%%%     starts the process and returns its processID
%%%--------------------------------------------------------------------- 
%%% init(Requester_pid, Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, 
%%%      File_s%%% torage_pid)
%%%        initialize the process, starts the children and enters the loop  
%%%---------------------------------------------------------------------
%%% send(Pid, Type, Msg)
%%%      sends a sending request to msg_sender
%%%---------------------------------------------------------------------
%%% loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid)
%%%     the loop
%%%---------------------------------------------------------------------

-module(message_handler).

-export([start/6, send/3]).
-export([loop/4, init/6]).

start(Requester_pid, Socket, Peer_id, 
      Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) ->
    spawn(?MODULE, init,
	  [Requester_pid, Socket, Peer_id, 
	   Peer_mutex_pid, Piece_mutex_pid, File_storage_pid]).


send(Pid, Type, Msg) ->
    Pid ! {start_sending, Type, Msg}.


init(Requester_pid, Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, 
     File_storage_pid) ->
    process_flag(trap_exit, true),
    receive
	{uploader, Uploader_pid} ->
	    ok
    end,
    Msg_recver_pid = message_receiver:start(Requester_pid, Uploader_pid,
					    Peer_mutex_pid,
					    Piece_mutex_pid, 
					    File_storage_pid,
					    Socket, Peer_id),
    link(Msg_recver_pid),

    Msg_sender_pid = message_sender:start(Socket),
    link(Msg_sender_pid),

    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid).

loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid) ->
    receive
	{start_sending, Type, Msg} ->
	    ok = message_sender:send(Msg_sender_pid, Type, Msg),
	    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid);
	{'EXIT', _Pid, _Reason} ->
	    gen_tcp:close(Socket),
	    exit(self(), kill)
    end.
