%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%% This is the supervisor for message_receiver and message_sender
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_handler).

%% API
-export([start/6, send/3]).

%% internal functions
-export([loop/4, init/6]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc 
%% starts the process
%%                                           
%% @spec start(Requester_pid, Socket, Peer_id, 
%%      Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) -> Pid
%% @end
%%--------------------------------------------------------------------
start(Requester_pid, Socket, Peer_id, 
      Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) ->
    spawn(?MODULE, init,
	  [Requester_pid, Socket, Peer_id, 
	   Peer_mutex_pid, Piece_mutex_pid, File_storage_pid]).

%%--------------------------------------------------------------------
%% @doc 
%% sends a sending request to msg_sender
%%                                           
%% @spec send(Pid, Type, Msg) -> ok
%% @end
%%--------------------------------------------------------------------
send(Pid, Type, Msg) ->
    Pid ! {start_sending, Type, Msg}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% initialize the process, starts the children and enters the loop
%%
%% @spec init(Requester_pid, Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) -> void()
%% @end
%%--------------------------------------------------------------------
init(Requester_pid, Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid) ->
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

%%--------------------------------------------------------------------
%% @doc
%% the loop
%%
%% @spec loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid, 
%%            Send_requests) -> void()
%% @end
%%--------------------------------------------------------------------
loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid) ->
    receive
	{start_sending, Type, Msg} ->
	    ok = message_sender:send(Msg_sender_pid, Type, Msg),
	    loop(Socket, Peer_id, Msg_recver_pid, Msg_sender_pid);
	{'EXIT', _Pid, _Reason} ->
	    gen_tcp:close(Socket),
	    exit(self(), kill)
    end.
