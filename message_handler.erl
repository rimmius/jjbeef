%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_handler).
-behaviour(gen_fsm).
-export([start_link/3, send/3, done/1]).
-export([idle/2, busy/2, init/1, handle_event/3, handle_info/3]).

start_link(Piece_mutex_pid, Socket, Peer_id) ->
    io:format("~nFSM STAREDDDDDD~n"),
    gen_fsm:start_link(?MODULE, 
		       {Piece_mutex_pid, Socket, Peer_id},
		       []).

init({Piece_mutex_pid, Socket, Peer_id}) ->
    io:format("~ngoing to idle state~n"),
    {ok, idle, {Piece_mutex_pid, Socket, Peer_id}}.

send(Pid, Type, Msg) ->
    gen_fsm:send_all_state_event(Pid, {start_sending, Type, Msg}).

done(Pid) ->
    gen_fsm:send_event(Pid, {msg_done, self()}).

handle_event({start_sending, Type, Msg}, idle, 
	     {Piece_mutex_pid, Socket, Peer_id}) -> 
    Msg_sender_pid = message_sender:start(self(), Socket, Type, Msg),
    {next_state, busy, {Piece_mutex_pid, Socket, Peer_id, Msg_sender_pid,
			[]}};
handle_event({start_sending, Type, Msg}, busy, 
	     {Piece_mutex_pid, Socket, Peer_id, Msg_pid, Send_requests}) ->     
    {next_state, busy, {Piece_mutex_pid, Socket, Peer_id, Msg_pid,
			[{Type, Msg} | Send_requests]}}.

handle_info({'EXIT', Msg_pid, _Reason}, busy,
	    {Piece_mutex_pid, Socket, Peer_id, Msg_pid, Send_requests}) ->
    case Send_requests of 
	[] ->
	    {next_state, idle, {Piece_mutex_pid, Socket, Peer_id}, 1000};
	[{Type, Msg} | Rest] ->
	    New_sender = message_sender:start(self(), Socket, Type, Msg),
	    {next_state, busy, {Piece_mutex_pid, Socket, Peer_id, New_sender, 
				Rest}}
    end.

idle(timeout, {Piece_mutex_pid, Socket, Peer_id}) ->    
    Msg_recver_pid = message_receiver:start(self(), Piece_mutex_pid, Socket, Peer_id),
    {next_state, busy, {Piece_mutex_pid, Socket, Peer_id, Msg_recver_pid, 
			[]}}.

busy({msg_done, Msg_pid}, 
     {Piece_mutex_pid, Socket, Peer_id, Msg_pid, Send_requests}) ->
    case Send_requests of 
	[] ->
	    {next_state, idle, {Piece_mutex_pid, Socket, Peer_id}, 1000};
	[{Type, Msg} | Rest] ->
	    Msg_sender_pid = message_sender:start(self(), Socket, Type, Msg),
	    {next_state, busy, {Piece_mutex_pid, Socket, Peer_id, Msg_sender_pid, 
				Rest}}
    end.
