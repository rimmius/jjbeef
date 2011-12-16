%%%---------------------------------------------------------------------
%%% Created by: Bruce Yinhe
%%% Creation date: 2011-10-18
%%%--------------------------------------------------------------------- 
%%% Description module message_receiver
%%%--------------------------------------------------------------------- 
%%% This module receives messages from the socket and pass it to 
%%% message_reader
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start(Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, 
%%%       File_storage_pid, Socket, Peer_id)
%%%     starts the process and returns its processID
%%%--------------------------------------------------------------------- 
%%% init(Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, 
%%%     File_storage_pid, Socket, Peer_id)     
%%%     initializes the process
%%%---------------------------------------------------------------------
%%% loop(Socket, Msg_reader_pid)
%%%      the loop
%%%---------------------------------------------------------------------

-module(message_receiver).

%% API
-export([start/7]).

%% internal function
-export([init/7, loop/2]).

%%%===================================================================
%%% API
%%%===================================================================

start(Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, 
      File_storage_pid, Socket, Peer_id) ->
    spawn(?MODULE, init, [Requester_pid, Uploader_pid, Peer_mutex_pid,
			  Piece_mutex_pid, File_storage_pid, Socket, Peer_id]).

%%%===================================================================
%%% internal functions
%%%===================================================================


init(Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, 
     File_storage_pid, Socket, Peer_id) ->
    process_flag(trap_exit, true),
    Msg_reader_pid = message_reader:start(Requester_pid, Uploader_pid, 
					  Peer_mutex_pid, Piece_mutex_pid, 
					  File_storage_pid,Peer_id),
    link(Msg_reader_pid),
    loop(Socket, Msg_reader_pid).


loop(Socket, Msg_reader_pid) ->
    case do_recv(Socket, Msg_reader_pid) of
	{ok, done} ->
	    loop(Socket, Msg_reader_pid);
	{error, Reason} ->
	    exit(self(), Reason)		
    end.

%%--------------------------------------------------------------------
%% Function:do_recv/2
%% Purpose: start receiving from the socket
%% Agrs: Socket,processID of msg_reader
%% Returns: {ok, done} | {error, Reason}
%%--------------------------------------------------------------------
do_recv(Socket, Msg_reader_pid) ->    
    case gen_tcp:recv(Socket, 4, 240000) of
	{ok, <<0,0,0,0>>} ->
	    %% keep-alive,
	    message_reader:read_msg(Msg_reader_pid, keep_alive, []),
	    {ok, done};
	{ok, <<0,0,0,1>>} ->
	    case gen_tcp:recv(Socket, 1) of
		{ok, <<0>>} ->
		    %%choke
		    message_reader:read_msg(Msg_reader_pid, am_choked, [1]),
		    {ok, done};
		{ok, <<1>>} ->
		    %% unchoke
		    message_reader:read_msg(Msg_reader_pid, am_choked, [0]),
		    {ok, done};
		{ok, <<2>>} ->
		    %% interested
		    message_reader:read_msg(Msg_reader_pid, is_interested, [1]),
		    {ok, done};
		{ok, <<3>>} ->
		    %% uninterested
		    message_reader:read_msg(Msg_reader_pid, is_interested, [0]),
		    {ok, done};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{ok, <<0,0,0,5>>} ->
	    case gen_tcp:recv(Socket, 5) of
		{ok, <<4, Piece_index:32>>} ->
		    %%have
		    message_reader:read_msg(Msg_reader_pid, have, 
					    [Piece_index]),
		    {ok, done};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{ok, <<0,0,0,13>>} ->
	    case gen_tcp:recv(Socket, 13) of
		{ok, <<6, Index:32, Begin:32, Length:32>>} ->
		    %%request
		    message_reader:read_msg(Msg_reader_pid, request, 
					    [Index, Begin, Length]),
		    {ok, done};
		{ok, <<8, Index:32, Begin:32, Length:32>>} ->
		    %%cancel
		    message_reader:read_msg(Msg_reader_pid, cancel, 
					    [Index, Begin, Length]),
		    {ok, done};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{ok, <<0,0,0,3>>} ->
	    case gen_tcp:recv(Socket, 3) of
		{ok, <<9, Listen_port:16>>} ->
		    %%port
		    message_reader:read_msg(Msg_reader_pid, port,
					    [Listen_port]),
		    {ok, done};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{ok, <<Len:32/integer-big>>} ->
	    Bitfield_len = Len*8-8, Block_len = Len*8-72,
	    case gen_tcp:recv(Socket, Len) of
		{ok, <<5, Bitfield:Bitfield_len>>} ->
		    %%bitfield
		    message_reader:read_msg(Msg_reader_pid, bitfield,
					    [Bitfield, Bitfield_len]),
		    {ok, done};
		{ok, <<7, Index:32, Begin:32, Block:Block_len>>} ->
		    %%piece
		    message_reader:read_msg(Msg_reader_pid, piece,
					    [Index, Begin, Block, Block_len]),
		    {ok, done};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{ok, _Data} ->
	    {error, unknown_data};
	{error, Reason} ->
	    {error, Reason};	    
	_ ->
	    {error, other}
    end.

