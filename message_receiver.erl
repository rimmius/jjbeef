%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_receiver).
-export([start/8, start_receiving/1]).
-export([init/8, loop/3]).

start(Requester_pid, Uploader_pid, Parent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid,
      Socket, Peer_id) ->
    spawn(?MODULE, init, [Requester_pid, Uploader_pid, Parent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid,
			       Socket, Peer_id]).

init(Requester_pid, Uploader_pid, Parent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid,
     Socket, Peer_id) ->
    process_flag(trap_exit, true),
    Msg_reader_pid = message_reader:start(Requester_pid, Uploader_pid, 
					  Peer_mutex_pid, Piece_mutex_pid, File_storage_pid,
					  Peer_id),
    link(Msg_reader_pid),
    loop(Parent, Socket, Msg_reader_pid).

start_receiving(Pid) ->
    Pid ! start_receiving,
    ok.

loop(Parent, Socket, Msg_reader_pid) ->
    receive
	start_receiving ->
	    do_recv(Parent, Socket, Msg_reader_pid),
	    loop(Parent, Socket, Msg_reader_pid);
	{'EXIT', Pid, Reason} ->
	    gen_tcp:close(Socket),
	    message_handler:error(Parent, self())
    end.

do_recv(Parent, Socket, Msg_reader_pid) ->    
    case gen_tcp:recv(Socket, 4, 240000) of
	{ok, <<0,0,0,0>>} ->
	    %% keep-alive
	    io:format("~n~n*****~w*****keep-alive len=0 ~n", [self()]),
	    message_reader:read_msg(Msg_reader_pid, keep_alive, []),
	    message_handler:done(Parent);
	%% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,1>>} ->
	    case gen_tcp:recv(Socket, 1) of
		{ok, <<0>>} ->
		    %%choke
		    io:format("~n*****~w*****Choke len=1, id=0~n", [self()]),
		    message_reader:read_msg(Msg_reader_pid, am_choked, [1]),
		    message_handler:done(Parent);
		{ok, <<1>>} ->
		    %% unchoke
		    io:format("~n*****~w*****Unchoke len=1, id=1 ~n", [self()]),
		    message_reader:read_msg(Msg_reader_pid, am_choked, [0]),
		    message_handler:done(Parent);
		{ok, <<2>>} ->
		    %% interested
		    io:format("~n*****Interested len=1, id=2~n"),
		    message_reader:read_msg(Msg_reader_pid, is_interested, [1]),
		    message_handler:done(Parent);
		{ok, <<3>>} ->
		    %% uninterested
		    io:format("~n*****Uninterested len=1, id=3~n"),
		    message_reader:read_msg(Msg_reader_pid, is_interested, [0]),
		    message_handler:done(Parent);
		{error, Reason} ->
		    io:format("~n*****~w*****HALFWAY message receiving error: ~w~n", [self(), Reason]),
		    message_handler:error(Parent, self())
	    end;
	%% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,5>>} ->
	    case gen_tcp:recv(Socket, 5) of
		{ok, <<4, Piece_index:32>>} ->
		    %%have
		    message_reader:read_msg(Msg_reader_pid, have, [Piece_index]),
		    message_handler:done(Parent);
		{error, Reason} ->
		    io:format("~n*****~w*****HALFWAY message receiving error: ~w~n", [self(), Reason]),
		    message_handler:error(Parent, self())
	    end;
	%% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,13>>} ->
	    case gen_tcp:recv(Socket, 13) of
		{ok, <<6, Index:32, Begin:32, Length:32>>} ->
		    %%request
		    io:format("~n*****~w*****Request len=13, id=6, index=~w, begin=~w, length=~w~n", 
			      [self(), Index, Begin, Length]),
		    message_reader:read_msg(Msg_reader_pid, request, [Index, Begin, Length]),
		    message_handler:done(Parent);
		{ok, <<8, Index:32, Begin:32, Length:32>>} ->
		    %%cancel
		    io:format("~n*****~w*****Cancel len=13, id=8, index=~w, begin=~w, length=~w~n", 
			      [self(), Index, Begin, Length]),
		    message_reader:read_msg(Msg_reader_pid, cancel, [Index, Begin, Length]),
		    message_handler:done(Parent);
		{error, Reason} ->
		    io:format("~n*****~w*****HALFWAY message receiving error: ~w~n", [self(), Reason]),
		    message_handler:error(Parent, self())
	    end;
				  %% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,3>>} ->
	    case gen_tcp:recv(Socket, 3) of
		{ok, <<9, Listen_port:16>>} ->
		    %%port
		    io:format("~n*****~w*****Port len=3, id=9, listen_port=~w ~n", 
			      [self(), Listen_port]),
		    message_reader:read_msg(Msg_reader_pid, port, [Listen_port]),
		    message_handler:done(Parent);
		{error, Reason} ->
		    io:format("~n*****~w*****HALFWAY message receiving error: ~w~n", [self(), Reason]),
		    message_handler:error(Parent, self())
	    end;
	   %% do_recv(Socket, Pid_message_reader);
	{ok, <<Len:32/integer-big>>} ->
	    Bitfield_len = Len*8-8,
	    Block_len = Len*8-72,
	    case gen_tcp:recv(Socket, Len) of
		{ok, <<5, Bitfield:Bitfield_len>>} ->
		    %%bitfield
		    io:format("~n*****~w*****bitfield len=1+~w, id=5, bitfield=~w ~n", 
			      [self(), Len-1, Bitfield]),
		    message_reader:read_msg(Msg_reader_pid, bitfield, [Bitfield, Bitfield_len]),
		    message_handler:done(Parent);
		{ok, <<7, Index:32, Begin:32, Block:Block_len>>} ->
		    %%piece
		    io:format("~n*****~w*****piece len=9+~w, id=7, index=~w, begin=~w~n", 
			      [self(), Len-9, Index, Begin]),
		    io:format("~n~n~n~nPIEEEEECEEEEEEE~n~n~n"),
		    message_reader:read_msg(Msg_reader_pid, piece, [Index, Begin, Block, Block_len]),
		    message_handler:done(Parent);
		{error, Reason} ->
		    io:format("~n*****~w*****HALFWAY message receiving error: ~w~n", [self(), Reason]),
		    message_handler:error(Parent, self())
	    end;
	    %%do_recv(Socket, Pid_message_reader);
	{ok, _Data} ->
	    io:format("~nother messages, cannot read~n"),
	    message_handler:error(Parent, self());
	    %%do_recv(Socket, Pid_message_reader); %% not sure
	{error, Reason} ->
	    io:format("~n*****~w*****INITIAL message receiving error: ~w~n", [self(), Reason]),
	    message_handler:error(Parent, self());	    
	_ ->
	    io:format("WWWWWWTTTTTFFFFF FYYYYYYYYYYYY FAANANAFNAFNAFNAFANFANNA~n"),
	    message_handler:error(Parent, self())
    end.
