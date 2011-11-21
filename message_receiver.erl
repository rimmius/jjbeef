%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_receiver).
-export([start/4]).
-export([init/4]).

start(Parent, Piece_mutex_pid, 
      Socket, Peer_id) ->
    spawn(?MODULE, init, [Parent, Piece_mutex_pid, 
			  Socket, 
			  Peer_id]).

init(Parent, Piece_mutex_pid, 
     Socket, Peer_id) ->
    Msg_reader_pid = message_reader:start(Piece_mutex_pid, Peer_id),
    do_recv(Parent, Socket, Msg_reader_pid).

do_recv(Parent, Socket, Msg_reader_pid) ->    
    case gen_tcp:recv(Socket, 4) of
	{ok, <<0,0,0,0>>} ->
	    %% keep-alive
	    io:format("~n~n**********keep-alive len=0 ~n"),
	    message_handler:done(Parent);
	%% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,1>>} ->
	    case gen_tcp:recv(Socket, 1) of
		{ok, <<0>>} ->
		    %%choke
		    io:format("~n*****Choke len=1, id=0~n"),
		    Msg_reader_pid ! {choke, 1};
		{ok, <<1>>} ->
		    %% unchoke
		    io:format("~n*****~w*****Unchoke len=1, id=1 ~n", [self()]),
		    Msg_reader_pid ! {choke, 0};
		{ok, <<2>>} ->
		    %% interested
		    io:format("~n*****Interested len=1, id=2~n"),
		    Msg_reader_pid ! {interested, 1};
		{ok, <<3>>} ->
		    %% uninterested
		    io:format("~n*****Uninterested len=1, id=3~n"),
		    Msg_reader_pid ! {interested, 0}
	    end,	    
	    message_handler:done(Parent);
	%% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,5>>} ->
	    case gen_tcp:recv(Socket, 5) of
		{ok, <<4, Piece_index:32>>} ->
		    %%have
		    io:format("~n*****~w*****Have len=5, id=4, piece_index=~w ~n", 
			      [self(), Piece_index]),
		    Msg_reader_pid ! {have, Piece_index}
	    end,	    
	    message_handler:done(Parent);
	%% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,13>>} ->
	    case gen_tcp:recv(Socket, 13) of
		{ok, <<6, Index:32, Begin:32, Length:32>>} ->
		    %%request
		    io:format("~n*****Request len=13, id=6, index=~w, begin=~w, length=~w~n", 
			      [Index, Begin, Length]);
		{ok, <<8, Index:32, Begin:32, Length:32>>} ->
		    %%cancel
		    io:format("~n*****Cancel len=13, id=8, index=~w, begin=~w, length=~w~n", 
			      [Index, Begin, Length])
	    end,	    
	    message_handler:done(Parent);
				  %% do_recv(Socket, Pid_message_reader);
	{ok, <<0,0,0,3>>} ->
	    case gen_tcp:recv(Socket, 3) of
		{ok, <<9, Listen_port:16>>} ->
		    %%port
		    io:format("~n*****Port len=3, id=9, listen_port=~w msg_handler: ~w~n", 
			      [Listen_port, self()]),
		    Msg_reader_pid ! {port, Listen_port}
	    end,
	    message_handler:done(Parent);
	   %% do_recv(Socket, Pid_message_reader);
	{ok, <<Len:32/integer-big>>} ->
	    Bitfield_len = Len*8-8,
	    Block_len = Len*8-72,
	    case gen_tcp:recv(Socket, Len) of
		{ok, <<5, Bitfield:Bitfield_len>>} ->
		    %%bitfield
		    io:format("~n*****bitfield len=1+~w, id=5, bitfield=~w msg_handler: ~w~n", 
			      [Len-1, Bitfield, self()]),
		   Msg_reader_pid ! {bitfield, Bitfield, Bitfield_len};
		{ok, <<7, Index:32, Begin:32, Block:Block_len>>} ->
		    %%piece
		    io:format("~n*****piece len=9+~w, id=7, index=~w, begin=~w, block=~w~n", 
			      [Len-9, Index, Begin, Block])
	    end,	    
	    message_handler:done(Parent);
	    %%do_recv(Socket, Pid_message_reader);
	{ok, _Data} ->
	    io:format("~nother messages, cannot read~n"),
	    message_handler:done(Parent);
	    %%do_recv(Socket, Pid_message_reader); %% not sure
	{error, Reason} ->
	    io:format("~nmessage receiving error: ~w~n", [Reason]);
	_ ->
	    io:format("WWWWWWTTTTTFFFFF FYYYYYYYYYYYY FAANANAFNAFNAFNAFANFANNA~n")
    end.
