%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(message_handler).
-export([start/2, recv_loop/2]).

start(Parent, Socket) ->
    spawn(?MODULE, recv_loop, [Parent, Socket]).

recv_loop(Parent, Socket) ->    
    case gen_tcp:recv(Socket, 0) of
	%% case {ok, Socket} of %% for testing
	{ok, <<0,0,0,0>>} ->
	    %% keep-alive: <len=0000>
	    io:format("~n~n**********keep-alive~n"),
	    Parent ! {reply, self(), "Replied"},
	    recv_loop(Parent, Socket);
	{ok, <<Len:32/integer-big, Id:8>>} ->
	    case {Len, Id} of
		{1, 0} ->
		    %% choke: <len=0001><id=0>
		    io:format("~n~n**********len=~w id=~w choke~n", [Len, Id]),
		    Parent ! {reply, self(), "Replied"};
		{1, 1} ->
		    %% unchoke: <len=0001><id=1>
		    io:format("~n~n**********len=~w id=~w unchoke~n", [Len, Id]),
		    Parent ! {reply, self(), "Replied"};
		{1, 2} ->
		    %% interested: <len=0001><id=2>
		    io:format("~n~n**********len=~w id=~w interested~n", [Len, Id]),
		    Parent ! {reply, self(), "Replied"};
		{1, 3} ->
		    %% not interested: <len=0001><id=3>
		    io:format("~n~n**********len=~w id=~w not_interested~n", [Len, Id]),
		    Parent ! {reply, self(), "Replied"}
	    end,
	    recv_loop(Parent, Socket);
	{ok, <<Len:32/integer-big, Id:8, Payload/binary>>} ->
	    case {Len, Id, Payload} of
			{5, 4, Piece_index} ->
		    %% have: <len=0005><id=4><piece index>
		    io:format("~n~n**********len=~w id=~w piece index=~w~nhave~n", 
			      [Len, Id, Piece_index]),
		    Parent ! {reply, self(), "Replied"};
		{X, 5, <<Bitfield/binary>>} ->
			    %% bitfield: <len=0001+X><id=5><bitfield>
		    io:format("~n~n**********len=1+~w id=~w bitfield=~w~nbitfield~n",
			      [X-1, Id, Bitfield]),
		    Parent ! {reply, self(), "Replied"};
		{13, 6, <<Index:32, Begin:32, Length:32>>} ->
		    %% request: <len=0013><id=6><index><begin><length>
		    io:format("~n~n**********len=~w id=~w index=~w begin=~w length=~w~nrequest~n", 
			      [Len, Id, Index, Begin, Length]),
		    Parent ! {reply, self(), "Replied"};
		{X, 7, <<Index:32, Begin:32, Block/binary>>} ->
		    %% piece: <len=0009+X><id=7><index><begin><block>
		    io:format("~n~n**********len=9+~w id=~w index=~w begin=~w block=~w~npiece~n", 
			      [X-9, Id, Index, Begin, Block]),
		    Parent ! {reply, self(), "Replied"};
		{13, 8, <<Index:32, Begin:32, Length:32>>} ->
		    %% cancel: <len=0013><id=8><index><begin><length>
		    io:format("~n~n**********len=~w id=~w index=~w begin=~w length=~w~ncancel~n", 
			      [Len, Id, Index, Begin, Length]),
		    Parent ! {reply, self(), "Replied"};
		{3, 9, <<Listen_port:16>>} ->
		    %% port: <len=0003><id=9><listen-port>
		    io:format("~n~n**********len=~w id=~w listen-port=~wport~n", 
			      [Len, Id, Listen_port]),
		    Parent ! {reply, self(), "Replied"}
	    end,
	    io:format("GOING TO RELOOP MESSAGE HANDLER@@@@@@@~n"),
	    recv_loop(Parent, Socket);
	{error, closed} ->
	    ok;
	_ ->
	    Parent ! {reply, self(), "Other"},
	    io:format("GOING TO RELOOP MESSAGE HANDLER@@@@@@@~n"),
	    recv_loop(Parent, Socket)
    end.
