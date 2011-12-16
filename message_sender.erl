%%%---------------------------------------------------------------------
%%% Created by: Bruce Yinhe
%%% Creation date: 2011-10-28
%%%--------------------------------------------------------------------- 
%%% Description module message_sender
%%%--------------------------------------------------------------------- 
%%% This module sends messages to the socket
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start(Socket)
%%%   starts the process
%%%   returns Pid
%%%--------------------------------------------------------------------- 
%%% init(Pid, Type, Message)
%%%   sends a message
%%%   returms ok 
%%%---------------------------------------------------------------------

-module(message_sender).

%% API
-export([start/1, send/3]).

%% internal functions
-export([loop/1, do_send/3]).

%%%===================================================================
%%% API
%%%===================================================================

start(Socket) ->
    spawn(?MODULE, loop, [Socket]).

send(Pid, Type, Msg) ->
    Pid ! {do_send, Type, Msg, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Function: loop/1
%% Purpose: the loop 
%% Args: Socket 
%%--------------------------------------------------------------------

loop(Socket) ->
    receive
	{do_send, Type, Msg, From} ->
	    case do_send(Socket, Type, Msg) of
		ok ->
		    From ! {reply, ok},
		    loop(Socket);
		{error, Reason} -> 
		    From ! {reply, ok},
		    exit(self(), Reason)
	    end
    end.

%%--------------------------------------------------------------------
%% Function: do_send/3
%% Purpose: sends the message to the socket 
%% Args: Socket, Type, Msg
%% Returns: ok | {error, Reason}
%%--------------------------------------------------------------------

do_send(Socket, Type, Msg) ->
    case {Type, Msg} of 
	{keep_alive, _} ->
	    gen_tcp:send(Socket, <<0,0,0,0>>);
	{choke, _} ->
	    gen_tcp:send(Socket, <<0,0,0,1,0>>);
	{unchoke, _} ->
	    gen_tcp:send(Socket, <<0,0,0,1,1>>);
	{am_interested, true} ->
	    gen_tcp:send(Socket, <<0,0,0,1,2>>);
	{am_interested, false} ->
	    gen_tcp:send(Socket, <<0,0,0,1,3>>);
	{have, Piece_index} ->
	    gen_tcp:send(Socket, <<0,0,0,5,4, Piece_index:32/integer-big>>);
	{bitfield, Bitfield_in_list} ->
	    io:format("~nBITFIELD SENT~n"),
	    gen_tcp:send(Socket, handle_bitfield(Bitfield_in_list));
	{request, [Index, Begin, Length]} ->
	    gen_tcp:send(Socket, <<0,0,0,13,6, 
				Index:32/integer-big,
				Begin:32/integer-big,
				Length:32/integer-big>>);
	{piece, [Index, Begin, Block]} ->
	    io:format("~nBLOCK SENT~n"),
	    gen_tcp:send(Socket, handle_piece(Index, Begin, Block));
	{cancel, [Index, Begin, Length]} ->
	    gen_tcp:send(Socket, <<0,0,0,13,8,
				Index:32/integer-big,
				Begin:32/integer-big,
				Length:32/integer-big>>);
	{port, Listen_port} ->
	    gen_tcp:send(Socket, <<0,0,0,3,9,Listen_port:16>>)
    end.

%%--------------------------------------------------------------------
%% Function: handle_bitfield/1
%% Purpose: take a bitfield in list [0,0,0,1,1,...], return a bitfield message
%% in binary
%% Args: Bitfield_in_list
%% Returns: Message_in_bin
%%--------------------------------------------------------------------

handle_bitfield(Bitfield_in_list) ->
    {Bitfield_in_bits, Len} = make_bitfield(Bitfield_in_list, 1),
    <<(Len+1):32/integer-big, 5, Bitfield_in_bits/bitstring>>.

%%--------------------------------------------------------------------
%% Function: handle_piece/3
%% Purpose: take the piece index, begin, and block, return a piece message in
%% binary
%% Args: Index, Begin, Block
%% Returns: Message_in_bin
%%--------------------------------------------------------------------

handle_piece(Index, Begin, Block) ->
    Len = count_bin(Block, 0),
    list_to_binary([<<(Len+9):32/integer-big, 7, Index:32/integer-big, 
		      Begin:32/integer-big>>, Block]).

%%--------------------------------------------------------------------
%% Function: count_bin/2
%% Purpose: take a binary and an accumulator (from 0), return the byte 
%%          size of it
%% Args: Binary, Count
%% Returns: Byte_size
%%--------------------------------------------------------------------

count_bin(<<>>, Count) ->
    Count;
count_bin(<<_First:8, Rest/binary>>, Count) ->
    count_bin(Rest, Count+1).

%%--------------------------------------------------------------------
%% Function: make_bitfield/2
%% Purpose: take a list of bitfield [0,0,0,1,1,...], return the bitfield 
%%          in binary form and a length
%% Args: Bitfield_in_list, Index
%% Returns: {Bitfield_in_bits, Len}
%%--------------------------------------------------------------------

make_bitfield([H], Index) when Index rem 8 == 0 ->
    {<<H:1>>, Index div 8};
make_bitfield([H], Index) ->
    {Rest, X} = make_bitfield([0], Index+1),
    {list_to_bitstring([<<H:1>>, <<Rest/bits>>]), X};
make_bitfield([H|T], Index) ->
    {Rest, X} = make_bitfield(T, Index+1),
    {list_to_bitstring([<<H:1>>, <<Rest/bits>>]), X}.

