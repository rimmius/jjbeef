-module(message_sender).
-export([start/2, send/3]).
-export([loop/2, do_send/3]).

-export([handle_piece/3, count_bin/2]).

start(Parent, Socket) ->
    spawn(?MODULE, loop, [Parent, Socket]).

send(Pid, Type, Msg) ->
    Pid ! {do_send, Type, Msg},
    ok.

loop(Parent, Socket) ->
    receive
	{do_send, Type, Msg} ->
	    case do_send(Socket, Type, Msg) of
		ok ->
		    message_handler:done(Parent),
		    loop(Parent, Socket);
		{error, Reason} -> 
		    exit(self(), Reason)
	    end
    end.

do_send(Socket, Type, Msg) ->
    case {Type, Msg} of 
	{keep_alive, _} ->
	    gen_tcp:send(Socket, <<0,0,0,0>>);
	{choke, _} ->
	    gen_tcp:send(Socket, <<0,0,0,1,0>>);
	   %%  piece_uploader:send_event(Grandparent, is_choked, true);
	{unchoke, _} ->
	    gen_tcp:send(Socket, <<0,0,0,1,1>>);
	   %%  piece_uploader:send_event(Grandparent, is_choked, false);
	{am_interested, true} ->
	    gen_tcp:send(Socket, <<0,0,0,1,2>>);
	{am_interested, false} ->
	    gen_tcp:send(Socket, <<0,0,0,1,3>>);
	{have, Piece_index} ->
	    gen_tcp:send(Socket, 
			      <<0,0,0,5,4, Piece_index:32/integer-big>>);
	{bitfield, Bitfield_in_list} ->
	    gen_tcp:send(Socket, handle_bitfield(Bitfield_in_list));
	{request, [Index, Begin, Length]} ->
	    gen_tcp:send(Socket, 
			      <<0,0,0,13,6, 
				Index:32/integer-big,
				Begin:32/integer-big,
				Length:32/integer-big>>);
	{piece, [Index, Begin, Block]} ->
	    gen_tcp:send(Socket, handle_piece(Index, Begin, Block));
	{cancel, [Index, Begin, Length]} ->
	    gen_tcp:send(Socket, 
			      <<0,0,0,13,8,
				Index:32/integer-big,
				Begin:32/integer-big,
				Length:32/integer-big>>);
	{port, Listen_port} ->
	    gen_tcp:send(Socket,
			      <<0,0,0,3,9,Listen_port:16>>)
    end.

handle_bitfield(Bitfield_in_list) ->
    {Bitfield_in_bits, Len} = make_bitfield(Bitfield_in_list, 1),
    <<(Len+1):32/integer-big, 5, Bitfield_in_bits/bitstring>>.

handle_piece(Index, Begin, Block) ->
    Len = count_bin(Block, 0),
    list_to_binary([<<(Len+9):32/integer-big, 7, Index:32/integer-big, Begin:32/integer-big>>, Block]).
      
%% O.B.S This is only for binary
count_bin(<<>>, Count) ->
    Count;
count_bin(<<_First:8, Rest/binary>>, Count) ->
    count_bin(Rest, Count+1).

make_bitfield([H], Index) when Index rem 8 == 0 ->
    {<<H:1>>, Index div 8};
make_bitfield([H], Index) ->
    {Rest, X} = make_bitfield([0], Index+1),
    {list_to_bitstring([<<H:1>>, <<Rest/bits>>]), X};
make_bitfield([H|T], Index) ->
    {Rest, X} = make_bitfield(T, Index+1),
    {list_to_bitstring([<<H:1>>, <<Rest/bits>>]), X}.

