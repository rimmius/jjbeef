-module(message_sender).
-export([start/2, send/3]).
-export([loop/2, do_send/3]).

start(Parent, Socket) ->
    spawn(?MODULE, loop, [Parent, Socket]).

send(Pid, Type, Msg) ->
    Pid ! {do_send, Type, Msg},
    ok.

loop(Parent, Socket) ->
    receive
	{do_send, Type, Msg} ->
	    do_send(Socket, Type, Msg),
	    message_handler:done(Parent),
	    loop(Parent, Socket)
    end.

do_send(Socket, Type, Msg) ->
    case {Type, Msg} of 
	{keep_alive, _} ->
	    ok = gen_tcp:send(Socket, <<0,0,0,0>>);
	{choke, _} ->
	    ok = gen_tcp:send(Socket, <<0,0,0,1,0>>);
	{unchoke, _} ->
	    ok = gen_tcp:send(Socket, <<0,0,0,1,1>>);
	{interested, _} ->
	    ok = gen_tcp:send(Socket, <<0,0,0,1,2>>);
	{uninterested, _} ->
	    ok = gen_tcp:send(Socket, <<0,0,0,1,3>>);
	{have, [Piece_index]} ->
	    ok = gen_tcp:send(Socket, 
			      <<0,0,0,5,4, Piece_index:32/integer-big>>);
	{bitfield, [Bitfield_in_list]} ->
	    ok = gen_tcp:send(Socket, handle_bitfield(Bitfield_in_list));
	{request, [Index, Begin, Length]} ->
	    ok = gen_tcp:send(Socket, 
			      <<0,0,0,13,6, 
				Index:32/integer-big,
				Begin:32/integer-big,
				Length:32/integer-big>>);
	{piece, [Index, Begin, Block]} ->
	    ok = gen_tcp:send(Socket, handle_piece(Index, Begin, Block));
	{cancel, [Index, Begin, Length]} ->
	    ok = gen_tcp:send(Socket, 
			      <<0,0,0,13,8,
				Index:32/integer-big,
				Begin:32/integer-big,
				Length:32/integer-big>>);
	{port, [Listen_port]} ->
	    ok = gen_tcp:send(Socket,
			      <<0,0,0,3,9,Listen_port:16>>)
    end.

handle_bitfield(Bitfield_in_list) ->
    {Bitfield_in_bits, Len} = make_bitfield(Bitfield_in_list, 1),
    <<(Len+1):32/integer-big, 5, Bitfield_in_bits/bitstring>>.

handle_piece(Index, Begin, Block) ->
    Len = count_bin(Block, 0),
    <<(Len+9):32/integer-big, 7, Index:32/integer-big, Begin:32/integer-big, Block:(Len*8)>>.
      
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

