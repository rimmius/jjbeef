-module(message).
-export([recv/1]).

recv(Socket) ->
    %% case gen_tcp:recv(Socket, 0) of
    case {ok, Socket} of %% for testing
	{ok, <<0,0,0,0>>} ->
	    %% keep-alive: <len=0000>
	    io:format("keep-alive~n");
	{ok, <<Len:32/integer-big, Id:8>>} ->
	    case {Len, Id} of
		{1, 0} ->
		    %% choke: <len=0001><id=0>
		     io:format("len=~w id=~w~nchoke~n", [Len, Id]);
		{1, 1} ->
		    %% unchoke: <len=0001><id=1>
		     io:format("len=~w id=~w~nunchoke~n", [Len, Id]);
		{1, 2} ->
		    %% interested: <len=0001><id=2>
		     io:format("len=~w id=~w~ninterested~n", [Len, Id]);
		{1, 3} ->
		    %% not interested: <len=0001><id=3>
		    io:format("len=~w id=~w~nnot_interested~n", [Len, Id])
	    end;
	{ok, <<Len:32/integer-big, Id:8, Payload/binary>>} ->
	    case {Len, Id, Payload} of
		{5,      4, Piece_index} ->
		    %% have: <len=0005><id=4><piece index>
		    io:format("len=~w id=~w piece index=~w~nhave~n", [Len, Id, Piece_index]);
		{X,      5, <<Bitfield:(X-1)*8>>} ->
		    %% bitfield: <len=0001+X><id=5><bitfield>
		    io:format("len=1+~w id=~w bitfield=~w~nbitfield~n", [X-1, Id, Bitfield]);
		{13,     6, <<Index:32, Begin:32, Length:32>>} ->
		    %% request: <len=0013><id=6><index><begin><length>
		    io:format("len=~w id=~w index=~w begin=~w length=~w~nrequest~n", [Len, Id, Index, Begin, Length]);
		{X,     7, <<Index:32, Begin:32, Block/binary>>} ->
		    %% piece: <len=0009+X><id=7><index><begin><block>
		    io:format("len=9+~w id=~w index=~w begin=~w block=~w~npiece~n", [X-9, Id, Index, Begin, Block]);
		{13,     8, <<Index:32, Begin:32, Length:32>>} ->
		    %% cancel: <len=0013><id=8><index><begin><length>
		    io:format("len=~w id=~w index=~w begin=~w length=~w~ncancel~n", [Len, Id, Index, Begin, Length]);
		{3,      9, <<Listen_port/binary>>} ->
		    %% port: <len=0003><id=9><listen-port>
		    io:format("len=~w id=~w listen-port=~wport~n", [Len, Id, Listen_port])
	    end;
	{error, closed} ->
	    ok
    end.
