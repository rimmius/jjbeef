%%%Created by: Fredrik Gustafsson
%%%Date: 16-11-2011
-module(file_storage).
-export([start/3, init/3, get_bitfield/1, insert_piece/3]).

start(Parent, Files, Length) ->
    spawn(?MODULE, init, [Parent, Files, Length]).

init(Parent, Files, Length) ->
    Data = initiate_data(1, Length),
    io:format("~w~n", [Data]),
    loop(Parent, Files, Data).

initiate_data(Nr, Length) when Nr =< Length ->
    [0|initiate_data(Nr+1, Length)];
initiate_data(_Nr, _Length) ->
    [].

loop(Parent, Files, Data) ->
    receive
	{bitfield, From} ->
	    Bitfield = generate_bitfield(Data),
	    From ! {reply, Bitfield},
	    loop(Parent, Files, Data);
	{insert, From, {_Index, _Piece}} ->
	    From ! {reply, ok},
	    loop(Parent, Files, Data)
    end.

generate_bitfield([H|T]) ->
    [H|T].

get_bitfield(File_storage_pid) ->
    File_storage_pid ! {bitfield, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.

insert_piece(File_storage_pid, Index, Piece) ->
    File_storage_pid ! {insert, self(), {Index, Piece}},
    receive
	{reply, Reply} ->
	    Reply
    end.
