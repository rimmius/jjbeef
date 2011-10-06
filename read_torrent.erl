-module(read_torrent).
-export([file/2, loop/1, get_trackers/1]).
-define(DICT, dict).
file(Name, File) ->
    case  file:read_file(File) of
	{ok, Text} ->
	    {{dict, Dec}, _Remainder} = decode(Text),
	    register(Name, spawn(read_torrent, loop, [{dict, Dec}])),
	    process_flag(trap_exit, true),
	    ok;
	_  ->
	    {error, no_file}
    end.

loop({dict, Dec}) ->
    receive
	{From, request, trackers} ->
	    {_, List} = dict:fetch(<<"announce-list">>, Dec),
	    From ! {data, List},
	    loop({dict, Dec});
	{'EXIT', _Pid, _Reason} ->
	    loop(Dec)
    end.

get_trackers(Name) ->
    Name ! {self(), request, trackers},
    receive
	{data, List} ->
	    List
    end.

decode(<<$l, Tail/binary>>) ->
    decode_list(Tail, []);
decode(<<$d, Tail/binary>>) ->
    decode_dict(Tail, ?DICT:new());
decode(<<$i, Tail/binary>>) ->
    decode_integer(Tail, []);
decode(Data) ->
    decode_string(Data, []).

decode_integer(<<$e, T/binary>>, New_list) ->
    {list_to_integer(lists:reverse(New_list)), T};
decode_integer(<<H, T/binary>>, New_list) ->
    decode_integer(T, [H|New_list]).

decode_string(<<$:, T/binary>>, New_list) ->
    Integer = list_to_integer(lists:reverse(New_list)),
    <<String:Integer/binary, Rest/binary>> = T,
    {String, Rest};
decode_string(<<H, T/binary>>, New_list) ->
    decode_string(T, [H|New_list]).

decode_list(<<$e, T/binary>>, New_list) ->
    {{list, lists:reverse(New_list)}, T};
decode_list(Data, New_list) ->
    {Res, T} = decode(Data),
    decode_list(T, [Res|New_list]).

decode_dict(<<$e, T/binary>>, New_list) ->
    {{dict, New_list}, T};
decode_dict(Data, New_list) ->
    {Key, T1} = decode(Data),
    {Val, T2} = decode(T1),
    decode_dict(T2, ?DICT:store(Key, Val, New_list)).
