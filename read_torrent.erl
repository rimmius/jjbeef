%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111005.
%%%Info: Reads in and parses the bencoded torrent.
-module(read_torrent).
-export([file/2, init/1, get_trackers/1, get_info/1]).
-define(DICT, dict).
%%This function takes two arguments, Name and File, where Name is the name of the torrent and File is the path to it
file(Name, File) ->
    case  file:read_file(File) of
	{ok, Text} ->
	    {{dict, Dec}, _Remainder} = decode(Text),
	    register(Name, spawn(read_torrent, init, [{dict, Dec}])),
	    link(whereis(Name));
	_  ->
	    {error, no_file}
    end.
init({dict,Dec}) ->
    process_flag(trap_exit, true),
    loop({dict,Dec}).
%%This function is the loop which returns the information requested, it holds the information about the torrentfile.
loop({dict, Dec}) ->
    receive
	{From, request, trackers} ->
	    {_, List} = dict:fetch(<<"announce-list">>, Dec),
	    From ! {data, List},
	    loop({dict, Dec});
	{From, request, info_hash} ->
	    List = dict:fetch(<<"info">>, Dec),
	    From ! {data, List},
	    loop({dict, Dec});
	{'EXIT', _Pid, _Reason} ->
	    loop(Dec)
    end.
%%This function takes one argument which is the name of the torrent file, and returns the List of the trackers given in the torrent file
get_trackers(Name) ->
    Name ! {self(), request, trackers},
    receive
	{data, List} ->
	    make_list(List, [])
    end.
make_list([], New_list) ->
    New_list;
make_list([{_, H}|T], New_list) ->
    make_list(T, [binary_to_list(list_to_binary(H))|New_list]).
get_info(Name) ->
    Name ! {self(), request, info_hash},
    receive
	{data, List} ->
	   binary_to_list(iolist_to_binary(encode(List)))
    end.

%%Bencode decoding functions
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


%%Bencode encoding functions
encode(Integer) when is_integer(Integer) ->
    Bin = list_to_binary(integer_to_list(Integer)),
    [$i, Bin, $e];
encode(String) when is_list(String) ->
    encode(list_to_binary(String));
encode(String) when is_binary(String) ->
    Bin = list_to_binary(integer_to_list(size(String))),
    [Bin, $:, String];
encode({list, List}) when is_list(List) ->
    [$l, [encode(Elem) || Elem <- List], $e];
encode({dict, Dict}) ->
    Things = lists:map(
	     fun({Key, Val}) when is_list(Key) or is_binary(Key) ->
		     [encode(Key), encode(Val)]
	     end, lists:keysort(1, ?DICT:to_list(Dict))),
    [$d, Things, $e].
