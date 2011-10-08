%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111005.
%%%Info: Reads in and parses the bencoded torrent.
-module(read_torrent).
-export([file/2, init/2, get_trackers/1, get_info/1, get_tracker_info/1]).
%%This function takes two arguments, Name and File, where Name is the name of the torrent and File is the path to it
file(Name, File) ->
    case  file:read_file(File) of
	{ok, Text} ->
	    {{dict, Dec}, _Remainder} = bencode:decode(Text),
	    register(Name, spawn(read_torrent, init, [Name, {dict, Dec}]));
	_  ->
	    {error, no_file}
    end.
init(Name, {dict,Dec}) ->
    process_flag(trap_exit, true),
    link(whereis(Name)),
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
	  binary_to_list(iolist_to_binary(bencode:encode(List)))
    end.

get_tracker_info(Name) ->
    [H|T] = get_trackers(Name),
    Info = get_info(Name),
    connect_to_tracker:get_info(H ++ "?"  ++ Info).
