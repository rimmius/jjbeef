%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111005.
%%%Info: Reads in and parses the bencoded torrent and gets the information from the tracker.
-module(read_torrent).
-export([file/2, init/1, get_trackers/1, get_info/1, get_tracker_info/1, get_length/1]).

%%This function takes two arguments, Name and File, where Name is the name of the torrent and File is the path to it
file(Name, File) ->
    case  file:read_file(File) of
	{ok, Text} ->
	    {{dict, Dec}, _Remainder} = bencode:decode(Text),
	    register(Name, spawn(read_torrent, init, [{dict, Dec}]));
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
	{From, request, length} ->
	    %% Info = dict:fetch(<<"info">>, Dec),
	    %% io:format("~w~n", [Info]),
	    %% Length = dict:fetch(<<"length">>, Info),
	    %%-----------------------------------------------------START HERE FREDDYBOY-----------------------------------------------------------------------
	    Keys = dict:fetch_keys(Dec),
	    From ! {data, Keys},
	    loop({dict, Dec});
	{'EXIT', _Pid, _Reason} ->
	    {error, bad_torrent}
    end.

%%This function takes one argument which is the name of the torrent file, and returns the List of the trackers given in the torrent file
get_trackers(Name) ->
    Name ! {self(), request, trackers},
    receive
	{data, List} ->
	    make_list(List, [])
    end.

%%These functions make_list returns a proper list with the trackers defined in the torrent file
make_list([], New_list) ->
    New_list;
make_list([{_, H}|T], New_list) ->
    make_list(T, [binary_to_list(list_to_binary(H))|New_list]).

%%This function returns the info from the torrent file as a proper string/list
get_info(Name) ->
    Name ! {self(), request, info_hash},
    receive
	{data, List} ->
	  edoc_lib:escape_uri(binary_to_list(crypto:sha(bencode:encode(List))))
    end.
get_length(Name) ->
    Name ! {self(), request, length},
    receive
	{data, Length} ->
	    Length
    end.
%%This function returns the trackers information about the torrent requested.
get_tracker_info(Name) ->
    [H|T] = get_trackers(Name),
    Info = get_info(Name),
    loop ! {peer_id, self()},
    receive
	{peer_id, Peer_id} ->
	    connect_to_tracker:get_info(H ++ "?info_hash="  ++ Info ++ "&peer_id=" ++ Peer_id ++ "&port=" ++ "5678" ++ "&uploaded=0&downloaded=0")
    end.
