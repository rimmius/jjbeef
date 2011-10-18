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
	    register(Name, spawn_link(read_torrent, init, [{dict, Dec}]));
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
	    {_, Info_dict} = dict:fetch(<<"info">>, Dec),
	    Length = dict:fetch(<<"length">>, Info_dict),
	    From ! {data, Length},
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
    case string:substr(binary_to_list(list_to_binary(H)), 1, 1) of
	"u" ->
	    make_list(T, New_list);
	_ ->
	    make_list(T, [binary_to_list(list_to_binary(H))|New_list])
    end.

%%This function returns the info from the torrent file as a proper string/list
get_info(Name) ->
    Name ! {self(), request, info_hash},
    receive
	{data, List} ->
	    bencode:encode(List)
    end.
get_length(Name) ->
    Name ! {self(), request, length},
    receive
	{data, Length} ->
	    Length
    end.
%%This function returns the trackers information about the torrent requested.
get_tracker_info(Name) ->
    List = get_trackers(Name),
    loop ! {peer_id, self()},
    receive
	{peer_id, Peer_id} ->
	    get_tracker_info2(Peer_id, Name, List)
    end.

get_tracker_info2(_Peer_id, _Name, []) ->
    {error, not_connected_to_any_tracker};
get_tracker_info2(Peer_id, Name, [H|T]) ->
    Info = edoc_lib:escape_uri(binary_to_list(crypto:sha(get_info(Name)))),
    Length = get_length(Name),
    Pid = spawn(connect_to_tracker,start,[]),
    Pid ! {self(), H ++ "?info_hash="  ++ "12345678912345678911"}, % ++ "&peer_id=" ++ edoc_lib:escape_uri(Peer_id) ++ "&port=" ++ "6881" ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&event=started
    receive
	{ok,Result} ->
	    ok
    after 5000 ->
	    io:format("Timeout ~n"),
	    get_tracker_info2(Peer_id, Name, T)
    end.
