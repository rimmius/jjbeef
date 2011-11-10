%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111007.
%%%Info: Connects to specified tracker and retrieves info about the torrent specified.
-module(connect_to_tracker).
-export([start/0, make_list/2, give_more/3]).
%%Start function: spawn_links a new process to init function
start() ->
    spawn_link(fun() -> init() end).
%%Init function waits for the first connect message and then
init() ->
    receive
	{connect,From, {Info, Tracker, Length}, My_id} ->
	    Info_sha = sha:shaurl(Info),
	    io:format(My_id),
	    {Peers, Min_time} = get_info(Tracker ++ "?info_hash=" ++ Info_sha  ++  "&peer_id=" ++ My_id ++ "&port=" ++ "12345" ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&compact=1&event=started"),
	    From ! {ok, Peers},
	    loop(Info_sha, Min_time,My_id, Tracker, "12345",Length, false, false)
    end.
loop(Info, Time, My_id, Tracker, Port,Length, Peer_pid, Dl_pid) -> %%Change to get length from jing and eva later ::):):)
    receive
	{more_info, New_peer_pid, New_dl_pid} ->
	    loop(Info, Time, My_id, Tracker, Port, Length, New_peer_pid, New_dl_pid) %%SAME SAME
    after Time ->
	    case {Peer_pid, Dl_pid} of
		{false, false} ->
		    io:format("Cant connect to tracker yet~n~n"),
		    loop(Info, Time, My_id, Tracker, Port,Length, false, false);
		{_, _} ->
		    io:format("connect to tracker~n"),
		    {Peers, Min_time} = get_info(Tracker ++ "?info_hash=" ++ Info ++ "&peer_id=" ++ My_id ++ "&port=" ++ Port ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&compact=1&event=started"),
		    spawn(peers, insert_peers_later, [Peers, Peer_pid, Dl_pid]),
		    loop(Info, Min_time, My_id, Tracker, Port, Length, Peer_pid, Dl_pid)
	    end
    end.	

get_info(Url) ->
    inets:start(),
    {ok, {_,_,Result}} = httpc:request(Url),
    {{dict, Response_from_tracker}, _Remainder} = bencode:decode(list_to_binary(Result)),
    {get_peers(Response_from_tracker), get_time(Response_from_tracker)}.

get_peers(Response_from_tracker) ->
    binary_to_list(dict:fetch(<<"peers">>, Response_from_tracker)).
get_time(Response_from_tracker) ->
    dict:fetch(<<"interval">>, Response_from_tracker).

make_list([], New_list) ->
    New_list;
make_list([{_, H}|T], New_list) ->
    case string:substr(binary_to_list(list_to_binary(H)), 1, 1) of
	"u" ->
	    make_list(T, New_list);
	_ ->
	    make_list(T, [binary_to_list(list_to_binary(H))|New_list])
    end.
give_more(Peer_pid, Tracker_pid, Dl_pid) ->
    Tracker_pid ! {more_info, Peer_pid, Dl_pid}.
