%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111007.
%%%Info: Connects to specified tracker and retrieves info about the torrent specified.
-module(connect_to_tracker).
-export([start/3, make_list/2, give_more/3]).
%%Start function: spawn_links a new process to init function
start(Dl_pid, Peers_pid, Length) ->
    spawn_link(fun() -> init(Dl_pid, Peers_pid, Length) end).
%%Init function waits for the first connect message and then
init(Dl_pid, Peers_pid, Length) ->
    Info_sha = sha:shaurl(download_manager:get_info_clean(Dl_pid)),
    My_id = download_manager:get_my_id(Dl_pid),
    loop(Info_sha, 10000, My_id, none, "6881", Length, Peers_pid, Dl_pid).
loop(Info, Time, My_id, Tracker, Port,Length, Peers_pid, Dl_pid) ->
    receive
	{connect, From, H} ->
	    io:format("connect to tracker~n"),
	    io:format(H ++ "?info_hash=" ++ Info ++ "&peer_id=" ++ My_id ++ "&port=" ++ Port ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&compact=1&event=started"),
	    {Peers, Min_time} = get_info(H ++ "?info_hash=" ++ Info ++ "&peer_id=" ++ My_id ++ "&port=" ++ Port ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&compact=1&event=started"),
	    From ! {ok, Peers},
	    loop(Info, Min_time, My_id, H, Port, Length, Peers_pid, Dl_pid)
    after 10000 ->
	    case Tracker of
		none ->
		    loop(Info, Time, My_id, Tracker, Port, Length, Peers_pid, Dl_pid);
		_ ->
		    io:format("connect to tracker~n"),
		    {Peers, Min_time} = get_info(Tracker ++ "?info_hash=" ++ Info ++ "&peer_id=" ++ My_id ++ "&port=" ++ Port ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&compact=1"),
		    spawn(peers, insert_new_peers, [Peers, Peers_pid, Dl_pid]),
		    loop(Info, Min_time, My_id, Tracker, Port, Length, Peers_pid, Dl_pid)
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
