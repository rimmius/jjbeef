%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111007.
%%%Info: Connects to specified tracker and retrieves info about the torrent specified.
-module(connect_to_tracker).
-export([start/0, make_list/2]).
%%Start function: spawn_links a new process to init function
start() ->
    spawn_link(fun() -> init() end).
%%Init function waits for the first connect message and then
init() ->
    receive
	{connect,From, {Info, Tracker, Length}} ->
	    Info_sha = sha:shaurl(Info),
	    {Peers, Min_time} = get_info(Tracker ++ "?info_hash=" ++ Info_sha  ++  "&peer_id=" ++ "12345678912345678911" ++ "&port=" ++ "12345" ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&compact=1&event=started"),
	    From ! {ok, Peers},
	    loop(Info_sha, Min_time)
    end.
loop(Info, Time) ->
    receive
    after Time ->
	    io:format("connect to tracker~n"),
	    loop(Info, Time)
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
