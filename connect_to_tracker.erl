%%%Created by: Fredrik Gustafsson.
%%%Creation date: 111007.
%%%Info: Connects to specified tracker and retrieves info about the torrent specified.
-module(connect_to_tracker).
-export([start/0, make_list/2]).
%%This function takes one argument, the url to the tracker.

start() ->
    receive
	{connect,From, {Info, Tracker, Length}} ->
	    Info2 = sha:shaurl(Info),
	    From ! get_tracker_info(Tracker, Info2, Length)
    end.
get_info(Url) ->
    inets:start(),
    Result = httpc:request(Url),
    Result.

make_list([], New_list) ->
    New_list;
make_list([{_, H}|T], New_list) ->
    case string:substr(binary_to_list(list_to_binary(H)), 1, 1) of
	"u" ->
	    make_list(T, New_list);
	_ ->
	    make_list(T, [binary_to_list(list_to_binary(H))|New_list])
    end.

get_tracker_info(Tracker, Info, Length) ->
    %loop ! {peer_id, self()},
    %receive
	%{peer_id, Peer_id} ->
	    get_tracker_info2(Tracker, Info, Length).
    %end.

%%Recursively tries to connect to all the trackers given in the torrent file, basecase with not connected to any tracker.
%%It takes 5 arguments: the Peer_id which is the id of the client, the Name of the torrent file as process, the list of trackers, the info-hash and the length
%%(size)of the file.
get_tracker_info2(Tracker, Info, Length) ->
    io:format(Tracker  ++  "?info_hash=" ++  Info ++ "~n"),
    get_info(Tracker ++ "?info_hash=" ++ Info  ++  "&peer_id=" ++ "12345678912345678911" ++ "&port=" ++ "6881" ++ "&uploaded=0&downloaded=0&left=" ++ integer_to_list(Length) ++ "&compact=1"). % 
