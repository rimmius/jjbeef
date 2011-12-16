%%%---------------------------------------------------------------------
%%% Created by: Fredrik Gustafsson.
%%% Creation date: 2001-11-07
%%%--------------------------------------------------------------------- 
%%% Description module connecting to a tracker
%%%--------------------------------------------------------------------- 
%%% What this module does:
%%% Connects to a specified tracker returning information
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start()
%%% spawn_links a new process to init function 
%%%--------------------------------------------------------------------- 
%%% make_list()
%%% Makes a proper list of the links in the torrent-file
%%%--------------------------------------------------------------------- 

-module(connect_to_tracker).
-export([start/4, make_list/2]).

start(Dl_pid, Peers_pid, Length, File_storage_pid) ->
    spawn_link(fun() -> init(Dl_pid, Peers_pid, Length, File_storage_pid) end).

%%--------------------------------------------------------------------
%% Function: init/4
%% Purpose: waits for the first connect message and then
%% Args: Dl_pid: Pid of Download_manager
%% Peers_pid: Pid of Peers
%% Length: The total length of the file(s)
%% File_storage_pid: Pid of File_storage
%%--------------------------------------------------------------------

init(Dl_pid, Peers_pid, Length, File_storage_pid) ->
    Info_sha = sha:shaurl(download_manager:get_info_clean(Dl_pid)),
    My_id = download_manager:get_my_id(Dl_pid),
    loop(Info_sha, 10000, My_id, none, "6881", Length, Peers_pid, Dl_pid, 
	 File_storage_pid).

%%--------------------------------------------------------------------
%% Function:loop/8
%% Purpose: Connects to a specified tracker and spawning
%% a function in peers to insert the peers received from the tracker.
%% Updating the specified tracker with amount uploaded and downloaded
%% Args:  Info: The info-hash for the torrent-file
%% Time: Interval between connections the tracker
%% Tracker: The url to tracker
%% Length: The total size of the file(s) downloaded
%% Peers_pid: Pid of peers module
%% Dl_pid: Pid of Download_manager module
%% File_storage_pid: Pid of file_storage_pid 
%%--------------------------------------------------------------------

loop(Info, Time, My_id, Tracker, Port, Length, Peers_pid, Dl_pid, 
     File_storage_pid) ->
    receive
	{connect, From, H} ->
	    io:format("CONNECTING TO TRACKER~n"),
	    io:format(H ++ "?info_hash=" ++ Info ++ "&peer_id=" ++ My_id 
		      ++ "&port=" ++ Port ++ "&uploaded=0&downloaded=0&left=" 
		      ++ integer_to_list(Length) ++ "&compact=1&event=started"),
	    {Peers, Min_time} = get_info(H ++ "?info_hash=" ++ Info 
					 ++ "&peer_id=" ++ My_id ++ "&port=" 
					 ++ Port 
					 ++ "&uploaded=0&downloaded=0&left=" 
					 ++ integer_to_list(Length) 
					 ++ "&compact=1&event=started"),
	    From ! {ok, Peers},
	    loop(Info, Min_time, My_id, H, Port, Length, Peers_pid, 
		 Dl_pid, File_storage_pid)
    after 10000 ->
	    case Tracker of
		none ->
		    loop(Info, Time, My_id, Tracker, Port, Length, Peers_pid, 
			 Dl_pid, File_storage_pid);
		_ ->
		    io:format("connect to tracker~n"),
		    {We_have, Uploaded} = get_current_pieces(File_storage_pid),
		    Left = Length - We_have,
		    io:format("~n~nLeft=~w~n~n", [Left]),
		    {Peers, Min_time} = get_info(Tracker ++ "?info_hash=" 
						 ++ Info ++ "&peer_id=" 
						 ++ My_id ++ "&port=" 
						 ++ Port 
						 ++ "&uploaded=" 
						 ++ integer_to_list(Uploaded)
						 ++ "&downloaded=" 
						 ++ integer_to_list(We_have) 
						 ++ "&left=" 
						 ++ integer_to_list(Left) 
						 ++ "&compact=1"),
		    spawn(peers, insert_new_peers, [Peers, Peers_pid, Dl_pid]),
		    Dl_pid ! {this_tracker, Tracker},
		    loop(Info, Min_time, My_id, Tracker, Port, Length, 
			 Peers_pid, Dl_pid, File_storage_pid)
	    end
    end.

%%--------------------------------------------------------------------
%% Function: get_current_pieces/1
%% Purpose: Requests the current downloaded amout
%% and uploaded.
%% Args:  File_storage_pid: Pid of file_storage module
%% Returns: How_much: how much we downloaded
%% Uploaded: The current uploaded data
%%--------------------------------------------------------------------
	
get_current_pieces(File_storage_pid) ->
    {How_much, Uploaded} = mutex:request(File_storage_pid, how_much, []),
    mutex:received(File_storage_pid),
    {How_much, Uploaded}.

%%--------------------------------------------------------------------
%% Function: get_info/1
%% Purpose: Connecting to tracker
%% Args: Url: The url to the tracker
%% Returns: Peers handed back by the tracker and
%% Minimum requesting time to the tracker.
%%--------------------------------------------------------------------

get_info(Url) ->
    inets:start(),
    {ok, {_,_,Result}} = httpc:request(Url),
    {{dict, Response_from_tracker}, _Remainder} = 
	bencode:decode(list_to_binary(Result)),
    {get_peers(Response_from_tracker), get_time(Response_from_tracker)}.

%%--------------------------------------------------------------------
%% Function: get_peers/1
%% Purpose: Fetch peer list from tracker response.
%% Args: Response_from_tracker: The response from the tracker
%% Returns: The peers in a list.
%%--------------------------------------------------------------------

get_peers(Response_from_tracker) ->
    case dict:find(<<"peers">>, Response_from_tracker) of
	error ->
	    exit(self(), kill);
	{ok, Peers} ->
	    binary_to_list(Peers)
    end.

%%--------------------------------------------------------------------
%% Function: get_time/1
%% Purpose: Hands back the minimum interval between connections to
%% tracker.
%% Args: Response_from_tracker: The response from the tracker.
%% Returns: The minimum interval between connections to tracker.
%%--------------------------------------------------------------------

get_time(Response_from_tracker) ->
    case dict:find(<<"interval">>, Response_from_tracker) of
	error ->
	    exit(self(), kill);
	{ok, Interval} ->
	    Interval
    end.

%%--------------------------------------------------------------------
%% Defined at top of file under exported functions
%%--------------------------------------------------------------------

make_list([], New_list) ->
    New_list;
make_list([{_, H}|T], New_list) ->
    case string:substr(binary_to_list(list_to_binary(H)), 1, 1) of
	"u" ->
	    make_list(T, New_list);
	_ ->
	    make_list(T, [binary_to_list(list_to_binary(H))|New_list])
    end.
