%%%Created by:  Eva-Lisa Kedborn, Fredrik Gustafsson
%%%Creation date: 2011-10-18
%%%This module supervises the torrent parser module

-module(download_manager).
-export([start/2, init/2]).

start(Filepath, GUIPid) ->
    register(?MODULE, spawn(?MODULE, init, [Filepath, GUIPid])).

init(Filepath, GUIPid) ->
    process_flag(trap_exit, true),
    %%ets:new(torrent_info, [bag, named_table]),
    TPid = spawn_link(read_torrent, start, []),
    TPid ! {read, self(), Filepath},
    receive
	{reply, {dict, Dict}} ->
	    store_info({dict,Dict})
	    %%loop(GUIPid, {dict, Dict})
    end.

loop(GUIPid, {Need,Rec}) ->
    receive
	{'EXIT', _Pid, Reason} ->
	    case Reason of
		normal -> ok;
		killed -> GUIPid ! {error, Reason};
		_Other -> GUIPid ! {error, _Other}
	    end
    end.

store_info({dict,Dict}) ->
    Info_raw = dict:fetch(<<"info">>, Dict),
    Info =  bencode:encode(Info_raw),
    %%ets:insert(torrent_info, Info),
     case dict:find(<<"announce-list">>, Dict) of
	 {ok,{_, List}} ->
	     Tracker_list = List;
	 error ->
	     Tracker_list = []
     end,
    %List = dict:fetch(<<"announce">>, Dict),
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    Pieces = dict:fetch(<<"pieces">>, Info_dict),
    List_of_pieces = handle_pieces(binary_to_list(Pieces),[], 1, []), %%Send in to loop later :D
    case dict:find(<<"files">>, Info_dict) of
	{ok, {_,Files_dict}} ->
	    set_up_tracker(Info, Tracker_list, get_length(Files_dict, 0), {dict, Dict});
	error ->
	    set_up_tracker(Info, Tracker_list, dict:fetch(<<"length">>, Info_dict), {dict, Dict})
    end.
get_length([], Total) ->
    Total;
get_length([{_,H}|T], Total) ->
    {ok, Value} = dict:find(<<"length">>,H),
    get_length(T, Total+Value).
set_up_tracker(Info, List, Length, {dict, Dict}) ->
    TrackerPid = spawn_link(connect_to_tracker, start, []),
    New_list = connect_to_tracker:make_list(List, []) ++ binary_to_list(dict:fetch(<<"announce">>, Dict)),
    send_to_tracker(TrackerPid, Info, New_list, Length).
send_to_tracker(_Pid, _Info, [], _Length) ->
    exit(self(), kill);
send_to_tracker(Pid, Info, [H|T], Length) ->
    Pid ! {connect, self(), {Info, H, Length}},
    receive
	{ok, {_,_,Result}} ->
	    {{dict, New_dict}, _Remainder} = bencode:decode(list_to_binary(Result)),
	    List_peers = binary_to_list(dict:fetch(<<"peers">>, New_dict)),
	    Peer_pid = peers:start(),
	    peers:insert_new_peers(List_peers, Peer_pid, Info)
    after 2000 ->
	    io:format("baba"),
	    send_to_tracker(Pid, Info, T, Length)
    end.
handle_pieces([], Piece_list, _Byte, New_list) ->
    lists:reverse([lists:reverse(Piece_list)|New_list]);
handle_pieces([H|T],Piece_list, Byte, New_list) when Byte =< 20 ->
    handle_pieces(T,[H|Piece_list], Byte+1, New_list);
handle_pieces([H|T], Piece_list, Byte, New_list)  ->
    handle_pieces(T,[], 1, [lists:reverse(Piece_list)|New_list]).
