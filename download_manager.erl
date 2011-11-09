%%%Created by:  Eva-Lisa Kedborn, Fredrik Gustafsson
%%%Creation date: 2011-10-18
%%%This module supervises the torrent parser module

-module(download_manager).
-export([start/3, init/3, init/2, is_valid_info_hash/2, get_my_id/1, get_my_info_hash/1]).

start(Filepath, GUIPid, My_id) ->
    spawn_link(?MODULE, init, [Filepath, GUIPid, My_id]).

init(Filepath, GUIPid, My_id) ->
    process_flag(trap_exit, true),
    TPid = spawn_link(read_torrent, start, []),
    TPid ! {read, self(), Filepath},
    receive
	{reply, {dict, Dict}} ->
	    store_info({dict,Dict}, My_id)
    end.

init(Filepath, GUIPid) ->
    process_flag(trap_exit, true),
    TPid = spawn_link(read_torrent, start, []),
    TPid ! {read, self(), Filepath},
    receive
	{reply, {dict, Dict}} ->
	    store_info({dict,Dict}, guimain:createUniqueId())
    end.

loop(Info_hash, My_id) ->
    receive
	{valid_info, From, Info_from_peer} ->
	    From ! binary_to_list(Info_hash) =:= Info_from_peer,
	    loop(Info_hash, My_id);
	{get_id, From} ->
	    From ! {reply, My_id},
	    loop(Info_hash, My_id);
	{get_my_info_hash, From} ->
	    From ! {reply, Info_hash},
	    loop(Info_hash, My_id)
    end.
get_random_piece(Pieces_missing, Pieces_done) ->
    Rand = random:uniform(length(Pieces_missing)),
    Piece_grabbed = lists:nth(Rand, Pieces_missing),
    New_missing = lists:delete(Piece_grabbed, Pieces_missing),
    {Rand, Piece_grabbed, New_missing, [{Rand, Piece_grabbed}|Pieces_done]}.

store_info({dict,Dict}, My_id) ->
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
    List_of_pieces = handle_pieces(binary_to_list(Pieces),[], 1, []), %%Send in to eva-lisa and jing later :):)
    case dict:find(<<"files">>, Info_dict) of
	{ok, {_,Files_dict}} ->
	    set_up_tracker(Info, Tracker_list, get_length(Files_dict, 0), {dict, Dict}, List_of_pieces, My_id);
	error ->
	    set_up_tracker(Info, Tracker_list, dict:fetch(<<"length">>, Info_dict), {dict, Dict}, List_of_pieces, My_id)
    end.
get_length([], Total) ->
    Total;
get_length([{_,H}|T], Total) ->
    {ok, Value} = dict:find(<<"length">>,H),
    get_length(T, Total+Value).
set_up_tracker(Info, List, Length, {dict, Dict}, List_of_pieces, My_id) ->
    Announce_list = connect_to_tracker:make_list(List, []),
    case Announce_list of
	[] ->
	    Announce = dict:fetch(<<"announce">>, Dict),
	    send_to_tracker(Info, Announce, Length, List_of_pieces, My_id);
	_ ->
	    send_to_tracker(Info, Announce_list, Length, List_of_pieces, My_id)
    end.
send_to_tracker(_Info, [], _Length, _List_of_pieces, _My_id) ->
    exit(self(), kill);
send_to_tracker(Info, [H|T], Length, List_of_pieces, My_id) ->
    Tracker_pid = connect_to_tracker:start(),
    Tracker_pid ! {connect, self(), {Info, H, Length}, My_id},
    receive
	{ok, Peers} ->
	    connect_to_peers(Info, List_of_pieces, Peers, My_id)
    after 2000 ->
	    io:format("connecting to next tracker in list ~n"),
	    send_to_tracker(Info, T, Length, List_of_pieces, My_id)
    end.
handle_pieces([], Piece_list, _Byte, New_list) ->
    lists:reverse([lists:reverse(Piece_list)|New_list]);
handle_pieces([H|T],Piece_list, Byte, New_list) when Byte =< 20 ->
    handle_pieces(T,[H|Piece_list], Byte+1, New_list);
handle_pieces([_H|T], Piece_list, _Byte, New_list)  ->
    handle_pieces(T,[], 1, [lists:reverse(Piece_list)|New_list]).

connect_to_peers(Info, List_of_pieces, List_of_peers, My_id) ->
    Peer_pid = peers:start(),
    Info2 = list_to_binary(sha:sha1raw(Info)),
    Dl_pid = spawn(fun() -> loop(Info2, My_id) end),
    peers:insert_new_peers(List_of_peers, Peer_pid, Dl_pid).
    %Piece_pid = spawn(fun() -> loop({List_of_pieces,[]}, Peer_pid)end),
    %get_pieces(Piece_pid).

get_pieces(Piece_pid) ->
    Piece_pid ! {dl_piece, self()},
    receive
	{reply, Piece, Peer_pid, Nr} ->
	    dl_piece:start(Piece, Peer_pid, Nr),
	    get_pieces(Piece_pid);
	{error, no_pieces_left} ->
	    {start_building_file}
    end.
    
is_valid_info_hash(Info_from_peer, Pid) ->
    Pid ! {valid_info, self(), Info_from_peer},
    receive 
	Any -> Any end.

get_my_id(Dl_pid) ->
    Dl_pid ! {get_id, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.

get_my_info_hash(Dl_pid) ->
    Dl_pid ! {get_my_info_hash, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.
