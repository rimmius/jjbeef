%%%Created by:  Eva-Lisa Kedborn, Fredrik Gustafsson
%%%Creation date: 2011-10-18
%%%Refactored: 2011-11-23, Fredrik Gustafsson
%%%This module supervises the torrent parser module

-module(download_manager).
-export([start/2,  init/2, is_valid_info_hash/2, 
	 get_my_id/1, get_my_info_hash/1, get_info_clean/1]).

start(Filepath, GUIPid) ->
    spawn_link(?MODULE, init, [Filepath, GUIPid]).
    
init(Filepath, _GUIPid) ->
    process_flag(trap_exit, true),
    {dict, Dict} = get_torrent_data(Filepath),
    Info_raw = dict:fetch(<<"info">>, Dict),
    Info_bencoded =  bencode:encode(Info_raw),
    Info_hash = list_to_binary(sha:sha1raw(Info_bencoded)),
    Peers_pid = peers:start(self(), get_announce_list({dict, Dict}), get_pieces({dict, Dict}), get_piece_length({dict, Dict}), get_length_and_name({dict, Dict})),
    loop(Peers_pid, Info_hash,Info_bencoded, guimain:createUniqueId()).
    
get_torrent_data(Filepath) ->
    TPid = spawn_link(read_torrent, start, []),
    TPid ! {read, self(), Filepath},
    receive
	{reply, {dict, Dict}} ->
	    {dict,Dict}
    end.

get_announce_list({dict, Dict}) ->
    %%ets:insert(torrent_info, Info),
     case dict:find(<<"announce-list">>, Dict) of
	 {ok,{_, List}} ->
	     List;
	 error ->
	     []
     end.
get_pieces({dict, Dict}) ->
    %List = dict:fetch(<<"announce">>, Dict),
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    Pieces = dict:fetch(<<"pieces">>, Info_dict),
    handle_pieces(binary_to_list(Pieces),[], 1, []).

get_piece_length({dict, Dict}) ->
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    dict:fetch(<<"piece length">>, Info_dict).

get_length_and_name({dict, Dict}) ->
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    case dict:find(<<"files">>, Info_dict) of
	{ok, {_,Files_dict}} ->
	    {get_length(Files_dict, 0), ["multiple_files_not_supported_yet.error"]};
	error ->
	    Name_of_files = dict:fetch(<<"name">>, Info_dict),
	    {dict:fetch(<<"length">>, Info_dict), [binary_to_list(Name_of_files)]}
    end.

loop(Peers_pid, Info_hash, Info_clean, My_id) ->
    receive
	{get_clean_info, From} ->
	    From ! {reply, Info_clean},
	    loop(Peers_pid, Info_hash, Info_clean, My_id);
	{valid_info, From, Info_from_peer} ->
	    From ! binary_to_list(Info_hash) =:= Info_from_peer,
	    loop(Peers_pid, Info_hash, Info_clean, My_id);
	{get_id, From} ->
	    From ! {reply, My_id},
	    loop(Peers_pid, Info_hash, Info_clean, My_id);
	{get_my_info_hash, From} ->
	    From ! {reply, Info_hash},
	    loop(Peers_pid, Info_hash, Info_clean, My_id);
	{'EXIT', Peers_pid, Reason} ->
	    io:format("~w~n", Reason),
	    loop(peers_pid_crash, Info_hash, Info_clean, My_id)
    end.
    
get_length([], Total) ->
    Total;
get_length([{_,H}|T], Total) ->
    {ok, Value} = dict:find(<<"length">>,H),
    get_length(T, Total+Value).

handle_pieces([], Piece_list, _Byte, New_list) ->
    lists:reverse([lists:reverse(Piece_list)|New_list]);
handle_pieces([H|T],Piece_list, Byte, New_list) when Byte =< 20 ->
    handle_pieces(T,[H|Piece_list], Byte+1, New_list);
handle_pieces(List, Piece_list, _Byte, New_list)  ->
    handle_pieces(List,[], 1, [lists:reverse(Piece_list)|New_list]).


    %Piece_pid = spawn(fun() -> loop({List_of_pieces,[]}, Peer_pid)end),
    %get_pieces(Piece_pid).
    
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

get_info_clean(Dl_pid) ->
    Dl_pid ! {get_clean_info, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.
