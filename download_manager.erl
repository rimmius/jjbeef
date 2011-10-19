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

loop(GUIPid, {dict, Dict}) ->
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
    {_, List} = dict:fetch(<<"announce-list">>, Dict),
    %List = dict:fetch(<<"announce">>, Dict),
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    Length = dict:fetch(<<"length">>, Info_dict),
    set_up_tracker(Info, List, Length).

set_up_tracker(Info, List, Length) ->
    TrackerPid = spawn_link(connect_to_tracker, start, []),
    New_list = connect_to_tracker:make_list(List, []),
    send_to_tracker(TrackerPid, Info, New_list, Length).
send_to_tracker(_Pid, _Info, [], _Length) ->
    exit(self(), kill);
send_to_tracker(Pid, Info, [H|T], Length) ->
    Pid ! {connect, self(), {Info, H, Length}},
    receive
	{ok, Result} ->
	    Result
    after 2000 ->
	    send_to_tracker(Pid, Info, T, Length)
    end.
