%%%Created by:  Eva-Lisa Kedborn
%%%Creation date: 2011-10-18
%%%This module supervises the torrent parser module

-module(download_manager).
-export([start/2, init/2]).

start(Filepath, GUIPid) ->
    register(?MODULE, spawn(?MODULE, init, [Filepath, GUIPid])).

init(Filepath, GUIPid) ->
    process_flag(trap_exit, true),
    ets:new(torrent_info, [bag, named_table]),
    TorrentPid = spawn_link(read_torrent, file, [Filepath, self()]),
    receive
	{dict, Dict} ->
	    store_info(Dict, torrent_info),
	    loop(GUIPid, {dict, Dict})
    end.

loop(GUIPid, {dict, Dict}) ->
    receive
	{'EXIT', TorrentPid, Reason} ->
	    case Reason of
		normal -> ok;
		killed -> GUIPid ! {error, Reason};
		_Other -> GUIPid ! {error, _Other}
	    end
    end.

store_info(Dict, torrent_info) ->
    Info = dict:fetch(<<"info">>, Dict),
    ets:insert(torrent_info, Info).
