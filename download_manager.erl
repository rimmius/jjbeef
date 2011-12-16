%%%---------------------------------------------------------------------
%%% Created by: Fredrik Gustafsson
%%% Creation date: 2011-10-18
%%% Refactored date: 2011-11-23
%%%--------------------------------------------------------------------- 
%%% Description module downloading_manager
%%%--------------------------------------------------------------------- 
%%% What this module does...
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start(.*torrent file name, Pid of gui module)
%%%   spawns a new process running the init method and returns its pid.
%%%--------------------------------------------------------------------- 
%%% init(.*torrent file name, Pid of gui module)
%%%   
%%%---------------------------------------------------------------------
%%% is_valid_info_hash()
%%%   
%%%---------------------------------------------------------------------
%%% get_my_id()
%%%   
%%%---------------------------------------------------------------------
%%% get_my_info_hash()
%%%   
%%%---------------------------------------------------------------------
%%% get_info_clean()
%%%   
%%%---------------------------------------------------------------------

-module(download_manager).
-export([start/2,  init/2, is_valid_info_hash/2, 
	 get_my_id/1, get_my_info_hash/1, get_info_clean/1]).

%% Functions exported for testing only
-export([get_torrent_data/1, get_announce_list/1, get_pieces/1,
	 get_piece_length/1,get_length_and_name/1]).

start(File, GUIPid) ->
    spawn_link(?MODULE, init, [File, GUIPid]).
    
init(File, GUIPid) ->
    process_flag(trap_exit, true),
    link(GUIPid),
    {dict, Dict} = get_torrent_data(File),
    Info_raw = dict:fetch(<<"info">>, Dict),
    Info_bencoded =  bencode:encode(Info_raw),
    Info_hash = list_to_binary(sha:sha1raw(Info_bencoded)),
    {_, _, Path} = get_length_and_name({dict, Dict}),
    GUIPid ! {path, Path},
    Peers_pid = peers:start(self(), get_announce_list({dict, Dict}), 
			    get_pieces({dict, Dict}), 
			    get_piece_length({dict, Dict}), 
			    get_length_and_name({dict, Dict}), File),
    link(Peers_pid),
    loop(Peers_pid, Info_hash,Info_bencoded, guimain:createUniqueId(), 
	 GUIPid, 0).

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

%%--------------------------------------------------------------------
%% Function: loop/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------
  
loop(Peers_pid, Info_hash, Info_clean, My_id, GUI_pid, Counter) ->
    receive
	{get_clean_info, From} ->
	    From ! {reply, Info_clean},
	    loop(Peers_pid, Info_hash, Info_clean, My_id, GUI_pid, Counter);
	{valid_info, From, Info_from_peer} ->
	    From ! binary_to_list(Info_hash) =:= Info_from_peer,
	    loop(Peers_pid, Info_hash, Info_clean, My_id, GUI_pid, Counter);
	{get_id, From} ->
	    From ! {reply, My_id},
	    loop(Peers_pid, Info_hash, Info_clean, My_id, GUI_pid, Counter);
	{get_my_info_hash, From} ->
	    From ! {reply, Info_hash},
	    loop(Peers_pid, Info_hash, Info_clean, My_id, GUI_pid, Counter);
	{this_tracker, Tracker} ->
	    GUI_pid ! {tracker, Tracker},
	    loop(Peers_pid, Info_hash, Info_clean, My_id, GUI_pid, Counter);
	{'EXIT', Peers_pid, Reason} ->
	    io:format("Peerspid crashed!~w~n", [Reason]),
	    exit(self(), kill);
	stop -> ok
    after 3000 ->
	    Peers_pid ! {get_downloaded, self()},
	    receive
		{reply, Downloaded} ->
		    case Downloaded of
			100 ->
			    io:format("~n~nPERCENTAGE=~w%~n", [Downloaded]),
			    case Counter of
				0 ->
				    GUI_pid ! {percentage, Downloaded},
				     loop(Peers_pid, Info_hash, Info_clean, 
					  My_id, GUI_pid, 1);
				_ ->
				    ok
			    end;
			_ ->
			    io:format("~n~nPERCENTAGE=~w%~n", [Downloaded]),
			    GUI_pid ! {percentage, Downloaded}
		    end
	    end,
	    loop(Peers_pid, Info_hash, Info_clean, My_id, GUI_pid, Counter)
    end.

%%--------------------------------------------------------------------
%% Function: get_torrent_data/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_torrent_data(File) ->
    case  file:read_file(File) of
		{ok, Text} ->
		    case bencode:decode(Text) of
			{{dict, Dict}, _Remainder} ->
			    {dict, Dict}
		    end;
		_  ->
		    {error, no_file}
	    end.

%%--------------------------------------------------------------------
%% Function: get_announce_list/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_announce_list({dict, Dict}) ->
    %%ets:insert(torrent_info, Info),
     case dict:find(<<"announce-list">>, Dict) of
	 {ok,{_, List}} ->
	     List;
	 error ->
	     {ok, Link} = dict:find(<<"announce">>, Dict),
	     io:format("~w~n", [Link]),
	     [{ok, binary_to_list(Link)}]
     end.

%%--------------------------------------------------------------------
%% Function: get_pieces/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_pieces({dict, Dict}) ->
    %List = dict:fetch(<<"announce">>, Dict),
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    Pieces = dict:fetch(<<"pieces">>, Info_dict),
    handle_pieces(binary_to_list(Pieces),[], 1, []).

%%--------------------------------------------------------------------
%% Function: get_piece_length/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_piece_length({dict, Dict}) ->
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    dict:fetch(<<"piece length">>, Info_dict).

%%--------------------------------------------------------------------
%% Function: get_length_and_name/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_length_and_name({dict, Dict}) ->
    {_, Info_dict} = dict:fetch(<<"info">>, Dict),
    case dict:find(<<"files">>, Info_dict) of
	{ok, {_,Files_dict}} ->
	    {get_length(Files_dict, 0), 
	     get_names(Files_dict), get_lengths_list(Files_dict), 
	     dict:fetch(<<"name">>, Info_dict)};
	error ->
	    Name_of_files = dict:fetch(<<"name">>, Info_dict),
	    Length = dict:fetch(<<"length">>, Info_dict),
	    {Length, 
	     [binary_to_list(Name_of_files)], [Length], ""}
    end.

%%--------------------------------------------------------------------
%% Function: get_length/2
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_length([], Total) ->
    Total;
get_length([{_,H}|T], Total) ->
    {ok, Value} = dict:find(<<"length">>, H),
    get_length(T, Total+Value).

%%--------------------------------------------------------------------
%% Function: get_lengths_list/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_lengths_list([]) ->
    [];
get_lengths_list([{_,H}|T]) ->
    {ok, Value} = dict:find(<<"length">>,H),
    [Value|get_lengths_list(T)].

%%--------------------------------------------------------------------
%% Function: get_names/1
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

get_names([]) ->
    [];
get_names([{_, H}|T]) ->
    {ok, {list, [Value]}} = dict:find(<<"path">>, H),
    io:format("~n~w~n", [Value]),
    [binary_to_list(Value)|get_names(T)].

%%--------------------------------------------------------------------
%% Function: handle_pieces/4
%% Purpose: 
%% Args: 
%% Returns:
%%--------------------------------------------------------------------

handle_pieces([], Piece_list, _Byte, New_list) ->
    lists:reverse([lists:reverse(Piece_list)|New_list]);
handle_pieces([H|T],Piece_list, Byte, New_list) when Byte =< 20 ->
    handle_pieces(T,[H|Piece_list], Byte+1, New_list);
handle_pieces(List, Piece_list, _Byte, New_list)  ->
    handle_pieces(List,[], 1, [lists:reverse(Piece_list)|New_list]).

    

