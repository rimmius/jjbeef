%%%---------------------------------------------------------------------
%%% Created by: Fredrik Gustafsson
%%% Creation date: 2011-10-25
%%%--------------------------------------------------------------------- 
%%% Description module peers
%%%--------------------------------------------------------------------- 
%%% Spawns all workers for the task plus keeping 
%%% track of peers, supervising and controlling
%%% their downloading
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start()
%%%   starts the peers module 
%%%   returns the pid
%%%--------------------------------------------------------------------- 
%%% insert_new_peers()
%%%   inserts new peers from connect_to_tracker
%%%   module
%%%---------------------------------------------------------------------
%%% insert_valid_peer()
%%%   insert_valid peer successfully handshaken.
%%%---------------------------------------------------------------------
%%% notice_have()
%%%   notices its children when we have a new piece
%%%---------------------------------------------------------------------
%%% init()
%%%   initiates the module
%%%---------------------------------------------------------------------

-module(peers).
-export([start/6, insert_new_peers/3, insert_valid_peer/3, notice_have/2]).
-export([init/9]).

start(Dl_pid, Tracker_list, Pieces, Piece_length, 
      {Length, File_names, Length_in_list, Path}, File_name) ->
    spawn(peers, init, [Dl_pid, Tracker_list, Pieces, Piece_length, 
			Length, File_names, Length_in_list, File_name, Path]).

%%--------------------------------------------------------------------
%% Function: init/9
%% Purpose: Starts the Mutex and stores the pid of it in the loop
%% Args: Download manager pid, List of trackers, List of pieces,
%%       the length of each piece, the amount of pieces, Names of files
%%       as a list, Length of files as a list, path to where the finished
%%       file should end up.
%%--------------------------------------------------------------------

init(Dl_pid, Tracker_list, List_of_pieces, Piece_length, Length, File_names, 
     Length_in_list, File_name, Path) ->

    process_flag(trap_exit, true),
    Dets_name = File_name ++ ".dets",
    Peer_storage_pid = mutex:start(peer_storage, []),
    link(Peer_storage_pid),
    Piece_storage_pid = mutex:start(piece_storage, [List_of_pieces, Dets_name]),
    link(Piece_storage_pid),
    Dl_storage_pid = mutex:start(downloading_storage, []),
    link(Dl_storage_pid),
    File_storage_pid = mutex:start(file_storage, [Dl_storage_pid, File_names, 
						  List_of_pieces, Piece_length,
						  Length_in_list, 
						  Piece_storage_pid, 
						  Dets_name, Path]),
    link(File_storage_pid),
    Peers_pid = self(),
    Port_listener_pid = port_listener:start(6881, Dl_pid, Peers_pid),
    link(Port_listener_pid),
   
    spawn(fun() ->start_send(connect_to_tracker:make_list(Tracker_list, []), 
			     Peers_pid, Dl_pid, Length, File_storage_pid) end),
    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, 
	 Dl_storage_pid, [], Length).

%%--------------------------------------------------------------------
%% Function: start_send/5
%% Purpose: initiates the connecting to trackers 
%%  in a list.
%% Args: [H|T] the list of trackers.
%%   Peers_pid: the pid of the peers module.
%%   Dl_pid: pid of download_manager module
%%   Length: the total length of the file.
%%   File_storage_pid: The pid of the file_storage 
%%   module
%% Returns: -
%%--------------------------------------------------------------------

start_send([H|T], Peers_pid, Dl_pid, Length, File_storage_pid) ->
    process_flag(trap_exit, true),
    send_to_tracker([H|T], Peers_pid, Dl_pid, Length, File_storage_pid).

%%--------------------------------------------------------------------
%% Function: send_to_tracker/5
%% Purpose: Tries to connect to the different trackers
%% in the list and kills them if they timeout or exits
%% with exception.
%% Args: [H|T]: The list of trackers
%% Peers_pid : The pid of peers module.
%% Dl_pid: The pid of download_manager module
%% Length: The total length of the task.
%% File_storage_pid: The pid of file_storage module
%% Returns: -
%%--------------------------------------------------------------------

send_to_tracker([],  Peers_pid, _Dl_pid, _Length, _File_storage_pid) ->
    io:format("KILLING PEERS"),
    exit(Peers_pid, kill);
send_to_tracker([H|T],  Peers_pid, Dl_pid, Length, File_storage_pid) ->
    Tracker_pid = connect_to_tracker:start(Dl_pid, Peers_pid, Length, 
					   File_storage_pid),
    link(Tracker_pid),
    Tracker_pid ! {connect, self(), H},
    receive
	{ok, Peers} ->
	    insert_new_peers(Peers, Peers_pid, Dl_pid);
	{'EXIT', Tracker_pid, _Reason} ->
	    io:format("connecting to next tracker in list from exit ~n"),
	    send_to_tracker(T, Peers_pid, Dl_pid, Length, File_storage_pid)
    after 10000 ->
	    io:format("connecting to next tracker in list ~n"),
	    send_to_tracker(T, Peers_pid, Dl_pid, Length, File_storage_pid)
    end.

%%--------------------------------------------------------------------
%% Function: loop/7
%% Purpose: Controlling messages coming in to the
%%   process / supervising its children. 
%% Args: Dl_pid: The pid of download_manager
%%             module
%% Peers_storage_pid: The pid of peer_storage
%% module
%% File_storage_pid: The pid of file_storage
%% module
%% Piece_storage_pid: The pid of the piece_storage
%% module
%% Dl_storage_pid: The pid of downloading_storage
%% module
%% Children: The list of peers we are supervising.
%% Length: The total length of our torrent-task.
%% Returns: -
%%--------------------------------------------------------------------
 
loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, 
     Dl_storage_pid, Children, Length) ->
    receive
	{insert_new_peer, From, {Host, Peer_id, Sock, Port}} ->
	    case length(Children) > 50 of
		true ->
		    From ! {error, no_more_peers},
		    gen_tcp:close(Sock),
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, 
			 Piece_storage_pid, Dl_storage_pid, Children, Length);
		_ ->
		    mutex:request(Peer_storage_pid, insert_new_peer, 
				  [Host,Peer_id, Sock, Port, undefined]),
		    mutex:received(Peer_storage_pid),
		    {ok, Pid} = piece_requester:start_link(self(), 
							   Peer_storage_pid, 
							   Piece_storage_pid, 
							   File_storage_pid, 
							   Dl_storage_pid, 
							   Sock, Peer_id),
		    link(Pid),
		    New_children = insertChild({Pid, Sock, Peer_id}, Children),
		    From ! ok,
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, 
			 Piece_storage_pid, Dl_storage_pid, New_children, 
			 Length)
	    end;
	{update_interest, Index} ->
	    update_interest(Children, Index),
	    loop(Dl_pid, Peer_storage_pid, File_storage_pid, 
		 Piece_storage_pid, Dl_storage_pid, Children, Length);
	{get_downloaded, From} ->
	    {How_much, _Uploaded} = mutex:request(File_storage_pid, 
						  how_much, []),
	    mutex:received(File_storage_pid),
	    Pieces_dl = mutex:request(File_storage_pid, 
				      c_downloaded_pieces, []),
	    mutex:received(File_storage_pid),
	    case How_much =:= 0 of
		true ->
		    From ! {reply, 0},
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, 
			 Piece_storage_pid, Dl_storage_pid, Children, Length);
		_ ->
		    Perc = How_much / Length,
		    case Perc >= 1 of
			true ->
			    From ! {reply, {100, Pieces_dl}};
			_ ->
			    From ! {reply, {trunc(Perc*100), Pieces_dl}}
		    end,
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, 
			 Piece_storage_pid, Dl_storage_pid, Children, Length)
	    end;
	{amount_children, From} ->
	    From ! {reply, length(Children)},
	    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid,
		 Dl_storage_pid, Children, Length);
	{'EXIT', Peer_storage_pid, Reason} -> 
	    io:format("exit peer_storage with reason: ~w~n", [Reason]),
	    io:format("looping without peer_storage"),
	    loop(Dl_pid, peer_storage_crash, File_storage_pid, 
		 Piece_storage_pid, Dl_storage_pid, Children, Length);		
	{'EXIT', Piece_storage_pid, Reason} ->
	    io:format("exit piece_storage with reason: ~w~n", [Reason]),
	    io:format("looping without piece_storage"),
	    loop(Dl_pid, Peer_storage_pid, File_storage_pid, 
		 piece_storage_crash, Dl_storage_pid, Children, Length);
	{'EXIT', File_storage_pid, Reason} ->
	    io:format("exit file_storage with reason: ~w~n", [Reason]),
	    io:format("looping without file_storage"),
	    loop(Dl_pid, Peer_storage_pid, file_storage_crash, 
		 Piece_storage_pid, Dl_storage_pid, Children, Length);
	{'EXIT', Child, _} ->
	    case mutex:request(Dl_storage_pid, put_back_with_only_pid, 
			       [Child]) of
		[] ->
		    mutex:received(Dl_storage_pid);
		List ->	
		    mutex:received(Dl_storage_pid),
		    New_list = mutex:request(File_storage_pid, check_piece, 
					     [List]),
		    mutex:received(File_storage_pid),
		    mutex:request(Piece_storage_pid, put_pieces_back, 
				  [New_list]),
		    mutex:received(Piece_storage_pid)
	    end,
	    {value, {_Pid, _Socket, Peer_id}} = lists:keysearch(Child, 1, 
								Children),
	    mutex:request(Peer_storage_pid, delete_peer, [Peer_id]),
	    mutex:received(Peer_storage_pid),
	    New_children = removeChild(Child, Children, []),
	    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid,
		 Dl_storage_pid, New_children, Length)
    end.

%%--------------------------------------------------------------------
%% Function: removeChild/3
%% Purpose: Removing a child from the list of childs
%% Args: Child: The pid of the process who terminated
%% List: The list of children
%% New_children: The new children
%% Returns: List of children without the crashed 
%% process
%%--------------------------------------------------------------------

removeChild(_Child, [], New_children) ->
    New_children;
removeChild(Child, [{Pid, Socket, Peer_id}|T], New_children) ->
    case Child of
	Pid ->
	    removeChild(Child, T, New_children);
	_  ->
	    removeChild(Child, T, [{Pid, Socket, Peer_id}|New_children])
    end.

%%--------------------------------------------------------------------
%% Function: insertChild/2
%% Purpose: Inserting a child to the list of children 
%% Args:  {Pid, Socket, Peer_id}: The pid, Socket
%% and Peer_id of the peer/process
%% List: the List of children
%% Returns: A new list with the new children insert-
%% ed.
%%--------------------------------------------------------------------

insertChild({Pid, Socket, Peer_id}, List) ->
    [{Pid, Socket, Peer_id}|List].

%%--------------------------------------------------------------------
%% Function: insert_new_peers/3
%% Purpose: Tries to connect and handshake with
%% new peers from tracker. 
%% Args: List_raw: The raw list of peers.
%% Peers_pid: The pid of the peers module
%% Dl_pid: THe pid of download_manager module
%% Returns: ok
%%--------------------------------------------------------------------

insert_new_peers(List_raw, Peers_pid, Dl_pid) ->
    List_of_peers = make_peer_list(List_raw, "", 1, []),
    Info_hash = download_manager:get_my_info_hash(Dl_pid),
    My_id = download_manager:get_my_id(Dl_pid),
    ok = handshake_all_peers(List_of_peers, Info_hash, My_id, Peers_pid).

%%--------------------------------------------------------------------
%% Function: handshake_all_peers/4
%% Purpose: Handshaking all the peers from tracker
%% Args: [{H, Port}|T] The list of Ip's and ports from
%% tracker
%% Info: The info hash of this task.
%% Peer_id: Our peer id.
%% Peers_pid: The pid of peer module.
%% Returns: ok
%%--------------------------------------------------------------------

handshake_all_peers([], _Info, _Peer_id, _Peers_pid) ->
    ok;
handshake_all_peers([{H, Port}|T], Info, Peer_id, Peers_pid) ->
    io:format(H),
    case send_handshake(H, Port, Info, Peer_id, Peers_pid) of
	{error, _Reason} ->
	    handshake_all_peers(T, Info, Peer_id, Peers_pid);
	{ok, inserted} ->
	    handshake_all_peers(T, Info, Peer_id, Peers_pid)
    end.

%%--------------------------------------------------------------------
%% Function: make_peer_list/4
%% Purpose: Makes a proper list with ip's and ports
%% from a raw list from tracker.
%% Args: [H|T]: The list of ip's and ports
%% Ip: the ip generated
%% Byte: Accumalating bytes
%% New_list: New list with proper ip's and ports
%% Returns: New list with proper ips and ports.
%%--------------------------------------------------------------------

make_peer_list([], _Ip, _Byte, New_list) ->
    New_list;
make_peer_list([H|T], Ip, Byte, New_list) when Byte =< 4 ->
    make_peer_list(T, [H|Ip], Byte+1, New_list);
make_peer_list([H|[H2|T]], Ip, Byte, New_list) when Byte =:= 5 ->
    <<P:16>> = <<H, H2>>,
    make_peer_list(T, "", 1, [{convert_to_ip(Ip, ""), P}|New_list]).

%%--------------------------------------------------------------------
%% Function: convert_to_ip/2
%% Purpose: Converts a integer to a ip
%% Args: [H|T]: The raw ip list
%% New_list: The new generated ip list
%% Returns: The Ip list
%%--------------------------------------------------------------------

convert_to_ip([], New_list) ->
    New_list;
convert_to_ip([H|T], New_list) ->
    case New_list of
	[] ->
	    convert_to_ip(T, integer_to_list(H) ++ New_list);
	_ ->
	    convert_to_ip(T,integer_to_list(H) ++ "." ++ New_list)
    end.

%%--------------------------------------------------------------------
%% Function: send_handshake/5
%% Purpose: Tries to send a handshake
%% Args: Host: The host/ip of the peer.
%% Port: The port of the ip at peer.
%% Info: The info hash of the torrent-task.
%% My_peer_id: Our peer id
%% Peers_pid: The pid of peers module
%% Returns: {ok, inserted} if we handshaked and
%% they handshaked us back or {error, Reason}
%% when something went bad in the handshaking
%% sequence.
%%--------------------------------------------------------------------

send_handshake(Host, Port, Info, My_peer_id, Peers_pid) -> 
    case handshake_handler:send_handshake({ip, Host, Port}, Info, My_peer_id) of
	{ok, Socket} ->
	    case handshake_handler:recv_handshake(Socket, Info) of
		{ok, {Socket, Peer_id}} ->
		    case  insert_valid_peer(Peers_pid, Peer_id, Socket, Host, 
					    Port) of
			{error, Reason} ->
			    {error, Reason};
			ok ->
			    {ok, inserted}
		    end;
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: insert_valid_peer/5
%% Purpose:  Inserts a valid peer
%% Args:  Peers_pid: the pid of peers module
%% Peer_id: the peer_id of the peer.
%% Sock: The socket that the peer is using.
%% Host: THe ip of the peer.
%% Port: The port that the peer is using.
%% Returns: ok or {error, Reason} if failed.
%%--------------------------------------------------------------------

insert_valid_peer(Peers_pid, Peer_id, Sock, Host, Port) ->
    Peers_pid ! {insert_new_peer, self(), {Host, Peer_id, Sock, Port}},
    receive
	ok -> ok;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: insert_valid_peer/3
%% Purpose: Inserts a valid peer from port_listener
%% Args: Peers_pid : the pid of peers module
%% Peer_id: The peer_id of the peer.
%% Sock: The socket of the peer. 
%% Returns:-
%%--------------------------------------------------------------------

insert_valid_peer(Peers_pid, Peer_id, Sock) ->
    insert_valid_peer(Peers_pid, Peer_id, Sock, unknown, unknown).

%%--------------------------------------------------------------------
%% Function: update_interest/2
%% Purpose: updates our interest 
%% Args:  [{Child, Socket, Peer_id}|Children] :
%% the list of children
%% Index: The index.
%% Returns:-
%%--------------------------------------------------------------------

update_interest([], _Index) ->
    ok;
update_interest([{Child, _Socket, _Peer_id} | Children], Index) ->
    spawn(piece_requester, update_interest, [Child, [Index], remove]),
    update_interest(Children, Index).

%%--------------------------------------------------------------------
%% Function: notice_have/2
%% Purpose: Notices our peers of our new piece 
%% Args: Pid: the pid of the peer.
%% Index: the index we just downloaded
%% Returns:-
%%--------------------------------------------------------------------

notice_have(Pid, Index) ->
    Pid ! {update_interest, Index}.
