%%%Created by: Fredrik Gustafsson
%%%Date: 2011-10-25

-module(peers).
-export([start/1, insert_new_peers/4, insert_valid_peer/3, 
	 insert_peers_later/3]).
-export([init/1]).

%%Starts the peers module and hands back the pid
start(List_of_pieces) ->
    spawn(peers, init, [List_of_pieces]).

%%Starts the Mutex and stores the pid of it in the loop
init(List_of_pieces) ->
    Peer_storage_pid = peer_storage:start(),
    link(Peer_storage_pid),
    Piece_storage_pid = piece_storage:start(List_of_pieces),
    link(Piece_storage_pid),
    File_storage_pid = file_storage:start(self(), [], length(List_of_pieces)),
    link(File_storage_pid),
    loop(Peer_storage_pid, File_storage_pid, Piece_storage_pid).

loop(Peer_storage_pid, File_storage_pid, Piece_storage_pid) ->
    receive
	{get_storage, From} ->
	    From ! {reply, {Peer_storage_pid, File_storage_pid, 
			    Piece_storage_pid}},
	    loop(Peer_storage_pid, File_storage_pid, Piece_storage_pid);
	{send_handshake, From, {Host, Port, Info, Peer_id}} ->
	    case gen_tcp:connect(Host, Port, [binary, {active, false},
					      {packet, 0}], 1000) of
		{ok, Sock} ->
		    Msg = list_to_binary([<<19>>,<<"BitTorrent protocol">>,
					  <<3,2,1,3,2,1,2,3>>,Info,
					  list_to_binary(Peer_id)]),
		    ok = gen_tcp:send(Sock, Msg),
		    io:format("Sent handshake to peer from tracker~n"),
		    From ! {reply, ok, Sock},
		    loop(Peer_storage_pid, File_storage_pid, Piece_storage_pid);
		{error, Reason} ->
		    io:format(Reason),
		    From ! {error, Reason},
		    loop(Peer_storage_pid, File_storage_pid, Piece_storage_pid)
	    end;
	{'EXIT', Peer_storage_pid, Reason} -> 
	    io:format("exit peer_storage with reason: ~w~n", [Reason]),
	    io:format("looping without peer_storage");		
	{'EXIT', Piece_storage_pid, Reason} ->
	    io:format("exit piece_storage with reason: ~w~n", [Reason]),
	    io:format("looping without piece_storage");
	{'EXIT', File_storage_pid, Reason} ->
	    io:format("exit file_storage with reason: ~w~n", [Reason]),
	    io:format("looping without file_storage")
    end.

insert_new_peers(List_raw, Peers_pid, Dl_pid, Tracker_pid) ->
    List_of_peers = make_peer_list(List_raw, "", 1, []),
    Port_listener_pid = port_listener:start(12345, Dl_pid, Peers_pid),
    link(Port_listener_pid),
    Info_hash = download_manager:get_my_info_hash(Dl_pid),
    My_id = download_manager:get_my_id(Dl_pid),
    ok = handshake_all_peers(List_of_peers, Info_hash, My_id, [], 
			     Peers_pid, Dl_pid),
    connect_to_tracker:give_more(Peers_pid, Tracker_pid, Dl_pid).

insert_peers_later(List_raw, Peers_pid, Dl_pid) ->
    List_of_peers = make_peer_list(List_raw, "", 1, []),
    Info_hash = download_manager:get_my_info_hash(Dl_pid),
    My_id = download_manager:get_my_id(Dl_pid),    
    io:format("~w~n", [Dl_pid]),
    ok = handshake_all_peers(List_of_peers, Info_hash, My_id, [], 
			     Peers_pid, Dl_pid),
    io:format("~n~nHANDSHAKED~n~n").

handshake_all_peers([], _Info, _Peer_id, New_list, _Peers_pid, _Dl_pid) ->
    ok;
handshake_all_peers([{H, Port}|T], Info, Peer_id, New_list, Peers_pid, 
		    Dl_pid) ->
    io:format(H),
    case send_handshake(H, Port, Info, Peer_id, Peers_pid, Dl_pid) of
	error ->
	    handshake_all_peers(T, Info, Peer_id, New_list, Peers_pid, Dl_pid);
	Sock ->
	    handshake_all_peers(T,Info, Peer_id, [{H, Port, Sock}|New_list], 
				Peers_pid, Dl_pid)
    end.

make_peer_list([], _Ip, _Byte, New_list) ->
    New_list;
make_peer_list([H|T], Ip, Byte, New_list) when Byte =< 4 ->
    make_peer_list(T, [H|Ip], Byte+1, New_list);
make_peer_list([H|[H2|T]], Ip, Byte, New_list) when Byte =:= 5 ->
    <<P:16>> = <<H, H2>>,
    make_peer_list(T, "", 1, [{convert_to_ip(Ip, ""), P}|New_list]).

convert_to_ip([], New_list) ->
    New_list;
convert_to_ip([H|T], New_list) ->
    case New_list of
	[] ->
	    convert_to_ip(T, integer_to_list(H) ++ New_list);
	_ ->
	    convert_to_ip(T,integer_to_list(H) ++ "." ++ New_list)
    end.

send_handshake(Host, Port, Info, Peer_id, Peers_pid, Dl_pid) ->
    Peers_pid ! {send_handshake, self(), {Host, Port, Info, Peer_id}},
    receive
	{reply, ok, Sock} ->
	    spawn(fun() -> recv_loop(Sock, Dl_pid, Host, Port, Peers_pid) end),
	    Sock;
	{error, _} ->
	    error
    end.

recv_loop(Socket, Dl_pid, Host,Port, Peers_pid) ->
    case gen_tcp:recv(Socket, 20) of
	{ok, <<19, "BitTorrent protocol">>} ->
	%%{ok, <<Pstrlen:8/integer, 
	%%     Pstr:(19*8), 
	%%   Reserved:64, 
	%% Info_hash:160,
	%%Peer_id:160>>} ->
	    case gen_tcp:recv(Socket, 48) of
		{ok, <<Reserved:64,
		       Info_hash:160,
		       Peer_id:160>>} ->
		    io:format("GOT HANDSHAKE BACK~n~n~n"),
		    Pid_h = handshake_handler:start(Dl_pid),
		    Pid_h ! {handshake, self(), Reserved, <<Info_hash:160>>, 
			     Peer_id},
		    receive
			{reply, Pid_h, ok} ->
			    io:format("HANDSHAKE BACK FROM TRACKER PEER 
                                              APPROVED~n~n~n"),
			    insert_valid_peer(Peers_pid, Peer_id, Socket, 
					      Host, Port),
			    io:format("WAITING FOR MESSAGE FROM TRACKER
                                         PEER~n~n~n");
			{reply, Pid_h, drop_connection} ->
			    gen_tcp:close(Socket)
		    end
	    end;
	{ok, Data} ->	
	    io:format("FYFAN DATA: " ++binary_to_list(Data) ++ "~n");
	{error, Reason} ->
	    io:format("FYFAN REASON: ~w~n", [Reason])
    end.

insert_valid_peer(Peers_pid, Peer_id, Sock, Host, Port) ->
    Peers_pid ! {get_storage, self()},
    receive
	{reply, {Peer_storage_pid, File_storage_pid, Piece_storage_pid}} ->
	    io:format("~n~n~nGot T_id and Mutex pid~n~n~n"),
	    peer_storage:insert_new_peer(Peer_storage_pid, Host,Peer_id, 
					 Sock, Port),
	    peer_storage:received(Peer_storage_pid),
	    message_handler:start(Peer_storage_pid,File_storage_pid, 
				  Piece_storage_pid, Sock, Peer_id)
    end.

insert_valid_peer(Peers_pid, Peer_id, Sock) ->
    insert_valid_peer(Peers_pid, Peer_id, Sock, unknown, unknown).
    %% A = gen_tcp:recv(Socket, 0),
    %% io:format("Received handshake back~n"),
    %% message_handler:start(Dl_pid, Socket),
    %% io:format("~n").
