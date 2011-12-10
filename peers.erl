%%%Created by: Fredrik Gustafsson
%%%Date: 2011-10-25

-module(peers).
-export([start/5, insert_new_peers/3, insert_valid_peer/3, notice_have/2]).
-export([init/7]).

%%Starts the peers module and hands back the pid
start(Dl_pid, Tracker_list, Pieces, Piece_length, {Length, File_names, Length_in_list}) ->
    spawn(peers, init, [Dl_pid, Tracker_list, Pieces, Piece_length, Length, File_names, Length_in_list]).

%%Starts the Mutex and stores the pid of it in the loop
init(Dl_pid, Tracker_list, List_of_pieces, Piece_length, Length, File_names, Length_in_list) ->
    process_flag(trap_exit, true),
    Peer_storage_pid = mutex:start(peer_storage, []),
    link(Peer_storage_pid),
    Piece_storage_pid = mutex:start(piece_storage, [List_of_pieces]),
    link(Piece_storage_pid),
    Dl_storage_pid = mutex:start(downloading_storage, []),
    link(Dl_storage_pid),
    File_storage_pid = mutex:start(file_storage, [Dl_storage_pid, File_names, length(List_of_pieces), Piece_length, Length_in_list, Piece_storage_pid]),
    link(File_storage_pid),

    Peers_pid = self(),
    Port_listener_pid = port_listener:start(6881, Dl_pid, Peers_pid),
    link(Port_listener_pid),
   
    spawn(fun() ->start_send(connect_to_tracker:make_list(Tracker_list, []), Peers_pid, Dl_pid, Length, File_storage_pid) end),
    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, [], Length).

start_send([H|T], Peers_pid, Dl_pid, Length, File_storage_pid) ->
    process_flag(trap_exit, true),
    send_to_tracker([H|T], Peers_pid, Dl_pid, Length, File_storage_pid).
send_to_tracker([],  Peers_pid, _Dl_pid, _Length, _File_storage_pid) ->
    io:format("KILLING PEERS"),
    exit(Peers_pid, kill);
send_to_tracker([H|T],  Peers_pid, Dl_pid, Length, File_storage_pid) ->
    Tracker_pid = connect_to_tracker:start(Dl_pid, Peers_pid, Length, File_storage_pid),
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
 
loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, Children, Length) ->
    receive
	{insert_new_peer, From, {Host, Peer_id, Sock, Port}} ->
	    case length(Children) > 50 of
		true ->
		    From ! {error, no_more_peers},
		    gen_tcp:close(Sock),
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, Children, Length);
		_ ->
		    mutex:request(Peer_storage_pid, insert_new_peer, [Host,Peer_id, 
								      Sock, Port, undefined]),
		    mutex:received(Peer_storage_pid),
		    {ok, Pid} = piece_requester:start_link(self(), Peer_storage_pid, Piece_storage_pid, File_storage_pid, Dl_storage_pid, Sock, Peer_id),
		    link(Pid),
		    New_children = insertChild({Pid, Sock}, Children),
		    From ! {reply, ok},
		    io:format("~n~nIN THE LOOP SENT MESS BACK"),
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, New_children, Length)
	    end;
	{update_interest, Index} ->
	    update_interest(Children, Index),
	    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, Children, Length);
	{get_downloaded, From} ->
	    How_much = mutex:request(File_storage_pid, how_much, []),
	    mutex:received(File_storage_pid),
	    case How_much =:= 0 of
		true ->
		    From ! {reply, 0},
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, Children, Length);
		_ ->
		    Perc = (Length div How_much) * 100,
		    From ! {reply, Perc},
		    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, Children, Length)
	    end;
	{'EXIT', Peer_storage_pid, Reason} -> 
	    io:format("exit peer_storage with reason: ~w~n", [Reason]),
	    io:format("looping without peer_storage"),
	    loop(Dl_pid, peer_storage_crash, File_storage_pid, Piece_storage_pid, Dl_storage_pid, Children, Length);		
	{'EXIT', Piece_storage_pid, Reason} ->
	    io:format("exit piece_storage with reason: ~w~n", [Reason]),
	    io:format("looping without piece_storage"),
	    loop(Dl_pid, Peer_storage_pid, File_storage_pid, piece_storage_crash, Dl_storage_pid, Children, Length);
	{'EXIT', File_storage_pid, Reason} ->
	    io:format("exit file_storage with reason: ~w~n", [Reason]),
	    io:format("looping without file_storage"),
	    loop(Dl_pid, Peer_storage_pid, file_storage_crash, Piece_storage_pid, Dl_storage_pid, Children, Length);
	{'EXIT', Child, _} ->
	    io:format("~n~nChild crashed!!!!!!!!!!!!!~w~n~n", [Child]),
	    case mutex:request(Dl_storage_pid, put_back, [Child]) of
		[] ->
		    io:format("~n~nTHAT PEER DID NOT HAVE A PIECE ~n~n"),
		    mutex:received(Dl_storage_pid);
		{Index, {Hash, Peers_piece}} ->	
		    io:format("~n~nTHAT PEER HAD A PIECE~n~n"),
		    mutex:received(Dl_storage_pid),
		    mutex:request(Piece_storage_pid, put_piece_back, [Index, Hash, Peers_piece]),
		    mutex:received(Piece_storage_pid),
		    io:format("~n~n~nmoved back that piece~n~n")
	    end,
	    New_children = removeChild(Child, Children, []),
	    loop(Dl_pid, Peer_storage_pid, File_storage_pid, Piece_storage_pid, Dl_storage_pid, New_children, Length)
    end.

removeChild(_Child, [], New_children) ->
    New_children;
removeChild(Child, [{Pid, Socket}|T], New_children) ->
    case Child of
	Pid ->
	    io:format("~n~nFOUND THAT PEER, REMOVING~n~n"),
	    removeChild(Child, T, New_children);
	_  ->
	    removeChild(Child, T, [{Pid, Socket}|New_children])
    end.
insertChild({Pid, Socket}, List) ->
    [{Pid, Socket}|List].

insert_new_peers(List_raw, Peers_pid, Dl_pid) ->
    List_of_peers = make_peer_list(List_raw, "", 1, []),
    Info_hash = download_manager:get_my_info_hash(Dl_pid),
    My_id = download_manager:get_my_id(Dl_pid),
    ok = handshake_all_peers(List_of_peers, Info_hash, My_id, [], 
			     Peers_pid, Dl_pid).

handshake_all_peers([], _Info, _Peer_id, _New_list, _Peers_pid, _Dl_pid) ->
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
    My_pid = self(),
    Hs_pid = spawn(fun() ->connect_and_handshake:start(Host, Port, Info, Peer_id, My_pid) end),
    receive
	{reply, ok, Sock} ->
	    spawn(fun() -> recv_loop(Sock, Dl_pid, Host, Port, Peers_pid) end),
	    Sock;
	{error, _Reason} ->
	    error
    after 5000 ->
	    exit(Hs_pid, kill),
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
		    Pid_h = handshake_handler:start(Dl_pid),
		    Pid_h ! {handshake, self(), Reserved, <<Info_hash:160>>, 
			     Peer_id},
		    receive
			{reply, Pid_h, ok} ->
			    io:format("~n~nGot handshake back from tracker_peer~n"),
			    insert_valid_peer(Peers_pid, Peer_id, Socket, 
					      Host, Port);
			{reply, Pid_h, drop_connection} ->
			    gen_tcp:close(Socket)
		    end
	    end;
	{ok, _Data} ->	
	    ok;
	{error, Reason} ->
	    io:format("FYFAN REASON: ~w~n", [Reason])
    end.

insert_valid_peer(Peers_pid, Peer_id, Sock, Host, Port) ->
    Peers_pid ! {insert_new_peer, self(), {Host, Peer_id, Sock, Port}},
    receive
	{reply, Reply} ->
	    Reply;
	{error, _} ->
	    ok
    end.

insert_valid_peer(Peers_pid, Peer_id, Sock) ->
    insert_valid_peer(Peers_pid, Peer_id, Sock, unknown, unknown).
    %% A = gen_tcp:recv(Socket, 0),
    %% io:format("Received handshake back~n"),
    %% message_handler:start(Dl_pid, Socket),
    %% io:format("~n").
update_interest([], _Index) ->
    ok;
update_interest([{Child, _Socket} | Children], Index) ->
    %% getting result? not sure
    spawn(piece_requester, update_interest, [Child, [Index], remove]),
    update_interest(Children, Index).

notice_have(Pid, Index) ->
    Pid ! {update_interest, Index}.
