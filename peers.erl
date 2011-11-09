-module(peers).
-export([start/0, get_a_peer/1, insert_new_peers/4]).
-export([init/0]).

start() ->
    spawn(peers, init, []).
init() ->
    loop({[], []}).

loop({Peers_nu, Peers_u}) ->
    receive
	{req_peer, From} ->
	    case fetch_a_peer({Peers_nu, Peers_u}) of
		 {{Ip, Port, Sock}, Peers_new, Peers_unew} ->
		    From ! {reply, {Ip, Port, Sock}},
		    loop({Peers_new, Peers_unew})
	    end;
	{insert_peers, From, List_of_peers} ->
	    From ! {reply, ok},
	    loop({List_of_peers, Peers_u});
	{send_handshake, From, {Host, Port, Info, Peer_id}} ->
	    case gen_tcp:connect(Host, Port, [binary, {packet, 0}], 1000) of
		{ok, Sock} ->
		    Info_hashed = list_to_binary(sha:sha1hash(Info)),
		    Msg = list_to_binary([<<19>>,<<"BitTorrent Protocol">>, <<3,2,1,3,2,1,2,3>>, Info_hashed,list_to_binary(Peer_id)]),
		    io:format("baba~n"),
		    ok = gen_tcp:send(Sock, Msg),
		    From ! {reply, ok, Sock},
		    loop({Peers_nu, Peers_u});
		{error, Reason} ->
		    io:format(Reason),
		    From ! {error, Reason},
		    loop({Peers_nu, Peers_u})
	    end
    end.
fetch_a_peer({[], [H|T]}) ->
    Rand = random:uniform(length([H|T])),
    {Ip, Port, Sock} = lists:nth(Rand, [H|T]),
    {{Ip, Port, Sock}, [], [H|T]};
fetch_a_peer({[{Ip, Port, Sock}|T], Peers_u}) ->
    {{Ip, Port, Sock}, T, [{Ip, Port}|Peers_u]}.
get_a_peer(Pid) ->
    Pid ! {req_peer, self()},
    receive
	{reply, Peer} ->
	    Peer
    end.
insert_new_peers(List_raw, Peers_pid, Info, Dl_pid) ->
    List_of_peers = make_peer_list(List_raw, "", 1, []),
    port_listener:start(12345, Dl_pid),
    List_handshaken = handshake_all_peers(List_of_peers, Info, download_manager:get_my_id(Dl_pid), [], Peers_pid),
    Peers_pid ! {insert_peers, self(), List_handshaken},
    receive
	{reply, Reply} ->
	    Reply
    end.

handshake_all_peers([], _Info, _Peer_id, New_list, _Peers_pid) ->
    New_list;
handshake_all_peers([{H, Port}|T], Info, Peer_id, New_list, Peers_pid) ->
    io:format(H),
    case send_handshake(H, Port, Info, Peer_id, Peers_pid) of
	error ->
	    handshake_all_peers(T, Info, Peer_id, New_list, Peers_pid);
	Sock ->
	    handshake_all_peers(T,Info, Peer_id, [{H, Port, Sock}|New_list], Peers_pid)
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

send_handshake(Host, Port, Info, Peer_id, Pid) ->
    Pid ! {send_handshake, self(), {Host, Port, Info, Peer_id}},
    receive
	{reply, ok, Sock} ->
	    spawn(fun() -> recv_loop(Sock) end),
	    Sock;
	{error, _} ->
	    error
    end.

recv_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, Msg} ->
	    io:format("~w~w~n", ["From handshaken", Msg]),
	    recv_loop(Socket);
	{error, Reason} ->
	    io:format("~w~n", [Reason])
    end.
