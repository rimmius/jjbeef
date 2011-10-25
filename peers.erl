-module(peers).
-export([start/0, get_a_peer/1, insert_new_peers/3]).
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
		    loop({Peers_new, Peers_unew});
		{{Ip, Port, Sock}, [], Peers_unew} ->
		    From ! {reply, {Ip, Port, Sock}},
		    loop({[], Peers_unew})
	    end;
	{insert_peers, From, List_of_peers} ->
	    From ! {reply, ok},
	    loop({List_of_peers, Peers_u})
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

insert_new_peers(List_raw, Pid, Info) ->
    List_of_peers = make_peer_list(List_raw, "", 1, []),
    Hs_pid = handshake:start(),
    List_handshaken = handshake_all_peers(List_of_peers, Info, "12345678912345678912345678911", [], Hs_pid),
    Pid ! {insert_peers, self(), List_handshaken},
    receive
	{reply, Reply} ->
	    Reply
    end.
handshake_all_peers([], _Info, _Peer_id, New_list, _Hs_pid) ->
    New_list;
handshake_all_peers([{H, Port}|T], Info, Peer_id, New_list, Hs_pid) ->
    io:format(H),
    case handshake:send_handshake(H, Port, Info, Peer_id, Hs_pid) of
	error ->
	    handshake_all_peers(T, Info, Peer_id, New_list, Hs_pid);
	Sock ->
	    handshake_all_peers(T,Info, Peer_id, [{H, Port, Sock}|New_list], Hs_pid)
    end.
make_peer_list([], _Ip, _Byte, New_list) ->
    New_list;
make_peer_list([H|T], Ip, Byte, New_list) when Byte =< 4 ->
    make_peer_list(T, [H|Ip], Byte+1, New_list);
make_peer_list([H|[H2|T]], Ip, Byte, New_list) when Byte =:= 5 ->
    <<P:16>> = <<H, H2>>,
    io:format("~w~n", [lists:reverse(Ip)]),
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
