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
	    {Peers_new, Peers_unew} = fetch_a_peer({Peers_nu, Peers_u}),
	    From ! {reply, hd(Peers_unew)},
	    loop({Peers_new, Peers_unew});
	{insert_peers, From, List_of_peers} ->
	    From ! {reply, ok},
	    loop({[List_of_peers|Peers_nu], Peers_u})
    end.
fetch_a_peer({[H|T], Peers_u}) ->
    {T, [H|Peers_u]}.
get_a_peer(Pid) ->
    Pid ! {req_peer, self()},
    receive
	{reply, Peer} ->
	    Peer
    end.

insert_new_peers(List_raw, Pid, Info) ->
    List_of_peers = make_peer_list(List_raw, "", 1, []),
    Hs_pid = handshake:start(),
    List_handshaken = handshake_all_peers(List_of_peers, Info, "12345678912345678912345678911", [], Hs_pid).
%Pid ! {insert_peers, self(), List_handshaken},
  %  receive
%	{reply, Reply} ->
%	    Reply
  %  end.
handshake_all_peers([], _Info, _Peer_id, New_list, _Hs_pid) ->
    New_list;
handshake_all_peers([{H, Port}|T], Info, Peer_id, New_list, Hs_pid) ->
    io:format(H),
    case handshake:send_handshake(H, Port, Info, Peer_id, Hs_pid) of
	ok ->
	    handshake_all_peers(T,Info, Peer_id, [H|New_list], Hs_pid);
	error ->
	    handshake_all_peers(T, Info, Peer_id, New_list, Hs_pid)
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
