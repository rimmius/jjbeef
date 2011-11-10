-module(dl_piece).
-export([start/3]).
-export([init/3]).
-define(TCP_OPTIONS, [binary, {packet, 0}]).
start(Piece, Peer_pid, Nr) ->
    spawn(dl_piece, init, [Piece, Peer_pid, Nr]).
init(Piece, Peer_pid, Nr) ->
    get_piece(Piece, Peer_pid, Nr).
get_piece(Piece, Peer_pid, Nr) ->
    {Ip, Port, Sock} = peers:get_a_peer(Peer_pid),
    connect_to_peer({Ip, Port, Sock}, Piece, Nr).

connect_to_peer({Ip, Port, Sock}, Piece, Nr) ->
    io:format("trying to send ~n"),
    Msg = "<len=0013><id=6><" ++ integer_to_list(Nr) ++ "><0>",
    case gen_tcp:send(Sock, Msg) of
	{error, closed} ->
	    io:format("could not establish ~n");
	ok ->
	    io:format("sent message ~n")
    end.
