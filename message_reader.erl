-module(message_reader).
-export([start/3]).
-export([loop/3]).

start(Parent, Table_id, Peer_id) ->
    spawn(?MODULE, loop, [Parent, Table_id, Peer_id]).

loop(Parent, Table_id, Peer_id) ->
    receive
	{bitfield, Bitfield} ->
	    mutex:update_peer(Parent, Peer_id, Table_id, bitfield, lol(Bitfield)),
	    loop(Parent, Table_id, Peer_id)
    end.

lol(<<P:1>>) ->
    [P];
lol(<<P:1, Rest/bits>>) ->
    [P | lol(Rest)].
     
