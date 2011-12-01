%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-18
%%% This module module stores piece info

-module(downloading_storage).
-export([start/0, init/0]).

start() ->
    spawn(?MODULE, init, []).

%% get the pid of piece_storage to be able to put the piece back
init() ->
    Tid = ets:new(db, []),
    receive
	{piece_storage, Piece_storage_pid} ->
	    loop(Tid, Piece_storage_pid)
    end.

loop(Tid, Piece_storage_pid) ->
    receive
	{request, Function, Args, From} ->
	    case Function of
		write_piece ->
		    [PieceIndex, Tuple,Pid]
			= Args,
		    From ! {reply, write(Tid, PieceIndex, Tuple,Pid)};
		delete_peer ->
		    [PieceIndex, PeerId] = Args,
		    Reply = delete_peer(Tid, PieceIndex, PeerId),
		    case Reply of
			true -> From ! {reply, Reply};
			_piece -> Piece_storage_pid ! {putback, Reply},
				  From ! {reply, has_putback}
				  %delete_piece(Tid, PieceIndex)
		    end
	    end,
	    loop(Tid, Piece_storage_pid);
	stop -> ok	
    end.

%% insert new piece that has chosen to be downloaded
write(Tid, PieceIndex,Tuple,Pid) ->
    {PieceIndex,{Hash,Peers}} = Tuple,
    ets:insert(Tid, {Pid, {PieceIndex,{Hash,Peers}}}).

%% if peer has disconnected, remove its peerId from the list storing which
%% peers are downloading the paticular piece.
delete_peer(Tid, PieceIndex, PeerId)->
    [PieceIndex, {PieceHash, AllPeerList, DownloadingPeerList}] =
	ets:lookup(Tid, PieceIndex),
    case DownloadingPeerList of
	[_H|[]]->
	    put_back(Tid, PieceIndex);
	[_H|_T]->
	    ets:insert(Tid, {PieceIndex, {PieceHash, AllPeerList,
				DownloadingPeerList -- [PeerId]}})
    end.

%% return the piece to be put back in the piece_storage


put_back(Tid, PieceIndex)->
   [PieceIndex, {PieceHash, AllPeerList,BadPeerId}] = 
	ets:lookup(Tid, PieceIndex),
   {PieceIndex, {PieceHash, AllPeerList--[BadPeerId]}}.
