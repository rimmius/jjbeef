%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-18

-module(downloading_storage).
-export([start/0,init/0,stop/1]).
-export([received/1,write/5,delete/2,peer_disconnect/3]).


start() ->
    spawn(?MODULE, init, []).

%% get the pid of piece_storage to be able to put the piece back
init() ->
    Tid = ets:new(db, []),
    receive
	{piece_storage,Piece_storage_pid}->
	    free(Tid,Piece_storage_pid)
    end.

stop(Pid) ->
    Pid ! stop.

%% insert new piece which is being downloaded
write(Pid,PieceIndex,PieceHash,AllPeerList,DownloadingPeerList)->
    Pid!{write,PieceIndex,PieceHash,AllPeerList,DownloadingPeerList,self()},
    receive
	{reply,Reply}->
	    Reply
    end.

%% delete piece from downloading table 
delete(Pid,PieceIndex)->
    Pid!{delete,PieceIndex,self()},
    receive 
	{reply,Reply}->
	    Reply
    end.

%% info when a peer has disconnected
peer_disconnect(Pid,PieceIndex,PeerId)->
    Pid!{peer_disconnect,PieceIndex,PeerId,self()},
    receive
	{reply,Reply}->
	    Reply
    end.

%% once client received reply the mutex should be set to free
received(Pid) ->
    Pid ! {received, self()}, ok.


free(Tid,Piece_storage_pid) ->
    receive
	{write,PieceIndex,PieceHash,AllPeerList,DownloadingPeerList,From}->
	    From!{reply,write_local(Tid,PieceIndex,PieceHash,AllPeerList,
				    DownloadingPeerList)},
	    busy(From,Tid,Piece_storage_pid);
	{delete,PieceIndex,From}->
	    From!{reply,delete_local(Tid,PieceIndex)},
	    busy(From,Tid,Piece_storage_pid);
	{peer_disconnect,PieceIndex,PeerId,From}->
	    Reply = delete_peer(Tid,PieceIndex,PeerId),
	    case Reply of
		true ->
		    From!{reply,Reply};
		_piece ->
		    Piece_storage_pid!{putback,Reply},
		    From!{reply,has_putback},
		    delete_local(Tid,PieceIndex)
            end;
	stop -> ok
    end.

busy(ClientPid,Tid,Piece_storage_pid) ->
    receive
	{received, ClientPid} ->
	    free(Tid,Piece_storage_pid)
    end.

%% insert new piece that has chosen to be downloaded
write_local(Tid,PieceIndex,PieceHash,AllPeerList,DownloadingPeerList)->
    ets:insert(Tid,{PieceIndex,{PieceHash,AllPeerList,DownloadingPeerList}}).

%% delete a piece that has been downloaded
delete_local(Tid,PieceIndex)->
    ets:delete(Tid,PieceIndex).

%% return the piece to be put back in the piece_storage
put_back(Tid,PieceIndex)->
   [PieceIndex,{PieceHash,AllPeerList,_}] = ets:lookup(Tid,PieceIndex),
   {PieceIndex,{PieceHash,AllPeerList}}.

%% if peer has disconnected, remove its peerId from the list storing which
%% peers are downloading the paticular piece.
delete_peer(Tid,PieceIndex,PeerId)->
    [PieceIndex,{PieceHash,AllPeerList,DownloadingPeerList}] =
	ets:lookup(Tid,PieceIndex),
    case DownloadingPeerList of
	[_H|[]]->
	    put_back(Tid,PieceIndex);
	[_H|_T]->
	    ets:insert(Tid,{PieceIndex,{PieceHash,AllPeerList,
				DownloadingPeerList--[PeerId]}})
    end.
