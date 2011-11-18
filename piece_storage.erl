%%% Created by: Francesco Cesarini,Simon Thompson 
%%% from the book Erlang Programming page 130 
%%% Modified by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-16

-module(piece_storage).
-export([start/1,init/1,stop/1]).
-export([received/1,insert_bitfield/3,read_piece/2]).

start(List)->
    spawn(?MODULE,init,[List]).

init(List)->
    Tid = ets:new(db,[ordered_set]),
    initiate_table(List,Tid),
    free(Tid).

stop(Pid)->
    Pid!stop.

initiate_table(List,Tid)->
    initiate_table(List,Tid,0).
initiate_table([H|T],Tid,Index)->
    ets:insert(Tid,{Index,{H,[]}}),
    initiate_table(T,Tid,Index+1);
initiate_table([],_Tid,_Index)->
    done.

insert_bitfield(Pid,PeerId,Bitfield)->
    Pid!{insert_bitfield,PeerId,Bitfield,self()},
    receive {reply,Reply}->
	    Reply
    end.

read_piece(Pid,Index)->
    Pid!{read_piece,Index,self()},
    receive {reply,Reply}->
	    Reply
    end.

received(Pid)->
    Pid!{received,self()},
    ok.

free(Tid)->
    receive
	{insert_bitfield,PeerId,Bitfield,From}->
	    Reply = insert_bitfield_local(Tid,PeerId,Bitfield),
	    case Reply of 
		true->io:format("we have inserted ~w, ~w, ~w~n",
				[Tid,PeerId,Bitfield])
	    end,
	    From!{reply,Reply},
	    busy(From,Tid);
	{read_piece,Index,From} ->
	    Reply = read_piece_local(Tid,Index),
	    io:format("we have read ~w, ~w~n",[Tid,Index]),
	    From!{reply,Reply},
	    busy(From,Tid);
	{putback,Piece} ->
	    putback(Piece,Tid)
    end.

busy(ClientPid,Tid)->
    receive
	{received,ClientPid}->
	    free(Tid)
    end.

insert_bitfield_local(Tid,PeerId,[H|T])->
    Has = [X||{1,X}<-[H|T]],
    insert_to_table(Has,PeerId,Tid).

insert_to_table([Has|T],PeerId,Tid)->
    [Index,{Hash,Peers}]=ets:lookup(Tid,Has),
     ets:insert(Tid,{Index,{Hash,[PeerId|Peers]}}),
     insert_to_table(T,PeerId,Tid);
insert_to_table([],_PeerId,_Tid) ->
     has_inserted.

read_piece_local(Tid,Index)->
    [Content]= ets:lookup(Tid,Index),
    Content.

%% insert the piece returned from downloading_storage
putback(Piece,Tid)->
    {PieceIndex,{Piecehash,AllPeersList}}=Piece,
    ets:insert(Tid, {PieceIndex,{Piecehash,AllPeersList}}).
