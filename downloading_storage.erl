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
    loop(Tid).

loop(Tid) ->
    receive
	{request, Function, Args, From} ->
	    case Function of
		write_piece ->
		    [PieceIndex, Tuple,Pid]
			= Args,
		    From ! {reply, write(Tid, PieceIndex, Tuple,Pid)};
		put_back ->
		    [Pid,Index] = Args,
		    From ! {reply, put_back(Tid, Pid,Index)};
		put_back_with_only_pid ->
		    [Pid] = Args,
		    From ! {reply,put_back_with_only_pid(Tid,Pid)};
		compare_hash ->
		    [Pid,FileIndex,FileHash]= Args,
		    From!{reply,compare_hash(Tid,Pid,FileIndex,FileHash)}

	    end,
	    loop(Tid);
	stop -> ok	
    end.

%% insert new piece that has chosen to be downloaded
write(Tid, PieceIndex,Tuple,Pid) ->
    {PieceIndex,{Hash,Peers}} = Tuple,
    ets:insert(Tid, {{Pid,PieceIndex}, {PieceIndex,{Hash,Peers}}}).



put_back(Tid, Pid,Index)->
    case ets:lookup(Tid, {Pid,Index}) of
	[] ->
	    [];
	[{{Pid,Index}, {Index, {Hash, Peers}}}] ->
	    {Index, {Hash, Peers}}
    end.


put_back_with_only_pid(Tid,FilePid)->
    First_key = ets:first(Tid),
    put_back_new(Tid,FilePid,First_key,[]).
put_back_new(Tid,FilePid,Key,List)->
   case ets:lookup(Tid,Key) of
       [] ->
	   [];
       [{{Pid,Index},{Index,{Hash,Peers}}}]  ->
	   case Pid =:= FilePid of
	       true->
		   Next_key = ets:next(Tid,Key),
		   case Next_key of
		       '$end_of_table'->
			   [{Index, {Hash, Peers}}|List];
		       _found ->
			   put_back_new(Tid,FilePid,Next_key,[{Index,{Hash,Peers}}|List])
		   end;
	       false ->
		   Next_key = ets:next(Tid,Key),
		   case Next_key of
		       '$end_of_table' ->
			   List;
		       _found ->
			   put_back_new(Tid,FilePid,Next_key,List)
			       
		   end
     end
end.

compare_hash(Tid,Pid,FileIndex,FileHash)->
    case ets:lookup(Tid, {Pid, FileIndex})of
	[]->
	    cannot_find_this_Pid;
	[{{Pid,Index},{Index,{Hash,_Peers}}}] ->
	    case Index==FileIndex of
		true->
		    Hash=:=FileHash;
		false ->
		    index_dont_match
	    end
    end.
    
    
	    
