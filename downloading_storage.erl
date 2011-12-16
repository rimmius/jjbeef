%%% Created by: Eva-Lisa Kedborn, Jing Liu
%%% Creation date: 2011-11-18
%%% This module module stores piece info
%%%--------------------------------------------------------------------- 
%%% Description module downloading_storage
%%%--------------------------------------------------------------------- 
%%% The downloading storage is for storing pieces that are being 
%%% downloaded by a process. They are moved from the piece storage 
%%% that stores all the pieces initially and everytime a process
%%% requests a piece it is deleted from the piece storage, to make sure no
%%% other process downloads the same piece, and moved into this storage.
%%% If a peer from whom we are downloading a piece then disconnects the  
%%% that piece will be moved back to the piece storage so it can be 
%%% requested for downloaded again.
%%%--------------------------------------------------------------------- 
%%% Exports start/0
%%%         init/0
%%%--------------------------------------------------------------------- 
%%% start()
%%%   spawns a new process running the init method
%%%--------------------------------------------------------------------- 
%%% init()
%%%   creates an ets table to store the pieces being downloaded
%%%   returns the TableID of the created table 
%%%---------------------------------------------------------------------  

-module(downloading_storage).
-export([start/0, init/0]).

start() ->
    spawn(?MODULE, init, []).

%%--------------------------------------------------------------------
%% Function: init/0
%% Purpose: Create an ets table to store the pieces being downloaded.
%% Args: none
%% Returns: TableID of the downloading table.
%%--------------------------------------------------------------------
init() ->
    Tid = ets:new(downloading_table, []),
    loop(Tid).

%%--------------------------------------------------------------------
%% Function:loop/1
%% Purpose: receive requests from downloading mutex about what functions to
%%          execute
%% Args:  TableID of downloading table
%% Returns: the requested information
%%--------------------------------------------------------------------
loop(Tid) ->
    receive
	{request, Function, Args, From} ->
	    case Function of
		write_piece ->
		    [PieceIndex, Tuple, Pid] = Args,
		    From ! {reply, write(Tid, PieceIndex, Tuple, Pid)};
		put_back ->
		    [Pid,Index] = Args,
		    From ! {reply, put_back(Tid, Pid, Index)};
		put_back_with_only_pid ->
		    [Pid] = Args,
		    From ! {reply, put_back_with_only_pid(Tid, Pid)};
		compare_hash ->
		    [Pid, FileIndex, FileHash] = Args,
		    From!{reply,compare_hash(Tid, Pid, FileIndex, FileHash)}
	    end,
	    loop(Tid);
	{lookup, Data, From} -> 
	    Result = ets:lookup(Tid, Data),
	    From ! {reply, Result},
	    loop(Tid);
	stop -> ok	
    end.

%%--------------------------------------------------------------------
%% Function:write/4
%% Purpose: insert new piece that has been  chosen to download
%% Args:  TableID of downloading table, piece index, the piece, processID
%%        of the peer.
%%--------------------------------------------------------------------
write(Tid, PieceIndex, Tuple, Pid) ->
    {PieceIndex, {Hash, Peers}} = Tuple,
    ets:insert(Tid, {{Pid, PieceIndex}, {PieceIndex, {Hash, Peers}}}).

%%--------------------------------------------------------------------
%% Function: put_back/3
%% Purpose: After a piece is downloaded, if its hash is corruped,this 
%%          function takes its piece index and peerPID,tries to find it
%%          in the downloading table, and returns the full piece info 
%%          to be put back to the piece table.
%% Args:  TableID of downloading table, ProcessId, piece index
%% Returns: either [] if no record is found, or {Index,{Hash,Peers}}which
%%          is the piece
%%--------------------------------------------------------------------
put_back(Tid, Pid, Index) ->
    case ets:lookup(Tid, {Pid, Index}) of
	[] ->
	    [];
	[{{Pid, Index}, {Index, {Hash, Peers}}}] ->
	    {Index, {Hash, Peers}}
    end.

%%--------------------------------------------------------------------
%% Function: put_back_withou_only_pid/2
%% Purpose: If a peer disconnects before we have received the entire
%%          piece,this function will try to find a list of pieces downloaded
%%          by this peer by its Pid. This list later will be put back to 
%%          the piece table so they are available to other peers.
%% Args: TableId of downloading table,Pid of the disconnected peer
%% Returns: either an empty list if no record is found or a list of pieces
%%--------------------------------------------------------------------
put_back_with_only_pid(Tid, FilePid) ->
    First_key = ets:first(Tid),
    put_back_new(Tid, FilePid, First_key, []).
put_back_new(Tid, FilePid, Key, List) ->
   case ets:lookup(Tid, Key) of
       [] ->
	   [];
       [{{Pid, Index}, {Index, {Hash, Peers}}}] ->
	   case Pid =:= FilePid of
	       true->
		   Next_key = ets:next(Tid, Key),
		   case Next_key of
		       '$end_of_table'->
			   [{Index, {Hash, Peers}}|List];
		       _found ->
			   put_back_new(Tid, FilePid, Next_key,
					[{Index, {Hash, Peers}}|List])
		   end;
	       false ->
		   Next_key = ets:next(Tid, Key),
		   case Next_key of
		       '$end_of_table' ->
			   List;
		       _found ->
			   put_back_new(Tid, FilePid, Next_key, List)
			       
		   end
	   end
   end.

%%--------------------------------------------------------------------
%% Function: compare_hash/4
%% Purpose: When a full piece has been downloaded,this method would verify
%%          its hash value.
%% Args: TableId of downloading table,ProcessId of the peer,piece index,
%%       piece hash.
%% Returns: true if the hash is good and false is the hash is corrupted
%%--------------------------------------------------------------------
compare_hash(Tid, Pid, FileIndex, FileHash) ->
    case ets:lookup(Tid, {Pid, FileIndex}) of
	[]->
	    cannot_find_this_Pid;
	[{{Pid, Index}, {Index, {Hash, _Peers}}}] ->
	    case Index == FileIndex of
		true->
		    Hash =:= FileHash;
		false ->
		    index_dont_match
	    end
    end.
    
    
	    
