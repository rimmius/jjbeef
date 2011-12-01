-module(piece_requester_sup).
-export([start/4, start_child/2, get_children_num/1, notice_have/2]).
-export([init/1, loop/2, remove_child/2, update_interest/2]).

-record(data, {socket, peer_id}).
-record(storage, {peer_storage, piece_storage, file_storage, download_storage}).

%% exported functions
start(Peer_storage_pid, Piece_storage_pid, File_storage_pid, Download_storage_pid) ->
    {ok, spawn_link(?MODULE, init, [#storage{peer_storage = Peer_storage_pid, 
					     piece_storage = Piece_storage_pid,
					     file_storage = File_storage_pid,
					     download_storage = Download_storage_pid}])}.

start_child(Pid, [Socket, Peer_id]) ->
    Pid ! {start_child, self(), #data{socket = Socket,
				      peer_id = Peer_id}},
    receive
	{reply, Reply} ->
	    Reply
    end.

get_children_num(Pid) ->
    Pid ! {get_children_num, self()},
    receive
	{reply, Reply} ->
	    Reply
    end.	

notice_have(Pid, Index) ->
    Pid ! {update_interest, self(), Index},
    receive
	{reply, Reply} ->
	    Reply
    end.
    
%% internal functions 
init(Storage) ->
    loop([], Storage).

loop(Children, Storage) ->
    process_flag(trap_exit, true),
    receive
	{start_child, From, Data} ->
	    Child = piece_requester:start_link(self(), Storage#storage.peer_storage, Storage#storage.piece_storage, Storage#storage.file_storage, Storage#storage.download_storage, Data#data.socket, Data#data.peer_id),
	    From ! {reply, ok},
	    loop([{Child, Data} | Children], Storage);
	{get_children_num, From} ->
	    From ! {reply, length(Children)},
	    loop(Children, Storage);
	{update_interest, From, Index} ->
	    From ! {reply, update_interest(Children, Index)},
	    loop(Children, Storage);
	{'EXIT', Pid, _Reason} ->
	    %% put back the piece
	    {Index, {Hash, Peers}} = mutex:request(Storage#storage.download_storage, put_back, [Pid]),
	    mutex:received(Storage#storage.download_storage),

	    mutex:request(Storage#storage.piece_storage, put_piece_back, [{Index, {Hash, Peers}}]),
	    mutex:received(Storage#storage.piece_storage),
	    
	    %% remove the child
	    loop(remove_child(Pid, Children), Storage)
    end.

remove_child(Child, [{Child, _Data} | Children]) ->
    Children;
remove_child(Child, [Other | Children]) ->
    [Other | remove_child(Child, Children)].

update_interest([], _Index) ->
    ok;
update_interest([Child | Children], Index) ->
    %% getting result? not sure
    spawn(piece_requester, update_interest, [Child, Index]),
    update_interest(Children, Index).
