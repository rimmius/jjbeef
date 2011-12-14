-module(message_reader).
-export([start/6, read_msg/3]).
-export([loop/6]).

%% exported
start(Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id) ->
    spawn(?MODULE, loop, [Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id]).

read_msg(Pid, Type, Args) ->
    Pid ! {Type, Args}.

loop(Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id) ->
    receive
	{keep_alive, []} ->
	    piece_requester:send_event(Requester_pid, keep_alive, ok);
	{bitfield, [Bitfield, Bitfield_len]} ->
	    %%io:format("~nBitfiled: ~w~n", [lol(<<Bitfield:Bitfield_len>>, 0)]),
	    Bitfield_in_list = make_bitfield_with_index(<<Bitfield:Bitfield_len>>, 0),
	    %% update piece storage
	    mutex:request(Piece_mutex_pid, insert_bitfield, [Peer_id, Bitfield_in_list]),
	    mutex:received(Piece_mutex_pid),
	    
	    %% get interest from file storage
	    {ok, List_of_interest} = mutex:request(File_storage_pid, compare_bitfield, [Bitfield_in_list]),
	    mutex:received(File_storage_pid),
	    
	    %% save in fsm bitfield and interest
	    piece_requester:update_interest(Requester_pid, List_of_interest, add);
%% 	    case length(List_of_interest) of
%% 		[] ->  piece_requester:send_event(Requester_pid, am_interested, false);
%% 		_ ->  piece_requester:send_event(Requester_pid, am_interested, true)
%% 	    end;
	   
	{have, [Piece_index]} ->
	    %% update piece storage
	    mutex:request(Piece_mutex_pid, update_bitfield, [Peer_id, Piece_index]),
	    mutex:received(Piece_mutex_pid),

	    %% get interest from file storage
	    Am_interested = mutex:request(File_storage_pid, have, [Piece_index]),
	    mutex:received(File_storage_pid),

	    case Am_interested of
		true -> 
		    piece_requester:update_interest(Requester_pid, [Piece_index], add);
		    %% piece_requester:send_event(Requester_pid, am_interested, Am_interested);
		false -> ok %% noting to do
	    end;
	{am_choked, [Arg]} ->
	    %% mutex:request(Peer_mutex_pid, update_peer, [Peer_id, choke, Arg]),
	    %% mutex:received(Peer_mutex_pid),
	    piece_requester:send_event(Requester_pid, am_choked, Arg);	
	{is_interested, [Arg]} ->
	    %% mutex:request(Peer_mutex_pid, update_peer, [Peer_id, interested, Arg]),
	    %% mutex:received(Peer_mutex_pid),
	    piece_uploader:send_event(Uploader_pid, is_interested, Arg);
	{port, [_Listen_port]} ->
	    %% mutex:request(Peer_mutex_pid, update_peer, [Peer_id, port, Listen_port]),
	    %% mutex:received(Peer_mutex_pid);
	    %% TODO
	    ok;
	{piece, [Index, Begin, Block, Block_len]} ->
	    Is_complete = mutex:request(File_storage_pid, insert_chunk, [Requester_pid, Index, Begin, Block, Block_len]),
	    mutex:received(File_storage_pid),
	    piece_requester:send_event(Requester_pid, piece, {Is_complete, Index});
	{request, [Index, Begin, Length]} ->
	    %%piece_uploader:send_event(Uploader_pid, request, [Index, Begin, Length]);
		ok;
	{cancel, [Index, Begin, Length]} ->
	    piece_uploader:send_event(Uploader_pid, cancel, [Index, Begin, Length])
    end,
    loop(Requester_pid, Uploader_pid, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Peer_id).

make_bitfield_with_index(<<P:1>>, Index) ->
    [{P, Index}];
make_bitfield_with_index(<<P:1, Rest/bits>>, Index) ->
    [{P, Index} | make_bitfield_with_index(Rest, Index+1)].
     
