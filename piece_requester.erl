%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 25 Nov 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(piece_requester).

-behaviour(gen_fsm).

%% API
-export([start_link/7, send_event/3, update_interest/3]).

%% gen_fsm callbacks
-export([init/1, am_choked_uninterested/2, am_choked_interested/2, am_unchoked_interested/2, am_unchoked_uninterested/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% -define(SERVER, ?MODULE).

-record(state, {parent, piece_storage, file_storage, download_storage, msg_handler, peer_id, interested_index = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Parent, Peer_mutex_pid, Piece_mutex_pid, 
	   File_storage_pid, Download_storage_pid, Socket, Peer_id) ->
    io:format("going to start fsm~n"),
    gen_fsm:start_link(?MODULE, [Parent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Download_storage_pid, Socket, Peer_id], []).

%%send_event(Pid, am_interested, Am_interested) ->
%%    case Am_interested of
%%	true -> gen_fsm:send_all_state_event(Pid, am_interested);
%%	false -> gen_fsm:send_all_state_event(Pid, am_not_interested)
%%    end;
send_event(Pid, am_choked, Am_choked) ->
    case Am_choked of
	1 -> gen_fsm:send_event(Pid, am_choked);
	0 -> gen_fsm:send_event(Pid, am_unchoked)
    end;
send_event(Pid, piece, {Is_complete, Index}) ->
    case Is_complete of
	true -> gen_fsm:send_event(Pid, {piece_complete, Index});
	false -> gen_fsm:send_event(Pid, {piece_incomplete, Index});
	error -> gen_fsm:send_event(Pid, {piece_error, Index})
    end;
send_event(Pid, keep_alive, _) ->
    gen_fsm:send_all_state_event(Pid, keep_alive).

%% send_event(Pid, interested_index, List_of_interest) ->
%%     gen_fsm:send_all_state_event(Pid, {interested_index, List_of_interest}).

update_interest(Pid, Index_in_list, Action) ->
    gen_fsm:send_all_state_event(Pid, {update_interest, Index_in_list, Action}).



%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
init([Parent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Download_storage_pid, Socket, Peer_id]) ->
    process_flag(trap_exit, true),
    Msg_handler_pid = message_handler:start(self(), Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid),
    link(Msg_handler_pid),
    io:format("msg_handler started~n"),
    My_bitfield_in_list = mutex:request(File_storage_pid, get_bitfield, []),
    mutex:received(File_storage_pid),
    message_handler:send(Msg_handler_pid, bitfield, My_bitfield_in_list),
    io:format("my bitfiled sent~n"),
    io:format("init complete~n"),
    {ok, am_choked_uninterested, #state{parent = Parent,
					piece_storage = Piece_mutex_pid,
					msg_handler = Msg_handler_pid,
					file_storage = File_storage_pid,
					download_storage = Download_storage_pid,
					peer_id = Peer_id}, 120000}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% @spec state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
%% state 1
am_choked_uninterested(am_unchoked, State) ->
    {next_state, am_unchoked_uninterested, State, 120000};
am_choked_uninterested(am_choked, State) ->
    {next_state, am_choked_uninterested, State, 120000};
am_choked_uninterested(timeout, State) ->
    message_handler:send(State#state.msg_handler, keep_alive, whatever),
    {next_state, am_choked_uninterested, State, 120000}.

%% state 2
am_choked_interested(am_unchoked, State) ->
    {next_state, am_unchoked_interested, State, 0};
am_choked_interested(am_choked, State) ->
    {next_state, am_choked_interested, State, 120000};
am_choked_interested(timeout, State) ->
    message_handler:send(State#state.msg_handler, keep_alive, whatever),
    {next_state, am_choked_interested, State, 120000}.
    
%% state 3
am_unchoked_interested(am_choked, State) ->
    {next_state, am_choked_interested, State, 120000};
am_unchoked_interested(am_unchoked, State) ->
	%% problem domain
	%% without timeout
    {next_state, am_unchoked_interested, State};
am_unchoked_interested({piece_complete, Index}, State) ->
    io:format("**piece_requester~w**  piece_complete received by piece_requester~n", [self()]),
    peers:notice_have(State#state.parent, Index),
    message_handler:send(State#state.msg_handler, have, Index),
    io:format("**piece_requester~w**  have sent. going to request again, see io:format below~n", [self()]),
    {next_state, am_unchoked_interested, State, 0};
am_unchoked_interested({piece_incomplete, Index}, State) ->
    Chunk_result = mutex:request(State#state.file_storage, what_chunk, [Index]),
    mutex:received(State#state.file_storage),
    
    case Chunk_result of
	{Begin, Length} ->
	    io:format("**piece_requester~w**Rdy to request index=~w, begin=~w, length=~w~n", [self(), Index, Begin, Length]),
	    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
	    %% without timeout, wait for complete/incomplete
	    {next_state, am_unchoked_interested, State};
	access_denied ->
	    io:format("~n~nALL PIECES ARE TAKEN~n~n"),
	    {next_state, am_unchoked_interested, State, 0}
    end;
am_unchoked_interested({piece_error, Index}, State) ->
    Reply = mutex:request(State#state.piece_storage, get_rarest_index_again, [State#state.peer_id, Index]),
    mutex:received(State#state.piece_storage),
	
	case Reply of
	{ok, Index, Data} ->
	    %% write the piece into dl_sto and remove it from piece_sto
	    mutex:request(State#state.download_storage, write_piece, [Index, Data, self()]),
	    mutex:received(State#state.download_storage),	    
	    
	    io:format("**piece_requester~w**  got rarest index = ~w, rdy to send request ~n", [self(), Index]),
	    Chunk_result = mutex:request(State#state.file_storage, what_chunk, [Index]),
	    mutex:received(State#state.file_storage),

	    case Chunk_result of
		{Begin, Length} ->
		    io:format("**piece_requester~w** Rdy to request index=~w, begin=~w, length=~w~n", [self(), Index, Begin, Length]),
		    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
		    %% without timeout, wait for complete/incomplete
		    {next_state, am_unchoked_interested, State};
		access_denied ->
		    io:format("~n~nALL PIECES ARE TAKEN~n~n"),
		    {next_state, am_unchoked_interested, State, 0}
	    end;		
	{hold} -> 
	    io:format("*****hold****** im not requesting any pieces for 20 seconds!!!! da shit wuha kung fu panda!!~n"),
	    {next_state, am_unchoked_interested, State, 20000}
    end;	
am_unchoked_interested(timeout, State) ->
    Reply = mutex:request(State#state.piece_storage, get_rarest_index, [State#state.peer_id]),
    mutex:received(State#state.piece_storage),
    
    %% TODO remove drom downloadin storage
    case Reply of
	{ok, Index, Data} ->
	    %% write the piece into dl_sto and remove it from piece_sto
	    mutex:request(State#state.download_storage, write_piece, [Index, Data, self()]),
	    mutex:received(State#state.download_storage),	    
	    
	    io:format("**piece_requester~w**  got rarest index = ~w, rdy to send request ~n", [self(), Index]),
	    Chunk_result = mutex:request(State#state.file_storage, what_chunk, [Index]),
	    mutex:received(State#state.file_storage),

	    case Chunk_result of
		{Begin, Length} ->
		    io:format("**piece_requester~w**Rdy to request index=~w, begin=~w, length=~w~n", [self(), Index, Begin, Length]),
		    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
		    %% without timeout, wait for complete/incomplete
		    {next_state, am_unchoked_interested, State};
		access_denied ->
		    io:format("~n~nALL PIECES ARE TAKEN~n~n"),
		    {next_state, am_unchoked_interested, State, 0}
	    end;		
	{hold} -> 
	    io:format("*****hold****** im not requesting any pieces for 20 seconds!!!! da shit wuha kung fu panda!!~n"),
	    {next_state, am_unchoked_interested, State, 20000}
    end.
%% and/or keep_alive
%% or timeout for pending requests

%% state 4
am_unchoked_uninterested(am_choked, State) ->
    {next_state, am_choked_uninterested, State, 120000};
am_unchoked_uninterested(am_unchoked, State) ->
    {next_state, am_unchoked_uninterested, State, 120000};
am_unchoked_uninterested(timeout, State) ->
    message_handler:send(State#state.msg_handler, keep_alive, whatever),
    {next_state, am_unchoked_uninterested, State, 120000}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/[2,3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
%% handle_event({interested_index, List_of_interest}, StateName, State) ->
%%     %% all states
%%     Am_interested = case List_of_interest of
%% 			[] -> false;
%% 			_ -> true
%% 		    end,

%%     NewStateName = case {StateName, Am_interested} of
%% 		       {am_choked_uninterested, true} -> am_choked_interested;
%% 		       {am_choked_interested, false} -> am_choked_uninterested;
%% 		       {am_unchoked_interested, false} -> am_unchoked_uninterested;
%% 		       {am_unchoked_uninterested, true} -> am_unchoked_interested;
%% 		       {_, _} -> StateName
%% 		   end,

%%     message_handler:send(State#state.msg_handler, am_interested, Am_interested),
%%     {next_state, NewStateName, State#state{interested_index = List_of_interest}};
handle_event({update_interest, Index_in_list, Action}, StateName, State) ->
    %% all states
    New_list_of_interest =  case Action of 
				add -> State#state.interested_index ++ Index_in_list;
				remove ->  
				    [Index] = Index_in_list,				    
				    message_handler:send(State#state.msg_handler, have, Index),
				    State#state.interested_index -- Index_in_list
			    end,
   
    Am_interested = case New_list_of_interest of
			[] ->  false;
			_ -> true
		    end,
    
    NewStateName = case {StateName, Am_interested} of
		       {am_choked_uninterested, true} -> am_choked_interested;
		       {am_choked_interested, false} -> am_choked_uninterested;
		       {am_unchoked_interested, false} -> am_unchoked_uninterested;
		       {am_unchoked_uninterested, true} -> am_unchoked_interested;
		       {_, _} -> StateName
		   end,
    
    message_handler:send(State#state.msg_handler, am_interested, Am_interested),
    {next_state, NewStateName, State#state{interested_index = New_list_of_interest}};
handle_event(keep_alive, StateName, State) ->
    {next_state, StateName, State, 120000}.

%% handle_event(am_interested, am_choked_uninterested, State) ->
%%     %% state 1
%%     message_handler:send(State#state.msg_handler, am_interested, true),
%%     {next_state, am_choked_interested, State};
%% handle_event(am_interested, am_unchoked_uninterested, State) ->
%%     %% state 4
%%     message_handler:send(State#state.msg_handler, am_interested, true),
%%     {next_state, am_unchoked_interested, State};
%% handle_event(am_interested, StateName, State) ->
%%     %% state 2 and 3
%%     {next_state, StateName, State};
%% handle_event(am_not_interested, am_choked_interested, State) ->
%%     %% state 2
%%     message_handler:send(State#state.msg_handler, am_interested, false),
%%     {next_state, am_choked_interested, State};
%% handle_event(am_not_interested, am_unchoked_interested, State) ->
%%     %% state 3
%%     message_handler:send(State#state.msg_handler, am_interested, false),
%%     {next_state, am_unchoked_interested, State};
%% handle_event(am_not_interested, StateName, State) ->
%%     %% state 1 and 4
%%     {next_state, StateName, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/[2,3], this function is called
%% to handle the event.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, _StateName, State) ->
    Msg_handler = State#state.msg_handler,
    Parent = State#state.parent,
    case Pid of
	Msg_handler ->	    
	    io:format("*****EXIT*****piece_requester (~w)'s child killed~n", [self()]);
	Parent ->
	    io:format("*****EXIT*****piece_requester (~w)'s parent killed~n", [self()]),
	    message_handler:close_socket(Msg_handler),
	    io:format("*****EXIT*****socket successfully closed~n")
    end,
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%
%% @spec terminate(Reason, StateName, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(normal, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
