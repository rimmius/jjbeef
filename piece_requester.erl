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
-export([init/1, am_choked_uninterested/2, am_choked_interested/2, am_unchoked_interested_unrequested/2, am_unchoked_interested_requested/2, am_unchoked_uninterested/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% -define(SERVER, ?MODULE).

-record(state, {parent, piece_storage, file_storage, download_storage, msg_handler, uploader,  peer_id, interested_index = [], sent_request}).

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
    gen_fsm:start_link(?MODULE, [Parent, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Download_storage_pid, Socket, Peer_id], []).

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

    {ok, Uploader_pid} = piece_uploader:start_link(self(), File_storage_pid, Msg_handler_pid),
    link(Uploader_pid),

    Msg_handler_pid ! {uploader, Uploader_pid},

    
    {ok, am_choked_uninterested, #state{parent = Parent,
					piece_storage = Piece_mutex_pid,
					file_storage = File_storage_pid,
					download_storage = Download_storage_pid,
					msg_handler = Msg_handler_pid,
					uploader = Uploader_pid,
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
    {next_state, am_unchoked_interested_unrequested, State, 0};
am_choked_interested(am_choked, State) ->
    {next_state, am_choked_interested, State, 120000};
am_choked_interested(timeout, State) ->
    message_handler:send(State#state.msg_handler, keep_alive, whatever),
    {next_state, am_choked_interested, State, 120000}.
    
%% state 3-1
am_unchoked_interested_unrequested(am_choked, State) ->
    {next_state, am_choked_interested, State, 120000};
am_unchoked_interested_unrequested(am_unchoked, State) ->
	%% problem domain
    {next_state, am_unchoked_interested_unrequested, State, 0};	
am_unchoked_interested_unrequested(timeout, State) ->
    Reply = mutex:request(State#state.piece_storage, get_rarest_index, [State#state.peer_id]),
    mutex:received(State#state.piece_storage),
    
    %% TODO remove drom downloadin storage
    case Reply of
	{ok, Index, Data} ->
	    %% write the piece into dl_sto and remove it from piece_sto
	    mutex:request(State#state.download_storage, write_piece, [Index, Data, self()]),
	    mutex:received(State#state.download_storage),	    
	    
	    %% io:format("**piece_requester~w**  got rarest index = ~w, rdy to send request ~n", [self(), Index]),
	    Chunk_result = mutex:request(State#state.file_storage, what_chunk, [Index]),
	    mutex:received(State#state.file_storage),

	    case Chunk_result of
		{Begin, Length} ->		
		    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
		    %% with 1 s timeout, wait for complete/incomplete
		    {next_state, am_unchoked_interested_requested, State#state{sent_request = {[Index, Begin, Length], 40}}, 1000};
		access_denied ->
		    {next_state, am_unchoked_interested_unrequested, State, 0}
	    end;		
	{hold} -> 
	    {next_state, am_unchoked_interested_unrequested, State, 20000}
    end.

%% state 3-2
am_unchoked_interested_requested(am_choked, State) ->
    {stop, normal, State};
    %% {next_state, am_choked_interested, State, 120000};
am_unchoked_interested_requested(am_unchoked, State) ->
	%% problem domain
    {next_state, am_unchoked_interested_requested, State, 1000};	
am_unchoked_interested_requested({piece_complete, Index}, State) ->
    peers:notice_have(State#state.parent, Index),
    message_handler:send(State#state.msg_handler, have, Index),
    {next_state, am_unchoked_interested_unrequested, State, 0};
am_unchoked_interested_requested({piece_incomplete, Index}, State) ->
    Chunk_result = mutex:request(State#state.file_storage, what_chunk, [Index]),
    mutex:received(State#state.file_storage),
    
    case Chunk_result of
	{Begin, Length} ->
	    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
	    %% with 1 s timeout, wait for complete/incomplete
	    {next_state, am_unchoked_interested_requested, State#state{sent_request = {[Index, Begin, Length], 40}}, 1000};
	access_denied ->
	    {next_state, am_unchoked_interested_unrequested, State, 0}
    end;
am_unchoked_interested_requested({piece_error, Old_index}, State) ->
    Reply = mutex:request(State#state.piece_storage, get_rarest_again, [State#state.peer_id, Old_index]),
    mutex:received(State#state.piece_storage),
	
	case Reply of
	{ok, Index, Data} ->
	    %% write the piece into dl_sto and remove it from piece_sto
	    mutex:request(State#state.download_storage, write_piece, [Index, Data, self()]),
	    mutex:received(State#state.download_storage),	    
	    
	    %% io:format("**piece_requester~w**  got rarest index = ~w, rdy to send request ~n", [self(), Index]),
	    Chunk_result = mutex:request(State#state.file_storage, what_chunk, [Index]),
	    mutex:received(State#state.file_storage),

	    case Chunk_result of
		{Begin, Length} ->
		    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
		    %% with 1 s timeout, wait for complete/incomplete
		    {next_state, am_unchoked_interested_requested, State#state{sent_request = {[Index, Begin, Length], 40}}, 1000};
		access_denied ->
		    {next_state, am_unchoked_interested_unrequested, State, 0}
	    end;		
	{hold} -> 
	    {next_state, am_unchoked_interested_unrequested, State, 20000}
	end;
am_unchoked_interested_requested(timeout, State) ->
    {Request, Timeout} = State#state.sent_request,
    case Timeout > 0 of
	false ->
	    io:format("~n**piece_requester~w** dropping lazy peer~n", [self()]),
	    %% message_handler:close_socket(State#state.msg_handler),
	    {stop, normal, State};
	true -> 
	    case (Timeout < 5) of
		true ->	io:format("~n~n~w Timeout: ~w - 1~n~n", [self(), Timeout]);
		false -> ok
	    end,
	    {next_state, am_unchoked_interested_requested, State#state{sent_request = {Request, Timeout - 1}}, 1000}
    end.


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
		       {am_choked_uninterested, true} -> am_choked_interested; %% state 1
		       {am_choked_interested, false} -> am_choked_uninterested; %% state 2
		       {am_unchoked_interested_unrequested, false} -> am_unchoked_uninterested; %% state 3-1
		       {am_unchoked_interested_requested, false} -> am_unchoked_uninterested; %% state 3-2 
		       %% ideally not possible, cuz each peer's assigned a different piece
		       {am_unchoked_uninterested, true} -> am_unchoked_interested_unrequested;	%% state 4			 
		       {_, _} -> StateName
		   end,

    TimeOut = case NewStateName of
		  am_unchoked_interested_unrequested -> 0; %% state 3-1      
		  am_unchoked_interested_requested -> 1000; %% state 3-2
		  _Other -> 120000
	      end,

    message_handler:send(State#state.msg_handler, am_interested, Am_interested),
    {next_state, NewStateName, State#state{interested_index = New_list_of_interest}, TimeOut};
handle_event(keep_alive, StateName, State) ->
    TimeOut = get_time_out(StateName),
    {next_state, StateName, State, TimeOut}.

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
handle_info({'EXIT', Pid, _Reason}, StateName, State) ->
    Msg_handler = State#state.msg_handler,
    Parent = State#state.parent,
    Uploader_pid = State#state.uploader,
    case Pid of
	Msg_handler ->	    
	    {stop, normal, State};
	Parent ->
	    %% message_handler:close_socket(Msg_handler),
	    {stop, normal, State};
	Uploader_pid ->
	    {ok, New_uploader_pid} = piece_uploader:start_link(self(), State#state.file_storage, State#state.msg_handler),
	    link(New_uploader_pid),
	    Timeout = get_time_out(StateName),
	    {next_state, StateName, State#state{uploader = New_uploader_pid}, Timeout}
    end.

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
terminate(normal, _StateName, State) ->
    exit(State#state.msg_handler, kill),
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
get_time_out(StateName) ->
    case StateName of
	am_choked_uninterested -> 120000; %% state 1 
	am_choked_interested -> 120000; %% state 2 
	am_unchoked_interested_unrequested -> 0; %% state 3-1
	am_unchoked_interested_requested -> 1000; %% state 3-2
	am_unchoked_uninterested -> 120000 %% state 4
    end.
