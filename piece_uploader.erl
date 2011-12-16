%%%---------------------------------------------------------------------
%%% Created by: Bruce Yinhe
%%% Creation date: 2011-12-09
%%%--------------------------------------------------------------------- 
%%% Description module piece_uploader
%%%--------------------------------------------------------------------- 
%%% This module is spawned by piece_requester. It receives interested/
%%% request messages from message_reader and sends out unchoke/choke/piece
%%%--------------------------------------------------------------------- 
%%% Exports 
%%%--------------------------------------------------------------------- 
%%% start_link(Piece_requester_pid, File_storage_pid, Msg_handler_pid)
%%%      Creates a gen_fsm process which calls Module:init/1 to
%%%      initialize. To ensure a synchronized start-up procedure, this
%%%      function does not return until Module:init/1 has returned.
%%%--------------------------------------------------------------------- 
%%% send_event(Pid, Type, Args)
%%%      sends a event to teh gen_fsm, to cahnge the states
%%%---------------------------------------------------------------------
-module(piece_uploader).

-behaviour(gen_fsm).

%% API
-export([start_link/3, send_event/3]).

%% gen_fsm callbacks
-export([init/1, is_choked_uninterested/2, is_choked_interested/2,
	 is_unchoked_uninterested/2, is_unchoked_interested_unrequested/2,
	 is_unchoked_interested_requested/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {piece_requester, file_storage, msg_handler, requests = []}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Piece_requester_pid, File_storage_pid, Msg_handler_pid) ->
    gen_fsm:start_link(?MODULE, [Piece_requester_pid, File_storage_pid, 
				 Msg_handler_pid], []).


send_event(Pid, is_interested, Arg) ->
    case Arg of
	1 -> gen_fsm:send_event(Pid, is_interested);
	0 -> gen_fsm:send_event(Pid, is_not_interested)
    end;
send_event(Pid, request, [Index, Begin, Length]) ->
    gen_fsm:send_event(Pid, {request, {Index, Begin, Length}});
send_event(Pid, cancel, [Index, Begin, Length]) ->
    gen_fsm:send_event(Pid, {cancel, {Index, Begin, Length}});
send_event(Pid, stop, []) ->
    gen_fsm:send_all_state_event(Pid, stop).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% Function: init/3
%% Purpose: Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%%          gen_fsm:start_link/[3,4], this function is called by the new
%%          process to initialize.
%% Args: pid of piece_requester,pid of file_storage,pid of msg_handler
%% Returns: {ok, StateName, State} 
%%--------------------------------------------------------------------
init([Piece_requester_pid, File_storage_pid, Msg_handler_pid]) ->
    process_flag(trap_exit, true),
    My_bitfield_in_list = mutex:request(File_storage_pid, get_bitfield, []),
    mutex:received(File_storage_pid),
    message_handler:send(Msg_handler_pid, bitfield, My_bitfield_in_list),
    {ok, is_choked_uninterested, #state{piece_requester = Piece_requester_pid,
					file_storage = File_storage_pid,
					msg_handler = Msg_handler_pid}}.

%%--------------------------------------------------------------------
%% Function: is_choked_uniterested/2
%% Purpose: state 1 
%%          This is a gen_fsm state. The peer is choked, not interested 
%%          in our pieces
%% Agrs: atom is_interested, State
%% Returns: {next_state, NextStateName, NextState} 
%%--------------------------------------------------------------------
is_choked_uninterested(is_interested, State) ->
    message_handler:send(State#state.msg_handler, unchoke, []),
    {next_state, is_unchoked_interested_unrequested, State};
is_choked_uninterested(is_not_interested, State) ->
    {next_state, is_choked_uninterested, State}.

%%--------------------------------------------------------------------
%% Function: is_choked_interested/2
%% Purpose: state 2
%%          This is a gen_fsm state. The peer is choked, interested in
%%          our pieces
%% Args: atom is_interested,State
%% Returns: {next_state, NextStateName, NextState} 
%%--------------------------------------------------------------------
is_choked_interested(is_interested, State) ->
    {next_state, is_choked_interested, State};
is_choked_interested(is_not_interested, State) ->
    {next_state, is_choked_uninterested, State}.

%%--------------------------------------------------------------------
%% Function: is_unchoked_interested_unrequested
%% Purpose: state 3-1
%%          This is a gen_fsm state. The peer is unchoked, interested
%%          in our pieces. It has not sent any requests to us.
%% Args: atom is_interested,State
%% Returns:  {next_state, NextStateName, NextState} |
%%           {next_state, NextStateName, NextState, Timeout}
%%--------------------------------------------------------------------
is_unchoked_interested_unrequested(is_interested, State) ->
    {next_state, is_unchoked_interested_unrequested, State};
is_unchoked_interested_unrequested(is_not_interested, State) ->
    {next_state, is_unchoked_uninterested, State};
is_unchoked_interested_unrequested({request, Data}, State) ->    
    {next_state, is_unchoked_interested_requested, State#state{requests =
    [Data]}, 0}.   

%%--------------------------------------------------------------------
%% Function: is_unchoked_interested_requested/2
%% Purpose: state 3-2
%%          This is a gen_fsm state. The peer is unchoked, interested in our
%%          pieces. It has sent one or more requests to us.
%% Args: atom is_interested,State
%% Returns: {next_state, NextStateName, NextState} |
%%          {next_state, NextStateName, NextState, Timeout}
%%--------------------------------------------------------------------
is_unchoked_interested_requested(is_interested, State) ->
    {next_state, is_unchoked_interested_requested, State, 0};
is_unchoked_interested_requested(is_not_interested, State) ->
    {next_state, is_unchoked_uninterested, State#state{requests = []}};
is_unchoked_interested_requested(timeout, State) ->
    case State#state.requests of
	[] -> 
	    {next_state, is_unchoked_interested_unrequested, State};
	[{Index, Begin, Length} | Rest] ->  
	    Result = mutex:request(State#state.file_storage, get_piece,
				   [Index, Begin, Length]),
	    mutex:received(State#state.file_storage),
    
	    case Result of
		{ok, Block_in_bin} ->  
		    message_handler:send(State#state.msg_handler, piece,
					 [Index, Begin, Block_in_bin]);
		{error, _Reason} ->
		    ok
	    end,

	    {next_state, is_unchoked_interested_requested, 
	     State#state{requests = Rest}, 0}
    end;
is_unchoked_interested_requested({request, Data}, State) ->
    New_requests = [Data | State#state.requests],
    {next_state, is_unchoked_interested_requested,
 State#state{requests = New_requests}, 0};
is_unchoked_interested_requested({cancel, Data}, State) ->
    New_requests = lists:delete(Data, State#state.requests),
    {next_state, is_unchoked_interested_requested, 
     State#state{requests = New_requests}, 0}.
	     
%%--------------------------------------------------------------------
%% Function: is_unchoked_uninterested/2
%% Purpose: state 4
%%          This is a gen_fsm state. The peer is unchoked, not interested
%%          in our pieces. 
%% Args: atom is_interested, State
%% Returns: {next_state, NextStateName, NextState} 
%%--------------------------------------------------------------------
is_unchoked_uninterested(is_interested, State) ->
    {next_state, is_unchoked_interested_unrequested, State};
is_unchoked_uninterested(is_not_interested, State) ->
    message_handler:send(State#state.msg_handler, choke, []),
    io:format("~n**piece_uploader~w** choke da peer~n", [self()]),
    {next_state, is_choked_uninterested, State}.

%%--------------------------------------------------------------------
%% Function: state_name/3
%% Purpose: There are not any sync states.
%% Agrs: Event, From, State
%% Returns: {reply, Reply, NextStateName, NextState} 
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: handle_event/3
%% Purpose: handles the stop event sent from piece_requester
%% Agrs: stop, _StateName, State
%% Returns:  {stop, normal, State}
%%--------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_sync_event
%% Purpose: There are not any sync events.
%% Args: Event, _From, StateName, State
%% Returns: {reply, Reply, NextStateName, NextState}
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/3
%% Purpose: handles the exit messages from other pids.
%% Agrs: Error msg,StateName,State
%% Returns: {stop, Reason, NewState}
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Reason}, _StateName, State) ->
    {stop, normal, State};
handle_info(_,_,_State) ->
    {stop, normal, _State}.

%%--------------------------------------------------------------------
%% Function: terminate/3
%% Purpose: This function is called by a gen_fsm when it is about to
%%          terminate. It should be the opposite of Module:init/1 and 
%%          do any necessary cleaning up. When it returns, the gen_fsm
%%          terminates with Reason. The return value is ignored.
%% Agrs: _Reason, _StateName, _State
%% Returns: ok
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function: code_change/4
%% Purpose: There are not any code changes
%% Agrs: OldVsn, StateName, State, Extra
%% Returns: {ok, StateName, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
