%%%-------------------------------------------------------------------
%%% @author  <Bruce@THINKPAD>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created :  9 Dec 2011 by  <Bruce@THINKPAD>
%%%-------------------------------------------------------------------
-module(piece_uploader).

-behaviour(gen_fsm).

%% API
-export([start_link/3, send_event/3]).

%% gen_fsm callbacks
-export([init/1, is_choked_uninterested/2, is_choked_interested/2, is_unchoked_uninterested/2, is_unchoked_interested_unrequested/2, is_unchoked_interested_requested/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {piece_requester, file_storage, msg_handler, requests = []}).

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
start_link(Piece_requester_pid, File_storage_pid, Msg_handler_pid) ->
    gen_fsm:start_link(?MODULE, [Piece_requester_pid, File_storage_pid, Msg_handler_pid], []).

%%--------------------------------------------------------------------
%% @doc
%% sends a event to teh gen_fsm, to cahnge the states
%%
%% @spec send_event(Pid, Type, Args) -> ok
%% @end
%%--------------------------------------------------------------------
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
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} 
%% @end
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
%% @private
%% @doc
%% state 1 
%% This is a gen_fsm state. The peer is choked, not interested in our
%% pieces
%%
%% @spec is_choked_uninterested(Event, State) ->
%%                   {next_state, NextStateName, NextState} 
%% @end
%%--------------------------------------------------------------------
is_choked_uninterested(is_interested, State) ->
    message_handler:send(State#state.msg_handler, unchoke, []),
    {next_state, is_unchoked_interested_unrequested, State};
is_choked_uninterested(is_not_interested, State) ->
    {next_state, is_choked_uninterested, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% state 2
%% This is a gen_fsm state. The peer is choked, interested in our
%% pieces
%%
%% @spec is_choked_interested(Event, State) ->
%%                   {next_state, NextStateName, NextState} 
%% @end
%%--------------------------------------------------------------------
is_choked_interested(is_interested, State) ->
    {next_state, is_choked_interested, State};
is_choked_interested(is_not_interested, State) ->
    {next_state, is_choked_uninterested, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% state 3-1
%% This is a gen_fsm state. The peer is unchoked, interested in our
%% pieces. It has not sent any requests to us.
%%
%% @spec is_unchoked_interested_unrequested(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout}
%% @end
%%--------------------------------------------------------------------
is_unchoked_interested_unrequested(is_interested, State) ->
    {next_state, is_unchoked_interested_unrequested, State};
is_unchoked_interested_unrequested(is_not_interested, State) ->
    {next_state, is_unchoked_uninterested, State};
is_unchoked_interested_unrequested({request, Data}, State) ->    
    {next_state, is_unchoked_interested_requested, State#state{requests = [Data]}, 0}.   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% state 3-2
%% This is a gen_fsm state. The peer is unchoked, interested in our
%% pieces. It has sent one or more requests to us.
%%
%% @spec is_unchoked_interested_requested(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout}
%% @end
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
	    Result = mutex:request(State#state.file_storage, get_piece, [Index, Begin, Length]),
	    mutex:received(State#state.file_storage),
    
	    case Result of
		{ok, Block_in_bin} ->  
		    message_handler:send(State#state.msg_handler, piece, [Index, Begin, Block_in_bin]);
		{error, _Reason} ->
		    ok
	    end,

	    {next_state, is_unchoked_interested_requested, State#state{requests = Rest}, 0}
    end;
is_unchoked_interested_requested({request, Data}, State) ->
    New_requests = [Data | State#state.requests],
    {next_state, is_unchoked_interested_requested, State#state{requests = New_requests}, 0};
is_unchoked_interested_requested({cancel, Data}, State) ->
    New_requests = lists:delete(Data, State#state.requests),
    {next_state, is_unchoked_interested_requested, State#state{requests = New_requests}, 0}.
	     
%%--------------------------------------------------------------------
%% @private
%% @doc
%% state 4
%% This is a gen_fsm state. The peer is unchoked, not interested in our
%% pieces. 
%%
%% @spec is_unchoked_uninterested(Event, State) ->
%%                   {next_state, NextStateName, NextState} 
%% @end
%%--------------------------------------------------------------------
is_unchoked_uninterested(is_interested, State) ->
    {next_state, is_unchoked_interested_unrequested, State};
is_unchoked_uninterested(is_not_interested, State) ->
    message_handler:send(State#state.msg_handler, choke, []),
    io:format("~n**piece_uploader~w** choke da peer~n", [self()]),
    {next_state, is_choked_uninterested, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There are not any sync states.
%%
%% @spec state_name(Event, From, State) ->
%%                   {reply, Reply, NextStateName, NextState} 
%% @end
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handles the stop event sent from piece_requester
%%
%% @spec handle_event(Event, StateName, State) ->
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There are not any sync events.
%%
%% @spec handle_sync_event(Event, From, StateName, State) ->
%%                   {reply, Reply, NextStateName, NextState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handles the exit messages from other pids.
%%
%% @spec handle_info(Info,StateName,State)->
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Reason}, _StateName, State) ->
    {stop, normal, State};
handle_info(_,_,_State) ->
    {stop, normal, _State}.

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
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There are not any code changes
%%
%% @spec code_change(OldVsn, StateName, State, Extra) ->
%%                   {ok, StateName, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
