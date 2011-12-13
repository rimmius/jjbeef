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
-export([init/1, is_choked_uninterested/2, is_choked_interested/2, is_unchoked_uninterested/2, is_unchoked_interested/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {piece_requester, file_storage, msg_handler}).

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

send_event(Pid, is_interested, Arg) ->
    case Arg of
	1 -> gen_fsm:send_event(Pid, is_interested);
	0 -> gen_fsm:send_event(Pid, is_not_interested)
    end;
send_event(Pid, request, [Index, Begin, Length]) ->
    gen_fsm:send_event(Pid, {request, Index, Begin, Length});
send_event(Pid, cancel, [Index, Begin, Length]) ->
    gen_fsm:send_event(Pid, {cancel, Index, Begin, Length}).

%%send_event(Pid, is_choked, Arg) ->
%%    gen_fsm:send_all_state_event(Pid, {is_choked, Arg}).

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
init([Piece_requester_pid, File_storage_pid, Msg_handler_pid]) ->
    My_bitfield_in_list = mutex:request(File_storage_pid, get_bitfield, []),
    %%My_bitfield_in_list = fake(227),
    mutex:received(File_storage_pid),
    message_handler:send(Msg_handler_pid, bitfield, My_bitfield_in_list),
    %% message_handler:send(Msg_handler_pid, choke, []),
    io:format("~nRequester(~w) my bitfield sent: ~n", [Piece_requester_pid]),
    {ok, is_choked_uninterested, #state{piece_requester = Piece_requester_pid,
					file_storage = File_storage_pid,
					msg_handler = Msg_handler_pid}}.

fake(0) ->
    [];
fake(Num) ->
    [1|fake(Num-1)].
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
is_choked_uninterested(is_interested, State) ->
    message_handler:send(State#state.msg_handler, unchoke, []),
    io:format("~n**piece_uploader~w** unchoke da peer~n", [self()]),
    {next_state, is_unchoked_interested, State};
is_choked_uninterested(is_not_interested, State) ->
    {next_state, is_choked_uninterested, State}.

is_choked_interested(is_interested, State) ->
    {next_state, is_choked_interested, State};
is_choked_interested(is_not_interested, State) ->
    {next_state, is_choked_uninterested, State}.

is_unchoked_interested(is_interested, State) ->
    %% NOTHING DONE
    {next_state, is_unchoked_interested, State};
is_unchoked_interested(is_not_interested, State) ->
    {next_state, is_unchoked_uninterested, State};
is_unchoked_interested({request, Index, Begin, Length}, State) ->
    Result = mutex:request(State#state.file_storage, get_piece, [Index, Begin, Length]),
    mutex:received(State#state.file_storage),
    
    case Result of
	{ok, Block} ->  
	    message_handler:send(State#state.msg_handler, piece, [Index, Begin, <<Block:(16384*8)>>]),
	    io:format("~n**piece_uploader~w** piece replied~n", [self()]);
	{error, _Reason} -> %%TODO
	    ok
    end,
    {next_state, unchoked_interested, State}.   

is_unchoked_uninterested(is_interested, State) ->
    {next_state, is_unchoked_interested, State};
is_unchoked_uninterested(is_not_interested, State) ->
    message_handler:send(State#state.msg_handler, choke, []),
    io:format("~n**piece_uploader~w** choke da peer~n", [self()]),
    {next_state, is_choked_uninterested, State}.

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
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

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
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

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
