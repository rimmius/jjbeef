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
-export([start_link/5, send_event/3]).

%% gen_fsm callbacks
-export([init/1, am_choked_uninterested/2, am_choked_interested/2, am_unchoked_interested/2, am_unchoked_uninterested/2, state_name/3, handle_event/3,
	 handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% -define(SERVER, ?MODULE).

-record(state, {piece_storage, file_storage, msg_handler, peer_id, bitfield}).

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
start_link(Peer_mutex_pid, Piece_mutex_pid, 
	   File_storage_pid, Socket, Peer_id) ->
    io:format("going to start fsm~n"),
    gen_fsm:start_link(?MODULE, [Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Socket, Peer_id], []),
    io:format("fsm started~n").

send_event(Pid, am_interested, Am_interested) ->
    case Am_interested of
	true -> gen_fsm:send_all_state_event(Pid, am_interested);
	false -> gen_fsm:send_all_state_event(Pid, am_not_interested)
    end;
send_event(Pid, am_choked, Am_choked) ->
    case Am_choked of
	1 -> gen_fsm:send_event(Pid, am_choked);
	0 -> gen_fsm:send_event(Pid, am_unchoked)
    end;
send_event(Pid, piece, {Is_complete, Index}) ->
    case Is_complete of
	1 -> gen_fsm:send_event(Pid, {piece_complete, Index});
	0 -> gen_fsm:send_event(Pid, {piece_incomplete, Index})
    end;
send_event(Pid, bitfield, Bitfield_in_list) ->
    gen_fsm:send_all_state_event(Pid, {bitfield, Bitfield_in_list}).


%% TODO! When other fsms got a complete pieces
%% someting useful lol
%%handle_event(have, StateName, State) ->
%%    file_storage:compare_bitfield(File_storage, Bitfield_in_list),	
%%    {next_state, StateName, State};


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
init([Peer_mutex_pid, Piece_mutex_pid, File_storage_pid, Socket, Peer_id]) ->
    Msg_handler_pid = message_handler:start(self(), Socket, Peer_id, Peer_mutex_pid, Piece_mutex_pid, File_storage_pid),
    io:format("msg_handler started~n"),
    message_handler:send(Msg_handler_pid, bitfield, file_storage:get_bitfield(File_storage_pid)),
    io:format("my bitfiled sent~n"),
    io:format("init complete~n"),
    {ok, am_choked_uninterested, #state{piece_storage = Piece_mutex_pid,
					msg_handler = Msg_handler_pid,
					file_storage = File_storage_pid,
					peer_id = Peer_id}}.

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
    {next_state, am_unchoked_uninterested, State}.
%% and keep_alive

%% state 2
am_choked_interested(am_unchoked, State) ->
    %% TODO,
    io:format("~n~n~n~w~nPIECE_STORAGE_PID~n", [State#state.piece_storage]),
    case mutex:request(State#state.piece_storage, get_rarest_index, [State#state.peer_id]) of
	{ok, Index} -> 
	    io:format("~w   got rarest index = ~w, rdy to send request ~n", [self(), Index]),
	    Any = file_storage:what_chunk(State#state.file_storage, Index),
	    io:format("msg handler is going to send request~n~w~n", [Any]),
	    {Begin, Length} = Any,
	    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
	    io:format("request sent, going reloop~n"),
	    {next_state, am_unchoked_interested, State};
	{hold} -> 
	    {next_state, am_choked_uninterested, State}
    end.

%% and keep_alive
    
%% state 3
am_unchoked_interested(am_choked, State) ->
    {next_state, am_choked_interested, State};
am_unchoked_interested({piece_complete, Index}, State) ->
    case mutex:request(State#state.piece_storage, get_rarest_index, State#state.peer_id) of
	{ok, Index} -> 
	    {Begin, Length} = file_storage:what_chunk(State#state.file_storage, Index),
	    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]),
	    {next_state, am_unchoked_interested, State};
	hold -> 
	    {next_state, am_choked_uninterested, State}
    end;
am_unchoked_interested({piece_incomplete, Index}, State) ->
    {Begin, Length} = file_storage:what_chunk(State#state.file_storage, Index),
    message_handler:send(State#state.msg_handler, request, [Index, Begin, Length]), %% length
    {next_state, am_unchoked_interested, State}.
%% and keep_alive

%% state 4
am_unchoked_uninterested(am_choked, State) ->
    {next_state, am_choked_uninterested, State}.
%% and keep_alive
    
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
handle_event({bitfield, Bitfield_in_list}, StateName, State) ->
    %% all states
    {next_state, StateName, State#state{bitfield = Bitfield_in_list}};
handle_event(am_interested, am_choked_uninterested, State) ->
    %% state 1
    message_handler:send(State#state.msg_handler, am_interested, true),
    {next_state, am_choked_interested, State};
handle_event(am_interested, am_unchoked_uninterested, State) ->
    %% state 4
    message_handler:send(State#state.msg_handler, am_interested, true),
    {next_state, am_unchoked_interested, State};
handle_event(am_interested, StateName, State) ->
    %% state 2 and 3
    {next_state, StateName, State};
handle_event(am_not_interested, am_choked_interested, State) ->
    %% state 2
    message_handler:send(State#state.msg_handler, am_interested, false),
    {next_state, am_choked_interested, State};
handle_event(am_not_interested, am_unchoked_interested, State) ->
    %% state 3
    message_handler:send(State#state.msg_handler, am_interested, false),
    {next_state, am_unchoked_interested, State};
handle_event(am_not_interested, StateName, State) ->
    %% state 1 and 4
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
