%%%-------------------------------------------------------------------
%% @copyright 2010 ngmoco:)
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@vsn}, {@date} {@time}
%% @doc Handles a single BertRPC connection
%% @end
%%%-------------------------------------------------------------------
-module(ngbs_conn).

-behaviour(gen_fsm).

-include("ng_log.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0]).

-export([connected/2
         ,wait_sock/2
         ,accept_sock/2
        ]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {sock, cmdinfo=[]}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_fsm:start_link(?MODULE, [#state{}], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([S = #state{}]) ->
    {ok, wait_sock, S}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------

accept_sock(Pid, Sock) ->
    gen_fsm:send_event(Pid, {accept_sock, Sock}).

wait_sock({accept_sock, Sock}, State = #state{sock=undefined}) ->
    NewState = State#state{sock={sock, Sock}},
    continue(NewState).

connected({info, Command, Args}, State = #state{cmdinfo=I}) ->
    continue(State#state{cmdinfo=[{Command, Args} | I]});
connected({cast, M, F, A}, State = #state{cmdinfo=I}) ->
    Reply = ngbs_dispatch:cast({M,F,A}, I),
    sock_send(State, Reply),
    continue(State);
connected({call, M, F, A}, State = #state{cmdinfo=I}) ->
    Reply = ngbs_dispatch:call({M,F,A}, I),
    sock_send(State, Reply),
    continue(State).

active_once(#state{sock={sock,Socket}}) ->
    active_once(Socket);
active_once(Socket) ->
    ok = inet:setopts(Socket, [{active, once}]).

sock_send(#state{sock={sock,Socket}}, Data) ->
    sock_send(Socket, Data);
sock_send(Socket, Data) when not is_binary(Data) ->
    sock_send(Socket, ngbs_bert:encode(Data));
sock_send(Socket, Bin) when is_binary(Bin) ->
    gen_tcp:send(Socket, Bin).

close(State = #state{sock={sock,Socket}}) ->
    gen_tcp:close(Socket),
    State#state{sock=undefined}.

continue(State) ->
    active_once(State),
    {next_state, connected, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, State) ->
    ?WARN("Unexpected event: ~p", [Event]),
    {noreply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info({tcp, Sock, TermBin}, StateName,
            State = #state{sock={sock, Sock}}) ->
    try ngbs_bert:decode(TermBin) of
        Term ->
            ?MODULE:StateName(Term, State)
    catch
        error:badarg ->
            ?WARN("Decode error: ~p:~p~nStack: ~p",
                  [error, badarg, erlang:get_stacktrace()]),
            sock_send(State,
                      ngbs_proto:error({protocol, data},
                                       "Couldn't decode bert packet due to atom creation or invalid types.",
                                       [],
                                       [])),
            {stop, decode_error, State};
        Type:Error ->
            ?WARN("Decode error: ~p:~p~nStack: ~p",
                  [Type, Error, erlang:get_stacktrace()]),
            {stop, decode_error, State}
    end;
handle_info({tcp_closed, Sock}, _StateName,
            State = #state{sock={sock, Sock}}) ->
    {stop, normal, State#state{sock=undefined}};
handle_info({tcp_error, Sock, Reason}, _StateName,
            State = #state{sock={sock, Sock}}) ->
    ?INFO("Client connection closed -- ~p", [Reason]),
    {stop, normal, close(State)};

handle_info(Info, StateName, State) ->
    ?INFO("Unexpected info msg ~p in state ~p.", [Info, StateName]),
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
