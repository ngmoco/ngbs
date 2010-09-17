%%%-------------------------------------------------------------------
%% @copyright 2010 ngmoco:)
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@vsn}, {@date} {@time}
%% @doc Erlang BERT client
%% @end
%%%-------------------------------------------------------------------
-module(ngbs_client).

-behaviour(gen_fsm).

-include("ng_log.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2
         ,start/2
         ,stop/1
         ,call/4
         ,wait_response/2
        ]).

-export([connect/2
         ,connected/3
         ,idle/3
        ]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(state, {host, port, sock, req}).

-define(RETRY_TIMEOUT, timer:seconds(10)).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(Host, Port) -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Host, Port) ->
    gen_fsm:start_link(?MODULE, [#state{host=Host, port=Port}], []).

start(Host, Port) ->
    gen_fsm:start(?MODULE, [#state{host=Host, port=Port}], []).

stop(Pid) ->
    gen_fsm:sync_send_all_state_event(Pid, stop).

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
init([State = #state{}]) ->
    {ok, idle, State}.

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

connect(timeout, State) ->
    case connect(State) of
        {ok, Sock} ->
            {next_state, connected, State#state{sock=Sock}};
        {error, E} ->
            ?WARN("Couldn't connect to ~p:~p -- ~p",
                  [State#state.host,State#state.port,E]),
            {next_state, connect, State, ?RETRY_TIMEOUT}
    end.

connect(#state{host=Host, port=Port}) ->
    gen_tcp:connect(Host, Port, tcp_opts()).

tcp_opts() ->
    [binary,
     {packet, 4},
     {active, true}].

wait_response({noreply}, State) ->
    S2 = reply(ok, State),
    {next_state, connected, S2};
wait_response(Resp = {error, {_Type, _Code, _Class, _Detail, _Stack}}, State) ->
    S2 = reply(Resp, State),
    {next_state, connected, S2};
wait_response({reply, Result}, State) ->
    S2 = reply(Result, State),
    {next_state, connected, S2}.    

reply(Response, State = #state{req={From, _Req}}) ->
    gen_fsm:reply(From, Response),
    State#state{req=undefined}.

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

call(Connection, M, F, A) ->
    gen_fsm:sync_send_event(Connection, {call, M, F, A}).

idle(Req, From, State) ->
    case connect(State) of
        {ok, Sock} ->
            connected(Req, From, State#state{sock=Sock});
        {error, E} ->
            {reply, {tcp_error, E}, idle, State}
    end.

connected(Req = {call, _M, _F, _A}, From, State) ->
    case send(Req, State) of
        ok ->
            {next_state, wait_response, State#state{req={From, Req}}};
        {error, closed} ->
            idle(Req, From, State)
    end.

send(Term, State) when not is_binary(Term) ->
    send(ngbs_bert:encode(Term), State);
send(Binary, #state{sock=Sock}) when is_binary(Binary) ->
    gen_tcp:send(Sock, Binary).

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
handle_sync_event(stop, _From, _StateName, State) ->
    {stop, normal, stopping, State};

handle_sync_event(_Event, _From, StateName, State) ->
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
            State = #state{sock=Sock}) ->
    try ngbs_bert:decode(TermBin) of
        Term ->
            ?MODULE:StateName(Term, State)
    catch
        Type:Error ->
            ?WARN("Decode error: ~p:~p", [Type, Error]),
            {stop, decode_error, State}
    end;

handle_info({tcp_closed, _Sock}, _StateName,
            State = #state{sock=undefined}) ->
    {next_state, idle, State};

handle_info({tcp_closed, Sock}, _StateName,
            State = #state{sock=Sock, req={From, _Req}}) ->
    gen_fsm:reply(From, {error, closed}),
    {next_state, idle, State#state{sock=undefined, req=undefined}};

handle_info({tcp_closed, Sock}, _StateName,
            State = #state{sock=Sock}) ->
    {next_state, idle, State#state{sock=undefined}};

handle_info({tcp_error, Sock, Reason}, _StateName,
            State = #state{sock=Sock}) ->
    ?INFO("Client connection closed -- ~p", [Reason]),
    gen_tcp:close(Sock),
    {next_state, idle, State#state{sock=undefined}};

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
terminate(Reason, Statename, State = #state{sock=S}) when S =/= undefined ->
    gen_tcp:close(S),
    terminate(Reason, Statename, State#state{sock=undefined});
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
