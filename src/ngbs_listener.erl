%%%-------------------------------------------------------------------
%% @copyright 2010 ngmoco:)
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@date} {@time}
%% @doc ngbs TCP listener
%% @end
%%%-------------------------------------------------------------------
-module(ngbs_listener).

-behaviour(gen_server).

-include("ng_log.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0
         ,start_link/1
         ,start_listening/0
         ,start_listening/1
         ,stop_listening/0
         ,listening/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port,lsock}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(ngbs_app:config(port)).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [#state{port=Port}], []).

start_listening() ->
    start_listening(current).

start_listening(Port) ->
    gen_server:call(?MODULE, {start_listening, Port}).

stop_listening() ->
    gen_server:call(?MODULE, stop_listening).

listening() ->
    gen_server:call(?MODULE, listening).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialises the server's state
%% @end
%%--------------------------------------------------------------------
init([S0 = #state{}]) ->
    case listen_on_start() of
        true ->
            case listen(S0) of
                {error, E} ->
                    {stop, {listen_failed, E}};
                {ok, S1 = #state{}} ->
                    {ok, S1}
            end;
        false ->
            {ok, S0}
    end.

listen(State = #state{lsock=undefined, port=Port}) ->
    case gen_tcp:listen(Port, listen_opts()) of
        {ok, LSock} ->
            ?INFO("Listening on port ~p", [Port]),
            async_accept(LSock),
            {ok, State#state{lsock=LSock}};
        {error, E} ->
            ?ERR("Couldn't listen on port ~p -- ~p", [E]),
            {error, E}
    end;
listen(_State) ->
    {error, already_listening}.

listen_opts() ->
    [binary,
     {packet, 4},
     {active, false},
     {reuseaddr, true},
     {backlog, 128}].

async_accept(LSock) ->
    case prim_inet:async_accept(LSock, -1) of
        {ok, _Ref} ->
            ok;
        {error, emfile} ->
            ?ERR("Couldn't accept another connection - out of file descriptors!", []),
            erlang:send_after(100, self(), try_async_accept),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Call message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_call(listening, _From, State = #state{lsock=LSock, port=Port}) ->
    case LSock of
        undefined ->
            {reply, not_listening, State};
        _ ->
            {reply, {port, Port}, State}
    end;

handle_call({start_listening, Port}, _From, State = #state{}) ->
    LPort = case Port of
                current -> State#state.port;
                _ -> Port
            end,
    case listen(State#state{port=LPort}) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, _} = E ->
            {reply, E, State}
    end;

handle_call(stop_listening, _From, State = #state{lsock=LSock}) ->
    case LSock of
        undefined ->
            {reply, ok, State};
        _ ->
            gen_tcp:close(LSock),
            {reply, ok, State#state{lsock=undefined}}
    end;

handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_cast(Msg, State) -> {noreply, State} |
%%                            {noreply, State, Timeout} |
%%                            {stop, Reason, State}
%% @doc Cast message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec 
%% handle_info(Info, State) -> {noreply, State} |
%%                             {noreply, State, Timeout} |
%%                             {stop, Reason, State}
%% @doc Non gen-server message handler callbacks
%% @end
%%--------------------------------------------------------------------
handle_info({inet_async, LSock, _Ref, {ok,Sock}}, State = #state{lsock=LSock}) ->
    try
        {ok, Pid} = ngbs_conn_sup:new_connection(),
        {ok, Opts} = prim_inet:getopts(LSock, [active, nodelay, keepalive, delay_send, priority, tos]),
        handover_socket(Sock, Pid, Opts)
    catch
        _:_ -> gen_tcp:close(Sock)
    end,
    async_accept(LSock),
    {noreply, State};

handle_info({inet_async, LSock, _Ref, Error}, State = #state{lsock=LSock}) ->
    ?WARN("Accept on ~p failed: ~p", [LSock, Error]),
    S1 = State#state{lsock=undefined},
    case listen(S1) of
        {error, E} -> {stop, {listen_failed, E}, S1};
        {ok, S2 = #state{}} -> {noreply, S2}
    end;

handle_info(try_async_accept, State = #state{lsock=LSock}) ->
    async_accept(LSock),
    {noreply, State};

handle_info({inet_async, _OldLSock, _Ref, {error, closed}}, State) ->
    %% Silently eat the close message from our old listen sock
    {noreply, State};

handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

listen_on_start() ->
    ngbs_app:config(listen_on_startup, false).

handover_socket(Sock, Pid, Opts) ->
    true = inet_db:register_socket(Sock, inet_tcp),
    ok = prim_inet:setopts(Sock, Opts),
    gen_tcp:controlling_process(Sock, Pid),
    ngbs_conn:accept_sock(Pid, Sock).
