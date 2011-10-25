%%%-------------------------------------------------------------------
%% @copyright 2010 ngmoco:)
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@date} {@time}
%% @doc Access control module for allowed functions
%% @end
%%%-------------------------------------------------------------------
-module(ngbs_acl).

-behaviour(gen_server).

-include_lib("ng_log.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0
         ,allowed_calls/0
         ,allow_call/1
         ,deny_call/1
         ,missing_calls/0
         ,is_allowed/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tab}).
-define(ACL_TAB, ?MODULE).

-type callspec() :: {module, Mod::atom()} |
                    {function, {Mod::atom(), Function::atom(),
                                Arity::non_neg_integer()}}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

allowed_calls() ->
    [ CS
      || CS <- ngbs_app:config(allowed_calls, []),
         allowed_call(CS) =:= allowed ].

allow_call(CS) ->
    gen_server:call(?MODULE, {allow_call, CS}).

deny_call(CS) ->
    gen_server:call(?MODULE, {deny_call, CS}).

allowed_call({module, M}) when is_atom(M) -> allowed;
allowed_call({function, {M, F, A}})
  when is_atom(M), is_atom(F), is_integer(A) -> allowed;
allowed_call(_) -> not_allowed.

missing_calls() ->
    ets:foldl(fun ({_,CS}, Acc) ->
                      case ensure_loaded(CS) of
                          loaded -> Acc;
                          not_loaded -> [CS | Acc]
                      end
              end,
              [],
             ?ACL_TAB).

%% @doc Check whether an MFA is allowed. (Ets lookup for a blanket
%% module-allow, then for a specific MFA allow, otherwise not_allowed.
-spec is_allowed(atom(), atom(), non_neg_integer()) -> 'allowed' |
                                                       'not_allowed'.
is_allowed(Module, Function, Arity)
  when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    case ets:lookup(?ACL_TAB, ets_key({module, Module})) of
        [_] -> allowed;
        [] ->
            case ets:lookup(?ACL_TAB, ets_key({function,
                                               {Module, Function, Arity}})) of
                [_] -> allowed;
                [] ->
                    not_allowed
            end
    end.

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
init([]) ->
    Tid = ets:new(?ACL_TAB, [{keypos, 1},
                             named_table,
                             protected,
                             set]),
    State = #state{tab=Tid},
    fill_table(),
    case missing_calls() of
        [] -> ok;
        _ -> ?WARN("Couldn't load some modules, "
                   "check the 'ngbs' 'allowed_calls' configuation option.",[]),
             ok
    end,
    {ok, State}.

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
handle_call({deny_call, CS}, _From, State) ->
    ets:delete(?ACL_TAB, ets_key(CS)),
    {reply, ok, State};

handle_call({allow_call, CS}, _From, State) ->
    case allowed_call(CS) of
        allowed ->
            i_allow_call(CS),
            {reply, ok, State};
        not_allowed ->
            {reply, {error, not_allowed}, State}
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

-spec ensure_loaded(callspec()) -> 'loaded' | 'not_loaded'.
ensure_loaded({function, {M,F,A}}) when is_atom(M), is_atom(F), is_integer(A) ->
    case ensure_loaded({module, M}) of
        not_loaded ->
            not_loaded;
        loaded ->
            case erlang:function_exported(M, F, A) of
                false ->
                    ?WARN("Module ~p loaded, but function ~p/~p not exported.",
                          [M, F, A]),
                    not_loaded;
                true -> loaded
            end
    end;

ensure_loaded({module, M}) when is_atom(M) ->
    case code:ensure_loaded(M) of
        {module, M} ->
            loaded;
        {error, Reason} ->
            ?WARN("Module ~p not loaded: ~p", [M, Reason]),
            not_loaded
    end.

ets_key({module, M}) -> M;
ets_key({function, MFA}) -> MFA.

i_allow_call(CS) ->
    ets:insert(?ACL_TAB, {ets_key(CS), CS}).

fill_table() ->
    fill_table(allowed_calls()).
fill_table(Calls) ->
    lists:foreach(fun i_allow_call/1, Calls).
