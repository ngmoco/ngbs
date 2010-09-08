%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <nem@erlang.geek.nz>
%% @version {@vsn}, {@date} {@time}
%% @doc ngbs top level supervisor
%% @end
%%%-------------------------------------------------------------------
-module(ngbs_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ACL = {acl,
           {ngbs_acl, start_link, []},
           permanent, 1000, worker,
           [ngbs_acl]},
    CS = {conn_sup,
          {ngbs_conn_sup,start_link,[]},
          permanent,2000,supervisor,
          [ngbs_conn_sup]},
    LS = {listener,
          {ngbs_listener,start_link,[]},
          permanent,2000,worker,
          [ngbs_conn_sup]},
    {ok,{{one_for_all,0,1}, [ACL, CS, LS]}}.

%%====================================================================
%% Internal functions
%%====================================================================
