%%%-------------------------------------------------------------------
%% @copyright 2010 ngmoco :)
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@date} {@time}
%% @doc NGBS function dispatch
%% @end
%%%-------------------------------------------------------------------
-module(ngbs_dispatch).

-include_lib("ng_log.hrl").

%% API
-export([call/2
         ,cast/2
        ]).

%% Tracing export.
-export([report_call_time/6]).

%%====================================================================
%% API
%%====================================================================

call(Cmd, Info) ->
    dispatch(Cmd, Info).

cast(Cmd, Info) ->
    case ngbs_cast_dispatcher:dispatch(Cmd, Info) of
        ok ->
            ngbs_proto:noreply();
        overloaded ->
            ngbs_proto:error({server, undesignated},
                             "Server is overloaded.", [], [])
    end.

dispatch({M,F,A}, []) when is_atom(M), is_atom(F), is_list(A) ->
    Arity = length(A),
    case ngbs_acl:is_allowed(M, F, Arity) of
        not_allowed ->
            ?WARN("Call to disallowed function: ~p:~p/~p.",
                  [M,F,Arity]),
            ngbs_proto:error({server, no_function},
                             "No such function '~p:~p/~p'.", [M, F, Arity],
                             []);
        allowed ->
            maybe_time_dispatch(M,F,A)
    end;
dispatch(Cmd, Info) when is_list(Info) ->
    ?WARN("Unsupported info directives: ~p~nFor command: ~p",
          [Info, Cmd]),
    dispatch(Cmd, []);

dispatch({M,F,A}, _Info) when is_atom(M), is_atom(F), not is_list(A) ->
    ngbs_proto:error({protocol, undesignated}, "Function arguments must be a list.", [], []);
dispatch({M,F,_A}, _Info) when is_atom(M), not is_atom(F) ->
    ngbs_proto:error({protocol, undesignated}, "Function name must be an atom.", [], []);
dispatch({M,_F,_A}, _Info) when not is_atom(M) ->
    ngbs_proto:error({protocol, undesignated},
                     "Module name must be an atom.", [], []).


maybe_time_dispatch(M,F,A) ->
    case ngbs_app:config(time_dispatch, undefined) of
        undefined ->
            eval(M,F,A);
        {report,Pid} when is_pid(Pid) ->
            {Result,Timing} = time_eval(M,F,A),
            Pid ! {?MODULE, time_eval, {{M,F,A},Timing}},
            Result;
        {apply, {Rm,Rf}} ->
            {Result,Timing} = time_eval(M,F,A),
            Rm:Rf({M,F,A}, Timing),
            Result;
        {threshold, Threshold} when is_integer(Threshold) ->
            {Result,{Start, Elapsed}} = time_eval(M,F,A),
            report_call_time(M,F,A,Start,Elapsed,Threshold),
            Result
    end.

time_eval(M,F,A) ->
    Start = erlang:now(),
    Result = eval(M,F,A),
    End = erlang:now(),
    Elapsed = timer:now_diff(End, Start),
    {Result,{Start,Elapsed}}.

report_call_time(M,F,A,_Start,Elapsed,Threshold) when Elapsed >= Threshold ->
    ?INFO("Slow (~pus) function: ~p:~p/~p.",
          [Elapsed, M, F, length(A)]);
report_call_time(_,_,_,_,_,_) -> ok.

%% Actually execute the call.
eval(M,F,A) ->
    try apply(M,F,A) of
        Result ->
            ngbs_proto:reply(Result)
    catch
        error:undef ->
            Stack = erlang:get_stacktrace(),
            ?WARN("No such function '~p:~p/~p'.~nStack: ~p",
                  [M, F, length(A), Stack]),
            ngbs_proto:error({server, no_function},
                             "No such function '~p:~p/~p'.", [M, F, length(A)],
                             Stack);
        Type:Error ->
            Stack = erlang:get_stacktrace(),
            ?ERR("~p:~p calling ~p:~p(~s).~nStack: ~p",
                 [Type, Error, M, F, args_to_list(A),
                  Stack]),
            ngbs_proto:error({server, undesignated},
                             "~p:~p calling ~p:~p(~s).",
                             [Type, Error, M, F, args_to_list(A)],
                             Stack)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

args_to_list(Args) ->
    string:join([ if is_pid(A) ->
                          "pid("++string:join(string:tokens(pid_to_list(A) -- "<>", "."), ",") ++ ")";
                     true ->
                          io_lib:format("~p", [A])
                  end
                  || A <- Args ],
                ", ").
