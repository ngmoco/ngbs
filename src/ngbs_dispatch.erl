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
    case ngbs_app:config(slow_query_threshold, undefined) of
        undefined ->
            eval(M,F,A);
        Threshold ->
            Start = erlang:now(),
            Result = eval(M,F,A),
            End = erlang:now(),
            case timer:now_diff(End, Start) of
                Elapsed when Elapsed > Threshold ->
                    ?INFO("Slow function: ~p:~p/~p (~pus).",
                          [M, F, length(A), Elapsed]);
                _ -> ok
            end,
            Result
    end.

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
