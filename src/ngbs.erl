%%%-------------------------------------------------------------------
%% @copyright Geoff Cant
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@vsn}, {@date} {@time}
%% @doc ngmoco:) BERT-RPC server API
%% @end
%%%-------------------------------------------------------------------
-module(ngbs).

%% API
-export([listen/0
         ,allow_call/1
        ]).

%%====================================================================
%% API
%%====================================================================

listen() ->
    ngbs_listener:start_listening().

allow_call(Call) ->
    ngbs_listener:allow_call(Call).

%%====================================================================
%% Internal functions
%%====================================================================
