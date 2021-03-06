%%%-------------------------------------------------------------------
%% @copyright 2010 ngmoco:)
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@date} {@time}
%% @doc ngmoco:) BERT-RPC server API
%% @end
%%%-------------------------------------------------------------------
-module(ngbs).

%% API
-export([listen/0
         ,listen/1
         ,allow_call/1
        ]).

%%====================================================================
%% API
%%====================================================================

listen() ->
    ngbs_listener:start_listening().

listen(Port) ->
    ngbs_listener:start_listening(Port).

allow_call(Call) ->
    ngbs_acl:allow_call(Call).

%%====================================================================
%% Internal functions
%%====================================================================
