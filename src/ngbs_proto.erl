%%%-------------------------------------------------------------------
%% @copyright 2010 ngmoco :)
%% @author Geoff Cant <gcant@ngmoco.com>
%% @version {@date} {@time}
%% @doc 
%% @end
%%%-------------------------------------------------------------------
-module(ngbs_proto).

%% API
-export([noreply/0
         ,reply/1
         ,error/4
         ,error/6
         ,format_error/1
         ,format_stack/1
        ]).

%%====================================================================
%% API
%%====================================================================

noreply() ->
    {noreply}.

reply(Term) ->
    {reply, Term}.

error(Err, Msg, Fmt, Stack) ->
    {Type, Code, Class} = format_error(Err),
    %% Call ?MODULE:error instead of error in order to allow us to
    %% easily trace connection errors. XXX Could be a hot upgrade bug
    %% path though.
    ?MODULE:error(Type, Code, Class, Msg, Fmt, Stack).

error(Type, Code, Class, Msg, Fmt, Stack) ->
    {error, {Type, Code, Class,
             iolist_to_binary(io_lib:format(Msg, Fmt)),
             format_stack(Stack)}}.
                            
format_error({protocol, undesignated}) ->
    {protocol, 0, <<"RequestError">>};
format_error({protocol, data}) ->
    {protocol, 2, <<"RequestError">>};
format_error({server, undesignated}) ->
    {server, 0, <<"ServerError">>};
format_error({server, no_module}) ->
    {server, 1, <<"RequestError">>};
format_error({server, no_function}) ->
    {server, 2, <<"RequestError">>}.

format_stack(Stack) ->
    [ iolist_to_binary(io_lib:format("~p", [Entry]))
      || Entry <- Stack ].

%%====================================================================
%% Internal functions
%%====================================================================
