%%% See http://github.com/mojombo/bert.erl for documentation.
%%% MIT License - Copyright (c) 2009 Tom Preston-Werner <tom@mojombo.com>
%%% Modifications Copyright (c) 2010 Geoff Cant, ngmoco:) <gcant@ngmoco.com>

-module(ngbs_bert).
-author("Tom Preston-Werner").

-export([encode/1, decode/1]).

%%---------------------------------------------------------------------------
%% Public API

-spec encode(term()) -> binary().

encode(Term) ->
    term_to_binary(encode_term(Term)).

-spec decode(binary()) -> term().

decode(Bin) ->
    decode_term(binary_to_term(Bin, [safe])).

%%---------------------------------------------------------------------------
%% Encode

-spec encode_term(term()) -> term().

encode_term(Term) ->
    case Term of
        [] -> {bert, nil};
        true -> {bert, true};
        false -> {bert, false};
        Dict when is_record(Term, dict, 8) ->
            {bert, dict, dict:to_list(Dict)};
        List when is_list(Term) ->
            [ encode_term(Element) || Element <- List ];
        Tuple when is_tuple(Term) ->
            list_to_tuple([ encode_term(Element)
                            || Element <- tuple_to_list(Tuple) ]);
        Bad when is_pid(Bad);
                 is_port(Bad);
                 is_function(Bad);
                 is_reference(Bad) ->
            erlang:error({badarg, Bad});
        _Good -> Term
    end.

%%---------------------------------------------------------------------------
%% Decode

-spec decode_term(term()) -> term().

decode_term(Term) ->
    case Term of
        {bert, nil} -> [];
        {bert, true} -> true;
        {bert, false} -> false;
        {bert, dict, Dict} ->
            dict:from_list(Dict);
        {bert, Other} ->
            {bert, Other};
        List when is_list(List) ->
            [decode_term(I) || I <- List];
        Tuple when is_tuple(Tuple) ->
            list_to_tuple([ decode_term(I)
                            || I <- tuple_to_list(Tuple) ]);
        Bad when is_pid(Bad);
                 is_port(Bad);
                 is_function(Bad);
                 is_reference(Bad) ->
            erlang:error({badarg, Bad});
        _Good -> Term
  end.
