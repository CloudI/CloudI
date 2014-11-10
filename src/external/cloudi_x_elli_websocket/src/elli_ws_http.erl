%%
%% Some simple parsing routines.
%%

-module(elli_ws_http).

-export([tokens/1]).

-define(IS_TOKEN_SEP(C), (C =:= $, orelse C =:= $\s orelse C=:= $\t)).

%% @doc Parse tokens 
tokens(L) when is_list(L) ->
    lists:flatten([tokens(V) || V <- L]);

tokens(Header) when is_binary(Header) ->
    parse_before(Header, []).

parse_before(<<>>, Acc) ->
    lists:reverse(Acc);
parse_before(<< C, Rest/bits >>, Acc) when ?IS_TOKEN_SEP(C) ->
    parse_before(Rest, Acc);
parse_before(Buffer, Acc) ->
    parse(Buffer, Acc, <<>>).

parse(<<>>, Acc, <<>>) ->
    lists:reverse(Acc);
parse(<<>>, Acc, Token) ->
    lists:reverse([Token|Acc]);
parse(<<C, Rest/bits>>, Acc, Token) when ?IS_TOKEN_SEP(C) ->
    parse_before(Rest, [Token|Acc]);
parse(<<C, Rest/bits>>, Acc, Token) ->
    parse(Rest, Acc, <<Token/binary, (elli_bstr:lchr(C))>>).

%%
%% Tests
%%

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

tokens_test() ->
    ?assertEqual([<<"test">>], tokens(<<"test">>)),
    ?assertEqual([<<"test">>, <<"bla">>, <<"x">>], tokens(<<"test,     bla,x">>)),
    ?assertEqual([<<"test">>, <<"een">>, <<"twee">>], tokens([<<"test">>, <<"een">>, <<"twee">>])),
    ?assertEqual([<<"test">>, <<"een">>, <<"twee">>], tokens([<<"test,,,">>, <<",,,een,,,">>, <<"twee">>])),
    ?assertEqual([], tokens(<<",,,">>)),
    ok.

-endif.

