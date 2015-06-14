%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==A trie data structure implementation.==
%%% The trie (i.e., from "retrieval") data structure was invented by
%%% Edward Fredkin (it is a form of radix sort).  The implementation stores
%%% string suffixes as a list because it is a PATRICIA trie
%%% (PATRICIA - Practical Algorithm to Retrieve Information
%%%  Coded in Alphanumeric, D.R.Morrison (1968)).
%%%
%%% This Erlang trie implementation uses binary keys.  Using binary keys
%%% means that other data structures are quicker alternatives, so this
%%% module is probably not a good choice, unless it is used for functions
%%% not available elsewhere.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2010-2013, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2010-2013 Michael Truog
%%% @version 1.4.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(btrie).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([append/3,
         append_list/3,
         erase/2,
         erase_similar/2,
         fetch/2,
         fetch_keys/1,
         fetch_keys_similar/2,
         find_prefix/2,
         find_prefixes/2,
         find_prefix_longest/2,
         filter/2,
         find/2,
         fold/3,
         foldl/3,
         foldr/3,
         fold_similar/4,
         foldl_similar/4,
         foldr_similar/4,
         foreach/2,
         from_list/1,
         is_key/2,
         map/2,
         merge/3,
         new/0,
         new/1,
         prefix/3,
         size/1,
         store/2,
         store/3,
         to_list/1,
         to_list_similar/2,
         update/3,
         update/4,
         update_counter/3,
         test/0]).

-define(MODE_BINARY, true).
-include("trie.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @private
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    {97,97,{{<<>>,empty}}} = btrie:new([<<"a">>]),
    {97,97,{{<<"b">>,empty}}} = btrie:new([<<"ab">>]),
    {97,97,{{<<"bc">>,empty}}} = btrie:new([<<"abc">>]),
    {97,97,{{<<"b">>,empty}}} = btrie:new([<<"ab">>]),
    {97,97,{{{97,98,{{<<>>,empty},{<<>>,empty}}},error}}} = 
        btrie:new([<<"ab">>,<<"aa">>]),
    {97,97,{{{97,98,{{<<"c">>,empty},{<<"c">>,empty}}},error}}} =
        btrie:new([<<"abc">>,<<"aac">>]),
    {97,97,{{{97,98,{{<<"c">>,2},{<<"c">>,1}}},error}}} =
        btrie:new([{<<"abc">>, 1},{<<"aac">>, 2}]),
    {97,97,{{{97,98,{{<<"c">>,2},{<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        RootNode0 = btrie:new([
            {<<"abcdefghijklmnopqrstuvwxyz">>, 1},{<<"aac">>, 2}]),
    {ok, 1} = btrie:find(<<"abcdefghijklmnopqrstuvwxyz">>, RootNode0),
    error = btrie:find(<<"abcdefghijklmnopqrstuvwxy">>, RootNode0),
    {ok, 1} = btrie:find_prefix(<<"abcdefghijklmnopqrstuvwxyz">>, RootNode0),
    prefix = btrie:find_prefix(<<"abcdefghijklmnopqrstuvwxy">>, RootNode0),
    error = btrie:find_prefix(<<"abcdefghijklmnopqrstuvwxyzX">>, RootNode0),
    prefix = btrie:find_prefix(<<"a">>, RootNode0),
    prefix = btrie:find_prefix(<<"aa">>, RootNode0),
    {ok, 2} = btrie:find_prefix(<<"aac">>, RootNode0),
    error = btrie:find_prefix(<<"aacX">>, RootNode0),
    {97,97,{{{97,98,{{{98,99,{{<<"cde">>,3},{<<>>,2}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        RootNode1 = btrie:store(<<"aabcde">>, 3, RootNode0),
    {97,97,{{{97,98,{{{98,99,{{<<"cde">>,13},{<<>>,12}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,11}}},error}}} =
        map(fun(_, V) -> V + 10 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{<<>>,error},{<<>>,error}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        filter(fun(_, V) -> V =< 1 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{<<>>,error},{<<>>,2}}},error},
     {<<"cdefghijklmnopqrstuvwxyz">>,1}}},error}}} =
        filter(fun(_, V) -> V =< 2 end, RootNode1),
    [<<"aabcde">>, <<"aac">>, <<"abcdefghijklmnopqrstuvwxyz">>] =
        btrie:fetch_keys(RootNode1),
    [{<<"aabcde">>, 3}, {<<"aac">>, 2},
     {<<"abcdefghijklmnopqrstuvwxyz">>, 1}] = btrie:to_list(RootNode1),
    [{<<"aabcde">>, 3}, {<<"aac">>, 12},
     {<<"abcdefghijklmnopqrstuvwxyz">>, 1}] =
        btrie:to_list(btrie:update(
            <<"aac">>, fun(I) -> I + 10 end, RootNode1)),
    [{<<"aaa">>, 4}, {<<"aabcde">>, 3}, {<<"aac">>, 2},
     {<<"abcdefghijklmnopqrstuvwxyz">>, 1}] =
        btrie:to_list(btrie:update(
            <<"aaa">>, fun(I) -> I + 10 end, 4, RootNode1)),
    6 = foldl(fun(_, I, A) -> I + A end, 0, RootNode1),
    [{<<"aabcde">>, 3},{<<"aac">>, 2},{<<"abcdefghijklmnopqrstuvwxyz">>, 1}] =
        foldr(fun(K, V, A) -> [{K,V} | A] end, [], RootNode1),
    [{<<"abcdefghijklmnopqrstuvwxyz">>, 1}, {<<"aac">>, 2}, {<<"aabcde">>, 3}] =
        foldl(fun(K, V, A) -> [{K,V} | A] end, [], RootNode1),
    error = btrie:find(<<"aabcde">>, RootNode0),
    {ok, 3} = btrie:find(<<"aabcde">>, RootNode1),
    RootNode2 = btrie:erase(<<"aac">>, RootNode0),
    {ok, 1} = btrie:find(<<"abcdefghijklmnopqrstuvwxyz">>, RootNode2),
    {97,98,{{{98,98,{{<<>>,[2]}}},[1]},{<<"c">>,[3]}}} =
        RootNode3 = btrie:new([{<<"a">>, [1]},{<<"ab">>, [2]},{<<"bc">>, [3]}]),
    {97,98,{{{98,98,{{<<>>,[2]}}},[1,2]},{<<"c">>,[3]}}} =
        btrie:append(<<"a">>, 2, RootNode3),

    RootNode4 = btrie:new([
        {<<"ammmmmmm">>,      7},
        {<<"aaaaaaaaaaa">>,   4},
        {<<"aaa">>,           2},
        {<<"ab">>,            0},
        {<<"ab">>,            5},
        {<<"aa">>,            1},
        {<<"aba">>,           6},
        {<<"aaaaaaaa">>,      3}]),
    {97,97,
     {{{97,109,
        {{{97,97,
           {{{97,97,
              {{{97,97,
                 {{{97,97,
                    {{{97,97,
                       {{{97,97,
                          {{{97,97,
                             {{<<"aa">>,4}}},
                            3}}},
                         error}}},
                     error}}},
                   error}}},
                error}}},
             2}}},
          1},
         {{97,97,{{<<>>,6}}},5},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<>>,error},
         {<<"mmmmmm">>,7}}},
       error}}} = RootNode4,
    [{<<"aa">>,1},
     {<<"aaa">>,2},
     {<<"aaaaaaaa">>,3},
     {<<"aaaaaaaaaaa">>,4},
     {<<"ab">>,5},
     {<<"aba">>,6},
     {<<"ammmmmmm">>,7}] = btrie:to_list(
        btrie:from_list(btrie:to_list(RootNode4))),
%    Liter =  ["aa", "aaa", "aaaaaaaa", "aaaaaaaaaaa", "ab", "aba"],
%    Fiter = fun(Key, _, Iter) ->
%        case lists:member(Key, Liter) of
%            true ->
%                Iter();
%            false ->
%                done
%        end
%    end,
%    Fitera = fun
%        (_, _, [], _) ->
%            done;
%        (Key, _, [Key | T], Iter) ->
%            Iter(T)
%    end,
%    ok = btrie:iter(Fiter, RootNode4),
%    done = btrie:itera(Fitera, Liter, RootNode4),
%    % btrie:map happens to go through in reverse order
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>,
     <<"ab">>,
     <<"aba">>,
     <<"ammmmmmm">>] = btrie:fetch_keys(RootNode4),
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>,
     <<"ab">>,
     <<"aba">>,
     <<"ammmmmmm">>] = btrie:foldr(
        fun(Key, _, L) -> [Key | L] end, [], RootNode4),
    [<<"ammmmmmm">>,
     <<"aba">>,
     <<"ab">>,
     <<"aaaaaaaaaaa">>,
     <<"aaaaaaaa">>,
     <<"aaa">>,
     <<"aa">>] = btrie:foldl(
        fun(Key, _, L) -> [Key | L] end, [], RootNode4),
    RootNode5 = btrie:store(<<"a">>, 0,
        btrie:store(<<"aaaa">>, 2.5, RootNode4)),
    {ok, 2.5} = btrie:find(<<"aaaa">>, RootNode5),
    error = btrie:find(<<"aaaa">>, RootNode4),
    {ok, 2.5} = btrie:find_prefix(<<"aaaa">>, RootNode5),
    prefix = btrie:find_prefix(<<"aaaa">>, RootNode4),
    error = btrie:find_prefix_longest(<<"a">>, RootNode4),
    {ok, <<"aa">>, 1} = btrie:find_prefix_longest(<<"aa">>, RootNode4),
    {ok, <<"aaa">>, 2} = btrie:find_prefix_longest(<<"aaaa">>, RootNode4),
    {ok, <<"ab">>, 5} = btrie:find_prefix_longest(<<"absolut">>, RootNode4),
    {ok, <<"aba">>, 6} = btrie:find_prefix_longest(<<"aba">>, RootNode4),
    {ok, <<"aaaaaaaa">>, 3} = btrie:find_prefix_longest(<<"aaaaaaaaa">>, RootNode4),
    error = btrie:find_prefix_longest(<<"bar">>, RootNode4),
    {ok, <<"aaaaaaaaaaa">>, 4} = btrie:find_prefix_longest(<<"aaaaaaaaaaaaaaaaaaaaaddddddaa">>, RootNode4),
    2.5 = btrie:fetch(<<"aaaa">>, RootNode5),
    {'EXIT', {if_clause, _}} = (catch btrie:fetch(<<"aaaa">>, RootNode4)),
    RootNode4 = btrie:erase(<<"a">>, btrie:erase(<<"aaaa">>, RootNode5)),
    true = btrie:is_key(<<"aaaa">>, RootNode5),
    false = btrie:is_key(<<"aaaa">>, RootNode4),
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>] = btrie:fetch_keys_similar(<<"aa">>, RootNode4),
    [<<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>] = btrie:fetch_keys_similar(<<"aaac">>, RootNode4),
    [<<"ab">>,
     <<"aba">>] = btrie:fetch_keys_similar(<<"abba">>, RootNode4),
    [<<"aa">>,
     <<"aaa">>,
     <<"aaaaaaaa">>,
     <<"aaaaaaaaaaa">>,
     <<"ab">>,
     <<"aba">>,
     <<"ammmmmmm">>] = btrie:fetch_keys_similar(<<"a">>, RootNode4),
    [] = btrie:fetch_keys_similar(<<"b">>, RootNode4),
%    {ok, "aa", 1} = btrie:find_similar("aa", RootNode4),
%    {ok, "aaa", 2} = btrie:find_similar("aaac", RootNode4),
%    {ok, "aaaaaaaa", 3} = btrie:find_similar("aaaa", RootNode4),
%    {ok, "ab", 5} = btrie:find_similar("abba", RootNode4),
%    {ok, "aa", 1} = btrie:find_similar("a", RootNode4),
%    true = btrie:is_prefixed("abacus", RootNode4),
%    false = btrie:is_prefixed("ac", RootNode4),
%    false = btrie:is_prefixed("abacus", "ab", RootNode4),
%    true = btrie:foldl(fun(K, _, L) -> [K | L] end, [], RootNode4) ==
%           btrie:fold_match("*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aaa"
%     ] = btrie:fold_match("*aa", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aaaaaaaaaaa",
%     "aaaaaaaa",
%     "aaa"
%     ] = btrie:fold_match("aa*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aba"
%     ] = btrie:fold_match("ab*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["ammmmmmm"
%     ] = btrie:fold_match("am*", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    ["aba",
%     "aaa"
%     ] = btrie:fold_match("a*a", fun(K, _, L) -> [K | L] end, [], RootNode4),
%    {'EXIT',badarg} = (catch btrie:fold_match("a**a", fun(K, _, L) -> [K | L] end, [], RootNode4)),
    _RootNode6 = btrie:new([
        {<<"*">>,      1},
        {<<"aa*">>,    2},
        {<<"aa*b">>,   3},
        {<<"aa*a*">>,  4},
        {<<"aaaaa">>,  5}]),
%    {ok,"aa*",2} = btrie:find_match("aaaa", RootNode6),
%    {ok,"aaaaa",5} = btrie:find_match("aaaaa", RootNode6),
%    {ok,"*",1} = btrie:find_match("aa", RootNode6),
%    {ok,"aa*",2} = btrie:find_match("aab", RootNode6),
%    {ok,"aa*b",3} = btrie:find_match("aabb", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aabab", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aababb", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aabbab", RootNode6),
%    {ok,"aa*a*",4} = btrie:find_match("aabbabb", RootNode6),
%    {'EXIT',badarg} = (catch btrie:find_match("aa*", RootNode6)),
%    {'EXIT',badarg} = (catch btrie:find_match("aaaa*", RootNode6)),
%    {'EXIT',badarg} = (catch btrie:find_match("aaaaa*", RootNode6)),
%    ["aa"] = btrie:pattern_parse("aa*", "aaaa"),
%    ["b"] = btrie:pattern_parse("aa*", "aab"),
%    ["b"] = btrie:pattern_parse("aa*b", "aabb"),
%    ["b", "b"] = btrie:pattern_parse("aa*a*", "aabab"),
%    ["b", "bb"] = btrie:pattern_parse("aa*a*", "aababb"),
%    ["bb", "b"] = btrie:pattern_parse("aa*a*", "aabbab"),
%    ["bb", "bb"] = btrie:pattern_parse("aa*a*", "aabbabb"),
%    error = btrie:pattern_parse("aa*a*", "aaabb"),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% make a new tuple with arity N and default D, then
%% move tuple T into the new tuple at index I
tuple_move(I, N, T, D)
    when is_integer(I), is_integer(N), is_tuple(T),
         (N - I + 1) >= tuple_size(T) ->
    tuple_move_i(I, 1, I + tuple_size(T), erlang:make_tuple(N, D), T).

tuple_move_i(N1, _, N1, T1, _) ->
    T1;

tuple_move_i(I1, I0, N1, T1, T0) ->
    tuple_move_i(I1 + 1, I0 + 1, N1,
        erlang:setelement(I1, T1, erlang:element(I0, T0)), T0).

binary_prefix(<<>>, _) ->
    true;
binary_prefix(KeyEnd1, KeyEnd2) ->
    case binary:match(KeyEnd2, KeyEnd1, []) of
        {0, _} ->
            true;
        _  ->
            false
    end.

%wildcard_match_lists_element(_, []) ->
%    error;
%
%wildcard_match_lists_element(_, [$* | _]) ->
%    erlang:exit(badarg);
%
%wildcard_match_lists_element(C, [C | L]) ->
%    {ok, L};
%
%wildcard_match_lists_element(C, [_ | L]) ->
%    wildcard_match_lists_element(C, L).
%
%wildcard_match_lists_valid([], Result) ->
%    Result;
%
%wildcard_match_lists_valid([$* | _], _) ->
%    erlang:exit(badarg);
%
%wildcard_match_lists_valid([_ | L], Result) ->
%    wildcard_match_lists_valid(L, Result).
%
%wildcard_match_lists([], []) ->
%    true;
%
%wildcard_match_lists([], [_ | _] = L) ->
%    wildcard_match_lists_valid(L, false);
%
%wildcard_match_lists([_ | _], [$* | _]) ->
%    erlang:exit(badarg);
%
%wildcard_match_lists([$*], [_ | L]) ->
%    wildcard_match_lists_valid(L, true);
%
%wildcard_match_lists([$*, C | Match], [_ | L]) ->
%    true = C =/= $*,
%    case wildcard_match_lists_element(C, L) of
%        {ok, NewL} ->
%            wildcard_match_lists(Match, NewL);
%        error ->
%            wildcard_match_lists_valid(L, false)
%    end;
%
%wildcard_match_lists([C | Match], [C | L]) ->
%    wildcard_match_lists(Match, L);
%
%wildcard_match_lists(_, L) ->
%    wildcard_match_lists_valid(L, false).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test_() ->
    [
        {"internal tests", ?_assertEqual(ok, test())}
    ].

% do not have a good way yet to have the proper test switch to use binaries
%proper_test_() ->
%    {timeout, 600, [
%        {"proper tests", ?_assert(trie_proper:qc_run(?MODULE))}
%    ]}.

-endif.

