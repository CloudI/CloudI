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
%%% This Erlang trie implementation uses string (list of integers) keys and
%%% is able to get performance close to the process dictionary when doing key
%%% lookups (find or fetch, see [http://okeuday.livejournal.com/16941.html]).
%%% Utilizing this trie, it is possible to avoid generating dynamic atoms
%%% in various contexts.  Also, an added benefit to using this trie is that
%%% the traversals preserve alphabetical ordering.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2010-2022 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2010-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(trie).
-author('mjtruog at protonmail dot com').

%% external interface
-export([append/3,
         append_list/3,
         erase/2,
         erase_similar/2,
         fetch/2,
         fetch_keys/1,
         fetch_keys_similar/2,
         filter/2,
         find/2,
         find_match/2,
         find_match2/2,
         find_prefix/2,
         find_prefixes/2,
         find_prefix_longest/2,
         find_similar/2,
         fold/3,
         foldl/3,
         foldr/3,
         fold_match/4,
         fold_similar/4,
         foldl_similar/4,
         foldr_similar/4,
         foreach/2,
         from_list/1,
         is_bytestring/1,
         is_bytestring_nonempty/1,
         is_key/2,
         is_pattern/1,
         is_pattern_bytes/1,
         is_pattern2/1,
         is_pattern2_bytes/1,
         is_prefix/2,
         is_prefixed/2,
         is_prefixed/3,
         iter/2,
         itera/3,
         map/2,
         merge/3,
         new/0,
         new/1,
         pattern_fill/2,
         pattern_fill/4,
         pattern_parse/2,
         pattern_parse/3,
         pattern_suffix/2,
         pattern2_fill/2,
         pattern2_fill/4,
         pattern2_parse/2,
         pattern2_parse/3,
         pattern2_suffix/2,
         prefix/3,
         size/1,
         store/2,
         store/3,
         take/2,
         to_list/1,
         to_list_similar/2,
         update/3,
         update/4,
         update_counter/3,
         test/0]).

-define(MODE_LIST, true).
-include("trie.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a match with patterns held within a trie.===
%% All patterns held within the trie use a wildcard character "*" to represent
%% a regex of ".+".  "**" within the trie will result in undefined behavior
%% (the pattern is malformed).  The function will search for the most specific
%% match possible, given the input string and the trie contents.  The input
%% string must not contain wildcard characters, otherwise a badarg exit
%% exception will occur.  If you instead want to supply a pattern string to
%% match the contents of the trie, see fold_match/4.
%% @end
%%-------------------------------------------------------------------------

-spec find_match(string(), trie()) -> {ok, any(), any()} | 'error'.

find_match(_, []) ->
    error;

find_match(Match, Node) ->
    find_match_node(Match, [], Node).

find_match_node([], _, _) ->
    error;

find_match_node([$* | _], _, _) ->
    erlang:exit(badarg);

find_match_node([H | T] = Match, Key, {I0, I1, Data} = Node)
    when is_integer(H) ->
    Result = if
        H < I0; H > I1 ->
            error;
        true ->
            {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
            if
                T =:= [] ->
                    if
                        is_tuple(ChildNode); ChildNode =:= [] ->
                            if
                                Value =:= error ->
                                    error;
                                true ->
                                    {ok, lists:reverse([H | Key]), Value}
                            end;
                        true ->
                            error
                    end;
                true ->
                    if
                        is_tuple(ChildNode) ->
                            find_match_node(T, [H | Key], ChildNode);
                        Value =:= error ->
                            error;
                        true ->
                            case wildcard_match_lists(ChildNode, T) of
                                true ->
                                    {ok, lists:reverse([H | Key],
                                                       ChildNode), Value};
                                false ->
                                    error
                            end
                    end
            end
    end,
    if
        Result =:= error ->
            find_match_pattern_1(Match, Key, Node);
        true ->
            Result
    end.

find_match_pattern_1([_ | T] = Match, Key, {I0, I1, Data})
    when $* >= I0, $* =< I1 ->
    {ChildNode, Value} = erlang:element($* - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            find_match_pattern_N(T, [$* | Key], Value, ChildNode);
        Value =:= error ->
            error;
        true ->
            Suffix = [$* | ChildNode],
            case wildcard_match_lists(Suffix, Match) of
                true ->
                    {ok, lists:reverse(Key, Suffix), Value};
                false ->
                    error
            end
    end;

find_match_pattern_1(_, _, _) ->
    error.

find_match_pattern_N([], _, error, _) ->
    error;

find_match_pattern_N([], Key, WildValue, _) ->
    {ok, lists:reverse(Key), WildValue};

find_match_pattern_N([$* | _], _, _, _) ->
    erlang:exit(badarg);

find_match_pattern_N([H | T], Key, WildValue, {I0, I1, _} = Node)
    when H < I0; H > I1 ->
    find_match_pattern_N(T, Key, WildValue, Node);

find_match_pattern_N([H | T], Key, WildValue, {I0, _, Data} = Node) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            case find_match_node(T, [H | Key], ChildNode) of
                error ->
                    find_match_pattern_N(T, Key, WildValue, Node);
                Result ->
                    Result
            end;
        Value =:= error ->
            find_match_pattern_N(T, Key, WildValue, Node);
        true ->
            case wildcard_match_lists(ChildNode, T) of
                true ->
                    {ok, lists:reverse([H | Key], ChildNode), Value};
                false ->
                    find_match_pattern_N(T, Key, WildValue, Node)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a match with patterns (using 2 wildcard characters) held within a trie.===
%% All patterns held within the trie use the wildcard character "*" or "?"
%% to represent a regex of ".+".  "**", "??", "*?", or "?*" within the
%% trie will result in undefined behavior (the pattern is malformed).
%% The function will search for the most specific match possible, given the
%% input string and the trie contents.  The input string must not contain
%% wildcard characters, otherwise a badarg exit exception will occur.
%% The "?" wildcard character consumes the shortest match to the next
%% character and must not be the the last character in the string
%% (the pattern would be malformed).
%% @end
%%-------------------------------------------------------------------------

-spec find_match2(string(), trie()) -> {ok, any(), any()} | 'error'.

find_match2(_, []) ->
    error;

find_match2(Match, Node) ->
    find_match2_node(Match, [], Node).

find_match2_node([], _, _) ->
    error;

find_match2_node([H | _], _, _)
    when H == $*; H == $? ->
    erlang:exit(badarg);

find_match2_node([H | T] = Match, Key, {I0, I1, Data} = Node)
    when is_integer(H) ->
    Result = if
        H < I0; H > I1 ->
            error;
        true ->
            {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
            if
                T =:= [] ->
                    if
                        is_tuple(ChildNode); ChildNode =:= [] ->
                            if
                                Value =:= error ->
                                    error;
                                true ->
                                    {ok, lists:reverse([H | Key]), Value}
                            end;
                        true ->
                            error
                    end;
                true ->
                    if
                        is_tuple(ChildNode) ->
                            find_match2_node(T, [H | Key], ChildNode);
                        Value =:= error ->
                            error;
                        true ->
                            case wildcard_match2_lists(ChildNode, T) of
                                true ->
                                    {ok, lists:reverse([H | Key],
                                                       ChildNode), Value};
                                false ->
                                    error
                            end
                    end
            end
    end,
    if
        Result =:= error ->
            ResultPattern0 = find_match2_pattern0_0(Match, Key, Node),
            if
                ResultPattern0 =:= error ->
                    find_match2_pattern1_1(Match, Key, Node);
                true ->
                    ResultPattern0
            end;
        true ->
            Result
    end.

find_match2_pattern0_0([H | T] = Match, Key, {I0, I1, Data})
    when $? >= I0, $? =< I1 ->
    {ChildNode, Value} = erlang:element($? - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            find_match2_pattern0_1(T, H, [$? | Key], ChildNode);
        Value =:= error; ChildNode =:= [] ->
            error;
        true ->
            Suffix = [$? | ChildNode],
            case wildcard_match2_lists(Suffix, Match) of
                true ->
                    {ok, lists:reverse(Key, Suffix), Value};
                false ->
                    error
            end
    end;

find_match2_pattern0_0(_, _, _) ->
    error.

find_match2_pattern0_1(T, H, Key, {I0, I1, _} = Node)
    when H < I0; H > I1 ->
    find_match2_pattern0_N(T, undefined, Key, Node);

find_match2_pattern0_1(T, H, Key, {I0, _, Data} = Node) ->
    {_, Value} = erlang:element(H - I0 + 1, Data),
    if
        Value =:= error ->
            find_match2_pattern0_N(T, undefined, Key, Node);
        true ->
            find_match2_pattern0_N(T, H, Key, Node)
    end.

find_match2_pattern0_N([], _, _, _) ->
    error;

find_match2_pattern0_N([$? | _], _, _, _) ->
    erlang:exit(badarg);

find_match2_pattern0_N([H | T], H, Key, Node) ->
    find_match2_pattern0_N(T, H, Key, Node);

find_match2_pattern0_N([H | T], Ignore, Key, {I0, I1, _} = Node)
    when H < I0; H > I1 ->
    find_match2_pattern0_N(T, Ignore, Key, Node);

find_match2_pattern0_N([H | T], Ignore, Key, {I0, _, Data} = Node) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            find_match2_node(T, [H | Key], ChildNode);
        Value =:= error ->
            find_match2_pattern0_N(T, Ignore, Key, Node);
        true ->
            case wildcard_match2_lists(ChildNode, T) of
                true ->
                    {ok, lists:reverse([H | Key], ChildNode), Value};
                false ->
                    find_match2_pattern0_N(T, Ignore, Key, Node)
            end
    end.

find_match2_pattern1_1([_ | T] = Match, Key, {I0, I1, Data})
    when $* >= I0, $* =< I1 ->
    {ChildNode, Value} = erlang:element($* - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            find_match2_pattern1_N(T, [$* | Key], Value, ChildNode);
        Value =:= error ->
            error;
        true ->
            Suffix = [$* | ChildNode],
            case wildcard_match2_lists(Suffix, Match) of
                true ->
                    {ok, lists:reverse(Key, Suffix), Value};
                false ->
                    error
            end
    end;

find_match2_pattern1_1(_, _, _) ->
    error.

find_match2_pattern1_N([], _, error, _) ->
    error;

find_match2_pattern1_N([], Key, WildValue, _) ->
    {ok, lists:reverse(Key), WildValue};

find_match2_pattern1_N([$* | _], _, _, _) ->
    erlang:exit(badarg);

find_match2_pattern1_N([H | T], Key, WildValue, {I0, I1, _} = Node)
    when H < I0; H > I1 ->
    find_match2_pattern1_N(T, Key, WildValue, Node);

find_match2_pattern1_N([H | T], Key, WildValue, {I0, _, Data} = Node) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            case find_match2_node(T, [H | Key], ChildNode) of
                error ->
                    find_match2_pattern1_N(T, Key, WildValue, Node);
                Result ->
                    Result
            end;
        Value =:= error ->
            find_match2_pattern1_N(T, Key, WildValue, Node);
        true ->
            case wildcard_match2_lists(ChildNode, T) of
                true ->
                    {ok, lists:reverse([H | Key], ChildNode), Value};
                false ->
                    find_match2_pattern1_N(T, Key, WildValue, Node)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find the first key/value pair in a trie where the key shares a common prefix.===
%% The first match is found based on alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec find_similar(Similar :: string(),
                   Node :: trie()) -> {ok, string(), any()} | 'error'.

find_similar([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    error;

find_similar(_, []) ->
    error;

find_similar(Similar, Node) ->
    find_similar_entry(Similar, [], error, Node).

find_similar_entry([H | _], Key, LastValue, {I0, I1, _} = Node)
    when H < I0; H > I1 ->
    if
        LastValue =:= error ->
            find_similar_element(Key, Node);
        true ->
            {ok, Key, LastValue}
    end;

find_similar_entry([H] = Suffix, Key, _, {I0, _, Data} = Node)
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            NewKey = Key ++ Suffix,
            if
                Value =:= error ->
                    find_similar_element(NewKey, ChildNode);
                true ->
                    {ok, NewKey, Value}
            end;
        Value =/= error, ChildNode =:= [] ->
            {ok, Key ++ Suffix, Value};
        true ->
            find_similar_element(Key, Node)
    end;

find_similar_entry([H | T] = Suffix, Key, _, {I0, _, Data} = Node)
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            find_similar_entry(T, Key ++ [H], Value, ChildNode);
        Value =/= error, ChildNode == T ->
            {ok, Key ++ Suffix, Value};
        true ->
            find_similar_element(Key, Node)
    end.

find_similar_element(Key, Node) ->
    {trie_itera_done, Result} = itera(fun(NewKey, Value, _, _) ->
        {ok, NewKey, Value}
    end, {trie_itera_done, error}, Key, Node),
    Result.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the keys within a trie that matches a pattern.===
%% Traverses in alphabetical order.  Uses "*" as a wildcard character
%% within the pattern (it acts like a ".+" regex, and "**" is forbidden).
%% The trie keys must not contain wildcard characters, otherwise a badarg
%% exit exception will occur. If you want to match a specific string
%% without wildcards on trie values that contain wildcard characters,
%% see find_match/2.
%% @end
%%-------------------------------------------------------------------------

-spec fold_match(Match :: string(),
                 F :: fun((string(), any(), any()) -> any()),
                 A :: any(),
                 Node :: trie()) -> any().

fold_match(_, _, A, []) ->
    A;

fold_match(Match, F, A, Node) ->
    fold_match_node_1(Match, F, A, [], Node).

fold_match_node_1([$*, $* | _], _, _, _, _) ->
    erlang:exit(badarg);

fold_match_node_1([$* | _] = Match, F, A, Prefix, {I0, I1, Data}) ->
    fold_match_element_1(Match, F, A, 1, I1 - I0 + 2, I0 - 1,
                         Prefix, [], Data);

fold_match_node_1([H | _], _, A, _, {I0, I1, _})
    when H < I0; H > I1 ->
    A;

fold_match_node_1([H], F, A, Prefix, {I0, _, Data})
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        Value =/= error ->
            if
                is_tuple(ChildNode); ChildNode =:= [] ->
                    F(lists:reverse([H | Prefix]), Value, A);
                true ->
                    A
            end;
        true ->
            A
    end;

fold_match_node_1([H | T], F, A, Prefix, {I0, _, Data})
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    NewPrefix = [H | Prefix],
    if
        is_tuple(ChildNode) ->
            fold_match_node_1(T, F, A, NewPrefix, ChildNode);
        Value =/= error ->
            case wildcard_match_lists(T, ChildNode) of
                true ->
                    F(lists:reverse(NewPrefix, ChildNode), Value, A);
                false ->
                    A
            end;
        true ->
            A
    end.

fold_match_element_1(_, _, A, N, N, _, _, _, _) ->
    A;

fold_match_element_1(Match, F, A, I, N, Offset, Prefix, Mid, Data)
    when I + Offset =:= $* ->
    case erlang:element(I, Data) of
        {[], error} ->
            fold_match_element_1(Match, F, A,
                I + 1, N, Offset, Prefix, Mid, Data);
        _ ->
            erlang:exit(badarg)
    end;

fold_match_element_1([$* | T] = Match, F, A, I, N, Offset, Prefix, Mid, Data) ->
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {I0, I1, NextData} ->
            NewMid = [(Offset + I) | Mid],
            NewA = if
                T =:= [], Value =/= error ->
                    F(lists:reverse(NewMid ++ Prefix), Value, A);
                true ->
                    A
            end,
            fold_match_element_1(Match, F,
                fold_match_element_N(Match, F, NewA,
                    1, I1 - I0 + 2, I0 - 1,
                    Prefix, NewMid, NextData),
                I + 1, N, Offset, Prefix, Mid, Data);
        _ ->
            NewA = if
                Value =/= error ->
                    Suffix = lists:reverse([(Offset + I) | Mid], Node),
                    case wildcard_match_lists(Match, Suffix) of
                        true ->
                            F(lists:reverse(Prefix, Suffix), Value, A);
                        false ->
                            A
                    end;
                true ->
                    A
            end,
            fold_match_element_1(Match, F, NewA,
                I + 1, N, Offset, Prefix, Mid, Data)
    end.

fold_match_element_N(_, _, A, N, N, _, _, _, _) ->
    A;

fold_match_element_N(Match, F, A, I, N, Offset, Prefix, Mid, Data)
    when I + Offset =:= $* ->
    case erlang:element(I, Data) of
        {[], error} ->
            fold_match_element_N(Match, F, A,
                I + 1, N, Offset, Prefix, Mid, Data);
        _ ->
            erlang:exit(badarg)
    end;

fold_match_element_N([$*] = Match, F, A, I, N, Offset, Prefix, Mid, Data) ->
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {I0, I1, NextData} ->
            NewMid = [(Offset + I) | Mid],
            NewA = if
                Value =/= error ->
                    F(lists:reverse(NewMid ++ Prefix),
                      Value, A);
                true ->
                    A
            end,
            fold_match_element_N(Match, F,
                fold_match_element_N(Match, F, NewA,
                    1, I1 - I0 + 2, I0 - 1,
                    Prefix, NewMid, NextData),
                I + 1, N, Offset, Prefix, Mid, Data);
        _ ->
            NewA = if
                Value =/= error ->
                    F(lists:reverse([(Offset + I) | Mid] ++ Prefix, Node),
                      Value, A);
                true ->
                    A
            end,
            fold_match_element_N(Match, F, NewA,
                I + 1, N, Offset, Prefix, Mid, Data)
    end;

fold_match_element_N([$* | T] = Match, F, A, I, N, Offset, Prefix, Mid, Data) ->
    {Node, Value} = erlang:element(I, Data),
    case T of
        [C | NewMatch] when C =:= Offset + I ->
            NewPrefix = [(Offset + I) | Mid] ++ Prefix,
            case NewMatch of
                [_ | _] when is_tuple(Node) ->
                    fold_match_node_1(NewMatch, F, A, NewPrefix, Node);
                [_ | _] ->
                    if
                        Value =/= error ->
                            case wildcard_match_lists(NewMatch, Node) of
                                true ->
                                    F(lists:reverse(NewPrefix, Node),
                                      Value, A);
                                false ->
                                    A
                            end;
                        true ->
                            A
                    end;
                [] ->
                    case Node of
                        [_ | _] ->
                            A;
                        _ when Value =/= error ->
                            F(lists:reverse(NewPrefix),
                              Value, A);
                        _ ->
                            A
                    end
            end;
        _ ->
            case Node of
                {I0, I1, NextData} ->
                    fold_match_element_N(Match, F,
                        fold_match_element_N(Match, F, A,
                            1, I1 - I0 + 2, I0 - 1,
                            Prefix, [(Offset + I) | Mid], NextData),
                        I + 1, N, Offset, Prefix, Mid, Data);
                _ ->
                    NewA = if
                        Value =/= error ->
                            Suffix = lists:reverse([(Offset + I) | Mid], Node),
                            case wildcard_match_lists(Match, Suffix) of
                                true ->
                                    F(lists:reverse(Prefix, Suffix),
                                      Value, A);
                                false ->
                                    A
                            end;
                        true ->
                            A
                    end,
                    fold_match_element_N(Match, F, NewA,
                        I + 1, N, Offset, Prefix, Mid, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Test if the parameter is a byte string.===
%% @end
%%-------------------------------------------------------------------------

-spec is_bytestring(L :: list(byte())) -> 'true' | 'false'.

is_bytestring([]) ->
    true;

is_bytestring([C | L])
    when C >= 0 andalso C =< 255 ->
    is_bytestring(L);

is_bytestring(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Test if the parameter is a nonempty byte string.===
%% @end
%%-------------------------------------------------------------------------

-spec is_bytestring_nonempty(L :: nonempty_list(byte())) -> 'true' | 'false'.

is_bytestring_nonempty([C | L])
    when C >= 0 andalso C =< 255 ->
    is_bytestring(L);

is_bytestring_nonempty(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Test to determine if a string is a pattern.===
%% "*" is the wildcard character (equivalent to the ".+" regex).
%% "**" is forbidden.
%% @end
%%-------------------------------------------------------------------------

-spec is_pattern(Pattern :: string()) -> 'true' | 'false'.

is_pattern(Pattern) ->
    is_pattern(Pattern, false).

is_pattern([], Result) ->
    Result;

is_pattern([$*, $* | _], _) ->
    erlang:exit(badarg);

is_pattern([$* | Pattern], _) ->
    is_pattern(Pattern, true);

is_pattern([C | Pattern], Result)
    when C >= 0 ->
    is_pattern(Pattern, Result);

is_pattern(_, _) ->
    erlang:exit(badarg).

%%-------------------------------------------------------------------------
%% @doc
%% ===Test to determine if a byte string is a pattern.===
%% "*" is the wildcard character (equivalent to the ".+" regex).
%% "**" is forbidden.
%% @end
%%-------------------------------------------------------------------------

-spec is_pattern_bytes(Pattern :: list(byte())) -> 'true' | 'false'.

is_pattern_bytes(Pattern) ->
    is_pattern_bytes(Pattern, false).

is_pattern_bytes([], Result) ->
    Result;

is_pattern_bytes([$*, $* | _], _) ->
    erlang:exit(badarg);

is_pattern_bytes([$* | Pattern], _) ->
    is_pattern_bytes(Pattern, true);

is_pattern_bytes([C | Pattern], Result)
    when C >= 0 andalso C =< 255 ->
    is_pattern_bytes(Pattern, Result);

is_pattern_bytes(_, _) ->
    erlang:exit(badarg).

%%-------------------------------------------------------------------------
%% @doc
%% ===Test to determine if a string is a pattern (using 2 wildcard characters).===
%% "*" and "?" are wildcard characters (equivalent to the ".+" regex).
%% "**", "??", "*?" and "?*" are forbidden.  "?" must not be the last
%% character in the pattern.
%% @end
%%-------------------------------------------------------------------------

-spec is_pattern2(Pattern :: string()) -> 'true' | 'false'.

is_pattern2(Pattern) ->
    is_pattern2(Pattern, false).

is_pattern2([], Result) ->
    Result;

is_pattern2([$?], _) ->
    erlang:exit(badarg);

is_pattern2([C0, C1 | _], _)
    when C0 == $* orelse C0 == $?,
         C1 == $* orelse C1 == $? ->
    erlang:exit(badarg);

is_pattern2([C | Pattern], _)
    when C == $*; C == $? ->
    is_pattern2(Pattern, true);

is_pattern2([C | Pattern], Result)
    when C >= 0 ->
    is_pattern2(Pattern, Result);

is_pattern2(_, _) ->
    erlang:exit(badarg).

%%-------------------------------------------------------------------------
%% @doc
%% ===Test to determine if a byte string is a pattern (using 2 wildcard characters).===
%% "*" and "?" are wildcard characters (equivalent to the ".+" regex).
%% "**", "??", "*?" and "?*" are forbidden.  "?" must not be the last
%% character in the pattern.
%% @end
%%-------------------------------------------------------------------------

-spec is_pattern2_bytes(Pattern :: list(byte())) -> 'true' | 'false'.

is_pattern2_bytes(Pattern) ->
    is_pattern2_bytes(Pattern, false).

is_pattern2_bytes([], Result) ->
    Result;

is_pattern2_bytes([$?], _) ->
    erlang:exit(badarg);

is_pattern2_bytes([C0, C1 | _], _)
    when C0 == $* orelse C0 == $?,
         C1 == $* orelse C1 == $? ->
    erlang:exit(badarg);

is_pattern2_bytes([C | Pattern], _)
    when C == $*; C == $? ->
    is_pattern2_bytes(Pattern, true);

is_pattern2_bytes([C | Pattern], Result)
    when C >= 0 andalso C =< 255 ->
    is_pattern2_bytes(Pattern, Result);

is_pattern2_bytes(_, _) ->
    erlang:exit(badarg).

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if the prefix provided has existed within a trie.===
%% The function returns true if the string supplied is a prefix
%% for a key that has previously been stored within the trie.
%% If no values with the prefix matching key(s) were removed from the trie,
%% then the prefix currently exists within the trie.
%% @end
%%-------------------------------------------------------------------------

-spec is_prefix(string(), trie()) -> 'true' | 'false'.

is_prefix([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    false;

is_prefix([H], {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {{_, _, _}, _} ->
            true;
        {_, error} ->
            false;
        {_, _} ->
            true
    end;

is_prefix([H | T], {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {{_, _, _} = Node, _} ->
            is_prefix(T, Node);
        {_, error} ->
            false;
        {T, _} ->
            true;
        {L, _} ->
            lists:prefix(T, L)
    end;

is_prefix(_, []) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if the provided string has a prefix within a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec is_prefixed(string(), trie()) -> 'true' | 'false'.

is_prefixed([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    false;

is_prefixed([H], {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {_, error} ->
            false;
        {{_, _, _}, _} ->
            true;
        {[], _} ->
            true;
        {[_ | _], _} ->
            false
    end;

is_prefixed([H | T], {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {{_, _, _} = Node, error} ->
            is_prefixed(T, Node);
        {{_, _, _}, _} ->
            true;
        {_, error} ->
            false;
        {T, _} ->
            true;
        {L, _} ->
            lists:prefix(L, T)
    end;

is_prefixed(_, []) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if the provided string has an acceptable prefix within a trie.===
%% The prefix within the trie must match at least 1 character that is not
%% within the excluded list of characters.
%% @end
%%-------------------------------------------------------------------------

-spec is_prefixed(string(), string(), trie()) -> 'true' | 'false'.

is_prefixed(Key, Exclude, Node) ->
    is_prefixed_match(Key, false, Exclude, Node).

is_prefixed_match([H | _], _, _, {I0, I1, _})
    when H < I0; H > I1 ->
    false;

is_prefixed_match([H], Matched, Exclude, {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {_, error} ->
            false;
        {{_, _, _}, _} ->
            Matched orelse (not lists:member(H, Exclude));
        {[], _} ->
            Matched orelse (not lists:member(H, Exclude));
        {[_ | _], _} ->
            false
    end;

is_prefixed_match([H | T], Matched, Exclude, {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {{_, _, _} = Node, error} ->
            is_prefixed_match(T, Matched orelse (not lists:member(H, Exclude)),
                              Exclude, Node);
        {{_, _, _} = Node, _} ->
            case (Matched orelse (not lists:member(H, Exclude))) of
                true ->
                    true;
                false ->
                    is_prefixed_match(T, false, Exclude, Node)
            end;
        {_, error} ->
            false;
        {L, _} ->
            is_prefixed_match_check(L, T,
                                    Matched orelse
                                    (not lists:member(H, Exclude)),
                                    Exclude)
    end;

is_prefixed_match(_, _, _, []) ->
    false.

is_prefixed_match_check([], _, Matched, _) ->
    Matched;

is_prefixed_match_check([H | T1], [H | T2], Matched, Exclude) ->
    is_prefixed_match_check(T1, T2,
                            Matched orelse (not lists:member(H, Exclude)),
                            Exclude);

is_prefixed_match_check(_, _, _, _) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Iterate over a trie.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec iter(F :: fun((string(), any(), fun(() -> any())) -> any()),
           Node :: trie()) -> ok.

iter(F, []) when is_function(F, 3) ->
    ok;

iter(F, Node) when is_function(F, 3) ->
    iter(F, [], Node),
    ok.

iter(F, Key, {I0, I1, Data}) ->
    iter_element(F, 1, I1 - I0 + 2, I0 - 1, Key, Data).

iter_element(_, N, N, _, _, _) ->
    trie_iter_done;

iter_element(F, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    if
        is_list(Node) =:= false ->
            if
                Value =:= error ->
                    iter(F, Key ++ [Offset + I], Node),
                    iter_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = Key ++ [Offset + I],
                    Iter = fun() ->
                        iter(F, NewKey, Node)
                    end,
                    case F(NewKey, Value, Iter) of
                        trie_iter_done ->
                            iter_element(F, I + 1, N, Offset, Key, Data);
                        _ ->
                            ok
                    end
            end;
        true ->
            if
                Value =:= error ->
                    iter_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    Iter = fun() ->
                        iter_element(F, I + 1, N, Offset, Key, Data)
                    end,
                    F((Key ++ [Offset + I]) ++ Node, Value, Iter)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Iterate over a trie with an accumulator.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec itera(F :: fun((string(), any(), any(), fun((any()) -> any())) -> any()),
            A :: any(),
            Node :: trie()) -> any().

itera(F, A, []) when is_function(F, 4) ->
    A;

itera(F, A, Node) when is_function(F, 4) ->
    {trie_itera_done, NewA} = itera(F, {trie_itera_done, A}, [], Node),
    NewA.

itera(F, ReturnValue, Key, {I0, I1, Data}) ->
    itera_element(F, ReturnValue, 1, I1 - I0 + 2, I0 - 1, Key, Data).

itera_element(_, {trie_itera_done, _} = ReturnValue, N, N, _, _, _) ->
    ReturnValue;

itera_element(F, {trie_itera_done, A} = ReturnValue, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    if
        is_list(Node) =:= false ->
            if
                Value =:= error ->
                    itera_element(F,
                        itera(F, ReturnValue, Key ++ [Offset + I], Node),
                        I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = Key ++ [Offset + I],
                    Iter = fun(NewA) ->
                        itera(F, {trie_itera_done, NewA}, NewKey, Node)
                    end,
                    case F(NewKey, Value, A, Iter) of
                        {trie_itera_done, _} = NewReturnValue ->
                            itera_element(F, NewReturnValue,
                                I + 1, N, Offset, Key, Data);
                        Result ->
                            {trie_itera_done, Result}
                    end
            end;
        true ->
            if
                Value =:= error ->
                    itera_element(F, ReturnValue, I + 1, N, Offset, Key, Data);
                true ->
                    Iter = fun(NewA) ->
                        itera_element(F, {trie_itera_done, NewA},
                            I + 1, N, Offset, Key, Data)
                    end,
                    case F((Key ++ [Offset + I]) ++ Node, Value, A, Iter) of
                        {trie_itera_done, _} = NewReturnValue ->
                            NewReturnValue;
                        Result ->
                            {trie_itera_done, Result}
                    end
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fill wildcard characters in a string.===
%% The "*" wildcard character may be used consecutively by this function
%% to have parameters concatenated.
%% @end
%%-------------------------------------------------------------------------

-spec pattern_fill(FillPattern :: string(),
                   Parameters :: list(string())) ->
    {ok, string()} |
    {error, parameters_ignored | parameter_missing}.

pattern_fill(FillPattern, Parameters) ->
    pattern_fill_insert(FillPattern, Parameters, true).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fill wildcard characters in a string.===
%% The "*" wildcard character may be used consecutively by this function
%% to have parameters concatenated.
%% @end
%%-------------------------------------------------------------------------

-spec pattern_fill(FillPattern :: string(),
                   Parameters :: list(string()),
                   ParametersSelected :: list(pos_integer()),
                   ParametersStrictMatching :: boolean()) ->
    {ok, string()} |
    {error,
     parameters_ignored | parameter_missing | parameters_selected_empty |
     {parameters_selected_ignored, list(pos_integer())} |
     {parameters_selected_missing, pos_integer()}}.

pattern_fill(FillPattern, Parameters, [], ParametersStrictMatching) ->
    pattern_fill_insert(FillPattern, Parameters, ParametersStrictMatching);
pattern_fill(FillPattern, Parameters, ParametersSelected,
             ParametersStrictMatching) ->
    pattern_fill_select(FillPattern, Parameters, ParametersSelected,
                        ParametersStrictMatching).

pattern_fill_strip([], NameOut) ->
    {ok, lists:reverse(NameOut)};
pattern_fill_strip([$* | FillPatternIn], NameOut) ->
    pattern_fill_strip(FillPatternIn, NameOut);
pattern_fill_strip([C | FillPatternIn], NameOut) ->
    pattern_fill_strip(FillPatternIn, [C | NameOut]).

pattern_fill_insert([], NameOut,
                    [], _) ->
    {ok, lists:reverse(NameOut)};
pattern_fill_insert([], NameOut,
                    [_ | _], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_ignored};
        true ->
            {ok, lists:reverse(NameOut)}
    end;
pattern_fill_insert([$* | FillPatternIn], NameOut,
                    [], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameter_missing};
        true ->
            pattern_fill_strip(FillPatternIn, NameOut)
    end;
pattern_fill_insert([$* | FillPatternIn], NameOut,
                    [Parameter | Parameters], ParametersStrictMatching) ->
    pattern_fill_insert(FillPatternIn, lists:reverse(Parameter) ++ NameOut,
                        Parameters, ParametersStrictMatching);
pattern_fill_insert([C | FillPatternIn], NameOut,
                    Parameters, ParametersStrictMatching) ->
    pattern_fill_insert(FillPatternIn, [C | NameOut],
                        Parameters, ParametersStrictMatching).

pattern_fill_insert(FillPatternIn, Parameters, ParametersStrictMatching) ->
    pattern_fill_insert(FillPatternIn, [],
                        Parameters, ParametersStrictMatching).

pattern_fill_select([], NameOut, _,
                    ParametersSelected, ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true, ParametersSelected /= [] ->
            {error, {parameters_selected_ignored, ParametersSelected}};
        true ->
            {ok, lists:reverse(NameOut)}
    end;
pattern_fill_select([$* | FillPatternIn], NameOut, _,
                    [], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_selected_empty};
        true ->
            pattern_fill_strip(FillPatternIn, NameOut)
    end;
pattern_fill_select([$* | FillPatternIn], NameOut, Parameters,
                    [I | ParametersSelected],
                    ParametersStrictMatching) ->
    try lists:nth(I, Parameters) of
        Parameter ->
            pattern_fill_select(FillPatternIn,
                                lists:reverse(Parameter) ++ NameOut,
                                Parameters, ParametersSelected,
                                ParametersStrictMatching)
    catch
        error:_ ->
            if
                ParametersStrictMatching =:= true ->
                    {error, {parameters_selected_missing, I}};
                true ->
                    pattern_fill_strip(FillPatternIn, NameOut)
            end
    end;
pattern_fill_select([C | FillPatternIn], NameOut, Parameters,
                    ParametersSelected, ParametersStrictMatching) ->
    pattern_fill_select(FillPatternIn, [C | NameOut], Parameters,
                        ParametersSelected, ParametersStrictMatching).

pattern_fill_select(FillPatternIn, Parameters,
                    ParametersSelected, ParametersStrictMatching) ->
    pattern_fill_select(FillPatternIn, [], Parameters,
                        ParametersSelected, ParametersStrictMatching).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string based on the supplied wildcard pattern.===
%% "*" is the wildcard character (equivalent to the ".+" regex).
%% "**" is forbidden.
%% @end
%%-------------------------------------------------------------------------

-spec pattern_parse(Pattern :: string(),
                    L :: string()) -> list(string()) | 'error'.

pattern_parse(Pattern, L) ->
    pattern_parse(Pattern, L, default).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string based on the supplied wildcard pattern.===
%% "*" is the wildcard character (equivalent to the ".+" regex).
%% "**" is forbidden.
%% @end
%%-------------------------------------------------------------------------

-spec pattern_parse(Pattern :: string(),
                    L :: string(),
                    Option :: default | with_suffix | expanded) ->
    list(string()) |                       % default
    {list(string()), string()} |           % with_suffix
    list(string() | {exact, string()}) |   % expanded
    'error'.

pattern_parse(Pattern, L, Option)
    when (Option =:= default) orelse
         (Option =:= with_suffix) orelse
         (Option =:= expanded) ->
    pattern_parse(Pattern, L, [], [], Option).

pattern_parse_result(default, Parameters, _) ->
    lists:reverse(Parameters);

pattern_parse_result(with_suffix, Parameters, Suffix) ->
    {lists:reverse(Parameters), lists:reverse(Suffix)};

pattern_parse_result(expanded, Parameters, Suffix) ->
    NewParameters = if
        Suffix /= [] ->
            [{exact, lists:reverse(Suffix)} | Parameters];
        true ->
            Parameters
    end,
    lists:reverse(NewParameters).

pattern_parse_element(_, [], _) ->
    error;

pattern_parse_element(C, [C | T], Segment) ->
    {ok, T, lists:reverse(Segment)};

pattern_parse_element(_, [$* | _], _) ->
    erlang:exit(badarg);

pattern_parse_element(C, [H | T], L) ->
    pattern_parse_element(C, T, [H | L]).

pattern_parse_pattern(Pattern, C, L, Segment, Parsed, Option) ->
    case pattern_parse_element(C, L, Segment) of
        {ok, NewL, NewSegment} ->
            case pattern_parse(Pattern, NewL,
                               [NewSegment | Parsed], [C], Option) of
                error ->
                    pattern_parse_pattern(Pattern, C, NewL,
                                          [C | lists:reverse(NewSegment)],
                                          Parsed, Option);
                Success ->
                    Success
            end;
        error ->
            error
    end.

pattern_parse([], [], Parsed, Suffix, Option) ->
    pattern_parse_result(Option, Parsed, Suffix);

pattern_parse([], [_ | _], _, _, _) ->
    error;

pattern_parse([_ | _], [$* | _], _, _, _) ->
    erlang:exit(badarg);

pattern_parse([$*], [_ | _] = L, Parsed, Suffix, Option) ->
    NewParsed = if
        Option =:= expanded, Suffix /= [] ->
            [{exact, lists:reverse(Suffix)} | Parsed];
        true ->
            Parsed
    end,
    pattern_parse_result(Option, [L | NewParsed], []);

pattern_parse([$*, $* | _], [_ | _], _, _, _) ->
    erlang:exit(badarg);

pattern_parse([$*, C | Pattern], [H | T], Parsed, Suffix, Option) ->
    NewParsed = if
        Option =:= expanded, Suffix /= [] ->
            [{exact, lists:reverse(Suffix)} | Parsed];
        true ->
            Parsed
    end,
    pattern_parse_pattern(Pattern, C, T, [H], NewParsed, Option);

pattern_parse([C | Pattern], [C | L], Parsed, Suffix, Option) ->
    pattern_parse(Pattern, L, Parsed, [C | Suffix], Option);

pattern_parse(_, _, _, _, _) ->
    error.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string based on the supplied wildcard pattern to return only the suffix after the pattern.===
%% "*" is the wildcard character (equivalent to the ".+" regex).
%% "**" is forbidden.
%% @end
%%-------------------------------------------------------------------------

-spec pattern_suffix(Pattern :: string(),
                     L :: string()) ->
    string() | 'error'.

pattern_suffix([], []) ->
    [];

pattern_suffix([], [_ | _] = L) ->
    L;

pattern_suffix([_ | _], [$* | _]) ->
    erlang:exit(badarg);

pattern_suffix([$*], [_ | _]) ->
    [];

pattern_suffix([$*, $* | _], [_ | _]) ->
    erlang:exit(badarg);

pattern_suffix([$*, C | Pattern], [_ | T]) ->
    pattern_suffix_pattern(Pattern, C, T);

pattern_suffix([C | Pattern], [C | L]) ->
    pattern_suffix(Pattern, L);

pattern_suffix(_, _) ->
    error.

pattern_suffix_element(_, []) ->
    error;

pattern_suffix_element(C, [C | T]) ->
    {ok, T};

pattern_suffix_element(_, [$* | _]) ->
    erlang:exit(badarg);

pattern_suffix_element(C, [_ | T]) ->
    pattern_suffix_element(C, T).

pattern_suffix_pattern(Pattern, C, L) ->
    case pattern_suffix_element(C, L) of
        {ok, NewL} ->
            case pattern_suffix(Pattern, NewL) of
                error ->
                    pattern_suffix_pattern(Pattern, C, NewL);
                Success ->
                    Success
            end;
        error ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fill wildcard characters in a string.===
%% The "*" and "?" wildcard characters may be used consecutively by this
%% function to have parameters concatenated (both are processed the same
%% way by this function).
%% @end
%%-------------------------------------------------------------------------

-spec pattern2_fill(FillPattern :: string(),
                    Parameters :: list(string())) ->
    {ok, string()} |
    {error, parameters_ignored | parameter_missing}.

pattern2_fill(FillPattern, Parameters) ->
    pattern2_fill_insert(FillPattern, Parameters, true).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fill wildcard characters in a string.===
%% The "*" and "?" wildcard characters may be used consecutively by this
%% function to have parameters concatenated (both are processed the same
%% way by this function).
%% @end
%%-------------------------------------------------------------------------

-spec pattern2_fill(FillPattern :: string(),
                    Parameters :: list(string()),
                    ParametersSelected :: list(pos_integer()),
                    ParametersStrictMatching :: boolean()) ->
    {ok, string()} |
    {error,
     parameters_ignored | parameter_missing | parameters_selected_empty |
     {parameters_selected_ignored, list(pos_integer())} |
     {parameters_selected_missing, pos_integer()}}.

pattern2_fill(FillPattern, Parameters, [], ParametersStrictMatching) ->
    pattern2_fill_insert(FillPattern, Parameters, ParametersStrictMatching);
pattern2_fill(FillPattern, Parameters, ParametersSelected,
              ParametersStrictMatching) ->
    pattern2_fill_select(FillPattern, Parameters, ParametersSelected,
                         ParametersStrictMatching).

pattern2_fill_strip([], NameOut) ->
    {ok, lists:reverse(NameOut)};
pattern2_fill_strip([C | FillPatternIn], NameOut)
    when C == $*; C == $? ->
    pattern2_fill_strip(FillPatternIn, NameOut);
pattern2_fill_strip([C | FillPatternIn], NameOut) ->
    pattern2_fill_strip(FillPatternIn, [C | NameOut]).

pattern2_fill_insert([], NameOut,
                     [], _) ->
    {ok, lists:reverse(NameOut)};
pattern2_fill_insert([], NameOut,
                     [_ | _], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_ignored};
        true ->
            {ok, lists:reverse(NameOut)}
    end;
pattern2_fill_insert([C | FillPatternIn], NameOut,
                     [], ParametersStrictMatching)
    when C == $*; C == $? ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameter_missing};
        true ->
            pattern2_fill_strip(FillPatternIn, NameOut)
    end;
pattern2_fill_insert([C | FillPatternIn], NameOut,
                     [Parameter | Parameters], ParametersStrictMatching)
    when C == $*; C == $? ->
    pattern2_fill_insert(FillPatternIn, lists:reverse(Parameter) ++ NameOut,
                         Parameters, ParametersStrictMatching);
pattern2_fill_insert([C | FillPatternIn], NameOut,
                     Parameters, ParametersStrictMatching) ->
    pattern2_fill_insert(FillPatternIn, [C | NameOut],
                         Parameters, ParametersStrictMatching).

pattern2_fill_insert(FillPatternIn, Parameters, ParametersStrictMatching) ->
    pattern2_fill_insert(FillPatternIn, [],
                         Parameters, ParametersStrictMatching).

pattern2_fill_select([], NameOut, _,
                     ParametersSelected, ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true, ParametersSelected /= [] ->
            {error, {parameters_selected_ignored, ParametersSelected}};
        true ->
            {ok, lists:reverse(NameOut)}
    end;
pattern2_fill_select([C | FillPatternIn], NameOut, _,
                     [], ParametersStrictMatching)
    when C == $*; C == $? ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_selected_empty};
        true ->
            pattern2_fill_strip(FillPatternIn, NameOut)
    end;
pattern2_fill_select([C | FillPatternIn], NameOut, Parameters,
                     [I | ParametersSelected],
                     ParametersStrictMatching)
    when C == $*; C == $? ->
    try lists:nth(I, Parameters) of
        Parameter ->
            pattern2_fill_select(FillPatternIn,
                                 lists:reverse(Parameter) ++ NameOut,
                                 Parameters, ParametersSelected,
                                 ParametersStrictMatching)
    catch
        error:_ ->
            if
                ParametersStrictMatching =:= true ->
                    {error, {parameters_selected_missing, I}};
                true ->
                    pattern2_fill_strip(FillPatternIn, NameOut)
            end
    end;
pattern2_fill_select([C | FillPatternIn], NameOut, Parameters,
                     ParametersSelected, ParametersStrictMatching) ->
    pattern2_fill_select(FillPatternIn, [C | NameOut], Parameters,
                         ParametersSelected, ParametersStrictMatching).

pattern2_fill_select(FillPatternIn, Parameters,
                     ParametersSelected, ParametersStrictMatching) ->
    pattern2_fill_select(FillPatternIn, [], Parameters,
                         ParametersSelected, ParametersStrictMatching).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string based on the supplied wildcard pattern (using 2 wildcard characters).===
%% "*" and "?" are wildcard characters (equivalent to the ".+" regex).
%% "**", "??", "*?" and "?*" are forbidden.  "?" must not be the last
%% character in the pattern.
%% @end
%%-------------------------------------------------------------------------

-spec pattern2_parse(Pattern :: string(),
                    L :: string()) -> list(string()) | 'error'.

pattern2_parse(Pattern, L) ->
    pattern2_parse(Pattern, L, default).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string based on the supplied wildcard pattern (using 2 wildcard characters).===
%% "*" and "?" are wildcard characters (equivalent to the ".+" regex).
%% "**", "??", "*?" and "?*" are forbidden.  "?" must not be the last
%% character in the pattern.
%% @end
%%-------------------------------------------------------------------------

-spec pattern2_parse(Pattern :: string(),
                    L :: string(),
                    Option :: default | with_suffix | expanded) ->
    list(string()) |                       % default
    {list(string()), string()} |           % with_suffix
    list(string() | {exact, string()}) |   % expanded
    'error'.

pattern2_parse(Pattern, L, Option)
    when (Option =:= default) orelse
         (Option =:= with_suffix) orelse
         (Option =:= expanded) ->
    pattern2_parse(Pattern, L, [], [], Option).

pattern2_parse_result(default, Parameters, _) ->
    lists:reverse(Parameters);

pattern2_parse_result(with_suffix, Parameters, Suffix) ->
    {lists:reverse(Parameters), lists:reverse(Suffix)};

pattern2_parse_result(expanded, Parameters, Suffix) ->
    NewParameters = if
        Suffix /= [] ->
            [{exact, lists:reverse(Suffix)} | Parameters];
        true ->
            Parameters
    end,
    lists:reverse(NewParameters).

pattern2_parse_element(_, [], _) ->
    error;

pattern2_parse_element(C, [C | T], Segment) ->
    {ok, T, lists:reverse(Segment)};

pattern2_parse_element(_, [H | _], _)
    when H == $*; H == $? ->
    erlang:exit(badarg);

pattern2_parse_element(C, [H | T], L) ->
    pattern2_parse_element(C, T, [H | L]).

pattern2_parse_pattern0(Pattern, C, L, Segment, Parsed, Option) ->
    case pattern2_parse_element(C, L, Segment) of
        {ok, NewL, NewSegment} ->
            pattern2_parse(Pattern, NewL,
                           [NewSegment | Parsed], [C], Option);
        error ->
            error
    end.

pattern2_parse_pattern1(Pattern, C, L, Segment, Parsed, Option) ->
    case pattern2_parse_element(C, L, Segment) of
        {ok, NewL, NewSegment} ->
            case pattern2_parse(Pattern, NewL,
                               [NewSegment | Parsed], [C], Option) of
                error ->
                    pattern2_parse_pattern1(Pattern, C, NewL,
                                            [C | lists:reverse(NewSegment)],
                                            Parsed, Option);
                Success ->
                    Success
            end;
        error ->
            error
    end.

pattern2_parse([], [], Parsed, Suffix, Option) ->
    pattern2_parse_result(Option, Parsed, Suffix);

pattern2_parse([], [_ | _], _, _, _) ->
    error;

pattern2_parse([_ | _], [H | _], _, _, _)
    when H == $*; H == $? ->
    erlang:exit(badarg);

pattern2_parse([$*], [_ | _] = L, Parsed, Suffix, Option) ->
    NewParsed = if
        Option =:= expanded, Suffix /= [] ->
            [{exact, lists:reverse(Suffix)} | Parsed];
        true ->
            Parsed
    end,
    pattern2_parse_result(Option, [L | NewParsed], []);

pattern2_parse([$?], _, _, _, _) ->
    erlang:exit(badarg);

pattern2_parse([C0, C1 | _], [_ | _], _, _, _)
    when C0 == $* orelse C0 == $?,
         C1 == $* orelse C1 == $? ->
    erlang:exit(badarg);

pattern2_parse([$?, C | Pattern], [H | T], Parsed, Suffix, Option) ->
    if
        C == H ->
            error;
        true ->
            NewParsed = if
                Option =:= expanded, Suffix /= [] ->
                    [{exact, lists:reverse(Suffix)} | Parsed];
                true ->
                    Parsed
            end,
            pattern2_parse_pattern0(Pattern, C, T, [H], NewParsed, Option)
    end;

pattern2_parse([$*, C | Pattern], [H | T], Parsed, Suffix, Option) ->
    NewParsed = if
        Option =:= expanded, Suffix /= [] ->
            [{exact, lists:reverse(Suffix)} | Parsed];
        true ->
            Parsed
    end,
    pattern2_parse_pattern1(Pattern, C, T, [H], NewParsed, Option);

pattern2_parse([C | Pattern], [C | L], Parsed, Suffix, Option) ->
    pattern2_parse(Pattern, L, Parsed, [C | Suffix], Option);

pattern2_parse(_, _, _, _, _) ->
    error.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string based on the supplied wildcard pattern (using 2 wildcard characters) to return only the suffix after the pattern.===
%% "*" and "?" are wildcard characters (equivalent to the ".+" regex).
%% "**", "??", "*?" and "?*" are forbidden.  "?" must not be the last
%% character in the pattern.
%% @end
%%-------------------------------------------------------------------------

-spec pattern2_suffix(Pattern :: string(),
                      L :: string()) ->
    string() | 'error'.

pattern2_suffix([], []) ->
    [];

pattern2_suffix([], [_ | _] = L) ->
    L;

pattern2_suffix([_ | _], [H | _])
    when H == $*; H == $? ->
    erlang:exit(badarg);

pattern2_suffix([$*], [_ | _]) ->
    [];

pattern2_suffix([$?], _) ->
    erlang:exit(badarg);

pattern2_suffix([C0, C1 | _], [_ | _])
    when C0 == $* orelse C0 == $?,
         C1 == $* orelse C1 == $? ->
    erlang:exit(badarg);

pattern2_suffix([$?, C | Pattern], [H | T]) ->
    if
        C == H ->
            error;
        true ->
            pattern2_suffix_pattern0(Pattern, C, T)
    end;

pattern2_suffix([$*, C | Pattern], [_ | T]) ->
    pattern2_suffix_pattern1(Pattern, C, T);

pattern2_suffix([C | Pattern], [C | L]) ->
    pattern2_suffix(Pattern, L);

pattern2_suffix(_, _) ->
    error.

pattern2_suffix_element(_, []) ->
    error;

pattern2_suffix_element(C, [C | T]) ->
    {ok, T};

pattern2_suffix_element(_, [H | _])
    when H == $*; H == $? ->
    erlang:exit(badarg);

pattern2_suffix_element(C, [_ | T]) ->
    pattern2_suffix_element(C, T).

pattern2_suffix_pattern0(Pattern, C, L) ->
    case pattern2_suffix_element(C, L) of
        {ok, NewL} ->
            pattern2_suffix(Pattern, NewL);
        error ->
            error
    end.

pattern2_suffix_pattern1(Pattern, C, L) ->
    case pattern2_suffix_element(C, L) of
        {ok, NewL} ->
            case pattern2_suffix(Pattern, NewL) of
                error ->
                    pattern2_suffix_pattern1(Pattern, C, NewL);
                Success ->
                    Success
            end;
        error ->
            error
    end.

%%-------------------------------------------------------------------------
%% @private
%% @doc
%% ===Regression test.===
%% @end
%%-------------------------------------------------------------------------

test() ->
    {97,97,{{[],empty}}} = trie:new(["a"]),
    {97,97,{{"b",empty}}} = trie:new(["ab"]),
    {97,97,{{"bc",empty}}} = trie:new(["abc"]),
    {97,97,{{"b",empty}}} = trie:new(["ab"]),
    {97,97,{{{97,98,{{[],empty},{[],empty}}},error}}} =
        trie:new(["ab","aa"]),
    {97,97,{{{97,98,{{"c",empty},{"c",empty}}},error}}} =
        trie:new(["abc","aac"]),
    {97,97,{{{97,98,{{"c",2},{"c",1}}},error}}} =
        trie:new([{"abc", 1},{"aac", 2}]),
    {97,97,{{{97,98,{{"c",2},{"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
        RootNode0 = trie:new([{"abcdefghijklmnopqrstuvwxyz", 1},{"aac", 2}]),
    {ok, 1} = trie:find("abcdefghijklmnopqrstuvwxyz", RootNode0),
    error = trie:find("abcdefghijklmnopqrstuvwxy", RootNode0),
    {ok, 1} = trie:find_prefix("abcdefghijklmnopqrstuvwxyz", RootNode0),
    prefix = trie:find_prefix("abcdefghijklmnopqrstuvwxy", RootNode0),
    error = trie:find_prefix("abcdefghijklmnopqrstuvwxyzX", RootNode0),
    prefix = trie:find_prefix("a", RootNode0),
    prefix = trie:find_prefix("aa", RootNode0),
    {ok, 2} = trie:find_prefix("aac", RootNode0),
    error = trie:find_prefix("aacX", RootNode0),
    {97,97,{{{97,98,{{{98,99,{{"cde",3},{[],2}}},error},
     {"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
        RootNode1 = trie:store("aabcde", 3, RootNode0),
    {97,97,{{{97,98,{{{98,99,{{"cde",13},{[],12}}},error},
     {"cdefghijklmnopqrstuvwxyz",11}}},error}}} =
        map(fun(_, V) -> V + 10 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{[],error},{[],error}}},error},
     {"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
        filter(fun(_, V) -> V =< 1 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{[],error},{[],2}}},error},
     {"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
        filter(fun(_, V) -> V =< 2 end, RootNode1),
    ["aabcde", "aac", "abcdefghijklmnopqrstuvwxyz"] =
        trie:fetch_keys(RootNode1),
    [{"aabcde", 3}, {"aac", 2}, {"abcdefghijklmnopqrstuvwxyz", 1}] =
        trie:to_list(RootNode1),
    [{"aabcde", 3}, {"aac", 12}, {"abcdefghijklmnopqrstuvwxyz", 1}] =
        trie:to_list(trie:update("aac", fun(I) -> I + 10 end, RootNode1)),
    [{"aaa", 4}, {"aabcde", 3}, {"aac", 2}, {"abcdefghijklmnopqrstuvwxyz", 1}] =
        trie:to_list(trie:update("aaa", fun(I) -> I + 10 end, 4, RootNode1)),
    6 = foldl(fun(_, I, A) -> I + A end, 0, RootNode1),
    [{"aabcde", 3},{"aac", 2},{"abcdefghijklmnopqrstuvwxyz", 1}] =
        foldr(fun(K, V, A) -> [{K,V} | A] end, [], RootNode1),
    [{"abcdefghijklmnopqrstuvwxyz", 1}, {"aac", 2}, {"aabcde", 3}] =
        foldl(fun(K, V, A) -> [{K,V} | A] end, [], RootNode1),
    error = trie:find("aabcde", RootNode0),
    {ok, 3} = trie:find("aabcde", RootNode1),
    RootNode2 = trie:erase("aac", RootNode0),
    {ok, 1} = trie:find("abcdefghijklmnopqrstuvwxyz", RootNode2),
    {97,98,{{{98,98,{{[],[2]}}},[1]},{"c",[3]}}} =
        RootNode3 = trie:new([{"a", [1]},{"ab", [2]},{"bc", [3]}]),
    {97,98,{{{98,98,{{[],[2]}}},[1,2]},{"c",[3]}}} =
        trie:append("a", 2, RootNode3),

    RootNode4 = trie:new([
        {"ammmmmmm",      7},
        {"aaaaaaaaaaa",   4},
        {"aaa",           2},
        {"ab",            0},
        {"ab",            5},
        {"aa",            1},
        {"aba",           6},
        {"aaaaaaaa",      3}]),
    {97,97,
     {{{97,109,
        {{{97,97,
           {{{97,97,
              {{{97,97,
                 {{{97,97,
                    {{{97,97,
                       {{{97,97,
                          {{{97,97,
                             {{"aa",4}}},
                            3}}},
                         error}}},
                     error}}},
                   error}}},
                error}}},
             2}}},
          1},
         {{97,97,{{[],6}}},5},
         {[],error},
         {[],error},
         {[],error},
         {[],error},
         {[],error},
         {[],error},
         {[],error},
         {[],error},
         {[],error},
         {[],error},
         {"mmmmmm",7}}},
       error}}} = RootNode4,
    [{"aa",1},
     {"aaa",2},
     {"aaaaaaaa",3},
     {"aaaaaaaaaaa",4},
     {"ab",5},
     {"aba",6},
     {"ammmmmmm",7}] = trie:to_list(trie:from_list(trie:to_list(RootNode4))),
    Liter =  ["aa", "aaa", "aaaaaaaa", "aaaaaaaaaaa", "ab", "aba"],
    Fiter = fun(Key, _, Iter) ->
        case lists:member(Key, Liter) of
            true ->
                Iter();
            false ->
                done
        end
    end,
    Fitera = fun
        (_, _, [], _) ->
            done;
        (Key, _, [Key | T], Iter) ->
            Iter(T)
    end,
    ok = trie:iter(Fiter, RootNode4),
    done = trie:itera(Fitera, Liter, RootNode4),
    % trie:map happens to go through in reverse order
    ["aa",
     "aaa",
     "aaaaaaaa",
     "aaaaaaaaaaa",
     "ab",
     "aba",
     "ammmmmmm"] = trie:fetch_keys(RootNode4),
    ["aa",
     "aaa",
     "aaaaaaaa",
     "aaaaaaaaaaa",
     "ab",
     "aba",
     "ammmmmmm"] = trie:foldr(fun(Key, _, L) -> [Key | L] end, [], RootNode4),
    ["ammmmmmm",
     "aba",
     "ab",
     "aaaaaaaaaaa",
     "aaaaaaaa",
     "aaa",
     "aa"] = trie:foldl(fun(Key, _, L) -> [Key | L] end, [], RootNode4),
    RootNode5 = trie:store("a", 0, trie:store("aaaa", 2.5, RootNode4)),
    {ok, 2.5} = trie:find("aaaa", RootNode5),
    error = trie:find("aaaa", RootNode4),
    {ok, 2.5} = trie:find_prefix("aaaa", RootNode5),
    prefix = trie:find_prefix("aaaa", RootNode4),
    [] = trie:find_prefixes("z", RootNode4),
    [{"aa", 1}] = trie:find_prefixes("aa", RootNode4),
    [{"aa", 1},
     {"aaa",2}] = trie:find_prefixes("aaaa", RootNode4),
    [{"ab",5}] = trie:find_prefixes("absolut", RootNode4),
    [{"ab",5},
     {"aba", 6}] = trie:find_prefixes("aba", RootNode4),
    [] = trie:find_prefixes("bar", RootNode4),
    [{"aa",1},
     {"aaa",2},
     {"aaaaaaaa",3},
     {"aaaaaaaaaaa",4}
     ] = trie:find_prefixes("aaaaaaaaaaaaaaaaaaaaaddddddaa", RootNode4),
    error = trie:find_prefix_longest("a", RootNode4),
    {ok, "aa", 1} = trie:find_prefix_longest("aa", RootNode4),
    {ok, "aaa", 2} = trie:find_prefix_longest("aaaa", RootNode4),
    {ok, "ab", 5} = trie:find_prefix_longest("absolut", RootNode4),
    {ok, "aba", 6} = trie:find_prefix_longest("aba", RootNode4),
    {ok, "aaaaaaaa", 3} = trie:find_prefix_longest("aaaaaaaaa", RootNode4),
    error = trie:find_prefix_longest("bar", RootNode4),
    {ok,
     "aaaaaaaaaaa",
     4} = trie:find_prefix_longest("aaaaaaaaaaaaaaaaaaaaaddddddaa", RootNode4),
    2.5 = trie:fetch("aaaa", RootNode5),
    {'EXIT', {if_clause, _}} = (catch trie:fetch("aaaa", RootNode4)),
    RootNode4 = trie:erase("a", trie:erase("aaaa", RootNode5)),
    {2.5, RootNodePre4} = trie:take("aaaa", RootNode5),
    {0, RootNode4} = trie:take("a", RootNodePre4),
    error = trie:take("a", RootNode4),
    true = trie:is_key("aaaa", RootNode5),
    false = trie:is_key("aaaa", RootNode4),
    ["aa",
     "aaa",
     "aaaaaaaa",
     "aaaaaaaaaaa"] = trie:fetch_keys_similar("aa", RootNode4),
    ["aaa",
     "aaaaaaaa",
     "aaaaaaaaaaa"] = trie:fetch_keys_similar("aaac", RootNode4),
    ["ab",
     "aba"] = trie:fetch_keys_similar("abba", RootNode4),
    ["aa",
     "aaa",
     "aaaaaaaa",
     "aaaaaaaaaaa",
     "ab",
     "aba",
     "ammmmmmm"] = trie:fetch_keys_similar("a", RootNode4),
    [] = trie:fetch_keys_similar("b", RootNode4),
    {ok, "aa", 1} = trie:find_similar("aa", RootNode4),
    {ok, "aaa", 2} = trie:find_similar("aaac", RootNode4),
    {ok, "aaaaaaaa", 3} = trie:find_similar("aaaa", RootNode4),
    {ok, "ab", 5} = trie:find_similar("abba", RootNode4),
    {ok, "aa", 1} = trie:find_similar("a", RootNode4),
    true = trie:is_prefixed("abacus", RootNode4),
    false = trie:is_prefixed("ac", RootNode4),
    false = trie:is_prefixed("abacus", "ab", RootNode4),
    true = trie:foldl(fun(K, _, L) -> [K | L] end, [], RootNode4) ==
           trie:fold_match("*", fun(K, _, L) -> [K | L] end, [], RootNode4),
    ["aaa"
     ] = trie:fold_match("*aa", fun(K, _, L) -> [K | L] end, [], RootNode4),
    ["aaaaaaaaaaa",
     "aaaaaaaa",
     "aaa"
     ] = trie:fold_match("aa*", fun(K, _, L) -> [K | L] end, [], RootNode4),
    ["aba"
     ] = trie:fold_match("ab*", fun(K, _, L) -> [K | L] end, [], RootNode4),
    ["ammmmmmm"
     ] = trie:fold_match("am*", fun(K, _, L) -> [K | L] end, [], RootNode4),
    ["aba",
     "aaa"
     ] = trie:fold_match("a*a", fun(K, _, L) -> [K | L] end, [], RootNode4),
    {'EXIT', badarg} = (catch trie:fold_match("a**a",
                                              fun(K, _, L) -> [K | L] end,
                                              [], RootNode4)),
    RootNode6 = trie:new([
        {"*",      1},
        {"aa*",    2},
        {"aa*b",   3},
        {"aa*a*",  4},
        {"aaaaa",  5}]),
    {ok,"aa*",2} = trie:find_match("aaaa", RootNode6),
    {ok,"aaaaa",5} = trie:find_match("aaaaa", RootNode6),
    {ok,"*",1} = trie:find_match("aa", RootNode6),
    {ok,"aa*",2} = trie:find_match("aab", RootNode6),
    {ok,"aa*b",3} = trie:find_match("aabb", RootNode6),
    {ok,"aa*a*",4} = trie:find_match("aabab", RootNode6),
    {ok,"aa*a*",4} = trie:find_match("aababb", RootNode6),
    {ok,"aa*a*",4} = trie:find_match("aabbab", RootNode6),
    {ok,"aa*a*",4} = trie:find_match("aabbabb", RootNode6),
    {ok,"aa*",2} = trie:find_match2("aaaa", RootNode6),
    {ok,"aaaaa",5} = trie:find_match2("aaaaa", RootNode6),
    {ok,"*",1} = trie:find_match2("aa", RootNode6),
    {ok,"aa*",2} = trie:find_match2("aab", RootNode6),
    {ok,"aa*b",3} = trie:find_match2("aabb", RootNode6),
    {ok,"aa*a*",4} = trie:find_match2("aabab", RootNode6),
    {ok,"aa*a*",4} = trie:find_match2("aababb", RootNode6),
    {ok,"aa*a*",4} = trie:find_match2("aabbab", RootNode6),
    {ok,"aa*a*",4} = trie:find_match2("aabbabb", RootNode6),
    {'EXIT',badarg} = (catch trie:find_match("aa*", RootNode6)),
    {'EXIT',badarg} = (catch trie:find_match("aaaa*", RootNode6)),
    {'EXIT',badarg} = (catch trie:find_match("aaaaa*", RootNode6)),
    {'EXIT',badarg} = (catch trie:find_match2("aa*", RootNode6)),
    {'EXIT',badarg} = (catch trie:find_match2("aaaa*", RootNode6)),
    {'EXIT',badarg} = (catch trie:find_match2("aaaaa*", RootNode6)),
    ["aa"] = trie:pattern_parse("aa*", "aaaa"),
    ["b"] = trie:pattern_parse("aa*", "aab"),
    ["b"] = trie:pattern_parse("aa*b", "aabb"),
    {["b"], "b"} = trie:pattern_parse("aa*b", "aabb", with_suffix),
    ["b", "b"] = trie:pattern_parse("aa*a*", "aabab"),
    {["b", "b"], ""} = trie:pattern_parse("aa*a*", "aabab", with_suffix),
    ["b", "bb"] = trie:pattern_parse("aa*a*", "aababb"),
    ["bb", "b"] = trie:pattern_parse("aa*a*", "aabbab"),
    ["bb", "bb"] = trie:pattern_parse("aa*a*", "aabbabb"),
    error = trie:pattern_parse("aa*a*", "aaabb"),
    ["file.name"] = trie:pattern_parse("*.txt", "file.name.txt"),
    ["//"] = trie:pattern_parse("*/", "///"),
    "/get" = trie:pattern_suffix("*.txt", "file.name.txt/get"),
    "/get" = trie:pattern_suffix("*/", "///get"),
    ["aa"] = trie:pattern2_parse("aa*", "aaaa"),
    ["b"] = trie:pattern2_parse("aa*", "aab"),
    ["b"] = trie:pattern2_parse("aa*b", "aabb"),
    {["b"], "b"} = trie:pattern2_parse("aa*b", "aabb", with_suffix),
    ["b", "b"] = trie:pattern2_parse("aa*a*", "aabab"),
    {["b", "b"], ""} = trie:pattern2_parse("aa*a*", "aabab", with_suffix),
    ["b", "bb"] = trie:pattern2_parse("aa*a*", "aababb"),
    ["b", "bb"] = trie:pattern2_parse("aa?a*", "aababb"),
    ["bb", "b"] = trie:pattern2_parse("aa*a*", "aabbab"),
    ["bb", "b"] = trie:pattern2_parse("aa?a*", "aabbab"),
    ["bb", "bb"] = trie:pattern2_parse("aa*a*", "aabbabb"),
    ["bb", "bb"] = trie:pattern2_parse("aa?a*", "aabbabb"),
    error = trie:pattern2_parse("aa*a*", "aaabb"),
    ["file.name"] = trie:pattern2_parse("*.txt", "file.name.txt"),
    ["//"] = trie:pattern2_parse("*/", "///"),
    "/get" = trie:pattern2_suffix("*.txt", "file.name.txt/get"),
    error = trie:pattern2_suffix("?.txt", "file.name.txt/get"),
    "/get" = trie:pattern2_suffix("?.txt", "file_name.txt/get"),
    "/get" = trie:pattern2_suffix("*/", "///get"),
    error = trie:pattern2_suffix("?/", "///get"),
    {ok, "/accounting/balances/fred",
     empty} = trie:find_match("/accounting/balances/fred",
                              trie:new(["/accounting/balances/*",
                                        "/accounting/balances/fred"])),
    {ok, "/permissions/fred/accounts/*",
     empty} = trie:find_match("/permissions/fred/accounts/add",
                              trie:new(["/permissions/*/accounts/*",
                                        "/permissions/fred/accounts/*",
                                        "/permissions/*/accounts/add",
                                        "/permissions/fred/accounts/remove"])),
    {ok,"*//get",empty} = trie:find_match("///get", trie:new(["*//get"])),
    {ok,"*/",empty} = trie:find_match("///", trie:new(["*/"])),
    [] = trie:pattern_parse("aaabb", "aaabb"),
    {[], "aaabb"} = trie:pattern_parse("aaabb", "aaabb", with_suffix),
    [{exact, "a"},
     "ddi",
     {exact, "t"},
     "io",
     {exact, "n"}] = trie:pattern_parse("a*t*n", "addition", expanded),
    ["w",{exact,"atch"}] = trie:pattern_parse("*atch", "watch", expanded),
    [{exact,"is"},"t"] = trie:pattern_parse("is*", "ist", expanded),
    [] = trie:pattern2_parse("aaabb", "aaabb"),
    {[], "aaabb"} = trie:pattern2_parse("aaabb", "aaabb", with_suffix),
    [{exact, "a"},
     "ddi",
     {exact, "t"},
     "io",
     {exact, "n"}] = trie:pattern2_parse("a*t*n", "addition", expanded),
    [{exact, "a"},
     "ddi",
     {exact, "t"},
     "io",
     {exact, "n"}] = trie:pattern2_parse("a?t?n", "addition", expanded),
    ["w",{exact,"atch"}] = trie:pattern2_parse("*atch", "watch", expanded),
    ["w",{exact,"atch"}] = trie:pattern2_parse("?atch", "watch", expanded),
    error = trie:pattern2_parse("?atch", "aatch", expanded),
    [{exact,"is"},"t"] = trie:pattern2_parse("is*", "ist", expanded),
    false = trie:is_pattern("abcdef"),
    false = trie:is_pattern("abcde?f"),
    true = trie:is_pattern("abc*d*ef"),
    true = trie:is_pattern_bytes("abc*d*ef"),
    {'EXIT',badarg} = (catch trie:is_pattern("abc**ef")),
    false = trie:is_pattern2("abcdef"),
    true = trie:is_pattern2("abc?def"),
    true = trie:is_pattern2("abc?d*ef"),
    true = trie:is_pattern2("abcd*ef"),
    true = trie:is_pattern2_bytes("abcd*ef"),
    {'EXIT',badarg} = (catch trie:is_pattern2("abc**ef")),
    {'EXIT',badarg} = (catch trie:is_pattern2("abc??ef")),
    {'EXIT',badarg} = (catch trie:is_pattern2("abc?*ef")),
    {'EXIT',badarg} = (catch trie:is_pattern2("abc*?ef")),
    {'EXIT',badarg} = (catch trie:is_pattern2("abcef?")),
    RootNode7 = trie:from_list([{"00", zeros}, {"11", ones}]),
    RootNode8 = trie:from_list([{"0", zero}, {"1", one}]),
    ["00"] = trie:fetch_keys_similar("02", RootNode7),
    [] = trie:fetch_keys_similar("2", RootNode7),
    ["11"] = trie:fetch_keys_similar("1", RootNode7),
    ["1"] = trie:fetch_keys_similar("1", RootNode8),
    ["0"] = trie:fetch_keys_similar("0", RootNode8),
    ["00"] = trie:fetch_keys_similar("0", RootNode7),
    RootNode9 = trie:new([{"abc", 123}]),
    {97,97,{{"bc",456}}} = trie:store("abc", 456, RootNode9),
    RootNode10 = trie:store("abc", value, trie:new()),
    RootNode11 = trie:store("abcd", value, RootNode10),
    true = trie:is_prefixed("abcdefghijk", RootNode10),
    true = trie:is_prefixed("abcdefghijk", RootNode11),
    true = trie:is_prefix("a", RootNode10),
    true = trie:is_prefix("a", RootNode11),
    true = trie:is_prefix("ab", RootNode10),
    true = trie:is_prefix("ab", RootNode11),
    true = trie:is_prefixed("abcdefghijk", "", RootNode10),
    true = trie:is_prefixed("abcdefghijk", "", RootNode11),
    false = trie:is_prefixed("abcdefghijk", "abc", RootNode10),
    true = trie:is_prefixed("abcdefghijk", "abc", RootNode11),
    true = trie:is_prefixed("abcdefghijk", "ac", RootNode10),
    true = trie:is_prefixed("abcdefghijk", "bc", RootNode10),
    true = trie:is_prefixed("abcdefghijk", "ab", RootNode10),
    RootNode12 = trie:new([
        {"*",      1},
        {"/?a",    2},
        {"/?/a",   3},
        {"/?/b" ,  4},
        {"/*/a",   5}]),
    {ok,"*",1} = trie:find_match2("/alba", RootNode12),
    {ok,"*",1} = trie:find_match2("/aa", RootNode12),
    {ok,"*",1} = trie:find_match2("/a", RootNode12),
    {ok,"/?a",2} = trie:find_match2("/ba", RootNode12),
    {ok,"/?a",2} = trie:find_match2("/bcdefghijklmno___a", RootNode12),
    {ok,"/?/a",3} = trie:find_match2("/aaaaaaaa/a", RootNode12),
    {ok,"/?/b",4} = trie:find_match2("/a/b", RootNode12),
    {ok,"/?/b",4} = trie:find_match2("/aa/b", RootNode12),
    {ok,"/?/b",4} = trie:find_match2("/ab/b", RootNode12),
    {'EXIT',badarg} = (catch trie:find_match2("/?a", RootNode12)),
    {'EXIT',badarg} = (catch trie:find_match2("/?b", RootNode12)),
    {'EXIT',badarg} = (catch trie:find_match2("/?a", RootNode12)),
    {ok,"/?/a",3} = trie:find_match2("/alba-white/a", RootNode12),
    {ok,"/*/a",5} = trie:find_match2("/alba/white/a", RootNode12),
    RootNode13 = trie:new([
        {"/?a",    1},
        {"/?bbb",  2},
        {"/?c",    3}]),
    {ok,"/?bbb",2} = trie:find_match2("/abbb", RootNode13),
    error = trie:find_match2("/abba", RootNode13),
    {ok,"/?c",3} = trie:find_match2("/abbc", RootNode13),
    RootNode14 = trie:new([
        {"*bc",   1},
        {"*bd",   2}]),
    error = trie:find_match("bb", RootNode14),
    error = trie:find_match2("bb", RootNode14),
    FillPattern0 = "/**/*",
    Parameters0 = ["a/", "b", "c"],
    Parameters1 = ["ignore1", "a/", "ignore2", "b", "c"],
    FillError0 = parameters_selected_empty,
    FillError1 = parameters_selected_ignored,
    FillError2 = parameters_selected_missing,
    {ok, "/a/b/c"} = trie:pattern_fill(FillPattern0, Parameters0),
    {ok, "/a/b/c"} = trie:pattern_fill(FillPattern0, Parameters1,
                                       [2, 4, 5], false),
    {ok, "/a/b/"} = trie:pattern_fill(FillPattern0, Parameters1,
                                       [2, 4], false),
    {ok, "/a/b/c"} = trie:pattern_fill(FillPattern0, Parameters1,
                                       [2, 4, 5], true),
    {error, FillError0} = trie:pattern_fill(FillPattern0, Parameters1,
                                            [2, 4], true),
    {error, {FillError1, [1]}} = trie:pattern_fill(FillPattern0, Parameters1,
                                                   [2, 4, 5, 1], true),
    {error, {FillError2, 6}} = trie:pattern_fill(FillPattern0, Parameters1,
                                                 [2, 4, 6], true),
    FillPattern1 = "/??/?",
    {ok, "/a/b/c"} = trie:pattern2_fill(FillPattern1, Parameters0),
    {ok, "/a/b/c"} = trie:pattern2_fill(FillPattern1, Parameters1,
                                        [2, 4, 5], false),
    {ok, "/a/b/"} = trie:pattern2_fill(FillPattern1, Parameters1,
                                        [2, 4], false),
    {ok, "/a/b/c"} = trie:pattern2_fill(FillPattern1, Parameters1,
                                        [2, 4, 5], true),
    {error, FillError0} = trie:pattern2_fill(FillPattern1, Parameters1,
                                             [2, 4], true),
    {error, {FillError1, [1]}} = trie:pattern2_fill(FillPattern1, Parameters1,
                                                    [2, 4, 5, 1], true),
    {error, {FillError2, 6}} = trie:pattern2_fill(FillPattern1, Parameters1,
                                                  [2, 4, 6], true),
    error = trie:pattern2_parse("?/","//"),
    error = trie:find_match2("//", trie:new(["?/"])),
    error = trie:find_match2("//", trie:new(["?a","?/"])),
    error = trie:find_match2("//b", trie:new(["?a","?/b"])),
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

wildcard_match_lists_element(_, []) ->
    error;

wildcard_match_lists_element(_, [$* | _]) ->
    erlang:exit(badarg);

wildcard_match_lists_element(C, [C | L]) ->
    {ok, L};

wildcard_match_lists_element(C, [_ | L]) ->
    wildcard_match_lists_element(C, L).

wildcard_match_lists_valid([], Result) ->
    Result;

wildcard_match_lists_valid([$* | _], _) ->
    erlang:exit(badarg);

wildcard_match_lists_valid([_ | L], Result) ->
    wildcard_match_lists_valid(L, Result).

wildcard_match_lists_pattern(Pattern, C, L) ->
    case wildcard_match_lists_element(C, L) of
        {ok, NewL} ->
            case wildcard_match_lists(Pattern, NewL) of
                true ->
                    true;
                false ->
                    wildcard_match_lists_pattern(Pattern, C, NewL)
            end;
        error ->
            wildcard_match_lists_valid(L, false)
    end.

wildcard_match_lists([], []) ->
    true;

wildcard_match_lists([], [_ | _] = L) ->
    wildcard_match_lists_valid(L, false);

wildcard_match_lists([_ | _], [$* | _]) ->
    erlang:exit(badarg);

wildcard_match_lists([$*], [_ | L]) ->
    wildcard_match_lists_valid(L, true);

wildcard_match_lists([$*, C | Pattern], [_ | L]) ->
    true = C =/= $*,
    wildcard_match_lists_pattern(Pattern, C, L);

wildcard_match_lists([C | Pattern], [C | L]) ->
    wildcard_match_lists(Pattern, L);

wildcard_match_lists(_, L) ->
    wildcard_match_lists_valid(L, false).

wildcard_match2_lists_element(_, []) ->
    error;

wildcard_match2_lists_element(_, [C | _])
    when C == $*; C == $? ->
    erlang:exit(badarg);

wildcard_match2_lists_element(C, [C | L]) ->
    {ok, L};

wildcard_match2_lists_element(C, [_ | L]) ->
    wildcard_match2_lists_element(C, L).

wildcard_match2_lists_valid([], Result) ->
    Result;

wildcard_match2_lists_valid([C | _], _)
    when C == $*; C == $? ->
    erlang:exit(badarg);

wildcard_match2_lists_valid([_ | L], Result) ->
    wildcard_match2_lists_valid(L, Result).

wildcard_match2_lists_pattern0(Pattern, C, L) ->
    case wildcard_match2_lists_element(C, L) of
        {ok, NewL} ->
            wildcard_match2_lists(Pattern, NewL);
        error ->
            wildcard_match2_lists_valid(L, false)
    end.

wildcard_match2_lists_pattern1(Pattern, C, L) ->
    case wildcard_match2_lists_element(C, L) of
        {ok, NewL} ->
            case wildcard_match2_lists(Pattern, NewL) of
                true ->
                    true;
                false ->
                    wildcard_match2_lists_pattern1(Pattern, C, NewL)
            end;
        error ->
            wildcard_match2_lists_valid(L, false)
    end.

wildcard_match2_lists([], []) ->
    true;

wildcard_match2_lists([], [_ | _] = L) ->
    wildcard_match2_lists_valid(L, false);

wildcard_match2_lists([_ | _], [C | _])
    when C == $*; C == $? ->
    erlang:exit(badarg);

wildcard_match2_lists([$?], [_ | _]) ->
    erlang:exit(badarg);

wildcard_match2_lists([$?, C | Pattern], [H | L]) ->
    true = (C =/= $*) andalso (C =/= $?),
    if
        C == H ->
            false;
        true ->
            wildcard_match2_lists_pattern0(Pattern, C, L)
    end;

wildcard_match2_lists([$*], [_ | L]) ->
    wildcard_match2_lists_valid(L, true);

wildcard_match2_lists([$*, C | Pattern], [_ | L]) ->
    true = (C =/= $*) andalso (C =/= $?),
    wildcard_match2_lists_pattern1(Pattern, C, L);

wildcard_match2_lists([C | Pattern], [C | L]) ->
    wildcard_match2_lists(Pattern, L);

wildcard_match2_lists(_, L) ->
    wildcard_match2_lists_valid(L, false).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("trie_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"internal tests", ?_assertOk(test())}
    ]}.

long_test_() ->
    test_condition([
        {"proper tests", ?_assert(trie_proper:qc_run(?MODULE))}
    ], ?CLOUDI_LONG_TEST_TIMEOUT).

-endif.
