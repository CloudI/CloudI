%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% This file contains trie functions utilized by both the string
%%% (list of integers) trie implementation and the binary trie
%%% implementation.
%%%
%%% MIT License
%%%
%%% Copyright (c) 2010-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%%------------------------------------------------------------------------

-ifdef(MODE_LIST).
-define(TYPE_NAME, string).
-define(TYPE_EMPTY, []).
-define(TYPE_CHECK(V), is_list(V)).
-define(TYPE_H0T0, [H | T]).
-define(TYPE_H0_, [H | _]).
-define(TYPE_H0, [H]).
-define(TYPE_H1T1, [H1 | T1]).
-define(TYPE_BHBT, [BH | BT]).
-define(TYPE_KEYH0, Key ++ [H]).
-define(TYPE_KEYH0T0, Key ++ [H] ++ T).
-define(TYPE_KEYH0CHILDNODE, Key ++ [H] ++ ChildNode).
-define(TYPE_KEYCHAR, Key ++ [Character]).
-define(TYPE_KEYCHARNODE, Key ++ [Character] ++ Node).
-define(TYPE_NEWKEYNODE, NewKey ++ Node).
-define(TYPE_NEWKEY, [H | Key]).
-define(TYPE_NEWKEY_REVERSE(X), lists:reverse(X)).
-define(TYPE_NEWKEY_REVERSE(X, Y), lists:reverse(X, Y)).
-define(TYPE_PREFIX(X, Y), lists:prefix(X, Y)).
-else.
-ifdef(MODE_BINARY).
-define(TYPE_NAME, binary).
-define(TYPE_EMPTY, <<>>).
-define(TYPE_CHECK(V), is_binary(V)).
-define(TYPE_H0T0, <<H:8, T/binary>>).
-define(TYPE_H0_, <<H:8, _/binary>>).
-define(TYPE_H0, <<H:8>>).
-define(TYPE_H1T1, <<H1:8, T1/binary>>).
-define(TYPE_BHBT, <<BH:8, BT/binary>>).
-define(TYPE_KEYH0, <<Key/binary, H:8>>).
-define(TYPE_KEYH0T0, <<Key/binary, H:8, T/binary>>).
-define(TYPE_KEYH0CHILDNODE, <<Key/binary, H:8, ChildNode/binary>>).
-define(TYPE_KEYCHAR, <<Key/binary, Character:8>>).
-define(TYPE_KEYCHARNODE, <<Key/binary, Character:8, Node/binary>>).
-define(TYPE_NEWKEYNODE, <<NewKey/binary, Node/binary>>).
-define(TYPE_NEWKEY, <<Key/binary, H:8>>).
-define(TYPE_NEWKEY_REVERSE(X), X).
-define(TYPE_NEWKEY_REVERSE(X, Y), <<X/binary, Y/binary>>).
-define(TYPE_PREFIX(X, Y), binary_prefix(X, Y)).
-endif.
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type nonempty_trie() :: {integer(), integer(), tuple()}.
-type empty_trie() :: ?TYPE_EMPTY.
-type trie() :: nonempty_trie() | empty_trie().
-export_type([nonempty_trie/0,
              empty_trie/0,
              trie/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value as a list element in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec append(Key :: ?TYPE_NAME(),
             Value :: any(),
             Node :: trie()) -> nonempty_trie().

append(Key, Value, Node) ->
    ValueList = [Value],
    update(Key, fun(OldValue) -> OldValue ++ ValueList end, ValueList, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a list of values as a list element in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec append_list(Key :: ?TYPE_NAME(),
                  ValueList :: list(),
                  Node :: trie()) -> nonempty_trie().

append_list(Key, ValueList, Node) ->
    update(Key, fun(OldValue) -> OldValue ++ ValueList end, ValueList, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec erase(Key :: ?TYPE_NAME(),
            Node :: trie()) -> trie().

erase(_, ?TYPE_EMPTY = Node) ->
    Node;

erase(?TYPE_H0T0, Node) ->
    erase_node(H, T, Node).

erase_node(H, _, {I0, I1, _} = Node)
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    Node;

erase_node(H, T, {I0, I1, Data} = OldNode)
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        T == Node ->
            if
                Value =:= error ->
                    OldNode;
                true ->
                    {I0, I1, erlang:setelement(I, Data, {?TYPE_EMPTY, error})}
            end;
        T =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    OldNode;
                true ->
                    {I0, I1, erlang:setelement(I, Data, {Node, error})}
            end;
        ?TYPE_CHECK(Node) ->
            OldNode;
        is_tuple(Node) ->
            ?TYPE_H1T1 = T,
            {I0, I1, erlang:setelement(I, Data,
                {erase_node(H1, T1, Node), Value})}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase all entries within a trie that share a common prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec erase_similar(Similar :: ?TYPE_NAME(),
                    Node :: trie()) -> list(?TYPE_NAME()).

erase_similar(Similar, Node) ->
    fold_similar(Similar, fun(Key, _, N) -> erase(Key, N) end, Node, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch a value from a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch(?TYPE_NAME(),
            nonempty_trie()) -> any().

fetch(?TYPE_H0T0, {_, _, _} = Node) ->
    fetch_node(H, T, Node).

fetch_node(H, T, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case T of
        ?TYPE_EMPTY ->
            if
                is_tuple(Node); Node =:= ?TYPE_EMPTY ->
                    if
                        Value =/= error ->
                            Value
                    end
            end;
        ?TYPE_H1T1 ->
            case Node of
                {_, _, _} ->
                    fetch_node(H1, T1, Node);
                T when Value =/= error ->
                    Value
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch all the keys in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch_keys(Node :: trie()) -> list(?TYPE_NAME()).

fetch_keys(Node) ->
    foldr(fun(Key, _, L) -> [Key | L] end, [], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch the keys within a trie that share a common prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch_keys_similar(Similar :: ?TYPE_NAME(),
                         Node :: trie()) -> list(?TYPE_NAME()).

fetch_keys_similar(Similar, Node) ->
    foldr_similar(Similar, fun(Key, _, L) -> [Key | L] end, [], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a trie with a predicate function.===
%% @end
%%-------------------------------------------------------------------------

-spec filter(F :: fun((?TYPE_NAME(), any()) -> boolean()),
             Node :: trie()) -> trie().

filter(F, ?TYPE_EMPTY = Node) when is_function(F, 2) ->
    Node;

filter(F, Node) when is_function(F, 2) ->
    filter_node(F, ?TYPE_EMPTY, Node).

filter_node(F, Key, {I0, I1, Data}) ->
    {I0, I1, filter_element(F, I1 - I0 + 1, I0 - 1, Key, Data)};

filter_node(_, _, Node)
    when ?TYPE_CHECK(Node) ->
    Node.

filter_element(_, 0, _, _, Data) ->
    Data;

filter_element(F, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    filter_element(F, I - 1, Offset, Key, Data);
                true ->
                    case F(?TYPE_KEYCHAR, Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data,
                                    {?TYPE_EMPTY, error}))
                    end
            end;
        Value =:= error ->
            filter_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {filter_node(F, ?TYPE_KEYCHAR, Node), Value}));
        true ->
            NewKey = ?TYPE_KEYCHAR,
            if
                ?TYPE_CHECK(Node) ->
                    case F(?TYPE_NEWKEYNODE, Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data,
                                    {?TYPE_EMPTY, error}))
                    end;
                true ->
                    case F(NewKey, Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data,
                                    {filter_node(F, NewKey, Node), error}))
                    end
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec find(?TYPE_NAME(), trie()) -> {ok, any()} | 'error'.

find(_, ?TYPE_EMPTY) ->
    error;

find(?TYPE_H0T0, {_, _, _} = Node) ->
    find_node(H, T, Node).

find_node(H, _, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    error;

find_node(H, ?TYPE_EMPTY, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    error;
                true ->
                    {ok, Value}
            end;
        true ->
            error
    end;

find_node(H, T, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            ?TYPE_H1T1 = T,
            find_node(H1, T1, Node);
        T ->
            if
                Value =:= error ->
                    error;
                true ->
                    {ok, Value}
            end;
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in a trie by prefix.===
%% The atom 'prefix' is returned if the string supplied is a prefix
%% for a key that has previously been stored within the trie, but no
%% value was found, since there was no exact match for the string supplied.
%% @end
%%-------------------------------------------------------------------------

-spec find_prefix(?TYPE_NAME(), trie()) -> {ok, any()} | 'prefix' | 'error'.

find_prefix(?TYPE_H0_, {I0, I1, _})
    when H < I0; H > I1 ->
    error;

find_prefix(?TYPE_H0, {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {{_, _, _}, error} ->
            prefix;
        {{_, _, _}, Value} ->
            {ok, Value};
        {_, error} ->
            error;
        {?TYPE_EMPTY, Value} ->
            {ok, Value};
        {_, _} ->
            prefix
    end;

find_prefix(?TYPE_H0T0, {I0, _, Data})
    when is_integer(H) ->
    case erlang:element(H - I0 + 1, Data) of
        {{_, _, _} = Node, _} ->
            find_prefix(T, Node);
        {_, error} ->
            error;
        {T, Value} ->
            {ok, Value};
        {L, _} ->
            case ?TYPE_PREFIX(T, L) of
                true ->
                    prefix;
                false ->
                    error
            end
    end;

find_prefix(_, ?TYPE_EMPTY) ->
    error.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find the longest key in a trie that is a prefix to the passed string.===
%% @end
%%-------------------------------------------------------------------------

-spec find_prefix_longest(Match :: ?TYPE_NAME(),
                          Node :: trie()) ->
    {ok, ?TYPE_NAME(), any()} | 'error'.

find_prefix_longest(Match, Node) when is_tuple(Node) ->
    find_prefix_longest(Match, ?TYPE_EMPTY, error, Node);

find_prefix_longest(_Match, ?TYPE_EMPTY) ->
    error.

find_prefix_longest(?TYPE_H0T0, Key, LastMatch, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            %% If the prefix matched and there are other child leaf nodes
            %% for this prefix, then update the last match to the current
            %% prefix and continue recursing over the trie.
            NewKey = ?TYPE_NEWKEY,
            NewMatch = if
                Value =:= error ->
                    LastMatch;
                true ->
                    {NewKey, Value}
            end,
            find_prefix_longest(T, NewKey, NewMatch, ChildNode);
        true ->
            %% If this is a leaf node and the key for the current node is a
            %% prefix for the passed value, then return a match on the current
            %% node. Otherwise, return the last match we had found previously.
            case ?TYPE_PREFIX(ChildNode, T) of
                true when Value =/= error ->
                    {ok, ?TYPE_NEWKEY_REVERSE(?TYPE_NEWKEY, ChildNode), Value};
                _ ->
                    case LastMatch of
                        {LastKey, LastValue} ->
                            {ok, ?TYPE_NEWKEY_REVERSE(LastKey), LastValue};
                        error ->
                            error
                    end
            end
    end;

find_prefix_longest(_Match, _Key, {LastKey, LastValue}, _Node) ->
    {ok, ?TYPE_NEWKEY_REVERSE(LastKey), LastValue};

find_prefix_longest(_Match, _Key, error, _Node) ->
    error.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find all the keys in a trie that are prefixes to the passed string.===
%% The entries are returned in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec find_prefixes(Match :: ?TYPE_NAME(),
                      Node :: trie()) ->
    list({?TYPE_NAME(), any()}).

find_prefixes(Match, Node) when is_tuple(Node) ->
    find_prefixes(Match, ?TYPE_EMPTY, [], Node);

find_prefixes(_Match, ?TYPE_EMPTY) ->
    [].

find_prefixes(?TYPE_H0T0, Key, Acc, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            %% If the prefix matched and there are other child leaf nodes
            %% for this prefix, then add the match to the current list
            %% and continue recursing over the trie.
            NewKey = ?TYPE_NEWKEY,
            NewAcc = if
                Value =:= error ->
                    Acc;
                true ->
                    [{?TYPE_NEWKEY_REVERSE(NewKey), Value} | Acc]
            end,
            find_prefixes(T, NewKey, NewAcc, ChildNode);
        true ->
            %% If this is a leaf node and the key for the current node is a
            %% prefix for the passed value, then add a match on the current
            %% node. Otherwise, return the last match we had found previously.
            NewAcc = case ?TYPE_PREFIX(ChildNode, T) of
                true when Value =/= error ->
                    [{?TYPE_NEWKEY_REVERSE(?TYPE_NEWKEY, ChildNode), Value} |
                     Acc];
                _ ->
                    Acc
            end,
            lists:reverse(NewAcc)
    end;

find_prefixes(_Match, _Key, Acc, _Node) ->
    lists:reverse(Acc).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the trie.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec fold(F :: fun((?TYPE_NAME(), any(), any()) -> any()),
           A :: any(),
           Node :: trie()) -> any().

fold(F, A, Node) when is_function(F, 3) ->
    foldl(F, A, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the trie.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldl(F :: fun((?TYPE_NAME(), any(), any()) -> any()),
            A :: any(),
            Node :: trie()) -> any().

foldl(F, A, ?TYPE_EMPTY) when is_function(F, 3) ->
    A;

foldl(F, A, Node) when is_function(F, 3) ->
    foldl(F, A, ?TYPE_EMPTY, Node).

foldl(F, A, Key, {I0, I1, Data}) ->
    foldl_element(F, A, 1, I1 - I0 + 2, I0 - 1, Key, Data).

foldl_element(_, A, N, N, _, _, _) ->
    A;

foldl_element(F, A, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        ?TYPE_CHECK(Node) =:= false ->
            if
                Value =:= error ->
                    foldl_element(F, foldl(F, A, ?TYPE_KEYCHAR, Node),
                        I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = ?TYPE_KEYCHAR,
                    foldl_element(F,
                        foldl(F, F(NewKey, Value, A), NewKey, Node),
                        I + 1, N, Offset, Key, Data)
            end;
        true ->
            if
                Value =:= error ->
                    foldl_element(F, A,
                        I + 1, N, Offset, Key, Data);
                true ->
                    foldl_element(F, F(?TYPE_KEYCHARNODE, Value, A),
                        I + 1, N, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the trie in reverse.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldr(F :: fun((?TYPE_NAME(), any(), any()) -> any()),
            A :: any(),
            Node :: trie()) -> any().

foldr(F, A, ?TYPE_EMPTY) when is_function(F, 3) ->
    A;

foldr(F, A, Node) when is_function(F, 3) ->
    foldr(F, A, ?TYPE_EMPTY, Node).

foldr(F, A, Key, {I0, I1, Data}) ->
    foldr_element(F, A, I1 - I0 + 1, I0 - 1, Key, Data).

foldr_element(_, A, 0, _, _, _) ->
    A;

foldr_element(F, A, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        ?TYPE_CHECK(Node) =:= false ->
            if
                Value =:= error ->
                    foldr_element(F, foldr(F, A, ?TYPE_KEYCHAR, Node),
                        I - 1, Offset, Key, Data);
                true ->
                    NewKey = ?TYPE_KEYCHAR,
                    foldr_element(F,
                        F(NewKey, Value, foldr(F, A, NewKey, Node)),
                        I - 1, Offset, Key, Data)
            end;
        true ->
            if
                Value =:= error ->
                    foldr_element(F, A,
                        I - 1, Offset, Key, Data);
                true ->
                    foldr_element(F, F(?TYPE_KEYCHARNODE, Value, A),
                        I - 1, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the keys within a trie that share a common prefix.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec fold_similar(Similar :: ?TYPE_NAME(),
                   F :: fun((?TYPE_NAME(), any(), any()) -> any()),
                   A :: any(),
                   Node :: trie()) -> any().

fold_similar(Similar, F, A, Node) ->
    foldl_similar(Similar, F, A, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the keys within a trie that share a common prefix.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldl_similar(Similar :: ?TYPE_NAME(),
                    F :: fun((?TYPE_NAME(), any(), any()) -> any()),
                    A :: any(),
                    Node :: trie()) -> any().

foldl_similar(?TYPE_H0_, _, A, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    A;

foldl_similar(_, _, A, ?TYPE_EMPTY) ->
    A;

foldl_similar(?TYPE_H0T0, F, A, Node) ->
    fold_similar_node(H, T, foldl, F, A, ?TYPE_EMPTY, error, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the keys within a trie that share a common prefix in reverse.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldr_similar(Similar :: ?TYPE_NAME(),
                    F :: fun((?TYPE_NAME(), any(), any()) -> any()),
                    A :: any(),
                    Node :: trie()) -> any().

foldr_similar(?TYPE_H0_, _, A, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    A;

foldr_similar(_, _, A, ?TYPE_EMPTY) ->
    A;

foldr_similar(?TYPE_H0T0, F, A, Node) ->
    fold_similar_node(H, T, foldr, F, A, ?TYPE_EMPTY, error, Node).

fold_similar_node(H, _, Fold, F, A, Key, LastValue, {I0, I1, _} = Node)
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    if
        LastValue =:= error ->
            fold_similar_element(Fold, F, A, Key, Node);
        Fold =:= foldl ->
            fold_similar_element(Fold, F, F(Key, LastValue, A), Key, Node);
        Fold =:= foldr ->
            F(Key, LastValue, fold_similar_element(Fold, F, A, Key, Node))
    end;

fold_similar_node(H, ?TYPE_EMPTY, Fold, F, A, Key, _, {I0, _, Data} = Node)
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            NewKey = ?TYPE_KEYH0,
            if
                Value =:= error ->
                    fold_similar_element(Fold, F, A, NewKey, ChildNode);
                Fold =:= foldl ->
                    fold_similar_element(Fold, F, F(NewKey, Value, A),
                                         NewKey, ChildNode);
                Fold =:= foldr ->
                    F(NewKey, Value,
                      fold_similar_element(Fold, F, A, NewKey, ChildNode))
            end;
        Value =/= error, ?TYPE_CHECK(ChildNode) ->
            F(?TYPE_KEYH0CHILDNODE, Value, A);
        true ->
            fold_similar_element(Fold, F, A, Key, Node)
    end;

fold_similar_node(H, T, Fold, F, A, Key, _, {I0, _, Data} = Node)
    when is_integer(H) ->
    {ChildNode, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(ChildNode) ->
            ?TYPE_H1T1 = T,
            fold_similar_node(H1, T1, Fold, F, A,
                ?TYPE_KEYH0, Value, ChildNode);
        Value =/= error, ?TYPE_CHECK(ChildNode) ->
            F(?TYPE_KEYH0CHILDNODE, Value, A);
        true ->
            fold_similar_element(Fold, F, A, Key, Node)
    end.

fold_similar_element(foldl, F, A, Key, Node) ->
    foldl(F, A, Key, Node);

fold_similar_element(foldr, F, A, Key, Node) ->
    foldr(F, A, Key, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Call a function for each element.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foreach(F :: fun((?TYPE_NAME(), any()) -> any()),
              Node :: trie()) -> any().

foreach(F, ?TYPE_EMPTY) when is_function(F, 2) ->
    ok;

foreach(F, Node) when is_function(F, 2) ->
    foreach(F, ?TYPE_EMPTY, Node).

foreach(F, Key, {I0, I1, Data}) ->
    foreach_element(F, 1, I1 - I0 + 2, I0 - 1, Key, Data).

foreach_element(_, N, N, _, _, _) ->
    ok;

foreach_element(F, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    if
        ?TYPE_CHECK(Node) =:= false ->
            if
                Value =:= error ->
                    foreach(F, ?TYPE_KEYCHAR, Node),
                    foreach_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = ?TYPE_KEYCHAR,
                    F(NewKey, Value),
                    foreach(F, NewKey, Node),
                    foreach_element(F, I + 1, N, Offset, Key, Data)
            end;
        true ->
            if
                Value =:= error ->
                    foreach_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    F(?TYPE_KEYCHARNODE, Value),
                    foreach_element(F, I + 1, N, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a trie from a list.===
%% @end
%%-------------------------------------------------------------------------

-spec from_list(list()) -> trie().

from_list(L) ->
    new(L).

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if a key exists in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec is_key(?TYPE_NAME(), trie()) -> boolean().

is_key(_, ?TYPE_EMPTY) ->
    false;

is_key(?TYPE_H0T0, {_, _, _} = Node) ->
    is_key_node(H, T, Node).

is_key_node(H, _, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    false;

is_key_node(H, ?TYPE_EMPTY, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            (Value =/= error);
        true ->
            false
    end;

is_key_node(H, T, {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            ?TYPE_H1T1 = T,
            is_key_node(H1, T1, Node);
        T ->
            (Value =/= error);
        _ ->
            false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Map a function over a trie.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec map(F :: fun((?TYPE_NAME(), any()) -> any()),
          Node :: trie()) -> trie().

map(F, ?TYPE_EMPTY = Node) when is_function(F, 2) ->
    Node;

map(F, Node) when is_function(F, 2) ->
    map_node(F, ?TYPE_EMPTY, Node).

map_node(F, Key, {I0, I1, Data}) ->
    {I0, I1, map_element(F, I1 - I0 + 1, I0 - 1, Key, Data)};

map_node(_, _, Node)
    when ?TYPE_CHECK(Node) ->
    Node.

map_element(_, 0, _, _, Data) ->
    Data;

map_element(F, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    Character = Offset + I,
    NewKey = ?TYPE_KEYCHAR,
    if
        Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    map_element(F, I - 1, Offset, Key, Data);
                true ->
                    map_element(F, I - 1, Offset, Key,
                        erlang:setelement(I, Data, {Node, F(NewKey, Value)}))
            end;
        Value =:= error ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node), Value}));
        ?TYPE_CHECK(Node) ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node), F(?TYPE_NEWKEYNODE, Value)}));
        true ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node), F(NewKey, Value)}))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Merge two trie instance.===
%% Update the second trie parameter with all of the elements
%% found within the first trie parameter.
%% @end
%%-------------------------------------------------------------------------

-spec merge(F :: fun((?TYPE_NAME(), any(), any()) -> any()),
            Node1 :: trie(),
            Node2 :: trie()) -> trie().

merge(F, Node1, ?TYPE_EMPTY) when is_function(F, 3) ->
    Node1;

merge(F, ?TYPE_EMPTY, Node2) when is_function(F, 3) ->
    Node2;

merge(F, Node1, Node2) when is_function(F, 3) ->
    fold(fun (Key, V1, Node) ->
            update(Key, fun (V2) -> F(Key, V1, V2) end, V1, Node)
         end, Node2, Node1).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec new() -> empty_trie().

new() ->
    ?TYPE_EMPTY.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new trie instance from a list.===
%% The list may contain either: strings, 2 element tuples with a string as the
%% first tuple element, or tuples with more than 2 elements (including records)
%% with a string as the first element (second element if it is a record).
%% If a list of records (or tuples larger than 2 elements) is provided,
%% the whole record/tuple is stored as the value.
%% @end
%%-------------------------------------------------------------------------

-spec new(L :: list()) -> trie().

new(L) ->
    new_instance(L, new()).

new_instance([], Node) ->
    Node;

new_instance([{Key, Value} | T], Node) ->
    new_instance(T, store(Key, Value, Node));

new_instance([Tuple | T], Node)
    when is_tuple(Tuple) ->
    FirstElement = erlang:element(1, Tuple),
    Key = if
        is_atom(FirstElement) ->
            erlang:element(2, Tuple);
        true ->
            FirstElement
    end,
    new_instance(T, store(Key, Tuple, Node));

new_instance([Key | T], Node) ->
    new_instance(T, store(Key, Node)).

new_instance_state(?TYPE_H0T0, V1, V0)
    when is_integer(H) ->
    {{H, H, {{T, V1}}}, V0}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Insert a value as the first list element in a trie instance.===
%% The reverse of append/3.
%% @end
%%-------------------------------------------------------------------------

-spec prefix(Key :: ?TYPE_NAME(),
             Value :: any(),
             Node :: trie()) -> nonempty_trie().

prefix(Key, Value, Node) ->
    update(Key, fun(OldValue) -> [Value | OldValue] end, [Value], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Size of a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec size(Node :: trie()) -> non_neg_integer().

size(Node) ->
    fold(fun(_, _, I) -> I + 1 end, 0, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store only a key in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: ?TYPE_NAME(),
            Node :: trie()) -> nonempty_trie().

store(Key, Node) ->
    store(Key, empty, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a key/value pair in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: ?TYPE_NAME(),
            NewValue :: any(),
            Node :: trie()) -> nonempty_trie().

store(?TYPE_H0T0, NewValue, ?TYPE_EMPTY) ->
    {H, H, {{T, NewValue}}};

store(?TYPE_H0T0, NewValue, Node) ->
    store_node(H, T, NewValue, Node).

store_node(H, T, NewValue, {I0, I1, Data})
    when is_integer(H), H < I0 ->
    NewData = erlang:setelement(1,
        tuple_move(I0 - H + 1, I1 - H + 1, Data, {?TYPE_EMPTY, error}),
        {T, NewValue}),
    {H, I1, NewData};

store_node(H, T, NewValue, {I0, I1, Data})
    when is_integer(H), H > I1 ->
    N = H - I0 + 1,
    NewData = erlang:setelement(N,
        tuple_move(1, N, Data, {?TYPE_EMPTY, error}),
        {T, NewValue}),
    {I0, H, NewData};

store_node(H, ?TYPE_EMPTY = T, NewValue, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            {I0, I1, erlang:setelement(I, Data, {Node, NewValue})};
        true ->
            NewNode = {I0, I1, erlang:setelement(I, Data,
                new_instance_state(Node, Value, error))},
            store_node(H, T, NewValue, NewNode)
    end;

store_node(H, ?TYPE_H1T1 = T, NewValue, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            {I0, I1, erlang:setelement(I, Data,
                {store_node(H1, T1, NewValue, Node), Value})};
        T ->
            {I0, I1, erlang:setelement(I, Data, {Node, NewValue})};
        ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    {I0, I1, erlang:setelement(I, Data, {T, NewValue})};
                true ->
                    {I0, I1, erlang:setelement(I, Data,
                        new_instance_state(T, NewValue, Value))}
            end;
        ?TYPE_BHBT ->
            NewNode = {I0, I1,
                erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            store_node(H, T, NewValue, NewNode)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Take a value from the trie.===
%% @end
%%-------------------------------------------------------------------------

-spec take(Key :: ?TYPE_NAME(),
           Node :: trie()) ->
    {any(), trie()} | 'error'.

take(_, ?TYPE_EMPTY) ->
    error;

take(?TYPE_H0T0, Node) ->
    take_node(H, T, Node).

take_node(H, _, {I0, I1, _})
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    error;

take_node(H, T, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        T == Node ->
            if
                Value =:= error ->
                    error;
                true ->
                    {Value,
                     {I0, I1,
                      erlang:setelement(I, Data, {?TYPE_EMPTY, error})}}
            end;
        T =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    error;
                true ->
                    {Value,
                     {I0, I1, erlang:setelement(I, Data, {Node, error})}}
            end;
        ?TYPE_CHECK(Node) ->
            error;
        is_tuple(Node) ->
            ?TYPE_H1T1 = T,
            case take_node(H1, T1, Node) of
                error ->
                    error;
                {OldValue, NewNode} ->
                    {OldValue,
                     {I0, I1, erlang:setelement(I, Data, {NewNode, Value})}}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert all entries in a trie to a list.===
%% The list is in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec to_list(Node :: trie()) -> list({?TYPE_NAME(), any()}).

to_list(Node) ->
    foldr(fun (Key, Value, L) -> [{Key, Value} | L] end, [], Node).
        
%%-------------------------------------------------------------------------
%% @doc
%% ===Return a list of all entries within a trie that share a common prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec to_list_similar(Similar :: ?TYPE_NAME(),
                      Node :: trie()) -> list({?TYPE_NAME(), any()}).

to_list_similar(Similar, Node) ->
    foldr_similar(Similar,
                  fun(Key, Value, L) -> [{Key, Value} | L] end, [], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update(?TYPE_NAME(),
             F :: fun((any()) -> any()),
             nonempty_trie()) -> nonempty_trie().

update(?TYPE_H0T0, F, {_, _, _} = Node)
    when is_function(F, 1) ->
    update_node(H, T, F, Node).

update_node(H, ?TYPE_EMPTY, F, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY, Value =/= error ->
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
    end;

update_node(H, T, F, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            ?TYPE_H1T1 = T,
            {I0, I1, erlang:setelement(I, Data,
                {update_node(H1, T1, F, Node), Value})};
        T ->
            true = Value =/= error,
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update or add a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Key :: ?TYPE_NAME(),
             F :: fun((any()) -> any()),
             Initial :: any(),
             Node :: trie()) -> nonempty_trie().

update(Key, _, Initial, ?TYPE_EMPTY = Node) ->
    store(Key, Initial, Node);

update(?TYPE_H0T0, F, Initial, {_, _, _} = Node)
    when is_function(F, 1) ->
    update_node(H, T, F, Initial, Node).

update_node(H, T, _, Initial, {I0, I1, Data})
    when is_integer(H), H < I0 ->
    NewData = erlang:setelement(1,
        tuple_move(I0 - H + 1, I1 - H + 1, Data, {?TYPE_EMPTY, error}),
        {T, Initial}),
    {H, I1, NewData};

update_node(H, T, _, Initial, {I0, I1, Data})
    when is_integer(H), H > I1 ->
    N = H - I0 + 1,
    NewData = erlang:setelement(N,
        tuple_move(1, N, Data, {?TYPE_EMPTY, error}),
        {T, Initial}),
    {I0, H, NewData};

update_node(H, ?TYPE_EMPTY = T, F, Initial, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node =:= ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    {I0, I1, erlang:setelement(I, Data, {Node, Initial})};
                true ->
                    {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
            end;
        true ->
            ?TYPE_BHBT = Node,
            NewNode = {I0, I1,
               erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            update_node(H, T, F, Initial, NewNode)
    end;

update_node(H, T, F, Initial, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            ?TYPE_H1T1 = T,
            {I0, I1, erlang:setelement(I, Data,
                {update_node(H1, T1, F, Initial, Node), Value})};
        T ->
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})};
        ?TYPE_EMPTY ->
            if
                Value =:= error ->
                    {I0, I1, erlang:setelement(I, Data, {T, Initial})};
                true ->
                    {I0, I1, erlang:setelement(I, Data,
                        new_instance_state(T, Initial, Value))}
            end;
        ?TYPE_BHBT ->
            NewNode = {I0, I1,
                erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            update_node(H, T, F, Initial, NewNode)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a counter in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update_counter(Key :: ?TYPE_NAME(),
                     Increment :: number(),
                     Node :: trie()) -> nonempty_trie().

update_counter(Key, Increment, Node) ->
    update(Key, fun(I) -> I + Increment end, Increment, Node).

