%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==A trie data structure implementation.==
%%% The trie (i.e., from "retrieval") data structure was invented by
%%% Edward Fredkin (it is a form of radix sort).
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2010-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2010-2011 Michael Truog
%%% @version 0.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(trie).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([append/3,
         append_list/3,
         erase/2,
         fetch/2,
         fetch_keys/1,
         filter/2,
         find/2,
         find_prefix/2,
         fold/3,
         foldl/3,
         foldr/3,
         foreach/2,
         from_list/1,
         is_key/2,
         is_prefix/2,
         iter/2,
         itera/3,
         map/2,
         merge/3,
         new/0,
         new/1,
         size/1,
         store/2,
         store/3,
         to_list/1,
         update/3,
         update/4,
         update_counter/3,
         test/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type trie() :: [] | {integer(), integer(), tuple()}.
-type trie_return() :: {integer(), integer(), tuple()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value as a list element in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec append(Key :: string(),
             Value :: any(),
             Node :: trie()) -> trie_return().

append([_ | _] = Key, Value, Node) ->
    ValueList = [Value],
    update(Key, fun(OldValue) -> OldValue ++ ValueList end, ValueList, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a list of values as a list element in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec append_list(Key :: string(),
                  ValueList :: list(),
                  Node :: trie()) -> trie_return().

append_list([_ | _] = Key, ValueList, Node) ->
    update(Key, fun(OldValue) -> OldValue ++ ValueList end, ValueList, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec erase(Key :: string(),
            Node :: trie()) -> trie().

erase([_ | _] = Key, Node) ->
    erase_node(Key, Node).

erase_node([_ | _], [] = OldNode) ->
    OldNode;

erase_node([H | _], {I0, I1, _} = OldNode)
    when is_integer(H), H < I0;
         is_integer(H), H > I1 ->
    OldNode;

erase_node([H | T], {I0, I1, Data} = OldNode)
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        T == Node ->
            if
                Value == error ->
                    OldNode;
                true ->
                    {I0, I1, erlang:setelement(I, Data, {[], error})}
            end;
        T == [] ->
            if
                Value == error ->
                    OldNode;
                true ->
                    {I0, I1, erlang:setelement(I, Data, {Node, error})}
            end;
        is_list(Node) ->
            OldNode;
        is_tuple(Node) ->
            {I0, I1, erlang:setelement(I, Data, {erase_node(T, Node), Value})}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch a value from a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch(string(),
            trie_return()) -> any().

fetch([H], {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node == [] ->
            if
                Value /= error ->
                    Value
            end
    end;

fetch([H | T], {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1 ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            fetch(T, Node);
        T when Value /= error ->
            Value
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fetch all the keys in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec fetch_keys(Node :: trie()) -> list(string()).

fetch_keys(Node) ->
    foldr(fun(Key, _, L) -> [Key | L] end, [], Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a trie with a predicate function.===
%% @end
%%-------------------------------------------------------------------------

-spec filter(F :: fun((string(), any()) -> boolean()),
             Node :: trie()) -> trie().

filter(F, [] = Node) when is_function(F, 2) ->
    Node;

filter(F, Node) when is_function(F, 2) ->
    filter_node(F, [], Node).

filter_node(F, Key, {I0, I1, Data}) ->
    {I0, I1, filter_element(F, I1 - I0 + 1, I0 - 1, Key, Data)};

filter_node(_, _, [_ | _] = L) ->
    L.

filter_element(_, 0, _, _, Data) ->
    Data;

filter_element(F, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    if
        Node == [] ->
            if
                Value == error ->
                    filter_element(F, I - 1, Offset, Key, Data);
                true ->
                    case F(Key ++ [Offset + I], Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data, {[], error}))
                    end
            end;
        Value == error ->
            filter_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {filter_node(F, Key ++ [Offset + I], Node), Value}));
        true ->
            NewKey = Key ++ [Offset + I],
            if
                is_list(Node) == true ->
                    case F(NewKey ++ Node, Value) of
                        true ->
                            filter_element(F, I - 1, Offset, Key, Data);
                        false ->
                            filter_element(F, I - 1, Offset, Key,
                                erlang:setelement(I, Data, {[], error}))
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

-spec find(string(), trie()) -> {ok, any()} | 'error'.

find([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    error;

find([H], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node == [] ->
            if
                Value == error ->
                    error;
                true ->
                    {ok, Value}
            end;
        true ->
            error
    end;

find([H | T], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            find(T, Node);
        T ->
            if
                Value == error ->
                    error;
                true ->
                    {ok, Value}
            end;
        _ ->
            error
    end;

find(_, []) ->
    error.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in a trie by prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec find_prefix(string(), trie()) -> {ok, any()} | 'prefix' | 'error'.

find_prefix([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    error;

find_prefix([H], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node) ->
            if
                Value == error ->
                    prefix;
                true ->
                    {ok, Value}
            end;
        Node == [] ->
            if
                Value == error ->
                    error;
                true ->
                    {ok, Value}
            end;
        true ->
            prefix
    end;

find_prefix([H | T], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            find_prefix(T, Node);
        [] ->
            prefix;
        T ->
            if
                Value == error ->
                    error;
                true ->
                    {ok, Value}
            end;
        L ->
            case lists:prefix(T, L) of
                true ->
                    prefix;
                false ->
                    error
            end
    end;

find_prefix(_, []) ->
    error.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the trie.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec fold(F :: fun((string(), any(), any()) -> any()),
           A :: any(),
           Node :: trie()) -> any().

fold(F, A, Node) when is_function(F, 3) ->
    foldl(F, A, Node).

-spec foldl(F :: fun((string(), any(), any()) -> any()),
            A :: any(),
            Node :: trie()) -> any().

foldl(F, A, []) when is_function(F, 3) ->
    A;

foldl(F, A, Node) when is_function(F, 3) ->
    foldl(F, A, [], Node).

foldl(F, A, Key, {I0, I1, Data}) ->
    foldl_element(F, A, 1, I1 - I0 + 2, I0 - 1, Key, Data).

foldl_element(_, A, N, N, _, _, _) ->
    A;

foldl_element(F, A, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    if
        is_list(Node) == false ->
            if
                Value == error ->
                    foldl_element(F, foldl(F, A, Key ++ [Offset + I], Node),
                        I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = Key ++ [Offset + I],
                    foldl_element(F,
                        foldl(F, F(NewKey, Value, A), NewKey, Node),
                        I + 1, N, Offset, Key, Data)
            end;
        true ->
            if
                Value == error ->
                    foldl_element(F, A,
                        I + 1, N, Offset, Key, Data);
                true ->
                    foldl_element(F, F((Key ++ [Offset + I]) ++ Node, Value, A),
                        I + 1, N, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Fold a function over the trie in reverse.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foldr(F :: fun((string(), any(), any()) -> any()),
            A :: any(),
            Node :: trie()) -> any().

foldr(F, A, []) when is_function(F, 3) ->
    A;

foldr(F, A, Node) when is_function(F, 3) ->
    foldr(F, A, [], Node).

foldr(F, A, Key, {I0, I1, Data}) ->
    foldr_element(F, A, I1 - I0 + 1, I0 - 1, Key, Data).

foldr_element(_, A, 0, _, _, _) ->
    A;

foldr_element(F, A, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    if
        is_list(Node) == false ->
            if
                Value == error ->
                    foldr_element(F, foldr(F, A, Key ++ [Offset + I], Node),
                        I - 1, Offset, Key, Data);
                true ->
                    NewKey = Key ++ [Offset + I],
                    foldr_element(F,
                        F(NewKey, Value, foldr(F, A, NewKey, Node)),
                        I - 1, Offset, Key, Data)
            end;
        true ->
            if
                Value == error ->
                    foldr_element(F, A,
                        I - 1, Offset, Key, Data);
                true ->
                    foldr_element(F, F((Key ++ [Offset + I]) ++ Node, Value, A),
                        I - 1, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Call a function for each element.===
%% Traverses in alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec foreach(F :: fun((string(), any()) -> any()),
              Node :: trie()) -> any().

foreach(F, []) when is_function(F, 2) ->
    ok;

foreach(F, Node) when is_function(F, 2) ->
    foreach(F, [], Node).

foreach(F, Key, {I0, I1, Data}) ->
    foreach_element(F, 1, I1 - I0 + 2, I0 - 1, Key, Data).

foreach_element(_, N, N, _, _, _) ->
    ok;

foreach_element(F, I, N, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    if
        is_list(Node) == false ->
            if
                Value == error ->
                    foreach(F, Key ++ [Offset + I], Node),
                    foreach_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    NewKey = Key ++ [Offset + I],
                    F(NewKey, Value),
                    foreach(F, NewKey, Node),
                    foreach_element(F, I + 1, N, Offset, Key, Data)
            end;
        true ->
            if
                Value == error ->
                    foreach_element(F, I + 1, N, Offset, Key, Data);
                true ->
                    F((Key ++ [Offset + I]) ++ Node, Value),
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

-spec is_key(string(), trie()) -> boolean().

is_key([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    false;

is_key([H], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node == [] ->
            (Value /= error);
        true ->
            false
    end;

is_key([H | T], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            is_key(T, Node);
        T ->
            (Value /= error);
        _ ->
            false
    end;

is_key(_, []) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if a prefix exists in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec is_prefix(string(), trie()) -> 'true' | 'false'.

is_prefix([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    false;

is_prefix([H], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node) ->
            true;
        Node == [] ->
            if
                Value == error ->
                    false;
                true ->
                    true
            end;
        true ->
            true
    end;

is_prefix([H | T], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    case Node of
        {_, _, _} ->
            is_prefix(T, Node);
        [] ->
            true;
        T ->
            if
                Value == error ->
                    false;
                true ->
                    true
            end;
        L ->
            lists:prefix(T, L)
    end;

is_prefix(_, []) ->
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
        is_list(Node) == false ->
            if
                Value == error ->
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
                Value == error ->
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
        is_list(Node) == false ->
            if
                Value == error ->
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
                Value == error ->
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
%% ===Map a function over a trie.===
%% Traverses in reverse alphabetical order.
%% @end
%%-------------------------------------------------------------------------

-spec map(F :: fun((string(), any()) -> any()),
          Node :: trie()) -> trie().

map(F, [] = Node) when is_function(F, 2) ->
    Node;

map(F, Node) when is_function(F, 2) ->
    map_node(F, [], Node).

map_node(F, Key, {I0, I1, Data}) ->
    {I0, I1, map_element(F, I1 - I0 + 1, I0 - 1, Key, Data)};

map_node(_, _, [_ | _] = L) ->
    L.

map_element(_, 0, _, _, Data) ->
    Data;

map_element(F, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    NewKey = Key ++ [Offset + I],
    if
        Node == [] ->
            if
                Value == error ->
                    map_element(F, I - 1, Offset, Key, Data);
                true ->
                    map_element(F, I - 1, Offset, Key,
                        erlang:setelement(I, Data, {Node, F(NewKey, Value)}))
            end;
        Value == error ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node), Value}));
        is_list(Node) == true ->
            map_element(F, I - 1, Offset, Key, erlang:setelement(I, Data,
                {map_node(F, NewKey, Node), F(NewKey ++ Node, Value)}));
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

-spec merge(F :: fun((string(), any(), any()) -> any()),
            Node1 :: trie(),
            Node2 :: trie()) -> trie().

merge(F, Node1, []) when is_function(F, 3) ->
    Node1;

merge(F, [], Node2) when is_function(F, 3) ->
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

-spec new() -> [].

new() ->
    [].

-spec new(L :: list()) -> trie().

new(L) ->
    new_instance(L, new()).

new_instance([], Node) ->
    Node;

new_instance([{[_ | _] = Key, Value} | T], Node) ->
    new_instance(T, store(Key, Value, Node));

new_instance([[_ | _] = Key | T], Node) ->
    new_instance(T, store(Key, Node));

new_instance([Tuple | T], Node)
    when is_tuple(Tuple) ->
    FirstElement = erlang:element(1, Tuple),
    Key = if
        is_atom(FirstElement) == true ->
            erlang:element(2, Tuple);
        true ->
            FirstElement
    end,
    new_instance(T, store(Key, Tuple, Node)).

new_instance_state([H | T], V1, V0)
    when is_integer(H) ->
    {{H, H, {{T, V1}}}, V0}.

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
%% ===Store a value in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

-spec store(Key :: string(),
            Node :: trie()) -> trie_return().

store(Key, Node) ->
    store(Key, empty, Node).

-spec store(Key :: string(),
            NewValue :: any(),
            Node :: trie()) -> trie_return().

store([H | T], NewValue, [])
    when is_integer(H) ->
    {H, H, {{T, NewValue}}};

store([H | T], NewValue, {I0, I1, Data})
    when is_integer(H), H < I0 ->
    NewData = erlang:setelement(1,
        tuple_move(I0 - H + 1, I1 - H + 1, Data, {[], error}),
        {T, NewValue}),
    {H, I1, NewData};

store([H | T], NewValue, {I0, I1, Data})
    when is_integer(H), H > I1 ->
    N = H - I0 + 1,
    NewData = erlang:setelement(N,
        tuple_move(1, N, Data, {[], error}),
        {T, NewValue}),
    {I0, H, NewData};

store([H] = Key, NewValue, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node == [] ->
            {I0, I1, erlang:setelement(I, Data, {Node, NewValue})};
        true ->
            NewNode = {I0, I1,
               erlang:setelement(I, Data,
                   new_instance_state(Node, Value, error))},
            store(Key, NewValue, NewNode)
    end;

store([H | T] = Key, NewValue, {I0, I1, Data})
    when is_integer(H) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            {I0, I1, erlang:setelement(I, Data,
                {store(T, NewValue, Node), Value})};
        T ->
            {I0, I1, erlang:setelement(I, Data, {Node, Value})};
        [] ->
            if
                Value == error ->
                    {I0, I1, erlang:setelement(I, Data, {T, NewValue})};
                true ->
                    {I0, I1, erlang:setelement(I, Data,
                        new_instance_state(T, NewValue, Value))}
            end;
        [BH | BT] ->
            NewNode = {I0, I1,
                erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            store(Key, NewValue, NewNode)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert all entries in a trie to a list.===
%% @end
%%-------------------------------------------------------------------------

-spec to_list(Node :: trie()) -> list({string(), any()}).

to_list([]) ->
    [];

to_list(Node) ->
    to_list_node([], [], Node).

to_list_node(L, Key, {I0, I1, Data}) ->
    to_list_element(L, I1 - I0 + 1, I0 - 1, Key, Data).

to_list_element(L, 0, _, _, _) ->
    L;

to_list_element(L, I, Offset, Key, Data) ->
    {Node, Value} = erlang:element(I, Data),
    NewKey = Key ++ [Offset + I],
    if
        is_list(Node) == false ->
            if
                Value == error ->
                    to_list_element(
                        to_list_node(L, NewKey, Node),
                        I - 1, Offset, Key, Data);
                true ->
                    to_list_element(
                        [{NewKey, Value} | to_list_node(L, NewKey, Node)],
                        I - 1, Offset, Key, Data)
            end;
        true ->
            if
                Value == error ->
                    to_list_element(L, I - 1, Offset, Key, Data);
                true ->
                    to_list_element([{NewKey ++ Node, Value} | L],
                        I - 1, Offset, Key, Data)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update(string(),
             F :: fun((any()) -> any()),
             trie_return()) -> trie_return().

update([H], F, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1, is_function(F, 1) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node == [], Value /= error ->
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
    end;

update([H | T], F, {I0, I1, Data})
    when is_integer(H), H >= I0, H =< I1, is_function(F, 1) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            {I0, I1, erlang:setelement(I, Data, {update(T, F, Node), Value})};
        T ->
            true = Value /= error,
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update or add a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Key :: string(),
             F :: fun((any()) -> any()),
             Initial :: any(),
             Node :: trie()) -> trie_return().

update(Key, _, Initial, [] = Node) ->
    store(Key, Initial, Node);

update([H | T], _, Initial, {I0, I1, Data})
    when H < I0 ->
    NewData = erlang:setelement(1,
        tuple_move(I0 - H + 1, I1 - H + 1, Data, {[], error}),
        {T, Initial}),
    {H, I1, NewData};

update([H | T], _, Initial, {I0, I1, Data})
    when H > I1 ->
    N = H - I0 + 1,
    NewData = erlang:setelement(N,
        tuple_move(1, N, Data, {[], error}),
        {T, Initial}),
    {I0, H, NewData};

update([H] = Key, F, Initial, {I0, I1, Data})
    when is_integer(H), is_function(F, 1) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    if
        is_tuple(Node); Node == [] ->
            if
                Value == error ->
                    {I0, I1, erlang:setelement(I, Data, {Node, Initial})};
                true ->
                    {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})}
            end;
        true ->
            [BH | BT] = Node,
            NewNode = {I0, I1,
               erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            update(Key, F, Initial, NewNode)
    end;

update([H | T] = Key, F, Initial, {I0, I1, Data})
    when is_integer(H), is_function(F, 1) ->
    I = H - I0 + 1,
    {Node, Value} = erlang:element(I, Data),
    case Node of
        {_, _, _} ->
            {I0, I1, erlang:setelement(I, Data,
                {update(T, F, Initial, Node), Value})};
        T ->
            {I0, I1, erlang:setelement(I, Data, {Node, F(Value)})};
        [] ->
            if
                Value == error ->
                    {I0, I1, erlang:setelement(I, Data, {T, Initial})};
                true ->
                    {I0, I1, erlang:setelement(I, Data,
                        new_instance_state(T, Initial, Value))}
            end;
        [BH | BT] ->
            NewNode = {I0, I1,
                erlang:setelement(I, Data, {{BH, BH, {{BT, Value}}}, error})},
            update(Key, F, Initial, NewNode)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a counter in a trie.===
%% @end
%%-------------------------------------------------------------------------

-spec update_counter(Key :: string(),
                     Increment :: number(),
                     Node :: trie()) -> trie_return().

update_counter(Key, Increment, Node) ->
    update(Key, fun(I) -> I + Increment end, Increment, Node).

%%-------------------------------------------------------------------------
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
    {97,97,{{{97,98,{{{98,99,{{"cde",3},{[],2}}},error},{"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
        RootNode1 = trie:store("aabcde", 3, RootNode0),
    {97,97,{{{97,98,{{{98,99,{{"cde",13},{[],12}}},error},{"cdefghijklmnopqrstuvwxyz",11}}},error}}} =
        map(fun(_, V) -> V + 10 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{[],error},{[],error}}},error},{"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
        filter(fun(_, V) -> V =< 1 end, RootNode1),
    {97,97,{{{97,98,{{{98,99,{{[],error},{[],2}}},error},{"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
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
    2.5 = trie:fetch("aaaa", RootNode5),
    {'EXIT', {if_clause, _}} = (catch trie:fetch("aaaa", RootNode4)),
    RootNode4 = trie:erase("a", trie:erase("aaaa", RootNode5)),
    true = trie:is_key("aaaa", RootNode5),
    false = trie:is_key("aaaa", RootNode4),
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

