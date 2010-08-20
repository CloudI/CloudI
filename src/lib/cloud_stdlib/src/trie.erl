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
%%% Copyright (c) 2010, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2010 Michael Truog
%%% @version 0.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(trie).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         new/1,
         find/2,
         find_prefix/2,
         store/2,
         store/3,
         test/0,
         test_size/0,
         test_size/1]).

-include_lib("kernel/include/file.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new trie instance.===
%% @end
%%-------------------------------------------------------------------------

new() ->
    [].

new([H | T]) ->
    {RootNode, _} = new_instance(H, T),
    RootNode.

new_instance({[_ | _] = S, V}, L) ->
    new_instance_modify(L, new_instance_state(S, V, error));
           
new_instance([_ | _] = S, L) ->
    new_instance_modify(L, new_instance_state(S, empty, error)).

new_instance_state([], V1, _) ->
    {[], V1};

new_instance_state([H | T], V1, V0)
    when is_integer(H) ->
    {{H, H, {{T, V1}}}, V0}.

new_instance_modify([], State) ->
    State;

new_instance_modify([{[_ | _] = S, V} | T], State) ->
    new_instance_modify(T, modify_state(S, V, State));

new_instance_modify([[_ | _] = S | T], State) ->
    new_instance_modify(T, modify_state(S, empty, State)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in a trie.===
%% @end
%%-------------------------------------------------------------------------

find([H | _], {I0, I1, _})
    when H < I0; H > I1 ->
    error;

find([H], {I0, _, Data})
    when is_integer(H) ->
    {Node, Value} = erlang:element(H - I0 + 1, Data),
    if
        is_tuple(Node); Node == [] ->
            Value;
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
            Value;
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find a value in a trie by prefix.===
%% @end
%%-------------------------------------------------------------------------

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
                    Value
            end;
        Node == [] ->
            Value;
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
            Value;
        L ->
            case lists:prefix(T, L) of
                true ->
                    prefix;
                false ->
                    error
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in a trie instance.===
%% @end
%%-------------------------------------------------------------------------

store([_ | _] = S, RootNode) ->
    store(S, empty, RootNode).

store([_ | _] = S, V, RootNode) ->
    {NewRootNode, _} = modify_state(S, V, {RootNode, error}),
    NewRootNode.

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
    1 = trie:find("abcdefghijklmnopqrstuvwxyz", RootNode0),
    error = trie:find("abcdefghijklmnopqrstuvwxy", RootNode0),
    1 = trie:find_prefix("abcdefghijklmnopqrstuvwxyz", RootNode0),
    prefix = trie:find_prefix("abcdefghijklmnopqrstuvwxy", RootNode0),
    error = trie:find_prefix("abcdefghijklmnopqrstuvwxyzX", RootNode0),
    prefix = trie:find_prefix("a", RootNode0),
    prefix = trie:find_prefix("aa", RootNode0),
    2 = trie:find_prefix("aac", RootNode0),
    error = trie:find_prefix("aacX", RootNode0),
    {97,97,{{{97,98,{{{98,99,{{"cde",3},{[],2}}},error},{"cdefghijklmnopqrstuvwxyz",1}}},error}}} =
        RootNode1 = trie:store("aabcde", 3, RootNode0),
    error = trie:find("aabcde", RootNode0),
    3 = trie:find("aabcde", RootNode1),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Size Test.===
%% Compare the size of a word list with the size of the resulting trie
%% data structure.
%% @end
%%-------------------------------------------------------------------------

test_size() ->
    test_size("/usr/share/dict/words").

test_size([_ | _] = FilePath) ->
    {ok, Info} = file:read_file_info(FilePath),
    FileSize = Info#file_info.size,
    {ok, F} = file:open(FilePath, [read_ahead, raw, read]),
    erlang:garbage_collect(),
    {memory, Size0} = erlang:process_info(self(), memory),
    Result = test_size_read(F, new()),
    erlang:garbage_collect(),
    {memory, Size1} = erlang:process_info(self(), memory),
    file:close(F),
    ResultRealSize = Size1 - Size0,
    ResultAbstractSize = abstract_binary_size(Result),
    ResultEstimateSize = estimate_binary_size(Result),
    io:format("file ~s (~w bytes)~n",
              [FilePath, FileSize]),
    io:format("trie data:~n"
              "  raw         ~16w bytes~n"
              "  estimated   ~16w bytes~n"
              "  characters  ~16w bytes~n",
              [ResultRealSize, ResultEstimateSize, ResultAbstractSize]),
    io:format("  characters stored  ~.1f %~n"
              "  data stored        ~.2f x~n",
              [erlang:round(ResultAbstractSize / FileSize * 1000) / 10,
               erlang:round(ResultRealSize / FileSize * 100) / 100]),
    ok.

test_size_read(F, State) ->
    case file:read_line(F) of
        {ok, Line} ->
            test_size_read(F, store(Line, State));
        eof ->
            State
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

modify_state([], AV, {Node, _}) ->
    {Node, AV};

modify_state([_ | _] = A, AV, {[], V}) ->
    new_instance_state(A, AV, V);

modify_state([AH | AT], AV, {{I0, I1, Data}, V})
    when is_integer(AH) ->
    if
        AH < I0 ->
            NewData = erlang:setelement(1,
                tuple_move(I0 - AH + 1, I1 - AH + 1, Data, {[], error}),
                {AT, AV}),
            {{AH, I1, NewData}, V};
        AH > I1 ->
            N = AH - I0 + 1,
            NewData = erlang:setelement(N,
                tuple_move(1, N, Data, {[], error}),
                {AT, AV}),
            {{I0, AH, NewData}, V};
        true ->
            I = AH - I0 + 1,
            {Node, BV} = State = erlang:element(I, Data),
            NewData = case Node of
                [] ->
                    if
                        BV == error ->
                            erlang:setelement(I, Data, {AT, AV});
                        true ->
                            erlang:setelement(I, Data,
                                new_instance_state(AT, AV, BV))
                    end;
                AT ->
                    erlang:setelement(I, Data, {AT, AV});
                [BH | BT] ->
                    erlang:setelement(I, Data,
                        modify_state(AT, AV,
                            {{BH, BH, {{BT, BV}}}, error}));
                {_, _, _} ->
                    erlang:setelement(I, Data,
                        modify_state(AT, AV, State))
            end,
            {{I0, I1, NewData}, V}
    end.

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

% Provide an estimate of the data size that is comparable to the
% trie input word list.
abstract_binary_size(T) ->
    abstract_binary_size(0, T).

abstract_binary_size(Size0, T0) when is_tuple(T0) ->
    lists:foldl(fun(I, Size1) ->
        abstract_binary_size(Size1, erlang:element(I, T0))
    end, Size0, lists:seq(1, erlang:tuple_size(T0)));

abstract_binary_size(Size0, []) ->
    Size0;

abstract_binary_size(Size0, [_ | _] = T0) ->
    lists:foldl(fun(T1, Size1) ->
        abstract_binary_size(Size1, T1)
    end, Size0, T0);

abstract_binary_size(Size0, T0) when is_atom(T0) ->
    Size0;

abstract_binary_size(Size0, T0) when is_integer(T0) ->
    true = T0 =< 255,
    % count only the characters for the abstract size
    Size0 + 1.

% Provide an estimate of the data size based on available data type information
% (http://erlang.org/doc/efficiency_guide/advanced.html#id2265992 and
%  experimentation on a 64bit system without HIPE...
%  the sizes do not match the html page,
%  since those results don't match reality)
estimate_binary_size(T) ->
    estimate_binary_size(0, T).

estimate_binary_size(Size0, T0) when is_tuple(T0) ->
    lists:foldl(fun(I, Size1) ->
        estimate_binary_size(Size1, erlang:element(I, T0))
    end, Size0 + 4, lists:seq(1, erlang:tuple_size(T0)));

estimate_binary_size(Size0, []) ->
    Size0 + 4;

estimate_binary_size(Size0, [_ | _] = T0) ->
    lists:foldl(fun(T1, Size1) ->
        estimate_binary_size(Size1, T1)
    end, Size0 + 4 + erlang:length(T0) * 4, T0);

estimate_binary_size(Size0, T0) when is_atom(T0) ->
    Size0 + 4;

estimate_binary_size(Size0, T0) when is_integer(T0) ->
    true = T0 =< 255,
    Size0 + 8.

