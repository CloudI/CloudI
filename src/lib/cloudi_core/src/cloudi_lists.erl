%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==List operations==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2016 Michael Truog
%%% @version 1.5.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_lists).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([delete_all/2, delete_checked/2,
         index/2,
         itera/3, itera2/4,
         member_any/2,
         split/2,
         take_values/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===lists:delete/2 functionality, but all instances are deleted.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_all(Elem :: any(), List :: list()) ->
    list().

delete_all(_, []) ->
    [];
delete_all(Elem, [Elem | T]) ->
    delete_all(Elem, T);
delete_all(Elem, [H | T]) ->
    [H | delete_all(Elem, T)].

%%-------------------------------------------------------------------------
%% @doc
%% ===lists:delete/2 functionality, but returns false when an element is not deleted.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_checked(Elem :: any(), List :: list()) ->
    list() |
    'false'.

delete_checked(Elem, List) when is_list(List) ->
    delete_checked(Elem, [], List).
delete_checked(Elem, L, [Elem | T]) ->
    lists:reverse(L) ++ T;
delete_checked(Elem, L, [H | T]) ->
    delete_checked(Elem, [H | L], T);
delete_checked(_, _, []) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find the index of the first occurrence of an item within a list.===
%% The integer index returned is 1-based.
%% @end
%%-------------------------------------------------------------------------

-spec index(Item :: any(),
            L :: list()) ->
    undefined | pos_integer().

index(Item, L)
    when is_list(L) ->
    index(Item, L, 1).

index(_, [], _) ->
    undefined;
index(Item, [Item | _], I) ->
    I;
index(Item, [_ | T], I) ->
    index(Item, T, I + 1).

%%-------------------------------------------------------------------------
%% @doc
%% ===Iterate on elements of a list with an accumulator.===
%% @end
%%-------------------------------------------------------------------------

-spec itera(F :: fun((any(), any(), fun((any()) -> any())) -> any()),
            Acc :: any(),
            list()) -> any().

itera(_, Acc, []) ->
    Acc;
itera(F, Acc, [H]) ->
    F(H, Acc, fun(V) -> V end);
itera(F, Acc, [H | T]) ->
    F(H, Acc, fun(V) -> itera(F, V, T) end).

%%-------------------------------------------------------------------------
%% @doc
%% ===Iterate on elements of a list with two accumulators.===
%% @end
%%-------------------------------------------------------------------------

-spec itera2(F :: fun((any(), any(), any(),
                       fun((any(), any()) -> {any(), any()})) ->
                      {any(), any()}),
             Acc0 :: any(),
             Acc1 :: any(),
             list()) -> any().

itera2(_, Acc0, Acc1, []) ->
    {Acc0, Acc1};
itera2(F, Acc0, Acc1, [H]) ->
    F(H, Acc0, Acc1, fun(V0, V1) -> {V0, V1} end);
itera2(F, Acc0, Acc1, [H | T]) ->
    F(H, Acc0, Acc1, fun(V0, V1) -> itera2(F, V0, V1, T) end).

%%-------------------------------------------------------------------------
%% @doc
%% ===lists:member/2 functionality, but with a list of elements.===
%% @end
%%-------------------------------------------------------------------------

-spec member_any(ElemL :: list(), List :: list()) ->
    boolean().

member_any([], _) ->
    false;
member_any(_, []) ->
    false;
member_any([Elem | ElemL], [_ | _] = List) ->
    case lists:member(Elem, List) of
        true ->
            true;
        false ->
            member_any(ElemL, List)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===lists:split/2 functionality, but without bounds checking.===
%% @end
%%-------------------------------------------------------------------------

-spec split(N, L) -> {L1, L2}
    when N :: non_neg_integer(),
         L :: list(E),
         L1 :: list(E),
         L2 :: list(E),
         E :: any().

split(N, L) when is_integer(N), N >= 0, is_list(L) ->
    split(N, L, []).
split(0, L2, L1) ->
    {lists:reverse(L1, []), L2};
split(_, [] = L2, L1) ->
    {lists:reverse(L1, []), L2};
split(N, [H | L2], L1) ->
    split(N - 1, L2, [H | L1]).

%%-------------------------------------------------------------------------
%% @doc
%% ===cloudi_proplists:take_values/2 functionality, but with any tuple list.===
%% @end
%%-------------------------------------------------------------------------

-spec take_values(DefaultList :: list({any(), any()}),
                  List :: list({any(), any()})) ->
    list().

take_values(DefaultList, List)
    when is_list(DefaultList), is_list(List) ->
    take_values([], DefaultList, List).

take_values(Result, [], List) ->
    lists:reverse(Result) ++ List;

take_values(Result, [{Key, Default} | DefaultList], List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            take_values([Default | Result], DefaultList, List);
        {value, {Key, Value}, RemainingList} ->
            take_values([Value | Result], DefaultList, RemainingList)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

delete_all_test() ->
    [b, c] = delete_all(a, [a, b, a, c, a]),
    ok.

delete_checked_test() ->
    false = delete_checked(d, [a, b, c]),
    [b, a, c, a] = delete_checked(a, [a, b, a, c, a]),
    ok.

index_test() ->
    4 = index(d, [a, b, c, d, e]),
    undefined = index(f, [a, b, c, d, e]),
    ok.

itera_test() ->
    [d, e, f] = itera(fun(V, A, Itr) ->
        if
            V > c ->
                Itr([V | A]);
            true ->
                A
        end
    end, [], [f, e, d, c, b, a]),
    ok.

itera2_test() ->
    {[d, e, f], 3} = itera2(fun(V, A1, A2, Itr) ->
        if
            V > c ->
                Itr([V | A1], A2 + 1);
            true ->
                {A1, A2}
        end
    end, [], 0, [f, e, d, c, b, a]),
    ok.

member_any_test() ->
    true = member_any([a, b], [a]),
    false = member_any([a, b], [c]),
    ok.

split_test() ->
    {[a, b, c], []} = split(10, [a, b, c]),
    {[a, b, c], [d, e, f]} = split(3, [a, b, c, d, e, f]),
    ok.

take_values_test() ->
    [A,
     B] = take_values([{a, 3},
                       {b, 2}], [{a, 1}]),
    true = A == 1,
    true = B == 2,
    ok.

-endif.

