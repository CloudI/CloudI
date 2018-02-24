%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==List operations==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2018 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2009-2018 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_lists).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([delete_all/2, delete_checked/2,
         index/2,
         iodata_to_list/1,
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
%% ===Convert iodata to a list of bytes and return the size of the list.===
%% @end
%%-------------------------------------------------------------------------

-spec iodata_to_list(iodata()) ->
    {Size :: non_neg_integer(), Bytes :: list(byte())}.

iodata_to_list(IOData)
    when is_binary(IOData) ->
    {byte_size(IOData), erlang:binary_to_list(IOData)};
iodata_to_list(IOData)
    when is_list(IOData) ->
    iodata_to_list([], IOData, 0).

iodata_to_list(ListOut, [Binary | IODataIn], Size)
    when is_binary(Binary) ->
    iodata_to_list(lists:reverse(erlang:binary_to_list(Binary), ListOut),
                   IODataIn, Size + byte_size(Binary));
iodata_to_list(ListOut0, [List | IODataIn], Size0)
    when is_list(List) ->
    {SizeN, ListOutN} = iodata_to_list(ListOut0, List, Size0),
    iodata_to_list(lists:reverse(ListOutN), IODataIn, SizeN);
iodata_to_list(ListOut, [], Size) ->
    {Size, lists:reverse(ListOut)};
iodata_to_list(ListOut, [Byte | IOData], Size)
    when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    iodata_to_list([Byte | ListOut], IOData, Size + 1);
iodata_to_list(ListOut, Byte, Size)
    when is_integer(Byte), Byte >= 0, Byte =< 255 ->
    {Size + 1, lists:reverse([Byte | ListOut])}.

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
             list()) -> {any(), any()}.

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

iodata_to_list_test() ->
	{10, "abcdefghij"} = iodata_to_list([<<"abc">>, $d, "ef",
                                         [[$g]], ["hi", $j]]),
	{10, "abcdefghij"} = iodata_to_list(<<"abcdefghij">>),
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

