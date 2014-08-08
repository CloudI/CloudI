%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==List operations==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_lists).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([itera/3, itera2/4,
         delete_checked/2, delete_all/2,
         take_values/2,
         compare_constant/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

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
%% ===lists:delete/2 functionality, but all instances are deleted.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_all(Elem :: any(), List :: list()) ->
    list().

delete_all(Elem, List) when is_list(List) ->
    delete_all(Elem, [], List).
delete_all(Elem, L, [Elem | T]) ->
    delete_all(Elem, L, T);
delete_all(Elem, L, [H | T]) ->
    delete_all(Elem, [H | L], T);
delete_all(_, L, []) ->
    lists:reverse(L).

%%-------------------------------------------------------------------------
%% @doc
%% ===cloudi_proplists:take_values/2 functionality, but with any tuple list.===
%% @end
%%-------------------------------------------------------------------------

-spec take_values(DefaultList :: list({any(), any()}),
                  List :: list({any(), any()})) ->
    list({any(), any()}).

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Time insensitive compare to avoid a timing leak.===
%% Use for password or other authentication comparisons.
%% Execution time is based on the length of Test.
%% @end
%%-------------------------------------------------------------------------

-spec compare_constant(Test :: string(),
                       Correct :: nonempty_string()) ->
    boolean().

compare_constant(Test, [_ | _] = Correct) when is_list(Test) ->
    compare_constant(Test, Correct, 0) =:= 0.

compare_constant([], [], Bits) ->
    Bits;
compare_constant([], [_ | _], _) ->
    1;
compare_constant([C | Test], [] = Correct, Bits) ->
    compare_constant(Test, Correct, Bits bor (C bxor -1));
compare_constant([C1 | Test], [C2 | Correct], Bits) ->
    compare_constant(Test, Correct, Bits bor (C1 bxor C2)).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

