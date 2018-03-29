%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Proplists Extensions Module==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2018 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_proplists).
-author('mjtruog at protonmail dot com').

-export([delete_all/2,
         find_any/2,
         partition/2,
         take_values/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type property() :: {atom(), any()}.
-export_type([property/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete all the instances of the keys provided.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_all(Keys :: list(atom()),
                 List :: list(property())) ->
    list(property()).

delete_all([], List) ->
    List;

delete_all([Key | Keys], List)
    when is_atom(Key) ->
    case lists:keytake(Key, 1, List) of
        {value, _, NewList} ->
            delete_all(Keys, NewList);
        false ->
            delete_all(Keys, List)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if any of the keys provided are present.===
%% @end
%%-------------------------------------------------------------------------

-spec find_any(Keys :: list(atom()),
               List :: list(property())) ->
    boolean().

find_any([], _) ->
    false;

find_any([Key | Keys], List)
    when is_atom(Key) ->
    case lists:keyfind(Key, 1, List) of
        {Key, _} ->
            true;
        false ->
            find_any(Keys, List)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Partition the proplist based on a key.===
%% @end
%%-------------------------------------------------------------------------

-spec partition(Key :: atom(),
                List :: list(property())) ->
    {list(property()), list(property())}.

partition(Key, List)
    when is_atom(Key) ->
    lists:partition(fun({K, _}) -> K == Key end, List).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove many keys from the proplist.===
%% The defaults are provided as a proplist
%% @end
%%-------------------------------------------------------------------------

-spec take_values(DefaultList :: list(property()),
                  List :: list(property())) ->
    list().

take_values(DefaultList, List) ->
    take_values([], DefaultList, List).

take_values(Result, [], List) ->
    lists:reverse(Result, List);

take_values(Result, [{Key, Default} | DefaultList], List)
    when is_atom(Key) ->
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
    [{d, true}] = delete_all([a, b, c], [{a, true}, {d, true}]),
    ok.

find_any_test() ->
    false = find_any([b], [{a, true}, {d, true}]),
    true = find_any([d], [{a, true}, {d, true}]),
    ok.

partition_test() ->
    {[{a, true}, {a, false}],
     [{b, false}]} = partition(a, [{a, true}, {a, false}, {b, false}]),
    ok.

take_values_test() ->
    [1, 5, 3] = take_values([{a, 1}, {b, 2}, {c, 3}],
                            [{b, 5}]),
    ok.

-endif.
