%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Key2Value==
%%% Maintain 2 lookups for 2 separate keys and 1 value.
%%% The interface creates a bidirectional lookup where key1 can store
%%% multiple key2 associations to the same value.
%%% The supplied data structure module must have dict interface functions
%%% (unless the module is maps).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(key2value).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([erase/3,
         erase1/2,
         erase2/2,
         fetch1/2,
         fetch2/2,
         find1/2,
         find2/2,
         fold1/3,
         fold2/3,
         is_key1/2,
         is_key2/2,
         new/0,
         new/1,
         store/4,
         update1/3,
         update2/3]).

-record(key2value,
    {
        module :: module(),
        lookup1 :: any(),
        lookup2 :: any()
    }).
-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.
-type key2value_dict(Key1, Key2, Value) ::
    {key2value,
     dict,
     dict_proxy(Key1, {list(Key2), Value}),
     dict_proxy(Key2, {list(Key1), Value})}.
-type key2value(Key1, Key2, Value) ::
    key2value_dict(Key1, Key2, Value) |
    #key2value{}.
-export_type([key2value/3]).
-type key1() :: any().
-type key2() :: any().
-type value() :: any().

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec erase(K1 :: key1(),
            K2 :: key2(),
            State :: key2value(key1(), key2(), value())) ->
    key2value(key1(), key2(), value()).

erase(K1, K2,
      #key2value{module = Module,
                 lookup1 = Lookup1,
                 lookup2 = Lookup2} = State) ->
    case module_find(Module, K1, Lookup1) of
        {ok, {[K2], _}} ->
            case module_find(Module, K2, Lookup2) of
                {ok, {[K1], _}} ->
                    State#key2value{
                        lookup1 = module_erase(Module, K1, Lookup1),
                        lookup2 = module_erase(Module, K2, Lookup2)};
                {ok, {L1, V1}} ->
                    State#key2value{
                        lookup1 = module_erase(Module, K1, Lookup1),
                        lookup2 = module_store(Module, K2,
                                               {lists:delete(K1, L1), V1},
                                               Lookup2)};
                error ->
                    State
            end;
        {ok, {L2, V2}} ->
            case module_find(Module, K2, Lookup2) of
                {ok, {[K1], _}} ->
                    State#key2value{
                        lookup1 = module_store(Module, K1,
                                               {lists:delete(K2, L2), V2},
                                               Lookup1),
                        lookup2 = module_erase(Module, K2, Lookup2)};
                {ok, {L1, V1}} ->
                    State#key2value{
                        lookup1 = module_store(Module, K1,
                                               {lists:delete(K2, L2), V2},
                                               Lookup1),
                        lookup2 = module_store(Module, K2,
                                               {lists:delete(K1, L1), V1},
                                               Lookup2)};
                error ->
                    State
            end;
        error ->
            State
    end.

-spec erase1(K :: key1(),
             State :: key2value(key1(), key2(), value())) ->
    key2value(key1(), key2(), value()).

erase1(K,
       #key2value{module = Module,
                  lookup1 = Lookup1} = State) ->
    case module_find(Module, K, Lookup1) of
        {ok, {L, _}} ->
            lists:foldl(fun(K2, D) ->
                erase(K, K2, D)
            end, State, L);
        error ->
            State
    end.

-spec erase2(K :: key2(),
             State :: key2value(key1(), key2(), value())) ->
    key2value(key1(), key2(), value()).

erase2(K,
       #key2value{module = Module,
                  lookup2 = Lookup2} = State) ->
    case module_find(Module, K, Lookup2) of
        {ok, {L, _}} ->
            lists:foldl(fun(K1, D) ->
                erase(K1, K, D)
            end, State, L);
        error ->
            State
    end.

-spec fetch1(K :: key1(),
             key2value(key1(), key2(), value())) ->
    {list(), any()}.

fetch1(K,
       #key2value{module = Module,
                  lookup1 = Lookup1}) ->
    module_fetch(Module, K, Lookup1).

-spec fetch2(K :: key2(),
             key2value(key1(), key2(), value())) ->
    {list(), any()}.

fetch2(K,
       #key2value{module = Module,
                  lookup2 = Lookup2}) ->
    module_fetch(Module, K, Lookup2).

-spec find1(K :: key1(),
            State :: key2value(key1(), key2(), value())) ->
    {ok, {list(), any()}} |
    error.

find1(K,
      #key2value{module = Module,
                 lookup1 = Lookup1}) ->
    module_find(Module, K, Lookup1).

-spec find2(K :: key2(),
            State :: key2value(key1(), key2(), value())) ->
    {ok, {list(), any()}} |
    error.

find2(K,
      #key2value{module = Module,
                 lookup2 = Lookup2}) ->
    module_find(Module, K, Lookup2).

-spec fold1(F :: fun((key1(), list(key2()), value(), any()) -> any()),
            A0 :: any(),
            State :: key2value(key1(), key2(), value())) ->
    any().

fold1(F, A0,
      #key2value{module = Module,
                 lookup1 = Lookup1})
    when is_function(F, 4) ->
    module_fold(Module, fun(K1, {L1, V1}, AN) ->
        F(K1, L1, V1, AN)
    end, A0, Lookup1).

-spec fold2(F :: fun((list(key1()), key2(), value(), any()) -> any()),
            A0 :: any(),
            State :: key2value(key1(), key2(), value())) ->
    any().

fold2(F, A0,
      #key2value{module = Module,
                 lookup2 = Lookup2})
    when is_function(F, 4) ->
    module_fold(Module, fun(K2, {L2, V2}, AN) ->
        F(L2, K2, V2, AN)
    end, A0, Lookup2).

-spec is_key1(K :: key1(),
              key2value(key1(), key2(), value())) ->
    boolean().

is_key1(K,
        #key2value{module = Module,
                   lookup1 = Lookup1}) ->
    module_is_key(Module, K, Lookup1).

-spec is_key2(K :: key2(),
              key2value(key1(), key2(), value())) ->
    boolean().

is_key2(K,
        #key2value{module = Module,
                   lookup2 = Lookup2}) ->
    module_is_key(Module, K, Lookup2).

-spec new() ->
    key2value_dict(key1(), key2(), value()).

new() ->
    #key2value{module = dict,
               lookup1 = dict:new(),
               lookup2 = dict:new()}.

-spec new(Module :: atom()) ->
    key2value(key1(), key2(), value()).

new(Module)
    when is_atom(Module) ->
    #key2value{module = Module,
               lookup1 = module_new(Module),
               lookup2 = module_new(Module)}.

-spec store(K1 :: key1(),
            K2 :: key2(),
            V :: value(),
            key2value(key1(), key2(), value())) ->
    key2value(key1(), key2(), value()).

store(K1, K2, V,
      #key2value{module = Module,
                 lookup1 = Lookup1,
                 lookup2 = Lookup2} = State) ->
    K1L = [K1],
    K2L = [K2],
    F1 = fun({L, _}) ->
        {lists:umerge(L, K2L), V}
    end,
    F2 = fun({L, _}) ->
        {lists:umerge(L, K1L), V}
    end,
    State#key2value{lookup1 = module_update(Module, K1, F1, {K2L, V}, Lookup1),
                    lookup2 = module_update(Module, K2, F2, {K1L, V}, Lookup2)}.

-spec update1(K1 :: key1(),
              F :: fun((value()) -> value()),
              key2value(key1(), key2(), value())) ->
    key2value(key1(), key2(), value()).

update1(K1, F,
        #key2value{module = Module,
                   lookup1 = Lookup1,
                   lookup2 = Lookup2} = State)
    when is_function(F, 1) ->
    {ok, {K2L, _}} = module_find(Module, K1, Lookup1),
    FN = fun({L, V}) ->
        {L, F(V)}
    end,
    NewLookup2 = lists:foldl(fun(K2, NextLookup2) ->
        module_update(Module, K2, FN, NextLookup2)
    end, Lookup2, K2L),
    State#key2value{lookup1 = module_update(Module, K1, FN, Lookup1),
                    lookup2 = NewLookup2}.

-spec update2(K2 :: key2(),
              F :: fun((value()) -> value()),
              key2value(key1(), key2(), value())) ->
    key2value(key1(), key2(), value()).

update2(K2, F,
        #key2value{module = Module,
                   lookup1 = Lookup1,
                   lookup2 = Lookup2} = State)
    when is_function(F, 1) ->
    {ok, {K1L, _}} = module_find(Module, K2, Lookup2),
    FN = fun({L, V}) ->
        {L, F(V)}
    end,
    NewLookup1 = lists:foldl(fun(K1, NextLookup1) ->
        module_update(Module, K1, FN, NextLookup1)
    end, Lookup1, K1L),
    State#key2value{lookup1 = NewLookup1,
                    lookup2 = module_update(Module, K2, FN, Lookup2)}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-compile({inline,
          [module_erase/3,
           module_fetch/3,
           module_find/3,
           module_fold/4,
           module_is_key/3,
           module_new/1,
           module_store/4,
           module_update/4,
           module_update/5]}).

module_erase(maps, Key, Lookup) ->
    maps:remove(Key, Lookup);
module_erase(Module, Key, Lookup) ->
    Module:erase(Key, Lookup).

module_fetch(maps, Key, Lookup) ->
    maps:get(Key, Lookup);
module_fetch(Module, Key, Lookup) ->
    Module:fetch(Key, Lookup).

module_find(Module, Key, Lookup) ->
    Module:find(Key, Lookup).

module_fold(Module, F, A, Lookup) ->
    Module:fold(F, A, Lookup).

module_is_key(Module, Key, Lookup) ->
    Module:is_key(Key, Lookup).

module_new(Module) ->
    Module:new().

module_store(maps, Key, Value, Lookup) ->
    maps:put(Key, Value, Lookup);
module_store(Module, Key, Value, Lookup) ->
    Module:store(Key, Value, Lookup).

module_update(maps, Key, F, Lookup) ->
    maps:update_with(Key, F, Lookup);
module_update(Module, Key, F, Lookup) ->
    Module:update(Key, F, Lookup).

module_update(maps, Key, F, Value, Lookup) ->
    maps:update_with(Key, F, Value, Lookup);
module_update(Module, Key, F, Value, Lookup) ->
    Module:update(Key, F, Value, Lookup).

