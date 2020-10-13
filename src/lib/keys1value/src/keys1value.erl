%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Keys1Value==
%%% Maintain an associative lookup for a list of keys and 1 value.
%%% The supplied data structure module must have dict interface functions
%%% (unless the module is maps).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(keys1value).
-author('mjtruog at protonmail dot com').

%% external interface
-export([erase/2,
         fetch/2,
         find/2,
         is_key/2,
         new/0,
         new/1,
         store/3,
         to_list/1]).

-record(keys1value,
    {
        module :: module(),
        lookup :: any()
    }).
-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.
-type keys1value_dict(Key, Value) ::
    {keys1value,
     dict,
     dict_proxy(Key, {list(Key), Value})}.
-type keys1value(Key, Value) ::
    keys1value_dict(Key, Value) |
    #keys1value{}.
-export_type([keys1value/2]).
-type key() :: any(). % not a list()
-type keys() :: nonempty_list(key()).
-type value() :: any().

-spec erase(Key :: key(),
            State :: keys1value(key(), value())) ->
    keys1value(key(), value()).

erase(Key,
      #keys1value{module = Module,
                  lookup = Lookup} = State)
    when not is_list(Key) ->
    case module_find(Module, Key, Lookup) of
        {ok, {Keys, _}} ->
            State#keys1value{
                lookup = lists:foldl(module_erase_f(Module), Lookup, Keys)};
        error ->
            State
    end.

-spec fetch(Key :: key(),
            keys1value(key(), value())) ->
    value().

fetch(Key,
      #keys1value{module = Module,
                  lookup = Lookup})
    when not is_list(Key) ->
    {_, Value} = module_fetch(Module, Key, Lookup),
    Value.

-spec find(Key :: key(),
           keys1value(key(), value())) ->
    {ok, value()} | error.

find(Key,
     #keys1value{module = Module,
                 lookup = Lookup})
    when not is_list(Key) ->
    case module_find(Module, Key, Lookup) of
        {ok, {_, Value}} ->
            {ok, Value};
        error ->
            error
    end.

-spec is_key(Key :: keys() | key(),
             keys1value(key(), value())) ->
    boolean().

is_key([_ | _] = Keys,
       #keys1value{module = Module,
                   lookup = Lookup}) ->
    lists:any(fun(K) ->
        module_is_key(Module, K, Lookup)
    end, Keys);
is_key(Key,
       #keys1value{module = Module,
                   lookup = Lookup})
    when not is_list(Key) ->
    module_is_key(Module, Key, Lookup).

-spec new() ->
    keys1value_dict(key(), value()).

new() ->
    new(dict).

-spec new(Module :: module()) ->
    keys1value(key(), value()).

new(Module)
    when is_atom(Module) ->
    {keys1value, Module, module_new(Module)}.

-spec store(Keys :: keys(),
            Value :: value(),
            State :: keys1value(key(), value())) ->
    keys1value(key(), value()).

store([_ | _] = Keys, Value,
      #keys1value{module = Module,
                  lookup = Lookup} = State) ->
    NewLookup = lists:foldl(fun(K, D) ->
        error = module_find(Module, K, D),
        module_store(Module, K, {Keys, Value}, D)
    end, Lookup, Keys),
    State#keys1value{lookup = NewLookup}.

-spec to_list(keys1value(key(), value())) ->
    list({keys(), value()}).

to_list(#keys1value{module = Module,
                    lookup = Lookup}) ->
    to_list(module_to_list(Module, Lookup), []).

to_list([], Output) ->
    Output;
to_list([{_, {Keys, _} = Entry} | L0], Output) ->
    LN = lists:foldl(fun(K, L1) ->
        lists:keydelete(K, 1, L1)
    end, L0, Keys),
    to_list(LN, [Entry | Output]).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-compile({inline,
          [module_erase_f/1,
           module_fetch/3,
           module_find/3,
           module_is_key/3,
           module_new/1,
           module_store/4,
           module_to_list/2]}).

module_erase_f(maps) ->
    fun maps:remove/2;
module_erase_f(Module) ->
    fun Module:erase/2.

module_fetch(maps, Key, Lookup) ->
    maps:get(Key, Lookup);
module_fetch(Module, Key, Lookup) ->
    Module:fetch(Key, Lookup).

module_find(Module, Key, Lookup) ->
    Module:find(Key, Lookup).

module_is_key(Module, Key, Lookup) ->
    Module:is_key(Key, Lookup).

module_new(Module) ->
    Module:new().

module_store(maps, Key, Value, Lookup) ->
    maps:put(Key, Value, Lookup);
module_store(Module, Key, Value, Lookup) ->
    Module:store(Key, Value, Lookup).

module_to_list(Module, Lookup) ->
    Module:to_list(Lookup).

