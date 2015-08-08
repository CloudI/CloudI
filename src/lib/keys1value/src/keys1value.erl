%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Keys1Value==
%%% Maintain an associative lookup for a list of keys and 1 value.
%%% The supplied data structure module must have dict interface functions.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(keys1value).
-author('mjtruog [at] gmail (dot) com').

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
    case Module:find(Key, Lookup) of
        {ok, {Keys, _}} ->
            State#keys1value{
                lookup = lists:foldl(fun Module:erase/2, Lookup, Keys)};
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
    {_, Value} = Module:fetch(Key, Lookup),
    Value.

-spec find(Key :: key(),
           keys1value(key(), value())) ->
    value().

find(Key,
     #keys1value{module = Module,
                 lookup = Lookup})
    when not is_list(Key) ->
    case Module:find(Key, Lookup) of
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
        Module:is_key(K, Lookup)
    end, Keys);
is_key(Key,
       #keys1value{module = Module,
                   lookup = Lookup})
    when not is_list(Key) ->
    Module:is_key(Key, Lookup).

-spec new() ->
    keys1value_dict(key(), value()).

new() ->
    new(dict).

-spec new(Module :: module()) ->
    keys1value(key(), value()).

new(Module)
    when is_atom(Module) ->
    {keys1value, Module, Module:new()}.

-spec store(Keys :: keys(),
            Value :: value(),
            State :: keys1value(key(), value())) ->
    keys1value(key(), value()).

store([_ | _] = Keys, Value,
      #keys1value{module = Module,
                  lookup = Lookup} = State) ->
    NewLookup = lists:foldl(fun(K, D) ->
        error = Module:find(K, D),
        Module:store(K, {Keys, Value}, D)
    end, Lookup, Keys),
    State#keys1value{lookup = NewLookup}.

-spec to_list(keys1value(key(), value())) ->
    list({keys(), value()}).

to_list(#keys1value{module = Module,
                    lookup = Lookup}) ->
    to_list(Module:to_list(Lookup), []).

to_list([], Output) ->
    Output;
to_list([{_, {Keys, _} = Entry} | L0], Output) ->
    LN = lists:foldl(fun(K, L1) ->
        lists:keydelete(K, 1, L1)
    end, L0, Keys),
    to_list(LN, [Entry | Output]).

