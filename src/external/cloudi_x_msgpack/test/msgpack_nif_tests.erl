%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2013 UENISHI Kota
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
%%

-module(msgpack_nif_tests).

-undef(NIF).
-ifdef(NIF).

-import(msgpack_nif, [pack/1, unpack/1]).

-include_lib("eunit/include/eunit.hrl").

msgpack_props_test_() ->
    {timeout,10000, ?_assertEqual([],proper:module(msgpack_props))}.

unpack_test_() ->
    [
     {"not binary",
      ?_assertEqual({error, {badarg, []}}, unpack([]))},

     {"incomplete: null binary",
      ?_assertEqual({error, incomplete}, unpack(<<>>))},
     
     {"incomplete: unknown binary",
      ?_assertEqual({error, incomplete}, unpack(<<16#DA>>))}
    ].

array_test_()->
    [
        {"length 16",
            fun() ->
                    List = lists:seq(0, 16),
                    Binary = pack(List),
                    ?assertEqual({ok, List}, unpack(Binary))
            end},
        {"length 32",
            fun() ->
                    List = lists:seq(0, 16#010000),
                    Binary = pack(List),
                    ?assertEqual({ok, List}, unpack(Binary))
            end},
        {"empty",
            fun() ->
                    EmptyList = [],
                    Binary = pack(EmptyList),
                    ?assertEqual({ok, EmptyList}, unpack(Binary))
            end}
    ].


map_test_()->
    [
        {"length 16",
            fun() ->
                    Map = {[ {X, X * 2} || X <- lists:seq(0, 16) ]},
                    Binary = pack(Map),
                    ?assertEqual({ok, Map}, unpack(Binary))
            end},
        {"length 32",
            fun() ->
                    Map = {[ {X, X * 2} || X <- lists:seq(0, 16#010000) ]},
                    Binary = pack(Map),
                    ?assertEqual({ok, Map}, unpack(Binary))
            end},
        {"empty",
            fun() ->
                    EmptyMap = {[]},
                    Binary = pack(EmptyMap),
                    ?assertEqual({ok, EmptyMap}, unpack(Binary))
            end}
    ].

int_test_() ->
    [
        {"",
            fun() ->
                    Term = -2147483649,
                    Binary = pack(Term),
                    ?assertEqual({ok, Term}, unpack(Binary))
            end}
    ].

error_test_()->
    [
        {"badarg atom",
            ?_assertEqual({error, {badarg, atom}},
                          pack(atom))},
        {"badarg tuple",
            fun() ->
                    Term = {"hoge", "hage", atom},
                    ?assertEqual({error, {badarg, Term}},
                                 pack(Term))
            end}
    ].

binary_test_() ->
    [
        {"0 byte",
            fun() ->
                    Binary = pack(<<>>),
                    ?assertEqual({ok, <<>>}, unpack(Binary))
            end}
    ].

-endif.

%% long_binary_test_()->
%%     [
%%         {"long binary",
%%             fun() ->
%%                     A = pack(1),
%%                     B = pack(10),
%%                     C = pack(100),
%%                     ?assertEqual({[1,10,100], <<>>},
%%                                  unpack(list_to_binary([A, B, C])))
%%             end}
%%     ].

%% benchmark_test()->
%%     Data = [test_data() || _ <- lists:seq(0, 10000)],
%%     {ok, S} = ?debugTime("  serialize", pack(Data)),
%%     {ok, Data} = ?debugTime("deserialize", unpack(S)),
%%     ?debugFmt("for ~p KB test data.", [byte_size(S) div 1024]),
%%     ok.

