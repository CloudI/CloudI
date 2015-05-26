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

-module(msgpack_ext_example_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-behaviour(msgpack_ext).

ext_test() ->
    Packer = fun({foobar, Me}, _) ->
                     {ok, {12, term_to_binary(Me)}}
             end,
    Unpacker = fun(12, Bin) ->
                       {ok, {foobar, binary_to_term(Bin)}}
               end,
    Ref = make_ref(),
    Opt = [{ext,{Packer,Unpacker}}],
    Bin = msgpack:pack({foobar, Ref}, Opt),
    {ok, {foobar, Ref}} = msgpack:unpack(Bin, Opt).

uuid_example_test() ->
    Packer =   fun({uuid, UUID}, _) when is_binary(UUID) ->
                       {ok, {42, UUID}}
               end,
    Unpacker = fun(42, Bin0) ->
                       {ok, {uuid, Bin0}}
               end,
    UUID0 = {uuid, <<221,85,73,226,102,90,82,118,40,26,166,74,52,42,61,207>>},
    Opt = [{ext,{Packer,Unpacker}}],
    Bin = msgpack:pack(UUID0, Opt),
    {ok, UUID0} = msgpack:unpack(Bin, Opt).

pack_native({native, Term}, _) when is_pid(Term) orelse
                                 is_reference(Term) orelse
                                 is_port(Term) orelse
                                 is_tuple(Term) orelse
                                 is_function(Term) ->
    {ok, {42, term_to_binary(Term)}}.

unpack_native(42, Bin) ->
    {ok, {native, binary_to_term(Bin)}}.

native_test() ->
    Opt = [{ext, {fun pack_native/2, fun unpack_native/2}}],
    Term = {native, {self(), make_ref(), foobar, fun() -> ok end}},
    {ok, Term} = msgpack:unpack(msgpack:pack(Term, Opt), Opt).

pack_ext(T, O) -> pack_native(T, O).
unpack_ext(I, B, _) -> unpack_native(I, B).

behaviour_test() ->
    Opt = [{ext, ?MODULE}],
    Term = {native, {self(), make_ref(), foobar, fun() -> ok end}},
    {ok, Term} = msgpack:unpack(msgpack:pack(Term, Opt), Opt).


ext_typecode_range_test() ->
    %% typecode range from msgpack spec. [-128,-1] is the "reserved"
    %% range, [0,127] is the "user-defined" range.
    TypecodeMin = -128,
    TypecodeMax = 127,
    Packer = fun ({thing, N}, _) ->
                     {ok, {N, msgpack:pack(N)}}
             end,
    Unpacker = fun(N, Bin, _) ->
                       Result = msgpack:unpack(Bin),
                       ?assertEqual({ok, N}, Result),
                       Result
               end,
    Opt = [{ext,{Packer,Unpacker}}],
    %% it should be possible to use an uncontroversial ext type code:
    Enc = msgpack:pack({thing,1}, Opt),
    ?assertMatch({ok, 1}, msgpack:unpack(Enc, Opt)),
    %% it should be possible to use ext typecodes covering the entire
    %% range specified in the msgpack specification:
    [begin
         Encoded = msgpack:pack({thing, N}, Opt),
         Result = msgpack:unpack(Encoded, Opt),
         ?assertMatch({ok, N}, Result)
     end || N <- lists:seq(TypecodeMin,TypecodeMax)],
    %% using codes outside the allowed range should fail:
    [?assertError({case_clause, _}, msgpack:pack({thing, N}, Opt))
     || N <- [-129, 128]],
    ok.
