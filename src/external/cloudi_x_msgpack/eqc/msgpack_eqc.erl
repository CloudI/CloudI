%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2014 UENISHI Kota
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

-module(msgpack_eqc).


-ifdef(TEST).
-ifdef(EQC).

-compile(export_all).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(NUMTESTS, 16).
-define(QC_OUT(P),
        eqc:on_output(fun(Str, Args) ->
                              io:format(user, Str, Args) end, P)).
-define(_assertProp(S),
        {timeout, ?NUMTESTS * 10,
         ?_assert(quickcheck(numtests(?NUMTESTS, ?QC_OUT(S))))}).

eqc_test_() ->
    {inparallel,
     [
      ?_assertProp(prop_msgpack()),
      ?_assertProp(prop_msgpack([{format, jiffy}])),
      ?_assertProp(prop_msgpack([{format, jsx}]))
      ]}.

prop_msgpack() ->
    ?FORALL(Obj, msgpack_object(),
            begin
                {ok, Obj} =:= msgpack:unpack(msgpack:pack(Obj))
            end).

prop_msgpack(Options) ->
    ?FORALL(Obj, msgpack_object(),
            begin
                {ok, Obj} =:= msgpack:unpack(msgpack:pack(Obj, Options), Options)
            end).

msgpack_object() ->
    oneof(container_types() ++ primitive_types()).

container_types() ->
    [ fix_array(), array16() ].
%% TODO: add map

primitive_types() ->
    [null(),
     positive_fixnum(), negative_fixnum(),
     int8(), int16(), int32(), int64(),
     uint8(), uint16(), uint32(), uint64(),
     eqc_gen:real(), eqc_gen:bool(),
     fix_raw(), raw16(), raw32()
    ].
          %% fix_raw(), raw16(), raw32()]).

positive_fixnum() -> choose(0, 127).
negative_fixnum() -> choose(-32, -1).

int8() ->  choose(-16#80, 16#7F).
int16() -> oneof([choose(-16#8000, -16#81),
                  choose(16#80, 16#7FFF)]).
int32() -> oneof([choose(-16#80000000, -16#8001),
                  choose(16#10000, 16#7FFFFFFF)]).
int64() -> oneof([choose(-16#8000000000000000, -16#80000001),
                  choose(16#100000000, 16#7FFFFFFFFFFFFFFF)]).

uint8() ->  choose(0, 16#FF).
uint16() -> choose(16#100, 16#FFFF).
uint32() -> choose(16#10000, 16#FFFFFFFF).
uint64() -> choose(16#100000000, 16#FFFFFFFFFFFFFFFF).

null() -> null.

fix_raw() ->
    ?LET(Integer, choose(0, 31),
         ?LET(Binary, binary(Integer), Binary)).

raw16() ->
    ?LET(Integer, uint16(),
         ?LET(Binary, binary(Integer), Binary)).

raw32() ->
    ?LET(Binary, binary(65537), Binary).

fix_array() ->
    eqc_gen:resize(16, eqc_gen:list(oneof(primitive_types()))).

array16() ->
    eqc_gen:resize(128, eqc_gen:list(oneof(primitive_types()))).

-endif.
-endif.
