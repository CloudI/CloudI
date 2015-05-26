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

%% @doc <a href="http://msgpack.org/">MessagePack</a> codec for Erlang.
%%
%%      APIs are almost compatible with <a href="http://redmine.msgpack.org/projects/msgpack/wiki/QuickStartC">C API</a>
%%      except for buffering functions (both copying and zero-copying), which are unavailable.
%%
%%   <table border="1">
%%     <caption>Equivalence between Erlang and <a href="http://msgpack.sourceforge.jp/spec">Msgpack type</a> :</caption>
%%     <tr><th>    erlang    </th><th>                            msgpack                                      </th></tr>
%%     <tr><td> integer()    </td><td> pos_fixnum/neg_fixnum/uint8/uint16/uint32/uint64/int8/int16/int32/int64 </td></tr>
%%     <tr><td> float()      </td><td> float/double                                                            </td></tr>
%%     <tr><td> nil          </td><td> nil                                                                     </td></tr>
%%     <tr><td> boolean()    </td><td> boolean                                                                 </td></tr>
%%     <tr><td> binary()     </td><td> fix_raw/raw16/raw32                                                     </td></tr>
%%     <tr><td> list()       </td><td> fix_array/array16/array32                                               </td></tr>
%%     <tr><td> {proplist()} </td><td> fix_map/map16/map32                                                     </td></tr>
%%     <tr><td> [{term(),term{}]|[{}] </td><td> fix_map/map16/map32                                                     </td></tr>
%%     <tr><td> map()        </td><td> fix_map/map16/map32                                                     </td></tr>
%%   </table>
%% @end

-module(msgpack).

-export([pack/1, unpack/1, unpack_stream/1,
         pack/2, unpack/2, unpack_stream/2,
         term_to_binary/1, binary_to_term/1, binary_to_term/2
        ]).

-include("msgpack.hrl").

%% for export
-export_type([object/0, msgpack_map/0, options/0]).
-type object() :: msgpack_term().
-type options() :: msgpack_list_options().

-spec term_to_binary(term()) -> binary().
term_to_binary(Term) ->
    msgpack_term:to_binary(Term).

-spec binary_to_term(binary()) -> term().
binary_to_term(Bin) ->
    msgpack_term:from_binary(Bin, []).

-spec binary_to_term(binary(), [safe]) -> term().
binary_to_term(Bin, Opt) ->
    msgpack_term:from_binary(Bin, Opt).

%% @doc Encode an erlang term into an msgpack binary.
%%      Returns {error, {badarg, term()}} if the input is illegal.
-spec pack(msgpack:object()) -> binary() | {error, {badarg, term()}}.
pack(Term) -> msgpack:pack(Term, []).

-spec pack(msgpack:object(), msgpack:options()) -> binary().
pack(Term, Opts) ->
    Option = parse_options(Opts),
    try
        msgpack_packer:pack(Term, Option)
    catch
        throw:Exception -> {error, Exception}
    end.

%%% @doc Decode an msgpack binary into an erlang terms.
%%%      It only decodes ONLY ONE msgpack packets contained in the binary. No packets should not remain.
%%%      Returns {error, {badarg, term()}} if the input is corrupted.
%%%      Returns {error, incomplete} if the input is not a full msgpack packet (caller should gather more data and try again).
-spec unpack(binary()) -> {ok, msgpack:object()}
                              | {error, not_just_binary} % a term deserilized, but binary remains
                              | {error, incomplete}      % too few binary to deserialize complete binary
                              | {error, {badarg, term()}}.
unpack(Bin) -> unpack(Bin, []).

-spec unpack(binary(), msgpack:options()) -> {ok, msgpack:object()} | {error, any()}.
unpack(Bin, Opts) ->
    case unpack_stream(Bin, Opts) of
        {error, _} = E -> E;
        {Term, <<>>} -> {ok, Term};
        {_, Binary} when is_binary(Binary)
                         andalso byte_size(Binary) > 0 ->
            {error, not_just_binary}
    end.

-spec unpack_stream(binary()) -> {msgpack:object(), binary()}
                                     | {error, incomplete}
                                     | {error, {badarg, term()}}.
unpack_stream(Bin) -> unpack_stream(Bin, []).

-spec unpack_stream(binary(), msgpack:options())->  {msgpack:object(), binary()}
                                                       | {error, incomplete}
                                                       | {error, {badarg, term()}}.
unpack_stream(Bin, Opts0) when is_binary(Bin) ->
    Opts = parse_options(Opts0),
    try
        msgpack_unpacker:unpack_stream(Bin, Opts)
    catch
        throw:Exception -> {error, Exception}
    end;
unpack_stream(Other, _) -> {error, {badarg, Other}}.

%% @private
-spec parse_options(msgpack:options()) -> msgpack_option().

parse_options(Opt) ->
    parse_options(Opt, ?OPTION{original_list=Opt}).

%% @private
-spec parse_options(msgpack:options(), msgpack_option()) -> msgpack_option().
parse_options([], Opt) -> Opt;

parse_options([jsx|TL], Opt0) ->
    Opt = Opt0?OPTION{interface=jsx,
                      map_unpack_fun=msgpack_unpacker:map_unpacker(jsx)},
    parse_options(TL, Opt);
parse_options([jiffy|TL], Opt0) ->
    Opt = Opt0?OPTION{interface=jiffy,
                      map_unpack_fun=msgpack_unpacker:map_unpacker(jiffy)},
    parse_options(TL, Opt);
parse_options([{format,Type}|TL], Opt0)
  when Type =:= jsx; Type =:= jiffy; Type =:= map->
    Opt = Opt0?OPTION{interface=Type,
                      map_unpack_fun=msgpack_unpacker:map_unpacker(Type)},
    parse_options(TL, Opt);

parse_options([{allow_atom,Type}|TL], Opt0) ->
    Opt = case Type of
              none -> Opt0?OPTION{allow_atom=none};
              pack -> Opt0?OPTION{allow_atom=pack}
          end,
    parse_options(TL, Opt);

parse_options([{enable_str,Bool}|TL], Opt0) ->
    Opt = Opt0?OPTION{enable_str=Bool},
    parse_options(TL, Opt);

parse_options([{ext, Module}|TL], Opt0) when is_atom(Module) ->
    Opt = Opt0?OPTION{ext_packer=fun Module:pack_ext/2,
                      ext_unpacker=fun Module:unpack_ext/3},
    parse_options(TL, Opt);
parse_options([{ext, {Packer,Unpacker}}|TL], Opt0) when
      is_function(Packer, 2) andalso
      (is_function(Unpacker, 3) orelse is_function(Unpacker, 2)) ->
    Opt = Opt0?OPTION{ext_packer=Packer, ext_unpacker=Unpacker},
    parse_options(TL, Opt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% unit tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_data()->
    [true, false, null,
     0, 1, 2, 123, 512, 1230, 678908, 16#FFFFFFFFFF,
     -1, -23, -512, -1230, -567898, -16#FFFFFFFFFF,
     -16#80000001,
     123.123, -234.4355, 1.0e-34, 1.0e64,
     [23, 234, 0.23],
     <<"hogehoge">>, <<"243546rf7g68h798j", 0, 23, 255>>,
     <<"hoasfdafdas][">>,
     [0,42, <<"sum">>, [1,2]], [1,42, null, [3]],
     -234, -40000, -16#10000000, -16#100000000,
     42
    ].

enable_str_test() ->
    ?assertEqual(<<167:8, (<<"saitama">>)/binary >>,
                 msgpack:pack(<<"saitama">>, [{enable_str, false}])),
    ?assertEqual(<<196,7,115,97,105,116,97,109,97>>,
                 msgpack:pack(<<"saitama">>, [{enable_str, true}])).

basic_test()->
    Tests = test_data(),
    MatchFun0 = fun(Term) ->
                        {ok, Term} = msgpack:unpack(msgpack:pack(Term)),
                        Term
                end,
    %% MatchFun1 = fun(Term) ->
    %%                     {ok, Term} = msgpack_nif:unpack(msgpack_nif:pack(Term)),
    %%                     Term
    %%             end,
    Tests = lists:map(MatchFun0, Tests).
    %% Tests = lists:map(MatchFun1, Tests).

test_p(Len,Term,OrigBin,Len) ->
    {ok, Term}=msgpack:unpack(OrigBin);

test_p(I,_,OrigBin,Len) when I < Len->
    <<Bin:I/binary, _/binary>> = OrigBin,
    ?assertEqual({error,incomplete}, msgpack:unpack(Bin)).

partial_test()-> % error handling test.
    Term = lists:seq(0, 45),
    Bin=msgpack:pack(Term),
    BinLen = byte_size(Bin),
    [test_p(X, Term, Bin, BinLen) || X <- lists:seq(0,BinLen)].

long_test()->
    Longer = lists:seq(0, 655),
    {ok, Longer} = msgpack:unpack(msgpack:pack(Longer)).


other_test()->
    ?assertEqual({error,incomplete},msgpack:unpack(<<>>)).

error_test()->
    ?assertEqual({error,{badarg, atom}}, msgpack:pack(atom)),
    Term = {"hoge", "hage", atom},
    ?assertEqual({error,{badarg, Term}}, msgpack:pack(Term)).

long_binary_test()->
    A = msgpack:pack(1),
    B = msgpack:pack(10),
    C = msgpack:pack(100),
    {1, Rem0} = msgpack:unpack_stream(<<A/binary, B/binary, C/binary>>),
    {10, Rem1} = msgpack:unpack_stream(Rem0),
    {100, _Rem2} = msgpack:unpack_stream(Rem1),
    ok.

-endif.
