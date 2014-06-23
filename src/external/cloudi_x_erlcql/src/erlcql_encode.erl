%% Copyright (c) 2013-2014 Krzysztof Rutka
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.

%% @doc Native protocol request encoding.
%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
-module(erlcql_encode).

%% API
-export([frame/3]).
-export([startup/2,
         credentials/1,
         options/0,
         'query'/2,
         prepare/1,
         execute/3,
         register/1]).

-include("erlcql.hrl").

%%-----------------------------------------------------------------------------
%% API function
%%-----------------------------------------------------------------------------

%% @doc Encodes the entire request frame.
-spec frame(request(), tuple(), integer()) -> Frame :: iolist().
frame({Opcode, Payload}, {Compression, _}, Stream) ->
    OpcodeByte = opcode(Opcode),
    {CompressionBit, Payload2} = maybe_compress(Compression, Payload),
    Length = int(iolist_size(Payload2)),
    [<<?REQUEST:1, ?VERSION:7, 0:7, CompressionBit:1>>,
     Stream, OpcodeByte, Length, Payload2].

%% @doc Encodes the startup request message body.
-spec startup(compression(), bitstring()) -> {startup, iolist()}.
startup(false, CQLVersion) ->
    {startup, string_map([{<<"CQL_VERSION">>, CQLVersion}])};
startup(Compression, CQLVersion) ->
    {startup, string_map([{<<"CQL_VERSION">>, CQLVersion},
                          {<<"COMPRESSION">>, compression(Compression)}])}.

%% @doc Encodes the credentials request message body.
-spec credentials([{K :: bitstring(), V :: bitstring()}]) ->
          {credentials, iolist()}.
credentials(Informations) ->
    {credentials, string_map(Informations)}.

%% @doc Encodes the options request message body.
-spec options() -> {options, iolist()}.
options() ->
    {options, []}.

%% @doc Encodes the 'query' request message body.
-spec 'query'(iodata(), consistency()) -> {'query', iolist()}.
'query'(QueryString, Consistency) ->
    {'query', [long_string(QueryString), consistency(Consistency)]}.

%% @doc Encodes the prepare request message body.
-spec prepare(iodata()) -> {prepare, iolist()}.
prepare(QueryString) ->
    {prepare, [long_string(QueryString)]}.

%% @doc Encodes the execute request message body.
-spec execute(binary(), values(), consistency()) -> {execute, iolist()}.
execute(QueryId, Values, Consistency) ->
    BinaryValues = erlcql_convert:to_binary(Values),
    {execute, [short_bytes(QueryId), BinaryValues,
               consistency(Consistency)]}.

%% @doc Encodes the register request message body.
-spec register([event_type()]) -> {register, iolist()}.
register(Events) ->
    {register, event_list(Events)}.

%%-----------------------------------------------------------------------------
%% Encode functions
%%-----------------------------------------------------------------------------

-spec int(integer()) -> binary().
int(X) ->
    <<X:?INT>>.

-spec short(integer()) -> binary().
short(X) ->
    <<X:?SHORT>>.

-spec string2(bitstring()) -> iolist().
string2(String) ->
    Length = iolist_size(String),
    [short(Length), String].

-spec long_string(iolist()) -> iolist().
long_string(String) ->
    Length = iolist_size(String),
    [int(Length), String].

-spec short_bytes(binary()) -> iolist().
short_bytes(Bytes) ->
    Length = iolist_size(Bytes),
    [short(Length), Bytes].

-spec string_map([{K :: bitstring(), V :: bitstring()}]) -> iolist().
string_map(KeyValues) ->
    N = length(KeyValues),
    [short(N) | [[string2(Key), string2(Value)]
                 || {Key, Value} <- KeyValues]].

-spec opcode(atom()) -> integer().
opcode(startup) -> 1;
opcode(credentials) -> 4;
opcode(options) -> 5;
opcode('query') -> 7;
opcode(prepare) -> 9;
opcode(execute) -> 10;
opcode(register) -> 11.

-spec consistency(consistency()) -> binary().
consistency(any) -> short(0);
consistency(one) -> short(1);
consistency(two) -> short(2);
consistency(three) -> short(3);
consistency(quorum) -> short(4);
consistency(all) -> short(5);
consistency(local_quorum) -> short(6);
consistency(each_quorum) -> short(7).

-spec event(event_type()) -> bitstring().
event(topology_change) -> <<"TOPOLOGY_CHANGE">>;
event(status_change) -> <<"STATUS_CHANGE">>;
event(schema_change) -> <<"SCHEMA_CHANGE">>.

-spec event_list([event_type()]) -> iolist().
event_list(Events) ->
    N = length(Events),
    [short(N) | [string2(event(Event)) || Event <- Events]].

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

%% @doc Compresses the payload if compression is enabled.
-spec maybe_compress(compression(), iolist()) -> {0 | 1, iolist()}.
maybe_compress(false, Payload) ->
    {0, Payload};
maybe_compress(_Any, []) ->
    {0, <<>>};
maybe_compress(snappy, Payload) ->
    {ok, CompressedPayload} = snappy:compress(Payload),
    {1, CompressedPayload};
maybe_compress(lz4, Payload) ->
    PayloadBinary = iolist_to_binary(Payload),
    Size = byte_size(PayloadBinary),
    {ok, CompressedPayload} = lz4:compress(PayloadBinary),
    {1, <<Size:32, CompressedPayload/binary>>}.

-spec compression(compression()) -> bitstring().
compression(snappy) -> <<"snappy">>;
compression(lz4) -> <<"lz4">>.
