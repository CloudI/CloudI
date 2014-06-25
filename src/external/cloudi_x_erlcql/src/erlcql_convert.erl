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

%% @doc Value encoding/decoding module.
%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
-module(erlcql_convert).

-export([from_binary/2, from_null/1,
         to_binary/1, to_binary/2]).

-include("erlcql.hrl").

%% From binary ----------------------------------------------------------------

-spec from_binary(option(), binary()) -> type().
from_binary(ascii, Binary) ->
    Binary;
from_binary(bigint, <<Int:64/signed>>) ->
    Int;
from_binary(blob, Binary) ->
    Binary;
from_binary(boolean, <<_:7, Int:1>>) ->
    Int == 1;
from_binary(counter, Value) ->
    from_binary(bigint, Value);
from_binary(decimal, <<Scale:32/signed, Binary/binary>>) ->
    Size = byte_size(Binary) * 8,
    <<Value:Size/signed>> = Binary,
    Value * math:pow(10, -Scale);
from_binary(double, <<Float:64/float>>) ->
    Float;
from_binary(float, <<Float:32/float>>) ->
    Float;
from_binary(inet, <<A:8, B:8, C:8, D:8>>) ->
    {A, B, C, D};
from_binary(inet, <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>) ->
    {A, B, C, D, E, F, G, H};
from_binary(int, <<Int:?INT>>) ->
    Int;
from_binary(text, Binary) ->
    Binary;
from_binary(timestamp, Value) ->
    from_binary(bigint, Value);
from_binary(timeuuid, Value) ->
    from_binary(uuid, Value);
from_binary(uuid, <<_:128>> = Uuid) ->
    uuid_to_string(Uuid);
from_binary(varchar, Binary) ->
    Binary;
from_binary(varint, Value) ->
    decode_signed(Value);
from_binary({list, Type}, <<N:?SHORT, Data/binary>>) ->
    list_from_binary(N, Type, Data, []);
from_binary({set, Type}, Value) ->
    from_binary({list, Type}, Value);
from_binary({map, KeyType, ValueType}, <<N:?SHORT, Data/binary>>) ->
    map_from_binary(N, {KeyType, ValueType}, Data, []);
from_binary({custom, _Name}, Binary) ->
    Binary.

-spec from_null(option()) -> collection_type().
from_null({list, _}) ->
    [];
from_null({set, _}) ->
    [];
from_null({map, _, _}) ->
    [];
from_null(_) ->
    null.

-spec list_from_binary(integer(), option_id(),
                       binary(), erlcql_list()) -> erlcql_list().
list_from_binary(0, _Type, <<>>, Values) ->
    lists:reverse(Values);
list_from_binary(N, Type, <<Size:?SHORT, Value:Size/binary,
                            Rest/binary>>, Values) ->
    Value2 = from_binary(Type, Value),
    list_from_binary(N - 1, Type, Rest, [Value2 | Values]).

-spec map_from_binary(integer(), {option_id(), option_id()},
                      binary(), erlcql_map()) -> erlcql_map().
map_from_binary(0, _Types, <<>>, Values) ->
    lists:reverse(Values);
map_from_binary(N, {KeyType, ValueType} = Types,
                <<KeySize:?SHORT, Key:KeySize/binary, ValueSize:?SHORT,
                  Value:ValueSize/binary, Rest/binary>>, Values) ->
    Key2 = from_binary(KeyType, Key),
    Value2 = from_binary(ValueType, Value),
    map_from_binary(N - 1, Types, Rest, [{Key2, Value2} | Values]).

-spec uuid_to_string(binary()) -> erlcql:uuid().
uuid_to_string(<<U0:32, U1:16, U2:16, U3:16, U4:48>>) ->
    Format = "~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
    iolist_to_binary(io_lib:format(Format, [U0, U1, U2, U3, U4])).

-spec decode_signed(binary()) -> integer().
decode_signed(Binary) ->
    Size = byte_size(Binary) * 8,
    <<Int:Size/signed>> = Binary,
    Int.

%% To binary ------------------------------------------------------------------

-spec to_binary(Values) -> Binaries when
      Values :: [Value | TypedValue],
      Value :: type() | null,
      TypedValue :: {Type, Value},
      Type :: option(),
      Binaries :: [iodata()].
to_binary(Values) ->
    list_to_binary2(Values, int).

-spec to_binary(Type, Value) -> Binary when
      Type :: option(),
      Value :: type(),
      Binary :: iodata() | null.
to_binary(_, null) ->
    null;
to_binary(_, undefined) ->
    null;
to_binary(ascii, Binary) ->
    Binary;
to_binary(bigint, Int) ->
    <<Int:64>>;
to_binary(blob, Binary) ->
    Binary;
to_binary(boolean, true) ->
    <<1>>;
to_binary(boolean, false) ->
    <<0>>;
to_binary(counter, Int) ->
    to_binary(bigint, Int);
to_binary(decimal, Float) ->
    float_to_decimal(Float, 0);
to_binary(double, Float) ->
    <<Float:64/float>>;
to_binary(float, Float) ->
    <<Float:32/float>>;
to_binary(inet, {A, B, C, D}) ->
    <<A:8, B:8, C:8, D:8>>;
to_binary(inet, {A, B, C, D, E, F, G, H}) ->
    <<A:16, B:16, C:16, D:16, E:16, F:16, G:16, H:16>>;
to_binary(int, Int) ->
    ?int(Int);
to_binary(text, Binary) ->
    Binary;
to_binary(timestamp, Timestamp) ->
    to_binary(bigint, Timestamp);
to_binary(timeuuid, UUID) ->
    to_binary(uuid, UUID);
to_binary(uuid, UUID) ->
    uuid_to_binary(UUID);
to_binary(varchar, Binary) ->
    Binary;
to_binary(varint, Int) ->
    encode_signed(Int, 1);
to_binary({list, Type}, List) ->
    List2 = [{Type, X} || X <- List],
    list_to_binary2(List2, short);
to_binary({set, Type}, Set) ->
    to_binary({list, Type}, Set);
to_binary({map, KeyType, ValueType}, Map) ->
    Map2 = [{{KeyType, Key}, {ValueType, Value}} || {Key, Value} <- Map],
    map_to_binary(Map2, short);
to_binary({custom, _Name}, Binary) ->
    Binary.

-spec encode(Value | TypedValue, Size) -> Binary when
      Value :: type() | null,
      TypedValue :: {Type, Value},
      Type :: option(),
      Size :: int | short,
      Binary :: iodata().
encode(null, _) ->
    <<-1:32>>;
encode({Type, Value}, Size) ->
    encode(to_binary(Type, Value), Size);
encode(Value, int) ->
    N = iolist_size(Value),
    [?int(N), Value];
encode(Value, short) ->
    N = iolist_size(Value),
    [?short(N), Value].

-spec list_to_binary2(List, Size) -> Binary when
      List :: [Value | TypedValue],
      Value :: type() | null,
      TypedValue :: {Type, Value},
      Type :: option(),
      Size :: int | short,
      Binary :: iolist().
list_to_binary2(BytesList, Size) ->
    N = length(BytesList),
    [?short(N) | [encode(Bytes, Size) || Bytes <- BytesList]].

-spec map_to_binary(Map, Size) -> Binary when
      Map :: [{T, T}],
      T :: Value | TypedValue,
      Value :: type() | null,
      TypedValue :: {Type, Value},
      Type :: option(),
      Size :: int | short,
      Binary :: iolist().
map_to_binary(BytesMap, Size) ->
    N = length(BytesMap),
    [?short(N) | [[encode(Key, Size), encode(Value, Size)]
                  || {Key, Value} <- BytesMap]].

-spec uuid_to_binary(UUID) -> Binary when
      UUID :: uuid(),
      Binary :: binary().
uuid_to_binary(UUID) ->
    Parts = re:split(UUID, <<"-">>),
    [A, B, C, D, E] = [part_to_integer(Part) || Part <- Parts],
    <<A:32, B:16, C:16, D:16, E:48>>.

-spec part_to_integer(Binary) -> Integer when
      Binary :: bitstring(),
      Integer :: integer().
part_to_integer(Binary) ->
    list_to_integer(bitstring_to_list(Binary), 16).

-spec float_to_decimal(float() | integer(), integer()) -> binary().
float_to_decimal(Float, Scale) ->
    Round = round(Float),
    if
        Round == Float ->
            case Round rem 10 of
                0 ->
                    float_to_decimal(Round div 10, Scale - 1);
                _ ->
                    Bin = encode_signed(Round, 1),
                    <<Scale:32, Bin/binary>>
            end;
        true ->
            float_to_decimal(Float * 10, Scale + 1)
    end.

-spec encode_signed(integer(), integer()) -> binary().
encode_signed(Int, Bytes) ->
    Min = - 1 bsl (8 * Bytes - 1),
    Max = 1 bsl (8 * Bytes - 1) - 1,
    if
        Int >= Min andalso Int =< Max ->
            Size = Bytes * 8,
            <<Int:Size>>;
        true ->
            encode_signed(Int, Bytes + 1)
    end.
