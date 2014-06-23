-module(erlcql_misc_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-define(E, 1 / 1000000000000).

%% Suite ----------------------------------------------------------------------

all() ->
    [signed_integer_encoding,
     decimal_encoding].

%% Tests ----------------------------------------------------------------------

signed_integer_encoding(_Config) ->
    Opts = [{numtests, 1000}],
    true = proper:quickcheck(prop_signed_integer(), Opts).

prop_signed_integer() ->
    ?FORALL(
       Integer, integer(),
       begin
           Binary = erlcql_convert:to_binary(varint, Integer),
           Decoded = erlcql_convert:from_binary(varint, Binary),
           Integer == Decoded
       end).

decimal_encoding(_Config) ->
    Opts = [{numtests, 1000}],
    true = proper:quickcheck(prop_decimal(), Opts).

prop_decimal() ->
    ?FORALL(
       Float, float(),
       begin
           Binary = erlcql_convert:to_binary(decimal, Float),
           Decoded = erlcql_convert:from_binary(decimal, Binary),
           Percent = abs(Float - Decoded) / Float * 100,
           true == (Percent < ?E)
       end).
