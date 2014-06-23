-module(erlcql_type_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include("erlcql_test.hrl").

-define(OPTS, [{numtests, 1000}]).

%% Suite ----------------------------------------------------------------------

all() ->
    [ascii,
     bigint,
     blob,
     boolean,
     counter,
     decimal,
     double,
     float,
     inet,
     int,
     text,
     timestamp,
     timeuuid,
     uuid,
     varchar,
     varint].

%% Fixtures -------------------------------------------------------------------

init_per_suite(Config) ->
    Keyspace = create_keyspace(),
    [{keyspace, Keyspace} | Config].

end_per_suite(Config) ->
    Keyspace = ?c(keyspace, Config),
    ok = drop_keyspace(Keyspace).

%% Tests ----------------------------------------------------------------------

ascii(Config) ->
    type_property(
      Config, <<"ascii">>,
      fun(Client, Table) ->
              ?FORALL(
                 Ascii, ascii_string(),
                 begin
                     Value = [$', escape_string(Ascii), $'],
                     insert_type(Client, Table, Value),
                     iolist_to_binary(Ascii) == get_value(Client, Table)
                 end)
      end).

bigint(Config) ->
    type_property(
      Config, <<"bigint">>,
      fun(Client, Table) ->
              ?FORALL(
                 BigInt, big_integer(),
                 begin
                     Value = list_to_binary(integer_to_list(BigInt)),
                     insert_type(Client, Table, Value),
                     BigInt == get_value(Client, Table)
                 end)
      end).

blob(Config) ->
    type_property(
      Config, <<"blob">>,
      fun(Client, Table) ->
              ?FORALL(
                 Binary, binary(),
                 begin
                     Value = binary_to_hex(Binary),
                     insert_type(Client, Table, Value),
                     Binary == get_value(Client, Table)
                 end)
      end).

boolean(Config) ->
    Type = <<"boolean">>,
    check_type(Config, Type, <<"true">>, true),
    check_type(Config, Type, <<"false">>, false).

counter(Config) ->
    Set = fun(Table, Value) when Value >= 0 ->
                  [<<"UPDATE ">>, Table, <<" SET v = v + ">>,
                   integer_to_list(Value), <<" WHERE k = 0">>];
             (Table, Value) ->
                  [<<"UPDATE ">>, Table, <<" SET v = v - ">>,
                   integer_to_list(-Value), <<" WHERE k = 0">>]
          end,
    type_property(
      Config, <<"counter">>,
      fun(Client, Table) ->
              ?FORALL(
                 Ints, non_empty(list(integer())),
                 begin
                     ['query'(Client, Set(Table, Int)) || Int <- Ints],
                     Counter = get_value(Client, Table),
                     'query'(Client, Set(Table, -Counter)),
                     lists:sum(Ints) == Counter
                 end)
      end).

decimal(Config) ->
    type_property(
      Config, <<"decimal">>,
      fun(Client, Table) ->
              ?FORALL(
                 Float, float(),
                 begin
                     Value = list_to_binary(float_to_list(Float)),
                     insert_type(Client, Table, Value),
                     Decimal = get_value(Client, Table),
                     compare_floats(Float, Decimal, 1.0e-12)
                 end)
      end).

double(Config) ->
    type_property(
      Config, <<"double">>,
      fun(Client, Table) ->
              ?FORALL(
                 Float, float(),
                 begin
                     Value = list_to_binary(float_to_list(Float)),
                     insert_type(Client, Table, Value),
                     Double = get_value(Client, Table),
                     compare_floats(Float, Double, 1.0e-12)
                 end)
      end).

float(Config) ->
    type_property(
      Config, <<"float">>,
      fun(Client, Table) ->
              ?FORALL(
                 Float, float(),
                 begin
                     Value = list_to_binary(float_to_list(Float)),
                     insert_type(Client, Table, Value),
                     Float2 = get_value(Client, Table),
                     compare_floats(Float, Float2, 1.0e-7)
                 end)
      end).

%% TODO: Generate IPv6 addresses as well
inet(Config) ->
    type_property(
      Config, <<"inet">>,
      fun(Client, Table) ->
              ?FORALL(
                 IP, ipv4(),
                 begin
                     Value = [$', erlcql_test:ntoa(IP), $'],
                     insert_type(Client, Table, Value),
                     IP == get_value(Client, Table)
                 end)
      end).

int(Config) ->
    type_property(
      Config, <<"int">>,
      fun(Client, Table) ->
              ?FORALL(
                 Int, small_integer(),
                 begin
                     Value = list_to_binary(integer_to_list(Int)),
                     insert_type(Client, Table, Value),
                     Int == get_value(Client, Table)
                 end)
      end).

text(Config) ->
    type_property(
      Config, <<"text">>,
      fun(Client, Table) ->
              ?FORALL(
                 Text, varchar_string(),
                 begin
                     Value = [$', escape_string(Text), $'],
                     insert_type(Client, Table, Value),
                     utf8_to_binary(Text) == get_value(Client, Table)
                 end)
      end).

%% TODO: Convert to property
%% TODO: Generate string representation
timestamp(Config) ->
    Type = <<"timestamp">>,
    check_type(Config, Type, <<"1385553738674">>, 1385553738674).

%% TODO: Convert to property
timeuuid(Config) ->
    Type = <<"timeuuid">>,
    UUIDs = [<<"4600fa40-5756-11e3-949a-0800200c9a66">>,
             <<"c962db20-941d-11e3-bca1-425861b86ab6">>,
             <<"d0504f76-941d-11e3-bca1-425861b86ab6">>],
    [check_type(Config, Type, UUID, UUID) || UUID <- UUIDs].

%% TODO: Convert to property
uuid(Config) ->
    Type = <<"uuid">>,
    UUIDs = [<<"591f0d7e-be9e-48d0-8742-0c096937a902">>,
             <<"43a4e79e-6db3-49b5-a450-6247359edd79">>,
             <<"107fb6ea-b484-477c-b1fb-83c72a234e90">>],
    [check_type(Config, Type, UUID, UUID) || UUID <- UUIDs].

varchar(Config) ->
    type_property(
      Config, <<"varchar">>,
      fun(Client, Table) ->
              ?FORALL(
                 VarChar, varchar_string(),
                 begin
                     Value = [$', escape_string(VarChar), $'],
                     insert_type(Client, Table, Value),
                     utf8_to_binary(VarChar) == get_value(Client, Table)
                 end)
      end).

varint(Config) ->
    type_property(
      Config, <<"varint">>,
      fun(Client, Table) ->
              ?FORALL(
                 VarInt, big_integer(),
                 begin
                     Value = list_to_binary(integer_to_list(VarInt)),
                     insert_type(Client, Table, Value),
                     VarInt == get_value(Client, Table)
                 end)
      end).

%% Helpers --------------------------------------------------------------------

type_property(Config, Type, PropertyFun) ->
    Keyspace = ?c(keyspace, Config),
    Client = start_client(Keyspace),
    Table = create_table(Client, Type),
    true = proper:quickcheck(PropertyFun(Client, Table), ?OPTS).

check_type(Config, Type, Value, Expected) ->
    Keyspace = ?c(keyspace, Config),
    Client = start_client(Keyspace),
    Table = create_table(Client, Type),
    insert_type(Client, Table, Value),
    Expected = get_value(Client, Table).

create_table(Pid, Type) ->
    Table = gen_table_name(),
    Query = [<<"CREATE TABLE ">>, Table,
             <<" (k int PRIMARY KEY, v ">>, Type, <<")">>],
    'query'(Pid, Query),
    Table.

insert_type(Pid, Table, Value) ->
    Query = [<<"INSERT INTO ">>, Table,
             <<" (k, v) VALUES (0, ">>, Value, <<")">>],
    'query'(Pid, Query),
    ok.

get_value(Pid, Table) ->
    Query = [<<"SELECT v FROM ">>, Table, <<" WHERE k = 0">>],
    {[[Value]], _} = 'query'(Pid, Query),
    Value.

%% Generators -----------------------------------------------------------------

ascii_string() ->
    list(ascii_char()).

ascii_char() ->
    integer(0, 127).

big_integer() ->
    union([integer(), integer(-(1 bsl 63), (1 bsl 63) - 1)]).

small_integer() ->
    union([integer(), integer(-(1 bsl 31), (1 bsl 31) - 1)]).

ipv4() ->
    {integer(0, 255), integer(0, 255), integer(0, 255), integer(0, 255)}.

varchar_string() ->
    list(unicode_char()).

unicode_char() ->
    ?SUCHTHAT(C, char(), C < 16#D800 orelse C > 16#DFFF).

%% Others ---------------------------------------------------------------------

-spec escape_string(string()) -> bitstring().
escape_string(String) ->
    Escape = fun($', Acc) ->
                     [$', $' | Acc];
                (C, Acc) ->
                     [C | Acc]
             end,
    EscapedString = lists:foldl(Escape, [], String),
    utf8_to_binary(lists:reverse(EscapedString)).

-spec utf8_to_binary(string()) -> bitstring().
utf8_to_binary(String) ->
    unicode:characters_to_binary(String, unicode, utf8).

-spec binary_to_hex(binary()) -> bitstring().
binary_to_hex(Binary) ->
    Binary2 = erlcql_test:hexencode(Binary),
    <<"0x", Binary2/binary>>.

-spec compare_floats(float(), float(), float()) -> boolean().
compare_floats(Float1, Float2, Epsilon) ->
    Diff = abs(Float1 - Float2),
    if
        Float1 == 0.0 orelse Float2 == 0.0 ->
            true;
        true ->
            (Diff / (abs(Float1) + abs(Float2))) < Epsilon
    end.
