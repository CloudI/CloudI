-ifndef(_debugProtoTest_types_included).
-define(_debugProtoTest_types_included, yeah).

-define(debugProtoTest_SomeEnum_ONE, 1).
-define(debugProtoTest_SomeEnum_TWO, 2).

%% struct doubles

-record(doubles, {nan :: float(),
                  inf :: float(),
                  neginf :: float(),
                  repeating :: float(),
                  big :: float(),
                  small :: float(),
                  zero :: float(),
                  negzero :: float()}).

%% struct oneOfEach

-record(oneOfEach, {im_true :: boolean(),
                    im_false :: boolean(),
                    a_bite = 127 :: integer(),
                    integer16 = 32767 :: integer(),
                    integer32 :: integer(),
                    integer64 = 10000000000 :: integer(),
                    double_precision :: float(),
                    some_characters :: string() | binary(),
                    zomg_unicode :: string() | binary(),
                    what_who :: boolean(),
                    base64 :: string() | binary(),
                    byte_list = [1,2,3] :: list(),
                    i16_list = [1,2,3] :: list(),
                    i64_list = [1,2,3] :: list()}).

%% struct bonk

-record(bonk, {type :: integer(),
               message :: string() | binary()}).

%% struct nesting

-record(nesting, {my_bonk :: #bonk{},
                  my_ooe :: #oneOfEach{}}).

%% struct holyMoley

-record(holyMoley, {big :: list(),
                    contain :: set(),
                    bonks :: dict()}).

%% struct backwards

-record(backwards, {first_tag2 :: integer(),
                    second_tag1 :: integer()}).

%% struct empty

-record(empty, {}).

%% struct wrapper

-record(wrapper, {foo :: #empty{}}).

%% struct randomStuff

-record(randomStuff, {a :: integer(),
                      b :: integer(),
                      c :: integer(),
                      d :: integer(),
                      myintlist :: list(),
                      maps :: dict(),
                      bigint :: integer(),
                      triple :: float()}).

%% struct base64

-record(base64, {a :: integer(),
                 b1 :: string() | binary(),
                 b2 :: string() | binary(),
                 b3 :: string() | binary(),
                 b4 :: string() | binary(),
                 b5 :: string() | binary(),
                 b6 :: string() | binary()}).

%% struct compactProtoTestStruct

-record(compactProtoTestStruct, {a_byte :: integer(),
                                 a_i16 :: integer(),
                                 a_i32 :: integer(),
                                 a_i64 :: integer(),
                                 a_double :: float(),
                                 a_string :: string() | binary(),
                                 a_binary :: string() | binary(),
                                 true_field :: boolean(),
                                 false_field :: boolean(),
                                 empty_struct_field :: #empty{},
                                 byte_list :: list(),
                                 i16_list :: list(),
                                 i32_list :: list(),
                                 i64_list :: list(),
                                 double_list :: list(),
                                 string_list :: list(),
                                 binary_list :: list(),
                                 boolean_list :: list(),
                                 struct_list :: list(),
                                 byte_set :: set(),
                                 i16_set :: set(),
                                 i32_set :: set(),
                                 i64_set :: set(),
                                 double_set :: set(),
                                 string_set :: set(),
                                 binary_set :: set(),
                                 boolean_set :: set(),
                                 struct_set :: set(),
                                 byte_byte_map :: dict(),
                                 i16_byte_map :: dict(),
                                 i32_byte_map :: dict(),
                                 i64_byte_map :: dict(),
                                 double_byte_map :: dict(),
                                 string_byte_map :: dict(),
                                 binary_byte_map :: dict(),
                                 boolean_byte_map :: dict(),
                                 byte_i16_map :: dict(),
                                 byte_i32_map :: dict(),
                                 byte_i64_map :: dict(),
                                 byte_double_map :: dict(),
                                 byte_string_map :: dict(),
                                 byte_binary_map :: dict(),
                                 byte_boolean_map :: dict(),
                                 list_byte_map :: dict(),
                                 set_byte_map :: dict(),
                                 map_byte_map :: dict(),
                                 byte_map_map :: dict(),
                                 byte_set_map :: dict(),
                                 byte_list_map :: dict()}).

%% struct singleMapTestStruct

-record(singleMapTestStruct, {i32_map = dict:new() :: dict()}).

%% struct exceptionWithAMap

-record(exceptionWithAMap, {blah :: string() | binary(),
                            map_field :: dict()}).

%% struct blowUp

-record(blowUp, {b1 :: dict(),
                 b2 :: dict(),
                 b3 :: dict(),
                 b4 :: dict()}).

%% struct reverseOrderStruct

-record(reverseOrderStruct, {first :: string() | binary(),
                             second :: integer(),
                             third :: integer(),
                             fourth :: integer()}).

%% struct structWithSomeEnum

-record(structWithSomeEnum, {blah :: integer()}).

%% struct testUnion

-record(testUnion, {string_field :: string() | binary(),
                    i32_field :: integer(),
                    struct_field :: #oneOfEach{},
                    struct_list :: list(),
                    other_i32_field :: integer(),
                    enum_field :: integer(),
                    i32_set :: set(),
                    i32_map :: dict()}).

%% struct testUnionMinusStringField

-record(testUnionMinusStringField, {i32_field :: integer(),
                                    struct_field :: #oneOfEach{},
                                    struct_list :: list(),
                                    other_i32_field :: integer(),
                                    enum_field :: integer(),
                                    i32_set :: set(),
                                    i32_map :: dict()}).

%% struct comparableUnion

-record(comparableUnion, {string_field :: string() | binary(),
                          binary_field :: string() | binary()}).

%% struct structWithAUnion

-record(structWithAUnion, {test_union :: #testUnion{}}).

%% struct primitiveThenStruct

-record(primitiveThenStruct, {blah :: integer(),
                              blah2 :: integer(),
                              bw :: #backwards{}}).

%% struct structWithASomemap

-record(structWithASomemap, {somemap_field :: dict()}).

%% struct bigFieldIdStruct

-record(bigFieldIdStruct, {field1 :: string() | binary(),
                           field2 :: string() | binary()}).

%% struct breaksRubyCompactProtocol

-record(breaksRubyCompactProtocol, {field1 :: string() | binary(),
                                    field2 :: #bigFieldIdStruct{},
                                    field3 :: integer()}).

%% struct tupleProtocolTestStruct

-record(tupleProtocolTestStruct, {field1 :: integer(),
                                  field2 :: integer(),
                                  field3 :: integer(),
                                  field4 :: integer(),
                                  field5 :: integer(),
                                  field6 :: integer(),
                                  field7 :: integer(),
                                  field8 :: integer(),
                                  field9 :: integer(),
                                  field10 :: integer(),
                                  field11 :: integer(),
                                  field12 :: integer()}).

-endif.
