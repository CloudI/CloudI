-ifndef(_thriftTest_types_included).
-define(_thriftTest_types_included, yeah).

-define(thriftTest_Numberz_ONE, 1).
-define(thriftTest_Numberz_TWO, 2).
-define(thriftTest_Numberz_THREE, 3).
-define(thriftTest_Numberz_FIVE, 5).
-define(thriftTest_Numberz_SIX, 6).
-define(thriftTest_Numberz_EIGHT, 8).

%% struct bonk

-record(bonk, {message :: string() | binary(),
               type :: integer()}).

%% struct bools

-record(bools, {im_true :: boolean(),
                im_false :: boolean()}).

%% struct xtruct

-record(xtruct, {string_thing :: string() | binary(),
                 byte_thing :: integer(),
                 i32_thing :: integer(),
                 i64_thing :: integer()}).

%% struct xtruct2

-record(xtruct2, {byte_thing :: integer(),
                  struct_thing :: #xtruct{},
                  i32_thing :: integer()}).

%% struct xtruct3

-record(xtruct3, {string_thing :: string() | binary(),
                  changed :: integer(),
                  i32_thing :: integer(),
                  i64_thing :: integer()}).

%% struct insanity

-record(insanity, {userMap :: dict(),
                   xtructs :: list()}).

%% struct crazyNesting

-record(crazyNesting, {string_field :: string() | binary(),
                       set_field :: set(),
                       list_field = [] :: list(),
                       binary_field :: string() | binary()}).

%% struct xception

-record(xception, {errorCode :: integer(),
                   message :: string() | binary()}).

%% struct xception2

-record(xception2, {errorCode :: integer(),
                    struct_thing :: #xtruct{}}).

%% struct emptyStruct

-record(emptyStruct, {}).

%% struct oneField

-record(oneField, {field :: #emptyStruct{}}).

%% struct versioningTestV1

-record(versioningTestV1, {begin_in_both :: integer(),
                           old_string :: string() | binary(),
                           end_in_both :: integer()}).

%% struct versioningTestV2

-record(versioningTestV2, {begin_in_both :: integer(),
                           newint :: integer(),
                           newbyte :: integer(),
                           newshort :: integer(),
                           newlong :: integer(),
                           newdouble :: float(),
                           newstruct :: #bonk{},
                           newlist :: list(),
                           newset :: set(),
                           newmap :: dict(),
                           newstring :: string() | binary(),
                           end_in_both :: integer()}).

%% struct listTypeVersioningV1

-record(listTypeVersioningV1, {myints :: list(),
                               hello :: string() | binary()}).

%% struct listTypeVersioningV2

-record(listTypeVersioningV2, {strings :: list(),
                               hello :: string() | binary()}).

%% struct guessProtocolStruct

-record(guessProtocolStruct, {map_field :: dict()}).

%% struct largeDeltas

-record(largeDeltas, {b1 :: #bools{},
                      b10 :: #bools{},
                      b100 :: #bools{},
                      check_true :: boolean(),
                      b1000 :: #bools{},
                      check_false :: boolean(),
                      vertwo2000 :: #versioningTestV2{},
                      a_set2500 :: set(),
                      vertwo3000 :: #versioningTestV2{},
                      big_numbers :: list()}).

%% struct nestedListsI32x2

-record(nestedListsI32x2, {integerlist :: list()}).

%% struct nestedListsI32x3

-record(nestedListsI32x3, {integerlist :: list()}).

%% struct nestedMixedx2

-record(nestedMixedx2, {int_set_list :: list(),
                        map_int_strset :: dict(),
                        map_int_strset_list :: list()}).

%% struct listBonks

-record(listBonks, {bonk :: list()}).

%% struct nestedListsBonk

-record(nestedListsBonk, {bonk :: list()}).

%% struct boolTest

-record(boolTest, {b = true :: boolean(),
                   s = "true" :: string() | binary()}).

%% struct structA

-record(structA, {s :: string() | binary()}).

%% struct structB

-record(structB, {aa :: #structA{},
                  ab = #structA{} :: #structA{}}).

-endif.
