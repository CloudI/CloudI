-ifndef(_docTest_types_included).
-define(_docTest_types_included, yeah).

-define(docTest_Numberz_ONE, 1).
-define(docTest_Numberz_TWO, 2).
-define(docTest_Numberz_THREE, 3).
-define(docTest_Numberz_FIVE, 5).
-define(docTest_Numberz_SIX, 6).
-define(docTest_Numberz_EIGHT, 8).

%% struct xtruct

-record(xtruct, {string_thing :: string() | binary(),
                 byte_thing :: integer(),
                 i32_thing :: integer(),
                 i64_thing :: integer()}).

%% struct xtruct2

-record(xtruct2, {byte_thing :: integer(),
                  struct_thing :: #xtruct{},
                  i32_thing :: integer()}).

%% struct insanity

-record(insanity, {userMap :: dict(),
                   xtructs :: list()}).

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

-endif.
