-ifndef(_denseLinkingTest_types_included).
-define(_denseLinkingTest_types_included, yeah).

%% struct oneOfEachZZ

-record(oneOfEachZZ, {im_true :: boolean(),
                      im_false :: boolean(),
                      a_bite :: integer(),
                      integer16 :: integer(),
                      integer32 :: integer(),
                      integer64 :: integer(),
                      double_precision :: float(),
                      some_characters :: string() | binary(),
                      zomg_unicode :: string() | binary(),
                      what_who :: boolean()}).

%% struct bonkZZ

-record(bonkZZ, {type :: integer(),
                 message :: string() | binary()}).

%% struct nestingZZ

-record(nestingZZ, {my_bonk :: #bonkZZ{},
                    my_ooe :: #oneOfEachZZ{}}).

%% struct holyMoleyZZ

-record(holyMoleyZZ, {big :: list(),
                      contain :: set(),
                      bonks :: dict()}).

%% struct backwardsZZ

-record(backwardsZZ, {first_tag2 :: integer(),
                      second_tag1 :: integer()}).

%% struct emptyZZ

-record(emptyZZ, {}).

%% struct wrapperZZ

-record(wrapperZZ, {foo :: #emptyZZ{}}).

%% struct randomStuffZZ

-record(randomStuffZZ, {a :: integer(),
                        b :: integer(),
                        c :: integer(),
                        d :: integer(),
                        myintlist :: list(),
                        maps :: dict(),
                        bigint :: integer(),
                        triple :: float()}).

-endif.
