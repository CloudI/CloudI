-ifndef(_optionalRequiredTest_types_included).
-define(_optionalRequiredTest_types_included, yeah).

%% struct oldSchool

-record(oldSchool, {im_int :: integer(),
                    im_str :: string() | binary(),
                    im_big :: list()}).

%% struct simple

-record(simple, {im_default :: integer(),
                 im_required :: integer(),
                 im_optional :: integer()}).

%% struct tricky1

-record(tricky1, {im_default :: integer()}).

%% struct tricky2

-record(tricky2, {im_optional :: integer()}).

%% struct tricky3

-record(tricky3, {im_required :: integer()}).

%% struct optionalDefault

-record(optionalDefault, {opt_int = 1234 :: integer(),
                          opt_str = "default" :: string() | binary()}).

%% struct complex

-record(complex, {cp_default :: integer(),
                  cp_required :: integer(),
                  cp_optional :: integer(),
                  the_map :: dict(),
                  req_simp = #simple{} :: #simple{},
                  opt_simp :: #simple{}}).

%% struct manyOpt

-record(manyOpt, {opt1 :: integer(),
                  opt2 :: integer(),
                  opt3 :: integer(),
                  def4 :: integer(),
                  opt5 :: integer(),
                  opt6 :: integer()}).

%% struct javaTestHelper

-record(javaTestHelper, {req_int :: integer(),
                         opt_int :: integer(),
                         req_obj :: string() | binary(),
                         opt_obj :: string() | binary(),
                         req_bin :: string() | binary(),
                         opt_bin :: string() | binary()}).

-endif.
