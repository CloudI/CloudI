-ifndef(_annotationTest_types_included).
-define(_annotationTest_types_included, yeah).

-define(annotationTest_weekdays_SUNDAY, 0).
-define(annotationTest_weekdays_MONDAY, 1).
-define(annotationTest_weekdays_TUESDAY, 2).
-define(annotationTest_weekdays_WEDNESDAY, 3).
-define(annotationTest_weekdays_THURSDAY, 4).
-define(annotationTest_weekdays_FRIDAY, 5).
-define(annotationTest_weekdays_SATURDAY, 6).

%% struct foo

-record(foo, {bar :: integer(),
              baz :: integer(),
              qux :: integer(),
              bop :: integer()}).

%% struct foo_error

-record(foo_error, {error_code :: integer(),
                    error_msg :: string() | binary()}).

-endif.
