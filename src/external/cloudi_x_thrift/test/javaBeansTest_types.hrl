-ifndef(_javaBeansTest_types_included).
-define(_javaBeansTest_types_included, yeah).

%% struct oneOfEachBeans

-record(oneOfEachBeans, {boolean_field :: boolean(),
                         a_bite :: integer(),
                         integer16 :: integer(),
                         integer32 :: integer(),
                         integer64 :: integer(),
                         double_precision :: float(),
                         some_characters :: string() | binary(),
                         base64 :: string() | binary(),
                         byte_list :: list(),
                         i16_list :: list(),
                         i64_list :: list()}).

-endif.
