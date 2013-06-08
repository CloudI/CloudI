-ifndef(_manyTypedefs_types_included).
-define(_manyTypedefs_types_included, yeah).

%% struct struct1

-record(struct1, {myint :: integer(),
                  mylist :: list()}).

%% struct exception1

-record(exception1, {alist :: list(),
                     mystruct :: #struct1{}}).

-endif.
