-ifndef(_thrift1151_types_included).
-define(_thrift1151_types_included, yeah).

%% struct structA

-record(structA, {x :: integer()}).

%% struct structB

-record(structB, {x :: integer()}).

%% struct structC

-record(structC, {x :: #structA{}}).

-endif.
