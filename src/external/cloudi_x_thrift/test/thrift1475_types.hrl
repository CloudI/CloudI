-ifndef(_thrift1475_types_included).
-define(_thrift1475_types_included, yeah).

%% struct structB

-record(structB, {x :: string() | binary()}).

%% struct structA

-record(structA, {a :: string() | binary(),
                  b :: string() | binary(),
                  c :: string() | binary(),
                  d :: string() | binary(),
                  e :: string() | binary(),
                  f :: string() | binary(),
                  g = "foo" :: string() | binary(),
                  h :: integer(),
                  i :: integer(),
                  j :: integer(),
                  k = 5 :: integer(),
                  l :: float(),
                  m :: float(),
                  n :: float(),
                  o = 3.14159 :: float(),
                  string_list :: list(),
                  byte_list = [1,2,3] :: list(),
                  rsl = [] :: list(),
                  osl :: list(),
                  string_set :: set(),
                  rss = sets:new() :: set(),
                  oss :: set(),
                  string_map :: dict(),
                  rsm = dict:new() :: dict(),
                  osm :: dict(),
                  structb :: #structB{}}).

-endif.
