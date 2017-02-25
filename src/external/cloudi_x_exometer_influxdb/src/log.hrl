%%
%% Log macros
%%
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

-include_lib("hut/include/hut.hrl").

-define(info(Fmt), ?log(info, Fmt)).
-define(info(Fmt, Args), ?log(info, Fmt, Args)).

-define(warning(Fmt), ?log(warning, Fmt)).
-define(warning(Fmt, Args), ?log(warning, Fmt, Args)).

-define(error(Fmt), ?log(error, Fmt)).
-define(error(Fmt, Args), ?log(error, Fmt, Args)).

-endif.
