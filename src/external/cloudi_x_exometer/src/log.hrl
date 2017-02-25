%%
%% Log macros
%%
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

-include_lib("hut/include/hut.hrl").

-define(debug(Fmt), ?log(debug, Fmt)).
-define(debug(Fmt, Args), ?log(debug, Fmt, Args)).
-define(debug(Attrs, Fmt, Args), ?log(debug, Fmt, Args, Attrs)).

-define(info(Fmt), ?log(info, Fmt)).
-define(info(Fmt, Args), ?log(info, Fmt, Args)).
-define(info(Attrs, Fmt, Args), ?log(info, Fmt, Args, Attrs)).

-define(notice(Fmt), ?log(notice, Fmt)).
-define(notice(Fmt, Args), ?log(notice, Fmt, Args)).
-define(notice(Attrs, Fmt, Args), ?log(notice, Fmt, Args, Attrs)).

-define(warning(Fmt), ?log(warning, Fmt)).
-define(warning(Fmt, Args), ?log(warning, Fmt, Args)).
-define(warning(Attrs, Fmt, Args), ?log(warning, Fmt, Args, Attrs)).

-define(error(Fmt), ?log(error, Fmt)).
-define(error(Fmt, Args), ?log(error, Fmt, Args)).
-define(error(Attrs, Fmt, Args), ?log(error, Fmt, Args, Attrs)).

-define(critical(Fmt), ?log(critical, Fmt)).
-define(critical(Fmt, Args), ?log(critical, Fmt, Args)).
-define(critical(Attrs, Fmt, Args), ?log(critical, Fmt, Args, Attrs)).

-define(alert(Fmt), ?log(alert, Fmt)).
-define(alert(Fmt, Args), ?log(alert, Fmt, Args)).
-define(alert(Attrs, Fmt, Args), ?log(alert, Fmt, Args, Attrs)).

-define(emergency(Fmt), ?log(emergency, Fmt)).
-define(emergency(Fmt, Args), ?log(emergency, Fmt, Args)).
-define(emergency(Attrs, Fmt, Args), ?log(emergency, Fmt, Args, Attrs)).

-endif.
