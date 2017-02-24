%%
%% Log macros
%%
-ifndef(__LOG_HRL__).
-define(__LOG_HRL__, true).

%% Lager logging levels
%%   info, warning, error

-define(info(Fmt), error_logger:info_msg(Fmt ++ "\n")).
-define(info(Fmt, Args), error_logger:info_msg(Fmt ++ "\n", Args)).

-define(warning(Fmt), error_logger:warning_msg(Fmt ++ "\n")).
-define(warning(Fmt, Args), error_logger:warning_msg(Fmt ++ "\n", Args)).

-define(error(Fmt), error_logger:error_msg(Fmt ++ "\n")).
-define(error(Fmt, Args), error_logger:error_msg(Fmt ++ "\n", Args)).

-endif.
