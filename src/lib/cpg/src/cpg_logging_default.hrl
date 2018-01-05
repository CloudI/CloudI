%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

-define(LOG_FATAL(Format, Args),
    error_logger:error_msg(Format ++ "~n", Args)).

-define(LOG_ERROR(Format, Args),
    error_logger:error_msg(Format ++ "~n", Args)).

-define(LOG_WARN(Format, Args),
    error_logger:warning_msg(Format ++ "~n", Args)).

-define(LOG_INFO(Format, Args),
    error_logger:info_msg(Format ++ "~n", Args)).

-define(LOG_DEBUG(Format, Args),
    error_logger:info_msg(Format ++ "~n", Args)).

-define(LOG_TRACE(Format, Args),
    error_logger:info_msg(Format ++ "~n", Args)).

