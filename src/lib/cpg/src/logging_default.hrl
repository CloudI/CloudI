%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-define(LOG_FATAL(Format, Args),
    error_logger:error_msg(Format, Args)).

-define(LOG_ERROR(Format, Args),
    error_logger:error_msg(Format, Args)).

-define(LOG_WARN(Format, Args),
    error_logger:warning_msg(Format, Args)).

-define(LOG_INFO(Format, Args),
    error_logger:info_msg(Format, Args)).

-define(LOG_DEBUG(Format, Args),
    error_logger:info_msg(Format, Args)).

-define(LOG_TRACE(Format, Args),
    error_logger:info_msg(Format, Args)).

