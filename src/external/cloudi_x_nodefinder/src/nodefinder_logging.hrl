%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

% include the proper logging macros

-ifdef(CLOUDI_LOGGER).
-include_lib("cloudi_core/include/cloudi_logger.hrl").
-else.
-include("nodefinder_logging_default.hrl").
-endif.

