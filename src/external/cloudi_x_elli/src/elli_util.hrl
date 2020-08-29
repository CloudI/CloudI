
-ifdef(OTP_RELEASE).
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_ERROR(Str), error_logger:error_msg(Str)).
-define(LOG_ERROR(Format,Data), error_logger:error_msg(Format, Data)).
-define(LOG_INFO(Format,Data), error_logger:info_msg(Format, Data)).
-endif.

-ifdef(OTP_RELEASE).
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-endif.

%% Bloody useful
-define(IF(Test,True,False), case Test of true -> True; false -> False end).
