%% -*- erlang -*-

-ifndef(__HUT_HRL__).
-define(__HUT_HRL__, true).

%% Supported logging levels (taken from lager):
-define(__log_levels, [debug, info, notice, warning, error, critical, alert, emergency]).
-define(__default_log_level, info).
-define(__default_use_log_level_gate, true).

%% Helper macros
-define(__fmt(__Fmt, __Args), lists:flatten(io_lib:format(__Fmt, __Args))).

-define(__maybe_log(__Level, __Fun),
        ((fun() ->
                   __UseGate = application:get_env(hut, use_log_level_gate, ?__default_use_log_level_gate),
                  case __UseGate of
                      true ->
                          __CurrentLevel = application:get_env(hut, level, ?__default_log_level),
                          __AllowedLevels = lists:dropwhile(fun(__Element) -> __Element =/= __CurrentLevel end, ?__log_levels),
                          __IsEnabled = lists:member(__Level, __AllowedLevels),
                          case __IsEnabled of
                              true ->
                                  __Fun();
                              _ ->
                                  ok
                          end;
                      _ ->
                          __Fun()
                  end
          end)())).


-define(__hut_logger_metadata,
        #{ mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY}
         , file => ?FILE
         , line => ?LINE
         }).

%% Lager support

-ifdef(HUT_LAGER).
-define(log_type, "lager").

-ifndef(HUT_LAGER_SINK).
-define(HUT_LAGER_SINK, lager).
-endif.

-define(log(__Level, __Fmt),
        ?HUT_LAGER_SINK:__Level([], __Fmt, [])).
-define(log(__Level, __Fmt, __Args),
        ?HUT_LAGER_SINK:__Level([], __Fmt, __Args)).
-define(log(__Level, __Fmt, __Args, __Opts),
        ?HUT_LAGER_SINK:__Level(__Opts, __Fmt, __Args)).

-else. %% HUT_LAGER

% Using plain `io:format/2`.

-ifdef(HUT_IOFORMAT).
-define(log_type, "ioformat").

-define(log(__Level, __Fmt),
        ?__maybe_log(__Level, fun() -> io:format("~p: " ++ __Fmt ++ "~n", [__Level]) end)).
-define(log(__Level, __Fmt, __Args),
        ?__maybe_log(__Level, fun() -> io:format("~p: " ++ __Fmt ++ "~n", [__Level] ++ __Args) end)).
-define(log(__Level, __Fmt, __Args, __Opts),
        ?__maybe_log(__Level, fun() -> io:format("~p: " ++ __Fmt ++ "; Opts: ~p~n", [__Level] ++ __Args ++ [__Opts]) end)).

-else. %% HUT_IOFORMAT

% All logging calls are passed into a custom logging callback module given by `HUT_CUSTOM_CB`.

-ifdef(HUT_CUSTOM).
-define(log_type, "custom").

-define(log(__Level, __Fmt),
        ?__maybe_log(__Level, fun() -> ?HUT_CUSTOM_CB:log(__Level, __Fmt, [], []) end)).
-define(log(__Level, __Fmt, __Args),
        ?__maybe_log(__Level, fun() -> ?HUT_CUSTOM_CB:log(__Level, __Fmt, __Args, []) end)).
-define(log(__Level, __Fmt, __Args, __Opts),
        ?__maybe_log(__Level, fun() -> ?HUT_CUSTOM_CB:log(__Level, __Fmt, __Args, __Opts) end)).

-ifdef(HUT_CUSTOM_SLOG).
-define(slog(__Level, __Data, __Meta),
        ?HUT_CUSTOM_CB:slog(__Level, __Data, maps:merge(?__hut_logger_metadata, __Meta))).
-define(slog(__Level, __Data),
        ?HUT_CUSTOM_CB:slog(__Level, __Data, ?__hut_logger_metadata)).
-endif. %% HUT_CUSTOM_SLOG

-else. %% HUT_CUSTOM

% All logging calls are ignored.

-ifdef(HUT_NOOP).
-define(log_type, "noop").

-define(log(__Level, __Fmt), true).
-define(log(__Level, __Fmt, __Args), true).
-define(log(__Level, __Fmt, __Args, __Opts), true).

-else. %% HUT_NOOP

-ifndef(OTP_RELEASE).
% If none of the above options were defined and OTP version is below 21, default to SASL
-ifndef(HUT_SASL).
-define(HUT_SASL, true).
-endif.
-endif. %% !OTP_RELEASE

-ifdef(HUT_SASL).

-define(log_type, "sasl").

-define(log(__Level, __Fmt),
        ?__maybe_log(__Level, fun() -> hut:log(?log_type, __Level, __Fmt, [], []) end)).
-define(log(__Level, __Fmt, __Args),
        ?__maybe_log(__Level, fun() -> hut:log(?log_type, __Level, __Fmt, __Args, []) end)).
-define(log(__Level, __Fmt, __Args, __Opts),
        ?__maybe_log(__Level, fun() -> hut:log(?log_type, __Level, __Fmt, __Args, __Opts) end)).

-else. %% HUT_SASL

% On OTP21+ use logger by default

-define(log_type, "logger").

-define(log(__Level, __Fmt, __Args, __Opts),
        logger:log(__Level, __Fmt ++ "; Opts ~p", __Args ++ [__Opts], ?__hut_logger_metadata)).
-define(log(__Level, __Fmt, __Args),
        logger:log(__Level, __Fmt, __Args, ?__hut_logger_metadata)).
-define(log(__Level, __Fmt),
        ?log(__Level, __Fmt, [])).

% Structured report:
-define(slog(__Level, __Data, __Meta),
        logger:log(__Level, __Data, maps:merge(?__hut_logger_metadata, __Meta))).
-define(slog(__Level, __Data),
        ?slog(__Level, __Data, #{})).

% Set metadata:
-define(set_process_metadata(__Meta),
        logger:set_process_metadata(__Meta)).

% End of all actual log implementation switches.
-endif. %% HUT_SASL
-endif. %% HUT_NOOP
-endif. %% HUT_CUSTOM
-endif. %% HUT_IOFORMAT
-endif. %% HUT_LAGER

-ifndef(slog).
%% Poor man's OTP21 `logger' structured log:

-define(__hut_slog_helper(__Level, __Data, __Meta0),
        ((fun() ->
              try
                  __Meta = case get(hut_process_metadata) of
                             __Map when is_map(__Map) ->
                                 maps:merge(__Map, __Meta0);
                             _ ->
                                 __Meta0
                           end,
                  ?log(__Level, "~p", [__Data], [__Meta])
              catch
                _:_ ->
                    ok
              end
          end)())).

-define(slog(__Level, __Data, __Meta),
        ?__hut_slog_helper(__Level, __Data, maps:merge(?__hut_logger_metadata, __Meta))).

-define(slog(__Level, __Data),
        ?slog(__Level, __Data, #{})).

-endif. %% !slog

-ifndef(set_process_metadata).
-define(set_process_metadata(__Meta),
        put(hut_process_metadata, __Meta)).
-endif. %% !set_process_metadata

% End of log declarations
-endif. %% __HUT_HRL__
