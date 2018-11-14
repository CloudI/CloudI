-module(hut).

-export([log/5]).

-include("hut.hrl").

log("default", info, Fmt, Args, Opts) ->
  error_logger:info_report([{msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("default", warning, Fmt, Args, Opts) ->
  error_logger:warning_report([{msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("default", error, Fmt, Args, Opts) ->
  error_logger:error_report([{msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("default", Level, Fmt, Args, Opts) when Level =:= debug; Level =:= notice ->
  error_logger:info_report([{sublevel, Level}, {msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("default", Level, Fmt, Args, Opts) when Level =:= critical; Level =:= alert; Level =:= emergency ->
  error_logger:error_report([{sublevel, Level}, {msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("default", _Level, _Fmt, _Args, _Opts) ->
  ok.
