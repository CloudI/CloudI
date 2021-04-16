-module(hut).

-export([log/5]).

-include("hut.hrl").

log("sasl", info, Fmt, Args, Opts) ->
  error_logger:info_report([{msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("sasl", warning, Fmt, Args, Opts) ->
  error_logger:warning_report([{msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("sasl", error, Fmt, Args, Opts) ->
  error_logger:error_report([{msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("sasl", Level, Fmt, Args, Opts) when Level =:= debug; Level =:= notice ->
  error_logger:info_report([{sublevel, Level}, {msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("sasl", Level, Fmt, Args, Opts) when Level =:= critical; Level =:= alert; Level =:= emergency ->
  error_logger:error_report([{sublevel, Level}, {msg, ?__fmt(Fmt, Args)}, {options, Opts}]);
log("sasl", _Level, _Fmt, _Args, _Opts) ->
  ok.
