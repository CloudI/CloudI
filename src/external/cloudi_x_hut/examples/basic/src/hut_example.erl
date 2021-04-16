-module(hut_example).

-export([start/0, log/4]).

-include_lib("hut/include/hut.hrl").

start() ->
    % Ensure hut is started so all app env are available
    ok = application:start(hut),
    ?log(info, "Testing hut log levels with type ~s", [?log_type]),
    lists:foreach(
      fun
          ({M}) ->
              log_m(M);
          ({M, A}) ->
              log_ma(M, A);
          ({M, A, O}) ->
              log_mao(M, A, O)
      end, messages()),
    lists:foreach(
      fun
          ({R}) ->
              slog(R);
          ({R, M}) ->
              slog(R, M)
      end, reports()),
    ?log(info, "Testing DONE"),
    ok.

log(Level, Fmt, Args, Opts) ->
    io:format("EXAMPLELOG ~p: " ++ Fmt ++ "; OPTIONS=~p~n", [Level] ++ Args ++ [Opts]),
    ok.

% INTERNAL

messages() ->
    [
     {"Example Log Entry"},
     {"Example Log Entry ~s ~s", ["with", "arguments"]},
     {"Example Log Entry", [], [with, options]}
    ].

reports() ->
    [
     {#{from => {127,0,0,1}, to => {127,0,0,1}}},
     {#{from => {127,0,0,1}, to => {127,0,0,1}}, #{domain => [test]}}
    ].

% Helper functions to test the HUT_LAGER option as well

log_m(M) ->
    ?log(debug, M),
    ?log(info, M),
    ?log(notice, M),
    ?log(warning, M),
    ?log(error, M),
    ?log(critical, M),
    ?log(alert, M),
    ?log(emergency, M),
    ok.

log_ma(M, A) ->
    ?log(debug, M, A),
    ?log(info, M, A),
    ?log(notice, M, A),
    ?log(warning, M, A),
    ?log(error, M, A),
    ?log(critical, M, A),
    ?log(alert, M, A),
    ?log(emergency, M, A),
    ok.

log_mao(M, A, O) ->
    ?log(debug, M, A, O),
    ?log(info, M, A, O),
    ?log(notice, M, A, O),
    ?log(warning, M, A, O),
    ?log(error, M, A, O),
    ?log(critical, M, A, O),
    ?log(alert, M, A, O),
    ?log(emergency, M, A, O),
    ok.

slog(R) ->
    ?slog(debug, R),
    ?slog(info, R),
    ?slog(notice, R),
    ?slog(warning, R),
    ?slog(error, R),
    ?slog(critical, R),
    ?slog(alert, R),
    ?slog(emergency, R),
    ok.

slog(R, M) ->
    ?set_process_metadata(#{foo => bar}),
    ?slog(debug, R, M),
    ?slog(info, R, M),
    ?slog(notice, R, M),
    ?slog(warning, R, M),
    ?slog(error, R, M),
    ?slog(critical, R, M),
    ?slog(alert, R, M),
    ?slog(emergency, R, M),
    ok.
