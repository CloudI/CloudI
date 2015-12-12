-module(exometer_test_util).

-export([ensure_all_started/1,
         majority/2,
         majority/3]).

-define(MAJORITY_COUNT_ENV, "CT_MAJORITY_COUNT").
-define(DEFAULT_MAJORITY_COUNT, 5).

%% This implementation is originally from Basho's Webmachine. On
%% older versions of Erlang, we don't have
%% application:ensure_all_started, so we use this wrapper function to
%% either use the native implementation or our own version, depending
%% on what's available.
-spec ensure_all_started(atom()) -> {ok, [atom()]} | {error, term()}.
ensure_all_started(App) ->
    case erlang:function_exported(application, ensure_all_started, 1) of
        true ->
            application:ensure_all_started(App);
        false ->
            ensure_all_started(App, [])
    end.

%% This implementation is originally from Basho's
%% Webmachine. Reimplementation of ensure_all_started. NOTE this does
%% not behave the same as the native version in all cases, but as a
%% quick hack it works well enough for our purposes. Eventually I
%% assume we'll drop support for older versions of Erlang and this can
%% be eliminated.
ensure_all_started(App, Apps0) ->
    case application:start(App) of
        ok ->
            {ok, lists:reverse([App | Apps0])};
        {error,{already_started,App}} ->
            {ok, lists:reverse(Apps0)};
        {error,{not_started,BaseApp}} ->
            {ok, Apps} = ensure_all_started(BaseApp, Apps0),
            ensure_all_started(App, [BaseApp|Apps])
    end.

majority(F, Cfg) ->
    case os:getenv(?MAJORITY_COUNT_ENV) of
        false ->
            majority(?DEFAULT_MAJORITY_COUNT, F, Cfg);
        Count ->
            case catch erlang:list_to_integer(Count) of
                C when is_integer(C) ->
                    majority(C, F, Cfg);
                _ ->
                    ct:pal("Invalid value for '~s' given", [?MAJORITY_COUNT_ENV]),
                    majority(?DEFAULT_MAJORITY_COUNT, F, Cfg)
            end
    end.

%% Run test N times. Success if a majority of the tests succeed.
%% Cleanup between runs done by calling F({cleanup, Config})
%% Returns 'ok' or {error, Info}.
%%
majority(N, F, Cfg) ->
    majority(N, F, Cfg, []).

majority(0, _, _, Hist) ->
    Failed = length([1 || {caught,_,_} <- Hist]),
    LogMsg = lists:flatten(io_lib:format("majority: Failed = ~p, Hist = ~p", [Failed, Hist])),
    ct:pal(LogMsg),
    case {Failed, length(Hist)} of
        {Lf, L} when Lf >= L div 2 ->
            ct:fail({error, {too_many_failures, Hist}});
        _ ->
            {comment, LogMsg}
    end;
majority(N, F, Cfg, Hist) when N > 0 ->
    Res = try F(Cfg)
          catch
              C:R ->
                  {caught, C, R}
          after
              F({cleanup, Cfg})
          end,
    majority(N-1, F, Cfg, [Res|Hist]).
