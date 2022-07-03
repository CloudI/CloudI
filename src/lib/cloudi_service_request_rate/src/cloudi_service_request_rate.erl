%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Request rate testing CloudI Service==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2022 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2014-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_request_rate).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_SERVICE_NAME,   "/tests/http_req/erlang.xml/get").
-define(DEFAULT_REQUEST_INFO,             <<>>).
-define(DEFAULT_REQUEST,           <<"value", 0,
                                     "40", 0>>).
-define(DEFAULT_RESPONSE_INFO,       undefined). % see below:
        % check the response_info for each service request with
        % the return value of an anonymous function (arity 1)
        % or as an exact match on a value
        % (undefined means no check occurs)
-define(DEFAULT_RESPONSE,
        fun
            (<<>>, <<>>) ->
                false;
            (_, _) ->
                true
        end).                                    % see below:
        % check the response for each service request with
        % the return value of an anonymous function (arity 2)
        % or as an exact match on a value
        % (undefined means no check occurs)
-define(DEFAULT_REQUEST_RATE,          dynamic). % requests/second
-define(DEFAULT_TICK_LENGTH,              5000). % ms (set as async timeout)
-define(DEFAULT_TICK_STABLE_COUNT,          24). % dynamic attempts for stable

-record(dynamic,
    {
        count_stable_max :: pos_integer(),
        count_stable = 0 :: non_neg_integer(),
        request_rate = 1000 :: pos_integer(),
        request_rate_stable = undefined :: pos_integer() | undefined,
        request_rate_max = undefined :: pos_integer() | undefined
    }).

-record(state,
    {
        service :: cloudi_service:source(),
        process_index :: non_neg_integer(),
        process_count :: pos_integer(),
        name :: string(),
        request_info :: any(),
        request :: any(),
        response_info :: any(),
        response :: any(),
        tick_length :: pos_integer(),
        results_name :: cloudi_service:service_name(),
        results_count = 0 :: non_neg_integer(),
        results = [] :: list(),
        request_rate :: pos_integer() | #dynamic{},
        request_rate_count = 0 :: non_neg_integer(),
        request_rate_avg = undefined :: float() | undefined,
        request_rate_min = undefined :: float() | undefined,
        request_rate_max = undefined :: float() | undefined,
        request_success = 0 :: non_neg_integer(),
        request_fail = 0 :: non_neg_integer(),
        request_latency = cloudi_statistics:new() :: cloudi_statistics:state(),
        request_ids = #{} :: #{cloudi_service:trans_id() := undefined}
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {service_name,           ?DEFAULT_SERVICE_NAME},
        {request_info,           ?DEFAULT_REQUEST_INFO},
        {request,                ?DEFAULT_REQUEST},
        {response_info,          ?DEFAULT_RESPONSE_INFO},
        {response,               ?DEFAULT_RESPONSE},
        {request_rate,           ?DEFAULT_REQUEST_RATE},
        {tick_length,            ?DEFAULT_TICK_LENGTH},
        {tick_stable_count,      ?DEFAULT_TICK_STABLE_COUNT}],
    [Name, RequestInfo0, Request0, ResponseInfo0, Response0,
     RequestRate0, TickLength, TickStableCount] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_list(Name) andalso is_integer(hd(Name)),
    RequestInfoN = if
        is_function(RequestInfo0) ->
            true = is_function(RequestInfo0, 0),
            RequestInfo0;
        true ->
            RequestInfo0
    end,
    RequestN = if
        is_function(Request0) ->
            true = is_function(Request0, 0),
            Request0;
        true ->
            Request0
    end,
    ResponseInfoN = if
        is_function(ResponseInfo0) ->
            true = is_function(ResponseInfo0, 1),
            ResponseInfo0;
        true ->
            ResponseInfo0
    end,
    ResponseN = if
        is_function(Response0) ->
            true = is_function(Response0, 2),
            Response0;
        true ->
            Response0
    end,
    true = is_integer(TickStableCount) andalso (TickStableCount > 0),
    RequestRateN = if
        RequestRate0 =:= dynamic ->
            #dynamic{count_stable_max = TickStableCount};
        is_number(RequestRate0) andalso (RequestRate0 > 0.0) ->
            RequestRate0
    end,
    true = is_integer(TickLength) andalso (TickLength >= 1000),
    Service = cloudi_service:self(Dispatcher),
    ProcessIndex = cloudi_service:process_index(Dispatcher),
    ProcessCount = cloudi_service:process_count(Dispatcher),
    true = ProcessCount >= 2,
    ProcessCountMin = cloudi_service:process_count_min(Dispatcher),
    ProcessCountMax = cloudi_service:process_count_max(Dispatcher),
    true = (ProcessCountMin =:= ProcessCountMax) andalso
           (ProcessCountMin =:= ProcessCount),
    if
        ProcessIndex == 0 ->
            % process 0 handles collecting and storing results
            ok = cloudi_service:subscribe(Dispatcher, "results");
        true ->
            ok = tick_start(Service)
    end,
    false = cloudi_service_name:pattern(Prefix),
    ResultsName = Prefix ++ "results",
    {ok, #state{service = Service,
                process_index = ProcessIndex,
                process_count = ProcessCount,
                name = Name,
                request_info = RequestInfoN,
                request = RequestN,
                response_info = ResponseInfoN,
                response = ResponseN,
                tick_length = TickLength,
                results_name = ResultsName,
                request_rate = RequestRateN}}.

cloudi_service_handle_request(_RequestType, ResultsName, ResultsName,
                              _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{process_count = ProcessCount,
                                     results_name = ResultsName,
                                     results_count = ResultsCount,
                                     results = Results} = State,
                              _Dispatcher) ->
    {ResultsCountNew,
     ResultsNew} = process_results(ResultsCount + 1,
                                   lists:keymerge(1, [Request], Results),
                                   ProcessCount - 1),
    {noreply, State#state{results_count = ResultsCountNew,
                          results = ResultsNew}}.

cloudi_service_handle_info(tick,
                           #state{service = Service,
                                  process_index = ProcessIndex,
                                  name = Name,
                                  request_info = RequestInfo,
                                  request = Request,
                                  tick_length = TickLength,
                                  request_rate = RequestRate} = State,
                           Dispatcher) ->
    {RequestCount,
     RequestRateNew} = request_count_sent(RequestRate, undefined,
                                          TickLength, ProcessIndex),
    ok = tick_send(TickLength, Service),
    RequestIds = tick_request_send(RequestCount, Name,
                                   RequestInfo, Request,
                                   TickLength, Dispatcher),
    {noreply, State#state{request_rate = RequestRateNew,
                          request_success = 0,
                          request_fail = 0,
                          request_ids = RequestIds}};
cloudi_service_handle_info({tick, T1},
                           #state{service = Service,
                                  process_index = ProcessIndex,
                                  name = Name,
                                  request_info = RequestInfo,
                                  request = Request,
                                  tick_length = TickLength,
                                  results_name = ResultsName,
                                  request_rate = RequestRate,
                                  request_rate_count = RequestRateCompleteCount,
                                  request_rate_avg = RequestRateCompleteAvg,
                                  request_rate_min = RequestRateCompleteMin,
                                  request_rate_max = RequestRateCompleteMax,
                                  request_success = RequestSuccess,
                                  request_fail = RequestFail,
                                  request_latency = RequestLatency} = State,
                           Dispatcher) ->
    Elapsed = (cloudi_timestamp:microseconds_monotonic() -
               T1) / 1000000.0, % seconds
    RequestRateComplete = RequestSuccess / Elapsed,
    {RequestCount,
     RequestRateNew} = request_count_sent(RequestRate, RequestRateComplete,
                                          TickLength, ProcessIndex),
    {RequestRateCompleteCountNew,
     RequestRateCompleteAvgNew,
     RequestRateCompleteMinNew,
     RequestRateCompleteMaxNew} = request_rate(RequestRateNew,
                                               RequestRateComplete,
                                               RequestRateCompleteCount,
                                               RequestRateCompleteAvg,
                                               RequestRateCompleteMin,
                                               RequestRateCompleteMax,
                                               Elapsed, Name, ProcessIndex),
    {ok, _} = cloudi_service:send_async(Dispatcher,
                                        ResultsName,
                                        {ProcessIndex,
                                         RequestFail,
                                         RequestRateCompleteCountNew,
                                         RequestRateCompleteAvgNew,
                                         RequestRateCompleteMinNew,
                                         RequestRateCompleteMaxNew,
                                         RequestLatency}),
    ok = tick_send(TickLength, Service),
    RequestIds = tick_request_send(RequestCount, Name,
                                   RequestInfo, Request,
                                   TickLength, Dispatcher),
    {noreply, State#state{request_rate = RequestRateNew,
                          request_rate_count = RequestRateCompleteCountNew,
                          request_rate_avg = RequestRateCompleteAvgNew,
                          request_rate_min = RequestRateCompleteMinNew,
                          request_rate_max = RequestRateCompleteMaxNew,
                          request_success = 0,
                          request_fail = 0,
                          request_latency = cloudi_statistics:new(),
                          request_ids = RequestIds}};
cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {noreply, request_count_recv(Request, State)}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ?LOG_INFO("terminate request_rate erlang", []),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

validate_f_return(Value) when is_boolean(Value) ->
    Value.

validate(undefined, undefined, _, _) ->
    true;
validate(RInfoF, RF, RInfo, R) ->
    (if
        RInfoF =:= undefined ->
            true;
        is_function(RInfoF) ->
            validate_f_return(RInfoF(RInfo));
        true ->
            RInfoF =:= RInfo
    end) andalso
    (if
        RF =:= undefined ->
            true;
        is_function(RF) ->
            validate_f_return(RF(RInfo, R));
        true ->
            RF =:= R
    end).

request_count_sent(RequestRate, TickLength) ->
    erlang:round(RequestRate * (TickLength / 1000.0)).

request_count_sent(#dynamic{request_rate = RequestRate} = Dynamic,
                   undefined, TickLength, _) ->
    {request_count_sent(RequestRate, TickLength), Dynamic};
request_count_sent(#dynamic{count_stable_max = CountStableMax,
                            count_stable = CountStable,
                            request_rate = RequestRateOld,
                            request_rate_stable = RequestRateStable,
                            request_rate_max = RequestRateMax} = Dynamic,
                   RequestRateComplete, TickLength, ProcessIndex) ->
    % result will be within 5% (or 1 request)
    Offset = erlang:max(erlang:round(RequestRateComplete * 0.05) - 1, 1),
    RequestRateCompleteOffset = erlang:round(RequestRateComplete) + Offset,
    #dynamic{request_rate = RequestRateNew,
             count_stable = CountStableNew} = DynamicNew = if
        RequestRateOld =< RequestRateCompleteOffset ->
            % success sending the request_rate
            if
                RequestRateMax =:= undefined ->
                    Dynamic#dynamic{
                        request_rate = RequestRateOld * 2};
                is_integer(RequestRateMax),
                RequestRateOld =< RequestRateMax ->
                    I = RequestRateMax - RequestRateOld,
                    if
                        I =< Offset ->
                            Dynamic#dynamic{
                                count_stable = CountStable + 1,
                                request_rate_stable = RequestRateOld};
                        true ->
                            Delta = erlang:max(erlang:round(I / 2.0), 1),
                            RequestRateNext = RequestRateOld + Delta,
                            Dynamic#dynamic{
                                request_rate = RequestRateNext}
                    end
            end;
        RequestRateOld > RequestRateCompleteOffset ->
            % failed to send the request_rate
            I = RequestRateOld - RequestRateComplete,
            Delta = erlang:max(erlang:round(I / 2.0), 1),
            RequestRateNext = erlang:max(RequestRateOld - Delta, 1),
            RequestRateMaxNext = if
                CountStable >= CountStableMax ->
                    Elapsed = (CountStable + 1) * TickLength / 1000.0,
                    ?LOG_WARN("~3.. w: failed ~w requests/second"
                              " after ~s",
                              [ProcessIndex, RequestRateStable,
                               seconds_to_string(Elapsed)]),
                    RequestRateStable - 1;
                true ->
                    RequestRateOld
            end,
            Dynamic#dynamic{
                count_stable = 0,
                request_rate = RequestRateNext,
                request_rate_stable = undefined,
                request_rate_max = RequestRateMaxNext}
    end,
    if
        CountStableNew == 0 ->
            ?LOG_DEBUG("~3.. w: attempt ~w requests/second",
                       [ProcessIndex, RequestRateNew]);
        true ->
            ok
    end,
    {request_count_sent(RequestRateNew, TickLength), DynamicNew};
request_count_sent(RequestRate, _, TickLength, _) ->
    {request_count_sent(RequestRate, TickLength), RequestRate}.

request_count_recv(#return_async_active{response_info = ResponseInfo,
                                        response = Response,
                                        trans_id = TransId},
                   #state{response_info = ValidateResponseInfo,
                          response = ValidateResponse,
                          request_success = RequestSuccess,
                          request_fail = RequestFail,
                          request_latency = RequestLatency,
                          request_ids = RequestIds} = State) ->
    case maps:find(TransId, RequestIds) of
        {ok, _} ->
            case validate(ValidateResponseInfo, ValidateResponse,
                          ResponseInfo, Response) of
                true ->
                    MicroSeconds = cloudi_trans_id:microseconds() -
                                   cloudi_trans_id:microseconds(TransId),
                    RequestLatencyNew = cloudi_statistics:add(MicroSeconds,
                                                              RequestLatency),
                    State#state{request_success = RequestSuccess + 1,
                                request_latency = RequestLatencyNew};
                false ->
                    State#state{request_fail = RequestFail + 1}
            end;
        error ->
            State
    end;
request_count_recv(#timeout_async_active{}, #state{} = State) ->
    State.

request_rate_stable(RequestRateComplete,
                    RequestRateCompleteCount, RequestRateCompleteAvg,
                    RequestRateCompleteMin, RequestRateCompleteMax) ->
    RequestRateCompleteCountNew = RequestRateCompleteCount + 1,
    RequestRateCompleteAvgNew = RequestRateCompleteAvg +
        (RequestRateComplete - RequestRateCompleteAvg) /
        RequestRateCompleteCountNew,
    RequestRateCompleteMinNew = float_min(RequestRateComplete,
                                          RequestRateCompleteMin),
    RequestRateCompleteMaxNew = float_max(RequestRateComplete,
                                          RequestRateCompleteMax),
    {RequestRateCompleteCountNew, RequestRateCompleteAvgNew,
     RequestRateCompleteMinNew, RequestRateCompleteMaxNew}.

request_rate(#dynamic{count_stable_max = CountStableMax,
                      count_stable = CountStable,
                      request_rate = RequestRate},
             RequestRateComplete,
             RequestRateCompleteCount, RequestRateCompleteAvg,
             RequestRateCompleteMin, RequestRateCompleteMax,
             Elapsed, Name, ProcessIndex) ->
    if
        CountStable >= CountStableMax ->
            request_rate_stable(RequestRateComplete,
                                RequestRateCompleteCount,
                                RequestRateCompleteAvg,
                                RequestRateCompleteMin,
                                RequestRateCompleteMax);
        is_float(RequestRateCompleteAvg),
        is_float(RequestRateCompleteMin),
        is_float(RequestRateCompleteMax) ->
            ?LOG_INFO("~3.. w: "
                      "unstable at ~.1f [~.1f .. ~.1f] requests/second~n"
                      "(to ~s,~n"
                      " stable during ~s,~n"
                      " sent ~w requests/second)",
                      [ProcessIndex,
                       round(RequestRateCompleteAvg * 10.0) / 10.0,
                       round(RequestRateCompleteMin * 10.0) / 10.0,
                       round(RequestRateCompleteMax * 10.0) / 10.0,
                       Name, seconds_to_string(Elapsed), RequestRate]),
            {0, 0.0, undefined, undefined};
        true ->
            {0, 0.0, undefined, undefined}
    end;
request_rate(_RequestRate, RequestRateComplete,
             RequestRateCompleteCount, RequestRateCompleteAvg,
             RequestRateCompleteMin, RequestRateCompleteMax,
             _Elapsed, _Name, _ProcessIndex) ->
    request_rate_stable(RequestRateComplete,
                        RequestRateCompleteCount,
                        RequestRateCompleteAvg,
                        RequestRateCompleteMin,
                        RequestRateCompleteMax).

process_results_data([],
                     RequestFailSum,
                     ResultsStable,
                     RequestRateCompleteAvgSum,
                     RequestRateCompleteMinSum,
                     RequestRateCompleteMaxSum,
                     RequestLatency) ->
    if
        RequestFailSum > 0 ->
            ?LOG_ERROR("~w requests failed validation",
                       [RequestFailSum]);
        true ->
            ok
    end,
    {ResultsStable,
     RequestRateCompleteAvgSum,
     RequestRateCompleteMinSum,
     RequestRateCompleteMaxSum,
     RequestLatency};
process_results_data([{_ProcessIndex,
                       RequestFail,
                       0,
                       0.0,
                       undefined,
                       undefined,
                       RequestLatency} | Results],
                     RequestFailSum,
                     _ResultsStable,
                     RequestRateCompleteAvgSum,
                     RequestRateCompleteMinSum,
                     RequestRateCompleteMaxSum,
                     RequestLatencyOld) ->
    process_results_data(Results,
                         RequestFailSum + RequestFail,
                         false,
                         RequestRateCompleteAvgSum,
                         RequestRateCompleteMinSum,
                         RequestRateCompleteMaxSum,
                         cloudi_statistics:merge(RequestLatency,
                                                 RequestLatencyOld));
process_results_data([{_ProcessIndex,
                       RequestFail,
                       RequestRateCompleteCount,
                       RequestRateCompleteAvg,
                       RequestRateCompleteMin,
                       RequestRateCompleteMax,
                       RequestLatency} | Results],
                     RequestFailSum,
                     ResultsStable,
                     RequestRateCompleteAvgSum,
                     RequestRateCompleteMinSum,
                     RequestRateCompleteMaxSum,
                     RequestLatencyOld) ->
    process_results_data(Results,
                         RequestFailSum + RequestFail,
                         ResultsStable andalso (RequestRateCompleteCount > 0),
                         RequestRateCompleteAvgSum + RequestRateCompleteAvg,
                         RequestRateCompleteMinSum + RequestRateCompleteMin,
                         RequestRateCompleteMaxSum + RequestRateCompleteMax,
                         cloudi_statistics:merge(RequestLatency,
                                                 RequestLatencyOld)).

process_results(ResultsCount, Results, ResultsCount) ->
    {ResultsStable,
     RequestRateCompleteAvgSum,
     RequestRateCompleteMinSum,
     RequestRateCompleteMaxSum,
     RequestLatency} = process_results_data(Results,
                                            0, true, 0.0, 0.0, 0.0,
                                            cloudi_statistics:new()),
    if
        ResultsStable =:= true ->
            StdDev = cloudi_statistics:standard_deviation(RequestLatency),
            Skewness = cloudi_statistics:skewness(RequestLatency),
            Kurtosis = cloudi_statistics:kurtosis(RequestLatency),
            ?LOG_INFO("stable at ~.1f [~.1f .. ~.1f] requests/second~n"
                      "  request latency:~n"
                      "    minimum  ~9.b us~n"
                      "    maximum  ~9.b us~n"
                      "    mean     ~9.b us~n"
                      "    stddev   ~12.2f~n"
                      "    skewness ~12.2f~n"
                      "    kurtosis ~12.2f",
                      [round(RequestRateCompleteAvgSum * 10.0) / 10.0,
                       round(RequestRateCompleteMinSum * 10.0) / 10.0,
                       round(RequestRateCompleteMaxSum * 10.0) / 10.0,
                       round(cloudi_statistics:minimum(RequestLatency)),
                       round(cloudi_statistics:maximum(RequestLatency)),
                       round(cloudi_statistics:mean(RequestLatency)),
                       round(StdDev * 100.0) / 100.0,
                       round(Skewness * 100.0) / 100.0,
                       round(Kurtosis * 100.0) / 100.0]);
        ResultsStable =:= false ->
            ok
    end,
    {0, []};
process_results(ResultsCount, Results, _) ->
    {ResultsCount, Results}.

tick_start(Service) ->
    erlang:send_after(500, Service, tick),
    ok.

tick_send(TickLength, Service) ->
    erlang:send_after(TickLength, Service,
                      {tick, cloudi_timestamp:microseconds_monotonic()}),
    ok.

tick_request_send(I, Name, RequestInfo, Request, Timeout, Dispatcher) ->
    RequestInfoData = if
        is_function(RequestInfo) ->
            RequestInfo();
        true ->
            RequestInfo
    end,
    RequestData = if
        is_function(Request) ->
            Request();
        true ->
            Request
    end,
    tick_request_send(I, #{}, Name,
                      RequestInfoData, RequestData, Timeout, Dispatcher).

tick_request_send(0, RequestIds, _, _, _, _, _) ->
    RequestIds;
tick_request_send(I, RequestIds, Name,
                  RequestInfo, Request, Timeout, Dispatcher) ->
    NewRequestIds = case cloudi_service:send_async_active(Dispatcher, Name,
                                                          RequestInfo, Request,
                                                          Timeout,
                                                          undefined) of
        {ok, TransId} ->
            maps:put(TransId, undefined, RequestIds);
        {error, _} ->
            RequestIds
    end,
    tick_request_send(I - 1, NewRequestIds, Name,
                      RequestInfo, Request, Timeout, Dispatcher).

seconds_to_string(Seconds)
    when Seconds > 60 * 60 ->
    Hours = Seconds / (60 * 60),
    io_lib:format("~.1f hours", [erlang:round(Hours * 10.0) / 10.0]);
seconds_to_string(Seconds)
    when Seconds > 60 ->
    Minutes = Seconds / 60,
    io_lib:format("~.1f minutes", [erlang:round(Minutes * 10.0) / 10.0]);
seconds_to_string(Seconds) ->
    io_lib:format("~.1f seconds", [erlang:round(Seconds * 10.0) / 10.0]).

float_max(Left, undefined)
    when is_float(Left) ->
    Left;
float_max(Left, Right)
    when is_float(Left), is_float(Right) ->
    if
        Left >= Right ->
            Left;
        true ->
            Right
    end.

float_min(Left, undefined)
    when is_float(Left) ->
    Left;
float_min(Left, Right)
    when is_float(Left), is_float(Right) ->
    if
        Left =< Right ->
            Left;
        true ->
            Right
    end.
