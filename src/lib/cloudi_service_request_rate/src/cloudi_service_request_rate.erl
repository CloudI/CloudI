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
%%% Copyright (c) 2014-2018 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_request_rate).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_MODE,                 isolated).
        % The mode determines how results are created.
        % In 'isolated' mode, each process logs the results
        % so a manual summary based on the log output is necessary.
        % In 'crdt' mode, the processes communicate their current
        % state so a complete summary may be logged as the result.
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
        mode :: isolated | crdt,
        crdt :: cloudi_crdt:state(),
        name :: string(),
        request_info :: any(),
        request :: any(),
        response_info :: any(),
        response :: any(),
        request_rate :: pos_integer() | #dynamic{},
        request_success :: non_neg_integer(),
        request_fail :: non_neg_integer(),
        request_ids :: #{cloudi_service:trans_id() := undefined},
        tick_length :: pos_integer()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {mode,                   ?DEFAULT_MODE},
        {service_name,           ?DEFAULT_SERVICE_NAME},
        {request_info,           ?DEFAULT_REQUEST_INFO},
        {request,                ?DEFAULT_REQUEST},
        {response_info,          ?DEFAULT_RESPONSE_INFO},
        {response,               ?DEFAULT_RESPONSE},
        {request_rate,           ?DEFAULT_REQUEST_RATE},
        {tick_length,            ?DEFAULT_TICK_LENGTH},
        {tick_stable_count,      ?DEFAULT_TICK_STABLE_COUNT}],
    [Mode, Name, RequestInfo0, Request0, ResponseInfo0, Response0,
     RequestRate0, TickLength, TickStableCount] =
        cloudi_proplists:take_values(Defaults, Args),
    true = (Mode =:= isolated) orelse (Mode =:= crdt),
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
    ProcessCountMin = cloudi_service:process_count_min(Dispatcher),
    ProcessCountMax = cloudi_service:process_count_max(Dispatcher),
    true = (ProcessCountMin =:= ProcessCountMax) andalso
           (ProcessCountMin =:= ProcessCount),
    CRDT = if
        Mode =:= isolated ->
            undefined;
        Mode =:= crdt ->
            cloudi_crdt:new(Dispatcher,
                            [{timeout_default, TickLength * 6},
                             {priority_default_offset, -1}])
    end,
    tick_start(Service),
    {ok, #state{service = Service,
                process_index = ProcessIndex,
                process_count = ProcessCount,
                mode = Mode,
                crdt = CRDT,
                name = Name,
                request_info = RequestInfoN,
                request = RequestN,
                response_info = ResponseInfoN,
                response = ResponseN,
                request_rate = RequestRateN,
                request_success = 0,
                request_fail = 0,
                request_ids = #{},
                tick_length = TickLength}}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              #state{mode = crdt,
                                     crdt = CRDT0} = State, Dispatcher) ->
    {ok, CRDTN} = cloudi_crdt:handle_request(Type, Name, Pattern,
                                             RequestInfo, Request,
                                             Timeout, Priority, TransId, Pid,
                                             CRDT0, Dispatcher),
    {noreply, State#state{crdt = CRDTN}}.

cloudi_service_handle_info(tick,
                           #state{service = Service,
                                  process_index = ProcessIndex,
                                  name = Name,
                                  request_info = RequestInfo,
                                  request = Request,
                                  request_rate = RequestRate,
                                  tick_length = TickLength} = State,
                           Dispatcher) ->
    {RequestCount,
     RequestRateNew} = request_count_sent(RequestRate, undefined,
                                          TickLength, ProcessIndex),
    tick_send(TickLength, Service),
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
                                  process_count = ProcessCount,
                                  mode = Mode,
                                  crdt = CRDT0,
                                  name = Name,
                                  request_info = RequestInfo,
                                  request = Request,
                                  request_rate = RequestRate,
                                  request_success = RequestSuccessIn,
                                  request_fail = RequestFailIn,
                                  tick_length = TickLength} = State,
                           Dispatcher) ->
    if
        RequestFailIn > 0 ->
            ?LOG_ERROR("~3.. w: ~w requests failed validation",
                       [ProcessIndex, RequestFailIn]);
        true ->
            ok
    end,
    Elapsed = (cloudi_timestamp:microseconds_monotonic() -
               T1) / 1000000.0, % seconds
    RequestRateComplete = RequestSuccessIn / Elapsed,
    {RequestCount,
     RequestRateNew} = request_count_sent(RequestRate, RequestRateComplete,
                                          TickLength, ProcessIndex),
    CRDTN = if
        Mode =:= isolated ->
            isolated_output(RequestRateNew, RequestRateComplete,
                            Elapsed, Name, ProcessIndex),
            undefined;
        Mode =:= crdt ->
            crdt_output(RequestRateNew, RequestRateComplete,
                        Elapsed, Name, ProcessIndex, ProcessCount,
                        CRDT0, Dispatcher)
    end,
    tick_send(TickLength, Service),
    RequestIds = tick_request_send(RequestCount, Name,
                                   RequestInfo, Request,
                                   TickLength, Dispatcher),
    {noreply, State#state{crdt = CRDTN,
                          request_rate = RequestRateNew,
                          request_success = 0,
                          request_ids = RequestIds}};
cloudi_service_handle_info(Request,
                           #state{mode = isolated} = State, _Dispatcher) ->
    {noreply, request_count_recv(Request, State)};
cloudi_service_handle_info(Request,
                           #state{mode = crdt,
                                  crdt = CRDT0} = State, Dispatcher) ->
    case cloudi_crdt:handle_info(Request, CRDT0, Dispatcher) of
        {ok, CRDTN} ->
            {noreply, State#state{crdt = CRDTN}};
        {ignored, CRDTN} ->
            {noreply, request_count_recv(Request, State#state{crdt = CRDTN})}
    end.

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
                          request_ids = RequestIds} = State) ->
    case maps:find(TransId, RequestIds) of
        {ok, _} ->
            case validate(ValidateResponseInfo, ValidateResponse,
                          ResponseInfo, Response) of
                true ->
                    State#state{request_success = RequestSuccess + 1};
                false ->
                    State#state{request_fail = RequestFail + 1}
            end;
        error ->
            State
    end;
request_count_recv(#timeout_async_active{}, #state{} = State) ->
    State.

isolated_output_log(RequestRate, RequestRateComplete,
                    Elapsed, Name, ProcessIndex) ->
    ?LOG_INFO("~3.. w: ~.1f requests/second~n"
              "(to ~s,~n"
              " stable during ~s,~n"
              " sent ~w requests/second)",
              [ProcessIndex,
               erlang:round(RequestRateComplete * 10.0) / 10.0, Name,
               seconds_to_string(Elapsed), RequestRate]).

isolated_output(#dynamic{count_stable_max = CountStableMax,
                         count_stable = CountStable,
                         request_rate_stable = RequestRateStable},
                RequestRateComplete, Elapsed, Name, ProcessIndex) ->
    if
        CountStable == CountStableMax ->
            isolated_output_log(RequestRateStable, RequestRateComplete,
                                Elapsed * CountStable, Name, ProcessIndex);
        true ->
            ok
    end;
isolated_output(RequestRate, RequestRateComplete,
                Elapsed, Name, ProcessIndex) ->
    isolated_output_log(RequestRate, RequestRateComplete,
                        Elapsed, Name, ProcessIndex).

crdt_output_log(undefined, _, _, _, _) ->
    ok;
crdt_output_log(Elapsed, Name, ProcessCount, CRDT, Dispatcher) ->
    case crdt_get_request_rates(ProcessCount, CRDT, Dispatcher) of
        {RequestRate, RequestRateComplete} ->
            ?LOG_INFO("~w processes at ~.1f requests/second~n"
                      "(to ~s,~n"
                      " stable during ~s,~n"
                      " sent ~w requests/second)",
                      [ProcessCount,
                       erlang:round(RequestRateComplete * 10.0) / 10.0, Name,
                       seconds_to_string(Elapsed), RequestRate]);
        undefined ->
            ok
    end.

crdt_output(#dynamic{count_stable_max = CountStableMax,
                     count_stable = CountStable,
                     request_rate_stable = RequestRateStable},
            RequestRateComplete, Elapsed, Name, ProcessIndex, ProcessCount,
            CRDT0, Dispatcher) ->
    CountStableKey = {count_stable, ProcessIndex},
    ElapsedKey = {elapsed, ProcessIndex},
    RequestRateKey = {request_rate, ProcessIndex},
    RequestRateCompleteKey = {request_rate_complete, ProcessIndex},
    CRDT1 = if
        CountStable == 0 ->
            cloudi_crdt:put(Dispatcher, ElapsedKey, Elapsed, CRDT0);
        true ->
            cloudi_crdt:incr(Dispatcher, ElapsedKey, Elapsed, CRDT0)
    end,
    CRDT2 = cloudi_crdt:put(Dispatcher, CountStableKey,
                            CountStable, CRDT1),
    CRDT3 = cloudi_crdt:put(Dispatcher, RequestRateKey,
                            RequestRateStable, CRDT2),
    CRDTN = cloudi_crdt:put(Dispatcher, RequestRateCompleteKey,
                            RequestRateComplete, CRDT3),
    if
        ProcessIndex == 0,
        CountStable >= CountStableMax ->
            ElapsedTotal = crdt_get_dynamic_elapsed(ProcessCount,
                                                    CountStableMax,
                                                    CRDTN, Dispatcher),
            crdt_output_log(ElapsedTotal, Name, ProcessCount,
                            CRDTN, Dispatcher);
        true ->
            ok
    end,
    CRDTN;
crdt_output(RequestRate, RequestRateComplete,
            Elapsed, Name, ProcessIndex, ProcessCount,
            CRDT0, Dispatcher) ->
    RequestRateKey = {request_rate, ProcessIndex},
    RequestRateCompleteKey = {request_rate_complete, ProcessIndex},
    CRDT1 = cloudi_crdt:put(Dispatcher, RequestRateKey,
                            RequestRate, CRDT0),
    CRDTN = cloudi_crdt:put(Dispatcher, RequestRateCompleteKey,
                            RequestRateComplete, CRDT1),
    if
        ProcessIndex == 0 ->
            crdt_output_log(Elapsed, Name, ProcessCount,
                            CRDTN, Dispatcher);
        true ->
            ok
    end,
    CRDTN.

crdt_get_dynamic_elapsed(ProcessCount, CountStableMax, CRDT, Dispatcher) ->
    crdt_get_dynamic_elapsed(0, 0, ProcessCount,
                             CountStableMax, CRDT, Dispatcher).

crdt_get_dynamic_elapsed(ElapsedSum, ProcessCount, ProcessCount, _, _, _) ->
    ElapsedSum;
crdt_get_dynamic_elapsed(ElapsedSum, I, ProcessCount,
                         CountStableMax, CRDT, Dispatcher) ->
     case cloudi_crdt:find(Dispatcher, {count_stable, I}, CRDT) of
        {ok, CountStableI}
            when CountStableI >= CountStableMax ->
            ElapsedI = cloudi_crdt:get(Dispatcher, {elapsed, I}, CRDT),
            crdt_get_dynamic_elapsed(ElapsedSum + ElapsedI,
                                     I + 1, ProcessCount,
                                     CountStableMax, CRDT, Dispatcher);
        _ ->
            undefined
    end.

crdt_get_request_rates(ProcessCount, CRDT, Dispatcher) ->
    crdt_get_request_rates(0, 0, 0, ProcessCount, CRDT, Dispatcher).

crdt_get_request_rates(RequestRateSum, RequestRateCompleteSum,
                       ProcessCount, ProcessCount, _, _) ->
    {RequestRateSum, RequestRateCompleteSum};
crdt_get_request_rates(RequestRateSum, RequestRateCompleteSum,
                       I, ProcessCount, CRDT, Dispatcher) ->
     RequestRateKey = {request_rate, I},
     RequestRateCompleteKey = {request_rate_complete, I},
     case cloudi_crdt:find(Dispatcher, RequestRateKey, CRDT) of
        {ok, RequestRateI} ->
            case cloudi_crdt:find(Dispatcher, RequestRateCompleteKey, CRDT) of
                {ok, RequestRateCompleteI} ->
                    crdt_get_request_rates(RequestRateSum +
                                           RequestRateI,
                                           RequestRateCompleteSum +
                                           RequestRateCompleteI,
                                           I + 1, ProcessCount,
                                           CRDT, Dispatcher);
                _ ->
                    undefined
            end;
        _ ->
            undefined
    end.

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

