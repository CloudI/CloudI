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
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_request_rate).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_SERVICE_NAME,   "/tests/http_req/erlang.xml/get").
-define(DEFAULT_REQUEST_INFO,             <<>>).
-define(DEFAULT_REQUEST,        <<(<<"value">>)/binary, 0:8,
                                  (<<"40">>)/binary, 0:8>>).
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
        process_index :: non_neg_integer(),
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
    ProcessIndex = cloudi_service:process_index(Dispatcher),
    tick_start(Dispatcher),
    {ok, #state{process_index = ProcessIndex,
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

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                trans_id = TransId},
                           #state{response_info = ValidateResponseInfo,
                                  response = ValidateResponse,
                                  request_success = RequestSuccess,
                                  request_fail = RequestFail,
                                  request_ids = RequestIds} = State,
                           _Dispatcher) ->
    case maps:find(TransId, RequestIds) of
        {ok, _} ->
            case validate(ValidateResponseInfo, ValidateResponse,
                          ResponseInfo, Response) of
                true ->
                    {noreply,
                     State#state{request_success = RequestSuccess + 1}};
                false ->
                    {noreply,
                     State#state{request_fail = RequestFail + 1}}
            end;
        error ->
            {noreply, State}
    end;
cloudi_service_handle_info(#timeout_async_active{},
                           #state{} = State,
                           _Dispatcher) ->
    {noreply, State};
cloudi_service_handle_info(tick,
                           #state{process_index = ProcessIndex,
                                  name = Name,
                                  request_info = RequestInfo,
                                  request = Request,
                                  request_rate = RequestRate,
                                  tick_length = TickLength} = State,
                           Dispatcher) ->
    {RequestCount,
     RequestRateNew} = request_count_sent(RequestRate, undefined,
                                          TickLength, ProcessIndex),
    tick_send(TickLength, Dispatcher),
    RequestIds = tick_request_send(RequestCount, Name,
                                   RequestInfo, Request, Dispatcher),
    {noreply, State#state{request_rate = RequestRateNew,
                          request_success = 0,
                          request_fail = 0,
                          request_ids = RequestIds}};
cloudi_service_handle_info({tick, T1},
                           #state{process_index = ProcessIndex,
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
    Elapsed = timer:now_diff(cloudi_timestamp:timestamp(),
                             T1) / 1000000.0, % seconds
    RequestRateComplete = RequestSuccessIn / Elapsed,
    {RequestCount,
     RequestRateNew} = request_count_sent(RequestRate, RequestRateComplete,
                                          TickLength, ProcessIndex),
    request_rate_output(RequestRateNew, Elapsed,
                        RequestRateComplete, Name, ProcessIndex),
    tick_send(TickLength, Dispatcher),
    RequestIds = tick_request_send(RequestCount, Name,
                                   RequestInfo, Request, Dispatcher),
    {noreply, State#state{request_rate = RequestRateNew,
                          request_success = 0,
                          request_ids = RequestIds}};
cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
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

request_rate_output_log(RequestRate, Elapsed,
                        RequestRateComplete, Name, ProcessIndex) ->
    ?LOG_INFO("~3.. w: ~.1f requests/second~n"
              "(to ~s,~n"
              " stable during ~s,~n"
              " sent ~w requests/second)",
              [ProcessIndex,
               erlang:round(RequestRateComplete * 10.0) / 10.0, Name,
               seconds_to_string(Elapsed), RequestRate]).

request_rate_output(#dynamic{count_stable_max = CountStableMax,
                             count_stable = CountStable,
                             request_rate_stable = RequestRateStable},
                    Elapsed, RequestRateComplete, Name, ProcessIndex) ->
    if
        CountStable == CountStableMax ->
            request_rate_output_log(RequestRateStable, Elapsed * CountStable,
                                    RequestRateComplete, Name, ProcessIndex);
        true ->
            ok
    end;
request_rate_output(RequestRate, Elapsed,
                    RequestRateComplete, Name, ProcessIndex) ->
    request_rate_output_log(RequestRate, Elapsed,
                            RequestRateComplete, Name, ProcessIndex).

tick_start(Dispatcher) ->
    erlang:send_after(500, cloudi_service:self(Dispatcher), tick),
    ok.

tick_send(TickLength, Dispatcher) ->
    erlang:send_after(TickLength,
                      cloudi_service:self(Dispatcher),
                      {tick, cloudi_timestamp:timestamp()}),
    ok.

tick_request_send(I, Name, RequestInfo, Request, Dispatcher) ->
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
                      RequestInfoData, RequestData, Dispatcher).

tick_request_send(0, RequestIds, _, _, _, _) ->
    RequestIds;
tick_request_send(I, RequestIds, Name,
                  RequestInfo, Request, Dispatcher) ->
    NewRequestIds = case cloudi_service:send_async_active(Dispatcher, Name,
                                                          RequestInfo, Request,
                                                          undefined,
                                                          undefined) of
        {ok, TransId} ->
            maps:put(TransId, undefined, RequestIds);
        {error, _} ->
            RequestIds
    end,
    tick_request_send(I - 1, NewRequestIds, Name,
                      RequestInfo, Request, Dispatcher).

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

