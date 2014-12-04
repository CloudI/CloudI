%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the request_rate Test==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014 Michael Truog
%%% @version 1.4.0 {@date} {@time}
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

%% Example Usage:

% Hardware:
%  Core i7 2670QM 2.2GHz 4 cores, 8 hyper-threads
%  L2:4×256KB L3:6MB RAM:8GB:DDR3-1333MHz
%  Sandy Bridge-HE-4 (Socket G2)
%
% Software:
%  Ubuntu 12.04.2 LTS (GNU/Linux 3.2.0-29-generic x86_64)
%  Erlang R16B03-1 (with lock counting, so probably slower than possible)
%
%
% Basic Speed Test:
% Max Stable (Total) Request Rate (during informal testing):
%  requests/second dest_refresh_method duo_mode sender receiver
%             1000           immediate    false      1        1
%            12000           immediate     true      1        1
%            13000                lazy     true      1        1
%            20000                lazy     true      2        4
%            27000                lazy     true      3        6 (overheating)
%
% (dest_refresh_method is set on cloudi_service_request_rate
%  duo_mode is set on cloudi_service_request_rate when true
%  sender is the number of cloudi_service_request_rate processes while
%  receiver is the number of cloudi_service_http_req processes)
%
% Basic Memory Test:
% Max Stable (Total) Request Rate (during informal testing):
%  requests/second dest_refresh_method duo_mode sender receiver queue_size(kB)
%              149           immediate    false      1        1            512 
%             1049           immediate    false      1        1           4096
%             1125           immediate    false      1        1         131072
%               97                lazy     true      1        1            512 
%              772                lazy     true      1        1           4096
%             8128                lazy     true      1        1         131072
%
%  requests/second dest_refresh_method duo_mode sender receiver    queue_limit
%                1           immediate    false      1        1              0
%                1                lazy     true      1        1              0
% 
% Hardware:
%  IBM POWER System S822 (POWER8) 3.42 GHz 2 cpus, 10 cores, 8 hyper-threads
%  L2:10x512KB L3:10x8MB L4:DIMMx16MB RAM:160GB:DDR3-1333MHz
%
% Software:
%  Fedora 20 (GNU/Linux 3.15.10-201.fc20.ppc64p7 ppc64)
%  Erlang 17.3
%
% Max Stable (Total) Request Rate
%  requests/second dest_refresh_method duo_mode sender receiver
%             1124           immediate    false      1        1
%             1199                lazy    false      1        1
%            11500           immediate     true      1        1
%            12199                lazy     true      1        1
%            15556                lazy     true      1        2
%            13995                lazy     true      1        3
%            13180                lazy     true      1        4
%            15484                lazy     true      2        2
%            22635                lazy     true      2        3
%            22132                lazy     true      2        4
%            21410                lazy     true      2        5
%            25743                lazy     true      3        3
%            30660                lazy     true      3        4
%            28407                lazy     true      3        5
%            27915                lazy     true      3        6
%            31000                lazy     true      4        4
%            30786                lazy     true      4        5
%            20734                lazy     true      5        5
%            33073                lazy     true      5        6


-define(DEFAULT_SERVICE_NAME,   "/tests/http_req/erlang.xml/get").
-define(DEFAULT_REQUEST,        <<(<<"value">>)/binary, 0:8,
                                  (<<"40">>)/binary, 0:8>>).
-define(DEFAULT_REQUEST_RATE,          dynamic). % requests/second
-define(DEFAULT_TICK_LENGTH,              5000). % ms (set as async timeout)
-define(DEFAULT_TICK_STABLE_COUNT,           4). % dynamic attempts for stable

-ifdef(ERLANG_OTP_VER_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.

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
        name :: string(),
        request :: any(),
        request_rate :: pos_integer() | #dynamic{},
        request_count :: non_neg_integer(),
        request_ids :: dict_proxy(cloudi_service:trans_id(), undefined),
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
        {request,                ?DEFAULT_REQUEST},
        {request_rate,           ?DEFAULT_REQUEST_RATE},
        {tick_length,            ?DEFAULT_TICK_LENGTH},
        {tick_stable_count,      ?DEFAULT_TICK_STABLE_COUNT}],
    [Name, Request0, RequestRate0, TickLength, TickStableCount] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_list(Name) andalso is_integer(hd(Name)),
    RequestN = case Request0 of
        {M, F} = Request1 ->
            case erlang:function_exported(M, F, 0) of
                true ->
                    fun M:F/0;
                false ->
                    Request1
            end;
        F when is_function(F) ->
            true = is_function(F, 0),
            F;
        Request1 ->
            Request1
    end,
    true = is_integer(TickStableCount) andalso (TickStableCount > 0),
    RequestRateN = if
        RequestRate0 =:= dynamic ->
            #dynamic{count_stable_max = TickStableCount};
        is_number(RequestRate0) andalso (RequestRate0 > 0.0) ->
            RequestRate0
    end,
    true = is_integer(TickLength) andalso (TickLength >= 1000),
    tick_start(Dispatcher),
    {ok, #state{name = Name,
                request = RequestN,
                request_rate = RequestRateN,
                tick_length = TickLength}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{} = State,
                              _Dispatcher) ->
    {noreply, State}.

cloudi_service_handle_info(#return_async_active{trans_id = TransId},
                           #state{request_count = RequestCount,
                                  request_ids = RequestIds} = State,
                           _Dispatcher) ->
    case dict:find(TransId, RequestIds) of
        {ok, _} ->
            {noreply, State#state{request_count = RequestCount + 1}};
        error ->
            {noreply, State}
    end;
cloudi_service_handle_info(#timeout_async_active{},
                           #state{} = State,
                           _Dispatcher) ->
    {noreply, State};
cloudi_service_handle_info(tick,
                           #state{name = Name,
                                  request = Request,
                                  request_rate = RequestRate,
                                  tick_length = TickLength} = State,
                           Dispatcher) ->
    {RequestCountOut,
     RequestRateNew} = request_count_sent(RequestRate, undefined, TickLength),
    tick_send(TickLength, Dispatcher),
    RequestIds = tick_request_send(RequestCountOut, Name, Request, Dispatcher),
    {noreply, State#state{request_rate = RequestRateNew,
                          request_count = 0,
                          request_ids = RequestIds}};
cloudi_service_handle_info({tick, T1},
                           #state{name = Name,
                                  request = Request,
                                  request_rate = RequestRate,
                                  request_count = RequestCountIn,
                                  tick_length = TickLength} = State,
                           Dispatcher) ->
    Elapsed = timer:now_diff(erlang:now(), T1) / 1000000.0, % seconds
    RequestRateComplete = RequestCountIn / Elapsed,
    {RequestCountOut,
     RequestRateNew} = request_count_sent(RequestRate,
                                          RequestRateComplete, TickLength),
    request_rate_output(RequestRateNew, Elapsed, RequestRateComplete, Name),
    tick_send(TickLength, Dispatcher),
    RequestIds = tick_request_send(RequestCountOut, Name, Request, Dispatcher),
    {noreply, State#state{request_rate = RequestRateNew,
                          request_count = 0,
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

request_count_sent(RequestRate, TickLength) ->
    erlang:round(RequestRate * (TickLength / 1000.0)).

request_count_sent(#dynamic{request_rate = RequestRate} = Dynamic,
                   undefined, TickLength) ->
    {request_count_sent(RequestRate, TickLength), Dynamic};
request_count_sent(#dynamic{count_stable_max = CountStableMax,
                            count_stable = CountStable,
                            request_rate = RequestRateOld,
                            request_rate_stable = RequestRateStable,
                            request_rate_max = RequestRateMax} = Dynamic,
                   RequestRateComplete, TickLength) ->
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
                    ?LOG_WARN("failed ~p requests/second after ~p ticks",
                              [RequestRateStable, CountStable + 1]),
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
            ?LOG_DEBUG("attempt ~p requests/second", [RequestRateNew]);
        true ->
            ok
    end,
    {request_count_sent(RequestRateNew, TickLength), DynamicNew};
request_count_sent(RequestRate, _, TickLength) ->
    {request_count_sent(RequestRate, TickLength), RequestRate}.

request_rate_output_log(RequestRate, Elapsed, RequestRateComplete, Name) ->
    ?LOG_INFO("~p requests/second~n"
              "(to ~s,~n"
              " during ~p seconds,~n"
              " sent ~p requests/second)",
              [erlang:round(RequestRateComplete * 10.0) / 10.0, Name,
               erlang:round(Elapsed * 10.0) / 10.0, RequestRate]).

request_rate_output(#dynamic{count_stable_max = CountStableMax,
                             count_stable = CountStable,
                             request_rate_stable = RequestRateStable},
                    Elapsed, RequestRateComplete, Name) ->
    if
        CountStable >= CountStableMax ->
            request_rate_output_log(RequestRateStable,
                                    Elapsed, RequestRateComplete, Name);
        true ->
            ok
    end;
request_rate_output(RequestRate, Elapsed, RequestRateComplete, Name) ->
    request_rate_output_log(RequestRate, Elapsed, RequestRateComplete, Name).

tick_start(Dispatcher) ->
    erlang:send_after(500, cloudi_service:self(Dispatcher), tick),
    ok.

tick_send(TickLength, Dispatcher) ->
    erlang:send_after(TickLength,
                      cloudi_service:self(Dispatcher),
                      {tick, erlang:now()}),
    ok.

tick_request_send(I, Name, Request, Dispatcher) ->
    RequestData = if
        is_function(Request) ->
            Request();
        true ->
            Request
    end,
    tick_request_send(I, dict:new(), Name, RequestData, Dispatcher).

tick_request_send(0, RequestIds, _, _, _) ->
    RequestIds;
tick_request_send(I, RequestIds, Name, Request, Dispatcher) ->
    NewRequestIds = case cloudi_service:send_async_active(Dispatcher,
                                                          Name, Request) of
        {ok, TransId} ->
            dict:store(TransId, undefined, RequestIds);
        {error, _} ->
            RequestIds
    end,
    tick_request_send(I - 1, NewRequestIds, Name, Request, Dispatcher).

