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
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_request_rate).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include("cloudi_logger.hrl").
-include("cloudi_service.hrl").

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
% Max Stable (Total) Request Rate (during informal testing):
%  requests/second dest_refresh_method duo_mode sender receiver
%             1000           immediate    false      1        1
%            12000           immediate     true      1        1
%            13000                lazy     true      1        1
%            20000                lazy     true      2        4
%            27000                lazy     true      3        6 (overheating)
%
% (sender is the number of cloudi_service_request_rate processes while
%  receiver is the number of cloudi_service_http_req processes)

-define(DEFAULT_SERVICE_NAME,   "/tests/http_req/erlang.xml/get").
-define(DEFAULT_REQUEST,        <<(<<"value">>)/binary, 0:8,
                                  (<<"40">>)/binary, 0:8>>).
-define(DEFAULT_REQUEST_RATE,            10000). % requests/second
-define(DEFAULT_TICK_LENGTH,              5000). % ms (set as async timeout)

-record(state, {
        name :: string(),
        request :: any(),
        request_rate :: pos_integer(),
        request_count :: non_neg_integer(),
        request_ids,
        tick_length :: pos_integer()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->

    Defaults = [
        {service_name,           ?DEFAULT_SERVICE_NAME},
        {request,                ?DEFAULT_REQUEST},
        {request_rate,           ?DEFAULT_REQUEST_RATE},
        {tick_length,            ?DEFAULT_TICK_LENGTH}],
    [Name, Request, RequestRate, TickLength] =
        cloudi_proplists:take_values(Defaults, Args),
    true = (is_list(Name) andalso is_integer(hd(Name))),
    true = (is_number(RequestRate) andalso (RequestRate > 0.0)),
    true = (is_integer(TickLength) andalso (TickLength >= 1000)),
    tick_start(Dispatcher),
    {ok, #state{name = Name,
                request = Request,
                request_rate = RequestRate,
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
    RequestCountOut = request_count_sent(RequestRate, TickLength),
    tick_send(TickLength, Dispatcher),
    RequestIds = tick_request_send(RequestCountOut, dict:new(),
                                   Name, Request, Dispatcher),
    {noreply, State#state{request_count = 0,
                          request_ids = RequestIds}};
cloudi_service_handle_info({tick, T1},
                           #state{name = Name,
                                  request = Request,
                                  request_rate = RequestRate,
                                  request_count = RequestCountIn,
                                  tick_length = TickLength} = State,
                           Dispatcher) ->
    Elapsed = timer:now_diff(erlang:now(), T1) / 1000000.0, % seconds
    RequestCountOut = request_count_sent(RequestRate, TickLength),
    RequestRateComplete = erlang:round((RequestCountIn /
                                        Elapsed) * 10.0) / 10.0,
    ?LOG_INFO("~p requests/second "
              "(to ~s, during ~p seconds, to attempt ~p requests/second)",
              [RequestRateComplete, Name,
               erlang:round(Elapsed * 10.0) / 10.0, RequestRate]),
    tick_send(TickLength, Dispatcher),
    RequestIds = tick_request_send(RequestCountOut, dict:new(),
                                   Name, Request, Dispatcher),
    {noreply, State#state{request_count = 0,
                          request_ids = RequestIds}};
cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

request_count_sent(RequestRate, TickLength) ->
    erlang:round(RequestRate * (TickLength / 1000.0)).

tick_start(Dispatcher) ->
    erlang:send_after(500, cloudi_service:self(Dispatcher), tick),
    ok.

tick_send(TickLength, Dispatcher) ->
    erlang:send_after(TickLength,
                      cloudi_service:self(Dispatcher),
                      {tick, erlang:now()}),
    ok.

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

