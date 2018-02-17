%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the count Test==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017-2018 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2017-2018 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_test_count).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state,
        {
            mode :: isolated | crdt,
            crdt = undefined :: cloudi_crdt:state() | undefined,
            count = 0 :: non_neg_integer()
        }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Timeout, Dispatcher) ->
    Defaults = [
        {mode,                             isolated}],
    [Mode] = cloudi_proplists:take_values(Defaults, Args),
    cloudi_service:subscribe(Dispatcher, "erlang/get"),
    State = if
        Mode =:= isolated ->
            #state{mode = Mode};
        Mode =:= crdt ->
            #state{mode = Mode,
                   crdt = cloudi_crdt:new(Dispatcher, Timeout,
                                          [{retry, 20}, % 5 minutes total
                                           {retry_delay, 15 * 1000}])} % 15 sec
    end,
    {ok, State}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{mode = isolated,
                                     count = Count0} = State, _Dispatcher) ->
    CountN = if
        Count0 == 4294967295 ->
            0;
        true ->
            Count0 + 1
    end,
    ?LOG_INFO("count == ~w erlang", [CountN]),
    Response = cloudi_string:format_to_binary("~w", [CountN]),
    {reply, Response, State#state{count = CountN}};
cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              #state{mode = crdt,
                                     crdt = CRDT0} = State, Dispatcher) ->
    case cloudi_crdt:handle_request(Type, Name, Pattern, RequestInfo, Request,
                                    Timeout, Priority, TransId, Pid,
                                    CRDT0, Dispatcher) of
        {ok, CRDTN} ->
            {noreply, State#state{crdt = CRDTN}};
        {ignored, CRDT1} ->
            {CountN,
             CRDTN} = case cloudi_crdt:find(Dispatcher, count, CRDT1) of
                {ok, Count0} ->
                    CRDT2 = if
                        Count0 >= 4294967295 ->
                            cloudi_crdt:zero(Dispatcher, count, CRDT1);
                        true ->
                            cloudi_crdt:incr(Dispatcher, count, CRDT1)
                    end,
                    {Count0, CRDT2};
                error ->
                    {1, cloudi_crdt:put(Dispatcher, count, 2, CRDT1)}
            end,
            ?LOG_INFO("count == ~w erlang (CRDT)", [CountN]),
            Response = cloudi_string:format_to_binary("~w", [CountN]),
            {reply, Response, State#state{crdt = CRDTN}}
    end.

cloudi_service_handle_info(Request,
                           #state{mode = crdt,
                                  crdt = CRDT0} = State, Dispatcher) ->
    {ok, CRDTN} = cloudi_crdt:handle_info(Request, CRDT0, Dispatcher),
    {noreply, State#state{crdt = CRDTN}}.

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
    ?LOG_INFO("terminate count erlang", []),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

