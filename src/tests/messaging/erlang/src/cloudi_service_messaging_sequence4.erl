%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the messaging Test (sequence4)==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_messaging_sequence4).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state, {
        current_state = state1
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------


%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, _Prefix, _Timeout, Dispatcher) ->
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    8 = cloudi_service:subscribe_count(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "sequence4"),
    {ok, #state{}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{current_state = CurrentState} = State,
                              Dispatcher) ->
    Prefix = cloudi_service:prefix(Dispatcher),
    if
        Request == "start" ->
            ?LOG_INFO("messaging sequence4 start erlang", []),
            sequence4(Dispatcher, Prefix),
            ?LOG_INFO("messaging sequence4 end erlang", []),
            % loop to find any infrequent problems, restart sequence1
            cloudi_service:send_async(Dispatcher,
                                      Prefix ++ "sequence1", "start"),
            {reply, "done", State};
        CurrentState =:= state1 ->
            {reply, <<"1">>, State#state{current_state = state2}};
        CurrentState =:= state2 ->
            {reply, <<"2">>, State#state{current_state = state3}};
        CurrentState =:= state3 ->
            {reply, <<"3">>, State#state{current_state = state4}};
        CurrentState =:= state4 ->
            {reply, <<"4">>, State#state{current_state = state5}};
        CurrentState =:= state5 ->
            {reply, <<"5">>, State#state{current_state = state6}};
        CurrentState =:= state6 ->
            {reply, <<"6">>, State#state{current_state = state7}};
        CurrentState =:= state7 ->
            {reply, <<"7">>, State#state{current_state = state8}};
        CurrentState =:= state8 ->
            {reply, <<"8">>, State#state{current_state = state1}}
    end.

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
    ?LOG_INFO("terminate messaging 4 erlang", []),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

sequence4(Dispatcher, Prefix) ->
    % the sending process is excluded from the services that receive
    % the asynchronous message, so in this case, the receiving process
    % will not be called, despite the fact it has subscribed to 'e',
    % to prevent a process from deadlocking with itself.
    {ok, TransIds} = cloudi_service:mcast_async(Dispatcher, Prefix ++ "h", " "),
    % 4 * 8 == 32, but only 3 out of 4 CloudI services can receive messages,
    % since 1 CloudI service is sending the mcast_async, so 3 * 8 == 24
    if
        erlang:length(TransIds) == 24 ->
            % all service processes have finished initialization
            {ok, Recvs} = cloudi_service:recv_asyncs(Dispatcher, TransIds),
            L = lists:foldl(fun({<<>>, Result, _TransId}, Results) ->
                lists:merge(Results, [Result])
            end, [], Recvs),
            true = L == [<<"1">>, <<"1">>, <<"1">>,
                         <<"2">>, <<"2">>, <<"2">>,
                         <<"3">>, <<"3">>, <<"3">>,
                         <<"4">>, <<"4">>, <<"4">>,
                         <<"5">>, <<"5">>, <<"5">>,
                         <<"6">>, <<"6">>, <<"6">>,
                         <<"7">>, <<"7">>, <<"7">>,
                         <<"8">>, <<"8">>, <<"8">>],
            ok;
        true ->
            % service processes have not finished initialization
            ?LOG_WARN("Waiting for ~p services to initialize",
                      [4 - (erlang:length(TransIds) / 8)]),
            {ok, _} = cloudi_service:recv_asyncs(Dispatcher, TransIds),
            % sleep
            {error, timeout} = cloudi_service:recv_async(Dispatcher, 1000),
            % retry
            sequence4(Dispatcher, Prefix)
    end.

