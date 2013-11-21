%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the messaging Test (sequence4)==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2013 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_messaging_sequence4).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include("cloudi_logger.hrl").

-record(state, {
        current_state = state1
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------


%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, _Prefix, Dispatcher) ->
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "h"),
    cloudi_service:subscribe(Dispatcher, "sequence4"),
    {ok, #state{}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{current_state = CurrentState} = State,
                              Dispatcher) ->
    Prefix = cloudi_service:prefix(Dispatcher),
    if
        Request == "start" ->
            ?LOG_INFO(" messaging sequence4 start erlang", []),
            sequence4(Dispatcher, Prefix),
            ?LOG_INFO(" messaging sequence4 end erlang", []),
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

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
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

