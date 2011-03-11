%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Timers==
%%% Provide functionality for sending messages on timers.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_timers).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface

%% cloudi_job callbacks
-export([cloudi_job_init/2,
         cloudi_job_handle_request/8,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-define(DEFAULT_TIMERS,                   []).

-record(state,
    {
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(Args, Dispatcher) ->
    Defaults = [
        {timers,          ?DEFAULT_TIMERS},
        {name,                  undefined}],
    [Timers, Name] =
        proplists2:take_values(Defaults, Args),
    true = is_list(Name),
    process_timers(Timers),
    cloudi_job:subscribe(Dispatcher, Name),
    {ok, #state{}}.

cloudi_job_handle_request(_Type, _Name, Request, _Timeout, _TransId, _Pid,
                          State,
                          _Dispatcher) ->
    {Type, Time, F, A} = if
        is_binary(Request) ->
            string2:binary_to_term(Request);
        is_list(Request) ->
            string2:list_to_term(Request);
        is_tuple(Request), tuple_size(Request) == 4 ->
            Request
    end,
    true = (Type == once) or (Type == always),
    TimeValue = time_to_milliseconds(Time),
    true = is_atom(F),
    true = is_list(A),
    true = (0 =< TimeValue) and (TimeValue =< 4294967295),
    erlang:send_after(TimeValue, self(), {Type, TimeValue, F, A}),
    {reply, ok, State}.

cloudi_job_handle_info({once, _, F, A}, State, Dispatcher) ->
    erlang:apply(cloudi_job, F, [Dispatcher | A]),
    {noreply, State};

cloudi_job_handle_info({always, TimeValue, F, A} = Message,
                       State, Dispatcher) ->
    erlang:apply(cloudi_job, F, [Dispatcher | A]),
    erlang:send_after(TimeValue, self(), Message),
    {noreply, State};

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

process_timers([]) ->
    ok;

process_timers([{Type, Time, F, A} | T]) ->
    true = (Type == once) or (Type == always),
    TimeValue = time_to_milliseconds(Time),
    true = is_atom(F),
    true = is_list(A),
    true = (0 =< TimeValue) and (TimeValue =< 4294967295),
    erlang:send_after(TimeValue, self(), {Type, TimeValue, F, A}),
    process_timers(T).

time_to_milliseconds({X, Unit})
    when is_integer(X) ->
    if
        Unit =:= weeks;    Unit =:= week    ->
            X * 7 * 24 * 60 * 60 * 1000;
        Unit =:= days;     Unit =:= day     ->
            X     * 24 * 60 * 60 * 1000;
        Unit =:= hours;    Unit =:= hour    ->
            X          * 60 * 60 * 1000;
        Unit =:= minutes;  Unit =:= minute  ->
            X               * 60 * 1000;
        Unit =:= seconds;  Unit =:= second  ->
            X                    * 1000
    end;

time_to_milliseconds(X)
    when is_integer(X) ->
    X.

