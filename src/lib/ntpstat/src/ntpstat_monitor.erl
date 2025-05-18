%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==NTP client ntpstat monitor==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2025 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2025 Michael Truog
%%% @version 2.0.8 {@date} {@time}
%%%------------------------------------------------------------------------

-module(ntpstat_monitor).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([start_link/3,
         stop_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TIMEOUT_MAX_ERLANG, 4294967295). % milliseconds

-type status() :: ok | ntpstat:status_error().
-type native_monotonic() :: integer().
-type event() :: {ntp, status(), native_monotonic()}.
-export_type([status/0,
              native_monotonic/0,
              event/0]).

-record(state,
        {
            receiver :: pid(),
            period :: timeout(), % milliseconds
            value = undefined :: status() | undefined,
            status :: ntpstat:status()
        }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start a process to monitor ntpstat values.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(ReceiverPid :: pid(),
                 PeriodSeconds :: pos_integer(),
                 Options :: ntpstat:options()) ->
    {ok, pid()} | {error, any()}.

start_link(ReceiverPid, PeriodSeconds, Options) ->
    case start(ReceiverPid, PeriodSeconds, Options) of
        {ok, Pid} = Success ->
            true = erlang:link(Pid),
            Success;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop a ntpstat monitor process.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_link(Pid :: pid()) ->
    ok.

stop_link(Pid) ->
    gen_server:cast(Pid, stop).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([ReceiverPid, PeriodSeconds, Options]) ->
    Status = ntpstat:new(Options),
    PeriodMilliSeconds = PeriodSeconds * 1000,
    self() ! update,
    {ok, #state{receiver = ReceiverPid,
                period = PeriodMilliSeconds,
                status = Status}}.

handle_call(Request, _, State) ->
    {stop, {call, Request}, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {cast, Request}, State}.

handle_info(update,
            #state{receiver = ReceiverPid,
                   period = PeriodMilliSeconds,
                   value = ValueOld,
                   status = StatusOld} = State) ->
    UpdateNativeStart = erlang:monotonic_time(),
    {ValueNew,
     StatusNew} = case ntpstat:update(PeriodMilliSeconds, StatusOld) of
        {ok, _} = Success ->
            Success;
        {error, ErrorStatus, Status} ->
            {ErrorStatus, Status}
    end,
    UpdateNativeEnd = erlang:monotonic_time(),
    DelayMilliSeconds = erlang:convert_time_unit(UpdateNativeEnd -
                                                 UpdateNativeStart,
                                                 native, millisecond),
    UpdateNextMilliSeconds = max(PeriodMilliSeconds - DelayMilliSeconds, 0),
    erlang:send_after(UpdateNextMilliSeconds, self(), update),
    if
        ValueOld /= ValueNew ->
            ReceiverPid ! {ntp, ValueNew, UpdateNativeEnd},
            ok;
        true ->
            ok
    end,
    {noreply, State#state{value = ValueNew,
                          status = StatusNew}};
handle_info(Request, State) ->
    {stop, {info, Request}, State}.

terminate(_, #state{status = Status}) ->
    ok = ntpstat:destroy(Status),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

start(ReceiverPid, PeriodSeconds, Options)
    when is_pid(ReceiverPid),
         is_integer(PeriodSeconds), PeriodSeconds > 0,
         PeriodSeconds < ?TIMEOUT_MAX_ERLANG div 1000,
         is_list(Options) ->
    gen_server:start(?MODULE,
                     [ReceiverPid, PeriodSeconds, Options], []).

