%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Flood Test==
%%% Flood CloudI services at rates specified as service arguments.
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
%%% @version 0.1.9 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_flood).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface
-export([flood/5]).

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/10,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-define(FLOOD_INTERVAL,    1000). % ms, interval for creating load
-define(STATUS_INTERVAL,  10000). % ms, report status

-record(state,
    {
        rates    % Name -> Delay
    }).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(Args, _Prefix, _Dispatcher) ->
    Rates = lists:foldl(fun({flood, Name, _Request, Count} = Info, Lookup) ->
        true = is_list(Name) and is_integer(Count),
        erlang:send_after(?FLOOD_INTERVAL, self(), Info),
        trie:store(Name, (?FLOOD_INTERVAL * 1.0) / Count, Lookup)
    end, trie:new(), Args),
    erlang:send_after(?STATUS_INTERVAL, self(), status),
    {ok, #state{rates = Rates}}.

cloudi_job_handle_request(_Type, _Name, _RequestInfo, _Request,
                          _Timeout, _Priority, _TransId, _Pid,
                          State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_job_handle_info({flood, Name, Request, Count} = Info,
                       #state{rates = Rates} = State,
                       Dispatcher) ->
    erlang:send_after(?FLOOD_INTERVAL, self(), Info),
    Delay = trie:fetch(Name, Rates),
    {Time, _} = timer:tc(cloudi_job_flood, flood,
                         [Count, erlang:round(Delay),
                          Dispatcher, Name, Request]),
    NewRates = trie:store(Name, (?FLOOD_INTERVAL /
                                 (Time * 0.001)) * Delay, Rates),
    {noreply, State#state{rates = NewRates}};

cloudi_job_handle_info(status, #state{rates = Rates} = State, _) ->
    Output = lists:flatten(trie:foldl(fun(Name, Delay, L) ->
        [io_lib:format("~10w req/s ~s~n", [Delay * 1000.0, Name]) | L]
    end, [], Rates)),
    ?LOG_INFO("~s", [Output]),
    erlang:send_after(?STATUS_INTERVAL, self(), status),
    {noreply, State};

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

flood(Count, Delay, Dispatcher, Name, Request) ->
    flood(0, Count, Delay, Dispatcher, Name, Request).

flood(N, N, _, _, _, _) ->
    ok;

flood(I, N, 0, Dispatcher, Name, Request) ->
    cloudi_job:send_async(Dispatcher, Name, Request, ?FLOOD_INTERVAL),
    flood(I + 1, N, 0, Dispatcher, Name, Request);

flood(I, N, Delay, Dispatcher, Name, Request) ->
    cloudi_job:send_async(Dispatcher, Name, Request, ?FLOOD_INTERVAL),
    receive after Delay -> ok end,
    flood(I + 1, N, Delay, Dispatcher, Name, Request).

