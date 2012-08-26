%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Work Module For messaging Test (sequence3)==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_messaging_sequence3).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/11,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-record(state, {
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------


%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(_Args, _Prefix, Dispatcher) ->
    cloudi_job:subscribe(Dispatcher, "f1"),
    cloudi_job:subscribe(Dispatcher, "f2"),
    cloudi_job:subscribe(Dispatcher, "g1"),
    cloudi_job:subscribe(Dispatcher, "sequence3"),
    {ok, #state{}}.

cloudi_job_handle_request(_Type, _Name, Pattern, _RequestInfo, Request,
                          _Timeout, _Priority, _TransId, _Pid,
                          #state{} = State,
                          Dispatcher) ->
    Prefix = cloudi_job:prefix(Dispatcher),
    Suffix = string:substr(Pattern, erlang:length(Prefix) + 1),
    case Suffix of
        "sequence3" ->
            ?LOG_INFO(" messaging sequence3 start erlang", []),
            sequence3(Dispatcher, Prefix),
            ?LOG_INFO(" messaging sequence3 end erlang", []),
            {reply, "end", State};
        "f1" ->
            RequestI = erlang:list_to_integer(Request),
            if
                RequestI == 4 ->
                    {reply, "done", State};
                true ->
                    RequestNew = RequestI + 2, % two steps forward
                    {forward, Prefix ++ "f2", <<>>,
                     cloudi_string:term_to_list(RequestNew), State}
            end;
        "f2" ->
            RequestI = erlang:list_to_integer(Request),
            RequestNew = RequestI - 1, % one step back
            {forward, Prefix ++ "f1", <<>>,
             cloudi_string:term_to_list(RequestNew), State};
        "g1" ->
            {reply, Request ++ "suffix", State}
    end.

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

sequence3(Dispatcher, Prefix) ->
    {ok, Test1Id} = cloudi_job:send_async(Dispatcher, Prefix ++ "f1", "0"),
    {ok, Test1Check} = cloudi_job:recv_async(Dispatcher, Test1Id),
    true = Test1Check == "done",
    {ok, Test2Check} = cloudi_job:send_sync(Dispatcher, Prefix ++ "g1",
                                            "prefix_"),
    true = Test2Check == "prefix_suffix",
    % loop to find any infrequent problems, restart sequence1
    cloudi_job:send_async(Dispatcher, Prefix ++ "sequence1", "start"),
    ok.
