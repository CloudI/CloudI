%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Configurator==
%%% Use the configuration information to start CloudI processes.
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
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configurator).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_configuration.hrl").
-include("cloudi_constants.hrl").

-record(state,
    {
        configuration
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Config)
    when is_record(Config, config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Config]) ->
    self() ! configure,
    {ok, #state{configuration = Config}}.

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, string2:format("Unknown call \"~p\"", [Request]), error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info(configure, #state{configuration = Config} = State) ->
    configure(Config),
    {stop, normal, State};

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

configure(Config) ->
    lists:foreach(fun(Job) -> start_job(Job) end, Config#config.jobs).

start_job(Job)
    when is_record(Job, config_job_internal) ->
    case code:is_loaded(Job#config_job_internal.module) of
        false ->
            code:load_file(Job#config_job_internal.module);
        _ ->
            ok
    end,
    start_job_internal(Job#config_job_internal.count_process, Job);

start_job(Job)
    when is_record(Job, config_job_external) ->
    start_job_external(Job#config_job_external.count_process, Job).

start_job_internal(0, _) ->
    ok;
start_job_internal(Count, Job)
    when is_record(Job, config_job_internal) ->
    case cloudi_services:monitor(cloudi_spawn, start_internal,
                                 [Job#config_job_internal.module,
                                  Job#config_job_internal.args,
                                  Job#config_job_internal.timeout_init,
                                  Job#config_job_internal.prefix,
                                  Job#config_job_internal.timeout_async,
                                  Job#config_job_internal.timeout_sync,
                                  Job#config_job_internal.dest_refresh,
                                  Job#config_job_internal.dest_list_deny,
                                  Job#config_job_internal.dest_list_allow],
                                 Job#config_job_internal.max_r,
                                 Job#config_job_internal.max_t) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error starting internal job (~p): ~p",
                       [Job#config_job_internal.module, Reason]),
            ok
    end,
    start_job_internal(Count - 1, Job).

start_job_external(0, _) ->
    ok;
start_job_external(Count, Job)
    when is_record(Job, config_job_external) ->
    case cloudi_services:monitor(cloudi_spawn, start_external,
                                 [Job#config_job_external.count_thread,
                                  Job#config_job_external.file_path,
                                  Job#config_job_external.args,
                                  Job#config_job_external.env,
                                  Job#config_job_external.protocol,
                                  Job#config_job_external.buffer_size,
                                  Job#config_job_external.timeout_init,
                                  Job#config_job_external.prefix,
                                  Job#config_job_external.timeout_async,
                                  Job#config_job_external.timeout_sync,
                                  Job#config_job_external.dest_refresh,
                                  Job#config_job_external.dest_list_deny,
                                  Job#config_job_external.dest_list_allow],
                                 Job#config_job_external.max_r,
                                 Job#config_job_external.max_t) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error starting external job (~p): ~p",
                       [Job#config_job_external.file_path, Reason]),
            ok
    end,
    start_job_external(Count - 1, Job).

