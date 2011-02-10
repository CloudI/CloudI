%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Application Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-include("cloudi_configuration.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the Cloudi application supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Config :: #config{}) -> {'ok', pid()} | {'error', any()}.

start_link(Config) when is_record(Config, config) ->
    supervisor:start_link(?MODULE, [Config]).

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([Config]) when is_record(Config, config) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    {ok,
     {{rest_for_one, MaxRestarts, MaxTime},
      [child_specification(cloudi_logger, Config),
       child_specification(list_pg),
       child_specification(cloudi_job_sup),
       child_specification(cloudi_os_spawn_pool),
       child_specification(cloudi_socket_sup)]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

child_specification(cloudi_logger, Config)
    when is_record(Config, config) ->
    Shutdown = 2000, % milliseconds
    {cloudi_logger,
     {cloudi_logger, start_link, [Config]},
     permanent, Shutdown, worker, [cloud_logger]}.

child_specification(list_pg) ->
    Shutdown = 2000, % milliseconds
    {list_pg,
     {list_pg, start_link, []},
     permanent, Shutdown, worker, [list_pg]};

child_specification(cloudi_job_sup) ->
    {cloudi_job_sup,
     {cloudi_job_sup, start_link, []},
     permanent, infinity, supervisor, [cloudi_job_sup]};

child_specification(cloudi_os_spawn_pool) ->
    Shutdown = 2000, % milliseconds
    {cloudi_os_spawn_pool,
     {pool2_sup, start_link,
      [cloudi_os_spawn, os_process_count(),
       {undefined, {cloudi_os_spawn, start_link, []},
        permanent, Shutdown, worker, [cloudi_os_spawn]}]},
     permanent, infinity, supervisor, [pool2_sup]};

child_specification(cloudi_socket_sup) ->
    {cloudi_socket_sup,
     {cloudi_socket_sup, start_link, []},
     permanent, infinity, supervisor, [cloudi_socket_sup]}.

os_process_count() ->
    1.
%os_process_count() ->
%    case erlang:system_info(logical_processors_available) of
%       unknown ->
%           2;
%       Count when Count > 2 ->
%           Count;
%       _ ->
%           2
%    end.

