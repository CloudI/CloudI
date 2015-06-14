%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Application Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the CloudI application supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Config :: #config{}) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(Config) when is_record(Config, config) ->
    case supervisor:start_link(?MODULE, [Config]) of
        {ok, Pid} = Success ->
            case cloudi_core_i_configurator:configure() of
                ok ->
                    Success;
                {error, _} = Error ->
                    erlang:exit(Pid, configure_failed),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

-ifdef(CLOUDI_CORE_STANDALONE).
-define(CHILDSPECS,
        [child_specification(cloudi_core_i_logger_sup, Config),
         child_specification(cloudi_core_i_logger, Config),
         child_specification(cloudi_core_i_nodes, Config),
         child_specification(cloudi_core_i_services_monitor),
         child_specification(cloudi_core_i_services_internal_sup),
         child_specification(cloudi_core_i_configurator, Config),
         child_specification(cloudi_core_i_services_internal_reload)]).
-define(CHECK,
        (is_tuple(child_specification(cloudi_core_i_services_external_sup))
         andalso
         is_tuple(child_specification(cloudi_core_i_os_spawn_pool)))).
-else.
-define(CHILDSPECS,
        [child_specification(cloudi_core_i_logger_sup, Config),
         child_specification(cloudi_core_i_logger, Config),
         child_specification(cloudi_core_i_nodes, Config),
         child_specification(cloudi_core_i_services_monitor),
         child_specification(cloudi_core_i_os_spawn_pool),
         child_specification(cloudi_core_i_services_external_sup),
         child_specification(cloudi_core_i_services_internal_sup),
         child_specification(cloudi_core_i_configurator, Config),
         child_specification(cloudi_core_i_services_internal_reload)]).
-define(CHECK,
        true).
-endif.

init([Config]) when is_record(Config, config) ->
    true = ?CHECK,
    MaxRestarts = 0,
    MaxTime = 1,
    {ok, {{one_for_all, MaxRestarts, MaxTime}, ?CHILDSPECS}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

child_specification(cloudi_core_i_logger_sup, Config) ->
    {cloudi_core_i_logger_sup,
     {cloudi_core_i_logger_sup, start_link, [Config]},
     permanent, infinity, supervisor, [cloudi_core_i_logger_sup]};

child_specification(cloudi_core_i_logger, Config) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_logger,
     {cloudi_core_i_logger, start_link, [Config]},
     permanent, Shutdown, worker, [cloudi_core_i_logger]};

child_specification(cloudi_core_i_nodes, Config) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_nodes,
     {cloudi_core_i_nodes, start_link, [Config]},
     permanent, Shutdown, worker, [cloudi_core_i_nodes]};

child_specification(cloudi_core_i_configurator, Config) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_configurator,
     {cloudi_core_i_configurator, start_link, [Config]},
     permanent, Shutdown, worker, [cloudi_core_i_configurator]}.

child_specification(cloudi_core_i_services_monitor) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_services_monitor,
     {cloudi_core_i_services_monitor, start_link, []},
     permanent, Shutdown, worker, [cloudi_core_i_services_monitor]};

child_specification(cloudi_core_i_services_internal_reload) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_services_internal_reload,
     {cloudi_core_i_services_internal_reload, start_link, []},
     permanent, Shutdown, worker, [cloudi_core_i_services_internal_reload]};

child_specification(cloudi_core_i_services_internal_sup) ->
    {cloudi_core_i_services_internal_sup,
     {cloudi_core_i_services_internal_sup, start_link, []},
     permanent, infinity, supervisor, [cloudi_core_i_services_internal_sup]};

child_specification(cloudi_core_i_os_spawn_pool) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_os_spawn_pool,
     {cloudi_core_i_pool_sup, start_link,
      [cloudi_core_i_os_spawn, os_process_count(),
       {undefined, {cloudi_core_i_os_spawn, start_link, []},
        permanent, Shutdown, worker, [cloudi_core_i_os_spawn]}]},
     permanent, infinity, supervisor, [cloudi_core_i_pool_sup]};

child_specification(cloudi_core_i_services_external_sup) ->
    {cloudi_core_i_services_external_sup,
     {cloudi_core_i_services_external_sup, start_link, []},
     permanent, infinity, supervisor, [cloudi_core_i_services_external_sup]}.

% determine the number of cloudi_core_i_os_spawn child processes within the
% cloudi_core_i_pool_sup, referenced by a cloudi_core_i_pool process with a
% locally registered name of 'cloudi_core_i_os_spawn'

%os_process_count() ->
%    1.
os_process_count() ->
    erlang:system_info(schedulers).

