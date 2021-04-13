%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Application Supervisor==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2021 Michael Truog
%%% @version 2.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_sup).
-author('mjtruog at protonmail dot com').

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
         is_tuple(child_specification(?OS_SPAWN_POOL))
         andalso
         is_tuple(child_specification(?OS_COMMAND_POOL)))).
-else.
-define(CHILDSPECS,
        [child_specification(cloudi_core_i_logger_sup, Config),
         child_specification(cloudi_core_i_logger, Config),
         child_specification(cloudi_core_i_nodes, Config),
         child_specification(?OS_SPAWN_POOL),
         child_specification(?OS_COMMAND_POOL),
         child_specification(cloudi_core_i_services_monitor),
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
     {cloudi_core_i_configurator, start_link, [Shutdown, Config]},
     permanent, Shutdown, worker, [cloudi_core_i_configurator]}.

child_specification(?OS_SPAWN_POOL = OSSpawnPool) ->
    Shutdown = 2000, % milliseconds
    {OSSpawnPool,
     {cloudi_x_supool_sup, start_link,
      [OSSpawnPool, os_process_count(),
       {undefined, {cloudi_core_i_os_spawn, start_link, []},
        permanent, Shutdown, worker, [cloudi_core_i_os_spawn]}, []]},
     permanent, infinity, supervisor, [cloudi_x_supool_sup]};
child_specification(?OS_COMMAND_POOL = OSCommandPool) ->
    Shutdown = 2000, % milliseconds
    {OSCommandPool,
     {cloudi_x_supool_sup, start_link,
      [OSCommandPool, os_process_count(),
       {undefined, {cloudi_core_i_os_command, start_link, []},
        permanent, Shutdown, worker, [cloudi_core_i_os_command]}, []]},
     permanent, infinity, supervisor, [cloudi_x_supool_sup]};
child_specification(cloudi_core_i_services_monitor) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_services_monitor,
     {cloudi_core_i_services_monitor, start_link, []},
     permanent, Shutdown, worker, [cloudi_core_i_services_monitor]};
child_specification(cloudi_core_i_services_external_sup) ->
    {cloudi_core_i_services_external_sup,
     {cloudi_core_i_services_external_sup, start_link, []},
     permanent, infinity, supervisor, [cloudi_core_i_services_external_sup]};
child_specification(cloudi_core_i_services_internal_sup) ->
    {cloudi_core_i_services_internal_sup,
     {cloudi_core_i_services_internal_sup, start_link, []},
     permanent, infinity, supervisor, [cloudi_core_i_services_internal_sup]};
child_specification(cloudi_core_i_services_internal_reload) ->
    Shutdown = 2000, % milliseconds
    {cloudi_core_i_services_internal_reload,
     {cloudi_core_i_services_internal_reload, start_link, []},
     permanent, Shutdown, worker, [cloudi_core_i_services_internal_reload]}.

os_process_count() ->
    erlang:system_info(schedulers).

