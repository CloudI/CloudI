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
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configurator).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         acl_add/2, acl_remove/2,
         services_add/2, services_remove/2, services_restart/2, services/1,
         nodes_add/2, nodes_remove/2,
         service_start/1,
         service_stop/1,
         service_restart/1,
         concurrency/1]).

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

acl_add(L, Timeout) ->
    gen_server:call(?MODULE, {acl_add, L,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

acl_remove(L, Timeout) ->
    gen_server:call(?MODULE, {acl_remove, L,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

services_add(L, Timeout) ->
    gen_server:call(?MODULE, {services_add, L,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

services_remove(L, Timeout) ->
    gen_server:call(?MODULE, {services_remove, L,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

services_restart(L, Timeout) ->
    gen_server:call(?MODULE, {services_restart, L,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

services(Timeout) ->
    gen_server:call(?MODULE, {services,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

nodes_add(L, Timeout) ->
    Nodes = [node() | nodes()],
    global:trans({{?MODULE, L}, self()},
                 fun() ->
                     gen_server:multi_call(Nodes, ?MODULE,
                                           {nodes_add, L,
                                            Timeout - ?TIMEOUT_DELTA}, Timeout)
                 end),
    ok.

nodes_remove(L, Timeout) ->
    Nodes = [node() | nodes()],
    global:trans({{?MODULE, L}, self()},
                 fun() ->
                     gen_server:multi_call(Nodes, ?MODULE,
                                           {nodes_remove, L,
                                            Timeout - ?TIMEOUT_DELTA}, Timeout)
                 end),
    ok.

service_start(Service)
    when is_record(Service, config_service_internal) ->
    case application:load(Service#config_service_internal.module) of
        ok ->
            % prefer application files to load internal services
            % (so that application dependencies can be clearly specified, etc.)
            application:start(Service#config_service_internal.module,
                              temporary);
        {error, _} ->
            % if no application file can be loaded, load it as a simple module
            case code:is_loaded(Service#config_service_internal.module) of
                false ->
                    code:load_file(Service#config_service_internal.module);
                _ ->
                    ok
            end
    end,
    service_start_internal(
        concurrency(Service#config_service_internal.count_process), Service);

service_start(Service)
    when is_record(Service, config_service_external) ->
    service_start_external(
        concurrency(Service#config_service_external.count_process), Service).

service_stop(Service)
    when is_record(Service, config_service_internal) ->
    service_stop_internal(Service);

service_stop(Service)
    when is_record(Service, config_service_external) ->
    service_stop_external(Service).

service_restart(Service)
    when is_record(Service, config_service_internal) ->
    service_restart_internal(Service);

service_restart(Service)
    when is_record(Service, config_service_external) ->
    service_restart_external(Service).

concurrency(I)
    when is_integer(I) ->
    I;
concurrency(I)
    when is_float(I) ->
    if
        I > 1.0 ->
            erlang:round((I * erlang:system_info(schedulers)) + 0.5);
        I > 0.0, I < 1.0 ->
            erlang:max(1, erlang:round(I * erlang:system_info(schedulers)));
        I == 1.0 ->
            erlang:system_info(schedulers)
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Config]) ->
    self() ! configure,
    {ok, #state{configuration = Config}}.

handle_call({acl_add, L, _}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:acl_add(L, Config),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({acl_remove, L, _}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:acl_remove(L, Config),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({services_add, L, _}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:services_add(L, Config),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({services_remove, L, _}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:services_remove(L, Config),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({services_restart, L, _}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:services_restart(L, Config),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({services, _}, _,
            #state{configuration = Config} = State) ->
    {reply, cloudi_configuration:services(Config), State};

handle_call({nodes_add, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:nodes_add(L, Config),
    cloudi_nodes:reconfigure(NewConfig, Timeout),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({nodes_remove, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:nodes_remove(L, Config),
    cloudi_nodes:reconfigure(NewConfig, Timeout),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info(configure, #state{configuration = Config} = State) ->
    configure(Config),
    {noreply, State};

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
    lists:foreach(fun service_start/1, Config#config.services).

service_start_internal(0, _) ->
    ok;
service_start_internal(Count0, Service)
    when is_record(Service, config_service_internal) ->
    Count1 = Count0 - 1,
    case cloudi_services:monitor(
        cloudi_spawn, start_internal,
        [Count1,
         Service#config_service_internal.module,
         Service#config_service_internal.args,
         Service#config_service_internal.timeout_init,
         Service#config_service_internal.prefix,
         Service#config_service_internal.timeout_async,
         Service#config_service_internal.timeout_sync,
         Service#config_service_internal.dest_refresh,
         Service#config_service_internal.dest_list_deny,
         Service#config_service_internal.dest_list_allow,
         Service#config_service_internal.options],
        Service#config_service_internal.max_r,
        Service#config_service_internal.max_t,
        Service#config_service_internal.uuid) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error starting internal service (~p):~n ~p",
                       [Service#config_service_internal.module, Reason]),
            ok
    end,
    service_start_internal(Count1, Service).

service_start_external(0, _) ->
    ok;
service_start_external(Count, Service)
    when is_record(Service, config_service_external) ->
    case cloudi_services:monitor(
        cloudi_spawn, start_external,
        [concurrency(
             Service#config_service_external.count_thread
         ),
         Service#config_service_external.file_path,
         Service#config_service_external.args,
         Service#config_service_external.env,
         Service#config_service_external.protocol,
         Service#config_service_external.buffer_size,
         Service#config_service_external.timeout_init,
         Service#config_service_external.prefix,
         Service#config_service_external.timeout_async,
         Service#config_service_external.timeout_sync,
         Service#config_service_external.dest_refresh,
         Service#config_service_external.dest_list_deny,
         Service#config_service_external.dest_list_allow,
         Service#config_service_external.options],
        Service#config_service_external.max_r,
        Service#config_service_external.max_t,
        Service#config_service_external.uuid) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error starting external service (~p):~n ~p",
                       [Service#config_service_external.file_path, Reason]),
            ok
    end,
    service_start_external(Count - 1, Service).

service_stop_internal(Service)
    when is_record(Service, config_service_internal) ->
    case cloudi_services:shutdown(Service#config_service_internal.uuid) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error stopping internal service (~p):~n ~p",
                       [Service#config_service_internal.module, Reason]),
            ok
    end.

service_stop_external(Service)
    when is_record(Service, config_service_external) ->
    case cloudi_services:shutdown(Service#config_service_external.uuid) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error stopping external service (~p):~n ~p",
                       [Service#config_service_external.file_path, Reason]),
            ok
    end.

service_restart_internal(Service)
    when is_record(Service, config_service_internal) ->
    case cloudi_services:restart(Service#config_service_internal.uuid) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error restarting internal service (~p):~n ~p",
                       [Service#config_service_internal.module, Reason]),
            ok
    end.

service_restart_external(Service)
    when is_record(Service, config_service_external) ->
    case cloudi_services:restart(Service#config_service_external.uuid) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error restarting external service (~p):~n ~p",
                       [Service#config_service_external.file_path, Reason]),
            ok
    end.

