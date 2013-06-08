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
%%% @version 1.2.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configurator).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         acl_add/2, acl_remove/2,
         services_add/2, services_remove/2, services_restart/2, services/1,
         nodes_add/2, nodes_remove/2,
         service_start/2,
         service_stop/3,
         service_restart/2,
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

service_start(#config_service_internal{module = Module,
                                       count_process = Count} = Service,
              Timeout) ->
    case service_start_find_internal(Service) of
        {ok, FoundService} ->
            service_start_internal(concurrency(Count), FoundService, Timeout),
            if
                is_list(Module) ->
                    FoundService#config_service_internal{file_path = Module};
                true ->
                    FoundService
            end;
        {error, Reason} ->
            ?LOG_ERROR("error finding internal service (~p):~n ~p",
                       [Module, Reason]),
            Service
    end;

service_start(#config_service_external{count_process = Count} = Service,
              Timeout) ->
    service_start_external(concurrency(Count), Service, Timeout),
    Service.

service_stop(Service, Remove, Timeout)
    when is_record(Service, config_service_internal), is_boolean(Remove) ->
    service_stop_internal(Service, Remove, Timeout);

service_stop(Service, false, Timeout)
    when is_record(Service, config_service_external) ->
    service_stop_external(Service, Timeout).

service_restart(Service, Timeout)
    when is_record(Service, config_service_internal) ->
    service_restart_internal(Service, Timeout);

service_restart(Service, Timeout)
    when is_record(Service, config_service_external) ->
    service_restart_external(Service, Timeout).

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

handle_call({services_add, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:services_add(L, Config, Timeout),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({services_remove, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:services_remove(L, Config, Timeout),
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({services_restart, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    NewConfig = cloudi_configuration:services_restart(L, Config, Timeout),
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
    {noreply, State#state{configuration = configure(Config, infinity)}};

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

configure(#config{services = Services} = Config, Timeout) ->
    Config#config{services = configure_service(Services, [], Timeout)}.

configure_service([], Configured, _) ->
    lists:reverse(Configured);
configure_service([Service | Services], Configured, Timeout) ->
    configure_service(Services,
                      [service_start(Service, Timeout) | Configured],
                      Timeout).

service_start_find_internal(#config_service_internal{module = Path} = Service)
    when is_list(Path) ->
    case filename:extension(Path) of
        ".erl" ->
            Module = erlang:list_to_atom(filename:basename(Path, ".erl")),
            case service_start_find_internal_add_pathz(Path) of
                {ok, FullPath} ->
                    case compile:file(FullPath,
                                      compiler_options(FullPath)) of
                        {ok, Module} ->
                            service_start_find_internal_module(Module, Service);
                        error ->
                            {error, compile};
                        {error, Errors, Warnings} ->
                            {error, {compile, Errors, Warnings}}
                    end;
                {error, _} = Error ->
                    Error
            end;
        ".beam" ->
            Module = erlang:list_to_atom(filename:basename(Path, ".beam")),
            case service_start_find_internal_add_pathz(Path) of
                {ok, _} ->
                    service_start_find_internal_module(Module, Service);
                {error, _} = Error ->
                    Error
            end;
        ".app" ->
            Application = erlang:list_to_atom(filename:basename(Path, ".app")),
            case service_start_find_internal_add_pathz(Path) of
                {ok, _} ->
                    service_start_find_internal_application(Application,
                                                            Service);
                {error, _} = Error ->
                    Error
            end;
        ".script" ->
            case filename:dirname(Path) of
                "." ->
                    case code:where_is_file(Path) of
                        non_existing ->
                            {error, {non_existing, Path}};
                        FullPath ->
                            service_start_find_internal_script(FullPath,
                                                               Service)
                    end;
                _ ->
                    service_start_find_internal_script(Path, Service)
            end;
        Extension ->
            {error, {invalid_extension, Extension}}
    end;
service_start_find_internal(#config_service_internal{module = Module} = Service)
    when is_atom(Module) ->
    % prefer application files to load internal services
    % (so that application dependencies can be clearly specified, etc.)
    case application:load(Module) of
        ok ->
            service_start_find_internal_application(Module, Service);
        {error, {already_loaded, Module}} ->
            service_start_find_internal_application(Module, Service);
        {error, _} ->
            % if no application file can be loaded, load it as a simple module
            service_start_find_internal_module(Module, Service)
    end.

compiler_options(FilePath) ->
    [{outdir, filename:dirname(FilePath)}].

service_start_find_internal_add_pathz(Path) ->
    CodePath = filename:dirname(Path),
    if
        CodePath == "." ->
            case code:where_is_file(Path) of
                non_existing ->
                    {error, {non_existing, Path}};
                FullPath ->
                    {ok, FullPath}
            end;
        true ->
            case code:add_pathz(CodePath) of
                true ->
                    {ok, Path};
                {error, Reason} ->
                    {error, {Reason, CodePath}}
            end
    end.

service_start_find_internal_module(Module, Service)
    when is_atom(Module) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_file(Module) of
                {module, Module} ->
                    {ok, Service#config_service_internal{module = Module}};
                {error, Reason} ->
                    {error, {Reason, Module}}
            end;
        _ ->
            {ok, Service#config_service_internal{module = Module}}
    end.

service_start_find_internal_application(Application, Service)
    when is_atom(Application) ->
    case cloudi_x_reltool_util:application_start(Application) of
        ok ->
            {ok, Service#config_service_internal{module = Application}};
        {error, _} = Error ->
            Error
    end.

service_start_find_internal_script(ScriptPath, Service)
    when is_list(ScriptPath) ->
    case cloudi_x_reltool_util:script_start(ScriptPath) of
        {ok, [Application | _]} ->
            {ok, Service#config_service_internal{module = Application}};
        {error, _} = Error ->
            Error
    end.

service_stop_remove_internal(#config_service_internal{module = Module,
                                                      file_path = Path},
                             Timeout)
    when is_atom(Module), is_list(Path) ->
    case filename:extension(Path) of
        ".erl" ->
            cloudi_x_reltool_util:module_purged(Module, Timeout);
        ".beam" ->
            cloudi_x_reltool_util:module_purged(Module, Timeout);
        ".app" ->
            cloudi_x_reltool_util:application_remove(Module, Timeout);
        ".script" ->
            cloudi_x_reltool_util:script_remove(Path, Timeout)
    end;
service_stop_remove_internal(#config_service_internal{module = Module},
                             Timeout)
    when is_atom(Module) ->
    case cloudi_x_reltool_util:application_running(Module, Timeout) of
        {ok, _} ->
            cloudi_x_reltool_util:application_remove(Module, Timeout);
        {error, {not_found, Module}} ->
            cloudi_x_reltool_util:module_purged(Module, Timeout);
        {error, _} = Error ->
            Error
    end.

service_start_internal(0, _, _) ->
    ok;
service_start_internal(Count0,
                       #config_service_internal{
                           module = Module,
                           args = Args,
                           timeout_init = TimeoutInit,
                           prefix = Prefix,
                           timeout_async = TimeoutAsync,
                           timeout_sync = TimeoutSync,
                           dest_refresh = DestRefresh,
                           dest_list_deny = DestListDeny,
                           dest_list_allow = DestListAllow,
                           options = Options,
                           max_r = MaxR,
                           max_t = MaxT,
                           uuid = UUID} = Service, Timeout) ->
    Count1 = Count0 - 1,
    case cloudi_services_monitor:monitor(cloudi_spawn, start_internal,
                                         [Count1,
                                          Module, Args, TimeoutInit,
                                          Prefix, TimeoutAsync, TimeoutSync,
                                          DestRefresh, DestListDeny,
                                          DestListAllow, Options, UUID],
                                         MaxR, MaxT, UUID, Timeout) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error starting internal service (~p):~n ~p",
                       [Module, Reason]),
            ok
    end,
    service_start_internal(Count1, Service, Timeout).

service_start_external(0, _, _) ->
    ok;
service_start_external(Count0,
                       #config_service_external{
                           count_thread = CountThread,
                           file_path = FilePath,
                           args = Args,
                           env = Env,
                           protocol = Protocol,
                           buffer_size = BufferSize,
                           timeout_init = TimeoutInit,
                           prefix = Prefix,
                           timeout_async = TimeoutAsync,
                           timeout_sync = TimeoutSync,
                           dest_refresh = DestRefresh,
                           dest_list_deny = DestListDeny,
                           dest_list_allow = DestListAllow,
                           options = Options,
                           max_r = MaxR,
                           max_t = MaxT,
                           uuid = UUID} = Service, Timeout) ->
    case cloudi_services_monitor:monitor(cloudi_spawn, start_external,
                                         [concurrency(CountThread),
                                          FilePath, Args, Env,
                                          Protocol, BufferSize, TimeoutInit,
                                          Prefix, TimeoutAsync, TimeoutSync,
                                          DestRefresh, DestListDeny,
                                          DestListAllow, Options, UUID],
                                         MaxR, MaxT, UUID, Timeout) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error starting external service (~p):~n ~p",
                       [FilePath, Reason]),
            ok
    end,
    service_start_external(Count0 - 1, Service, Timeout).

service_stop_internal(#config_service_internal{
                          module = Module,
                          uuid = UUID} = Service, Remove, Timeout) ->
    case cloudi_services_monitor:shutdown(UUID, Timeout) of
        ok when Remove =:= true ->
            % no service processes are using the service module
            % so it is safe to remove the service module
            % dependencies (applications, if they were used)
            case service_stop_remove_internal(Service, Timeout) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("error removing internal service (~p):~n ~p",
                               [Module, Reason])
            end,
            ok;
        ok when Remove =:= false ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error stopping internal service (~p):~n ~p",
                       [Module, Reason]),
            ok
    end.

service_stop_external(#config_service_external{
                          file_path = FilePath,
                          uuid = UUID}, Timeout) ->
    case cloudi_services_monitor:shutdown(UUID, Timeout) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error stopping external service (~p):~n ~p",
                       [FilePath, Reason]),
            ok
    end.

service_restart_internal(#config_service_internal{
                             module = Module,
                             uuid = UUID}, Timeout) ->
    case cloudi_services_monitor:restart(UUID, Timeout) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error restarting internal service (~p):~n ~p",
                       [Module, Reason]),
            ok
    end.

service_restart_external(#config_service_external{
                             file_path = FilePath,
                             uuid = UUID}, Timeout) ->
    case cloudi_services_monitor:restart(UUID, Timeout) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("error restarting external service (~p):~n ~p",
                       [FilePath, Reason]),
            ok
    end.

