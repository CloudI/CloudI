%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
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
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_configurator).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         configure/0,
         acl_add/2,
         acl_remove/2,
         service_subscriptions/2,
         services_add/2,
         services_remove/2,
         services_restart/2,
         services_search/2,
         services/1,
         nodes_add/2,
         nodes_remove/2,
         service_start/2,
         service_stop/3,
         service_restart/2,
         concurrency/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_configuration.hrl").
-include("cloudi_constants.hrl").
-include("cloudi_logger.hrl").

-record(state,
    {
        configuration
    }).

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(#config{} = Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

configure() ->
    gen_server:call(?MODULE, configure, infinity).

acl_add(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {acl_add, L,
                                 timeout_decr(Timeout)}, Timeout)).

acl_remove(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {acl_remove, L,
                                 timeout_decr(Timeout)}, Timeout)).

service_subscriptions(ServiceId, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {service_subscriptions, ServiceId,
                                 timeout_decr(Timeout)}, Timeout)).

services_add(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_add, L,
                                 timeout_decr(Timeout)}, Timeout)).

services_remove(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_remove, L,
                                 timeout_decr(Timeout)}, Timeout)).

services_restart(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_restart, L,
                                 timeout_decr(Timeout)}, Timeout)).

services_search(ServiceName, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_search, ServiceName,
                                 timeout_decr(Timeout)}, Timeout)).

services(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services,
                                 timeout_decr(Timeout)}, Timeout)).

nodes_add(L, Timeout) ->
    Nodes = [node() | nodes()],
    check_multi_call(global:trans({{?MODULE, L}, self()}, fun() ->
        gen_server:multi_call(Nodes, ?MODULE,
                              {nodes_add, L,
                               timeout_decr(Timeout)}, Timeout)
    end)).

nodes_remove(L, Timeout) ->
    Nodes = [node() | nodes()],
    check_multi_call(global:trans({{?MODULE, L}, self()}, fun() ->
        gen_server:multi_call(Nodes, ?MODULE,
                              {nodes_remove, L,
                               timeout_decr(Timeout)}, Timeout)
    end)).

service_start(#config_service_internal{
                  count_process = Count,
                  options = #config_service_options{
                      reload = Reload}} = Service,
              Timeout) ->
    case service_start_find_internal(Service, Timeout) of
        {ok, #config_service_internal{
                 module = Module} = FoundService} ->
            if
                Reload =:= true ->
                    ok = cloudi_services_internal_reload:service_add(Module);
                Reload =:= false ->
                    ok
            end,
            service_start_internal(concurrency(Count),
                                   FoundService,
                                   timeout_decr(Timeout));
        {error, _} = Error ->
            Error
    end;

service_start(#config_service_external{count_process = Count} = Service,
              Timeout) ->
    service_start_external(concurrency(Count), Service, timeout_decr(Timeout)).

service_stop(#config_service_internal{} = Service, Remove, Timeout)
    when is_boolean(Remove) ->
    service_stop_internal(Service, Remove, timeout_decr(Timeout));

service_stop(#config_service_external{} = Service, false, Timeout) ->
    service_stop_external(Service, timeout_decr(Timeout)).

service_restart(#config_service_internal{} = Service, Timeout) ->
    service_restart_internal(Service, timeout_decr(Timeout));

service_restart(#config_service_external{} = Service, Timeout) ->
    service_restart_external(Service, timeout_decr(Timeout)).

concurrency(I)
    when is_integer(I) ->
    I;
concurrency(I)
    when is_float(I) ->
    if
        I > 1.0 ->
            ceil(I * erlang:system_info(schedulers));
        I > 0.0, I < 1.0 ->
            erlang:max(1, erlang:round(I * erlang:system_info(schedulers)));
        I == 1.0 ->
            erlang:system_info(schedulers)
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Config]) ->
    {ok, #state{configuration = Config}}.

handle_call(configure, _, State) ->
    % the application startup configuration must not block
    % application startup (executing the cloudi.conf)
    self() ! configure,
    {reply, ok, State};

handle_call({acl_add, L, _}, _,
            #state{configuration = Config} = State) ->
    case cloudi_configuration:acl_add(L, Config) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({acl_remove, L, _}, _,
            #state{configuration = Config} = State) ->
    case cloudi_configuration:acl_remove(L, Config) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({service_subscriptions, ServiceId, Timeout}, _, State) ->
    case cloudi_services_monitor:pids(ServiceId, Timeout) of
        {ok, PidList} ->
            L = [sets:from_list(cloudi_x_cpg:which_groups(Pid, Timeout))
                 || Pid <- PidList],
            {reply, {ok, lists:sort(sets:to_list(sets:union(L)))}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_add, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    case cloudi_configuration:services_add(L, Config, Timeout) of
        {ok, IDs, NewConfig} ->
            {reply, {ok, IDs}, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_remove, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    case cloudi_configuration:services_remove(L, Config, Timeout) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_restart, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    case cloudi_configuration:services_restart(L, Config, Timeout) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_search, ServiceName, Timeout}, _,
            #state{configuration = Config} = State) ->
    case cloudi_x_cpg:get_local_members(ServiceName, Timeout) of
        {ok, _, PidList} ->
            case cloudi_services_monitor:search(PidList, Timeout) of
                {ok, []} ->
                    {reply, {ok, []}, State};
                {ok, L} ->
                    {reply, 
                     {ok, cloudi_configuration:services_search(L, Config)},
                     State};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, _} ->
            {reply, {ok, []}, State}
    end;

handle_call({services, _}, _,
            #state{configuration = Config} = State) ->
    {reply, {ok, cloudi_configuration:services(Config)}, State};

handle_call({nodes_add, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    case cloudi_configuration:nodes_add(L, Config) of
        {ok, NewConfig} ->
            case cloudi_nodes:reconfigure(NewConfig, Timeout) of
                ok ->
                    {reply, ok, State#state{configuration = NewConfig}};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({nodes_remove, L, Timeout}, _,
            #state{configuration = Config} = State) ->
    case cloudi_configuration:nodes_remove(L, Config) of
        {ok, NewConfig} ->
            case cloudi_nodes:reconfigure(NewConfig, Timeout) of
                ok ->
                    {reply, ok, State#state{configuration = NewConfig}};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info(configure,
            #state{configuration = Config} = State) ->
    case configure(Config, infinity) of
        {ok, NewConfig} ->
            {noreply, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            % cloudi_core application startup failed due to a problem
            % with the cloudi.conf file
            {stop, Error, State}
    end;

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
    case configure_service(Services, [], Timeout) of
        {ok, NewServices} ->
            {ok, Config#config{services = NewServices}};
        {error, _} = Error ->
            Error
    end.

configure_service([], Configured, _) ->
    {ok, lists:reverse(Configured)};
configure_service([Service | Services], Configured, Timeout) ->
    case service_start(Service, Timeout) of
        {ok, NewService} ->
            configure_service(Services, [NewService | Configured], Timeout);
        {error, _} = Error ->
            Error
    end.

service_start_find_internal(#config_service_internal{
                                module = Module,
                                options = #config_service_options{
                                    automatic_loading = false}} = Service,
                            _Timeout) ->
    if
        is_atom(Module) ->
            case code:is_loaded(Module) of
                false ->
                    {error, {not_loaded, Module}};
                _ ->
                    {ok, Service}
            end;
        true ->
            {error, {invalid_loaded_module, Module}}
    end;
service_start_find_internal(#config_service_internal{
                                module = FilePath} = Service, Timeout)
    when is_list(FilePath) ->
    case filename:extension(FilePath) of
        ".erl" ->
            Module = erlang:list_to_atom(filename:basename(FilePath,
                                                           ".erl")),
            case service_start_find_internal_add_pathz(FilePath) of
                {ok, FullFilePath} ->
                    case compile:file(FullFilePath,
                                      compiler_options(FullFilePath)) of
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
            Module = erlang:list_to_atom(filename:basename(FilePath,
                                                           ".beam")),
            case service_start_find_internal_add_pathz(FilePath) of
                {ok, _} ->
                    service_start_find_internal_module(Module, Service);
                {error, _} = Error ->
                    Error
            end;
        ".app" ->
            Application = erlang:list_to_atom(filename:basename(FilePath,
                                                                ".app")),
            case service_start_find_internal_add_pathz(FilePath) of
                {ok, _} ->
                    service_start_find_internal_application(Application,
                                                            Application,
                                                            Service, Timeout);
                {error, _} = Error ->
                    Error
            end;
        ".script" ->
            case filename:dirname(FilePath) of
                "." ->
                    case code:where_is_file(FilePath) of
                        non_existing ->
                            {error, {non_existing, FilePath}};
                        FullFilePath ->
                            service_start_find_internal_script(FullFilePath,
                                                               Service)
                    end;
                _ ->
                    service_start_find_internal_script(FilePath, Service)
            end;
        Extension ->
            {error, {internal_service_module_extension_invalid, Extension}}
    end;
service_start_find_internal(#config_service_internal{
                                module = Module,
                                options = #config_service_options{
                                    application_name = ApplicationNameForced
                                }} = Service, Timeout)
    when is_atom(Module) ->
    Application = if
        ApplicationNameForced =/= undefined ->
            ApplicationNameForced;
        ApplicationNameForced =:= undefined ->
            Module
    end,
    % prefer application files to load internal services
    % (so that application dependencies can be clearly specified, etc.)
    case application:load(Application) of
        ok ->
            service_start_find_internal_application(Application,
                                                    Module, Service, Timeout);
        {error, {already_loaded, Application}} ->
            service_start_find_internal_application(Application,
                                                    Module, Service, Timeout);
        {error, _} when ApplicationNameForced =/= undefined ->
            {error, {service_options_application_name_notfound,
                     ApplicationNameForced}};
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

service_start_find_internal_application(Application, Module, Service, Timeout)
    when is_atom(Application), is_atom(Module) ->
    case cloudi_x_reltool_util:application_start(Application, [], Timeout) of
        ok ->
            {ok, Service#config_service_internal{module = Module}};
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

service_stop_remove_internal(#config_service_internal{
                                options = #config_service_options{
                                    automatic_loading = false}},
                             _Timeout) ->
    ok;
service_stop_remove_internal(#config_service_internal{
                                 module = Module,
                                 file_path = FilePath},
                             Timeout)
    when is_atom(Module), is_list(FilePath) ->
    case filename:extension(FilePath) of
        ".erl" ->
            cloudi_x_reltool_util:module_purged(Module, Timeout);
        ".beam" ->
            cloudi_x_reltool_util:module_purged(Module, Timeout);
        ".app" ->
            cloudi_x_reltool_util:application_remove(Module, Timeout);
        ".script" ->
            cloudi_x_reltool_util:script_remove(FilePath, Timeout)
    end;
service_stop_remove_internal(#config_service_internal{
                                 module = Module},
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

service_start_internal(0, Service, _) ->
    {ok, Service};
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
                           uuid = ID} = Service, Timeout) ->
    Count1 = Count0 - 1,
    case cloudi_services_monitor:monitor(cloudi_spawn, start_internal,
                                         [Count1,
                                          Module, Args, TimeoutInit,
                                          Prefix, TimeoutAsync, TimeoutSync,
                                          DestRefresh, DestListDeny,
                                          DestListAllow, Options, ID],
                                         MaxR, MaxT, ID, Timeout) of
        ok ->
            service_start_internal(Count1, Service, Timeout);
        {error, _} = Error ->
            Error
    end.

service_start_external(0, Service, _) ->
    {ok, Service};
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
                           uuid = ID} = Service, Timeout) ->
    case cloudi_services_monitor:monitor(cloudi_spawn, start_external,
                                         [concurrency(CountThread),
                                          FilePath, Args, Env,
                                          Protocol, BufferSize, TimeoutInit,
                                          Prefix, TimeoutAsync, TimeoutSync,
                                          DestRefresh, DestListDeny,
                                          DestListAllow, Options, ID],
                                         MaxR, MaxT, ID, Timeout) of
        ok ->
            service_start_external(Count0 - 1, Service, Timeout);
        {error, _} = Error ->
            Error
    end.

service_stop_internal(#config_service_internal{
                          module = Module,
                          options = #config_service_options{
                              reload = Reload},
                          uuid = ID} = Service, Remove, Timeout) ->
    case cloudi_services_monitor:shutdown(ID, Timeout) of
        ok ->
            if
                Reload =:= true ->
                    ok = cloudi_services_internal_reload:service_remove(Module);
                Reload =:= false ->
                    ok
            end,
            if
                Remove =:= true ->
                    % no service processes are using the service module
                    % so it is safe to remove the service module
                    % dependencies (applications, if they were used)
                    service_stop_remove_internal(Service, Timeout);
                Remove =:= false ->
                    ok
            end;
        {error, _} = Error ->
            Error
    end.

service_stop_external(#config_service_external{
                          uuid = ID}, Timeout) ->
    case cloudi_services_monitor:shutdown(ID, Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end.

service_restart_internal(#config_service_internal{
                             uuid = ID}, Timeout) ->
    case cloudi_services_monitor:restart(ID, Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end.

service_restart_external(#config_service_external{
                             uuid = ID}, Timeout) ->
    case cloudi_services_monitor:restart(ID, Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end.

check_multi_call({Replies, _BadNodes}) ->
    % ignore bad nodes
    check_multi_call_replies(Replies).
    
check_multi_call_replies([]) ->
    ok;
check_multi_call_replies([{_, ok} | Replies]) ->
    check_multi_call_replies(Replies);
check_multi_call_replies([{_, Result} | _]) ->
    Result.

timeout_decr(infinity) ->
    infinity;
timeout_decr(Timeout) when is_integer(Timeout) ->
    Timeout - ?TIMEOUT_DELTA.

ceil(X) ->
    T = erlang:trunc(X),
    if
        X > T ->
            T + 1;
        true ->
            T
    end.

%floor(X) ->
%    T = erlang:trunc(X),
%    if
%        X < T ->
%            T - 1;
%        true ->
%            T
%    end.

