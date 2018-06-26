%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Configurator==
%%% Use the configuration information to start CloudI processes.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_configurator).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         configure/0,
         acl_add/2,
         acl_remove/2,
         acl/1,
         service_subscriptions/2,
         services_add/2,
         services_remove/2,
         services_restart/2,
         services_update/2,
         services_search/3,
         service_ids/1,
         services/1,
         nodes_set/2,
         nodes_get/1,
         nodes_add/2,
         nodes_remove/2,
         logging_set/2,
         logging_file_set/2,
         logging_stdout_set/2,
         logging_level_set/2,
         logging_syslog_set/2,
         logging_formatters_set/2,
         logging_redirect_set/2,
         logging/1,
         code_status/3,
         service_start/2,
         service_stop/3,
         service_restart/2,
         service_update/2,
         service_update_external/5,
         service_initialized_process/1,
         service_dead/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").
-include_lib("kernel/include/file.hrl").

-type error_reason_service_start() ::
    {service_internal_module_invalid |
     service_internal_module_not_loaded |
     service_internal_module_not_found |
     service_internal_module_compile |
     service_internal_application_invalid |
     service_internal_release_not_found |
     service_internal_release_invalid |
     service_internal_file_extension_invalid |
     service_internal_file_path_invalid |
     service_internal_start_failed |
     service_external_start_failed |
     service_options_application_name_not_found, any()}.
-type error_reason_service_stop() ::
    {service_internal_module_not_found |
     service_internal_application_not_found |
     service_internal_release_not_found |
     service_internal_stop_failed |
     service_external_stop_failed, any()}.
-type error_reason_service_restart() ::
    {service_internal_restart_failed |
     service_external_restart_failed, any()}.
-type error_reason_service_update() ::
    {service_internal_update_failed |
     service_external_update_failed, any()}.
-type error_reason_services_search() ::
    service_scope_invalid.
-type error_reason_code_status() ::
    {file, {file:filename(), file:posix() | badarg}}.
-export_type([error_reason_service_start/0,
              error_reason_service_stop/0,
              error_reason_service_restart/0,
              error_reason_service_update/0,
              error_reason_services_search/0,
              error_reason_code_status/0]).

-record(state,
    {
        configuration :: #config{}
    }).

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

-ifdef(CLOUDI_CORE_STANDALONE).
-dialyzer({no_match,
           [service_update_external/6]}).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(#config{} = Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

configure() ->
    gen_server:call(?MODULE, configure, infinity).

acl_add(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {acl_add, L}, Timeout)).

acl_remove(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {acl_remove, L}, Timeout)).

acl(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, acl, Timeout)).

service_subscriptions(ServiceId, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {service_subscriptions, ServiceId,
                                 timeout_decr(Timeout)}, Timeout)).

services_add(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_add, L}, Timeout)).

services_remove(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_remove, L}, Timeout)).

services_restart(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_restart, L}, Timeout)).

services_update(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_update, L}, Timeout)).

services_search(Scope, ServiceName, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {services_search, Scope, ServiceName,
                                 timeout_decr(Timeout)}, Timeout)).

service_ids(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, service_ids, Timeout)).

services(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, services, Timeout)).

nodes_set(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {nodes_set, L, local}, Timeout)).

nodes_get(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, nodes_get, Timeout)).

nodes_add(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {nodes_add, L, local}, Timeout)).
nodes_remove(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {nodes_remove, L, local}, Timeout)).

logging_set(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {logging_set, L}, Timeout)).

logging_file_set(FilePath, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {logging_file_set, FilePath}, Timeout)).

logging_stdout_set(Stdout, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {logging_stdout_set, Stdout}, Timeout)).

logging_level_set(Level, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {logging_level_set, Level}, Timeout)).

logging_syslog_set(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {logging_syslog_set, L}, Timeout)).

logging_formatters_set(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {logging_formatters_set, L}, Timeout)).

logging_redirect_set(L, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {logging_redirect_set, L}, Timeout)).

logging(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, logging, Timeout)).

code_status(SecondsStart, SecondsNow, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {code_status,
                                 SecondsStart, SecondsNow}, Timeout)).

-spec service_start(#config_service_internal{} |
                    #config_service_external{},
                    Timeout :: pos_integer() | infinity) ->
    {ok, #config_service_internal{} |
         #config_service_external{}} |
    {error, error_reason_service_start()}.

service_start(#config_service_internal{
                  count_process = CountProcess,
                  options = #config_service_options{
                      reload = Reload}} = Service,
              Timeout) ->
    case service_start_find_internal(Service, Timeout) of
        {ok, Application, #config_service_internal{
                              module = Module} = FoundService} ->
            GroupLeader = if
                Application =:= undefined ->
                    undefined;
                is_atom(Application) ->
                    application_controller:get_master(Application)
            end,
            if
                Reload =:= true ->
                    ok = cloudi_core_i_services_internal_reload:
                         service_add(Module);
                Reload =:= false ->
                    ok
            end,
            NewCountProcess = cloudi_concurrency:count(CountProcess),
            service_start_internal(FoundService, GroupLeader,
                                   NewCountProcess, Timeout);
        {error, _} = Error ->
            Error
    end;

service_start(#config_service_external{count_process = CountProcess,
                                       count_thread = CountThread} = Service,
              Timeout) ->
    NewCountProcess = cloudi_concurrency:count(CountProcess),
    NewCountThread = cloudi_concurrency:count(CountThread),
    service_start_external(Service, NewCountThread,
                           NewCountProcess, Timeout).

-spec service_stop(#config_service_internal{} |
                   #config_service_external{},
                   Remove :: boolean(),
                   Timeout :: pos_integer() | infinity) ->
    ok |
    {error, error_reason_service_stop()}.

service_stop(#config_service_internal{} = Service, Remove, Timeout)
    when is_boolean(Remove) ->
    service_stop_internal(Service, Remove, timeout_decr(Timeout));

service_stop(#config_service_external{} = Service, false, Timeout) ->
    service_stop_external(Service, timeout_decr(Timeout)).

-spec service_restart(#config_service_internal{} |
                      #config_service_external{},
                      Timeout :: pos_integer() | infinity) ->
    ok |
    {error, error_reason_service_restart()}.

service_restart(#config_service_internal{} = Service, Timeout) ->
    service_restart_internal(Service, timeout_decr(Timeout));

service_restart(#config_service_external{} = Service, Timeout) ->
    service_restart_external(Service, timeout_decr(Timeout)).

-spec service_update(#config_service_update{},
                     Timeout :: pos_integer() | infinity) ->
    {ok, nonempty_list(cloudi_service_api:service_id())} |
    {error, nonempty_list(cloudi_service_api:service_id()),
     error_reason_service_update()}.

service_update(#config_service_update{
                   type = Type,
                   uuids = ServiceIdList} = UpdatePlan, Timeout) ->
    ErrorReasonType = if
        Type =:= internal ->
            service_internal_update_failed;
        Type =:= external ->
            service_external_update_failed
    end,
    case cloudi_core_i_services_monitor:update(UpdatePlan, Timeout) of
        ok ->
            {ok, ServiceIdList};
        {error, Reason} ->
            {error, ServiceIdList, {ErrorReasonType, Reason}}
    end.

service_update_external(Pids, Ports, Arguments,
                        CountThread, CountProcess) ->
    service_update_external(0, Pids, Ports, Arguments,
                            CountThread, CountProcess).

service_initialized_process(Pid)
    when is_pid(Pid) ->
    ?MODULE ! {service_initialized_process, Pid},
    ok.

-spec service_dead(ID :: binary()) ->
    ok.

service_dead(ID)
    when is_binary(ID) ->
    gen_server:cast(?MODULE, {service_dead, ID}).

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

handle_call({acl_add, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:acl_add(L, Config) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({acl_remove, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:acl_remove(L, Config) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(acl, _,
            #state{configuration = Config} = State) ->
    {reply, {ok, cloudi_core_i_configuration:acl(Config)}, State};

handle_call({service_subscriptions, ServiceId, Timeout}, _, State) ->
    case cloudi_core_i_services_monitor:pids(ServiceId, Timeout) of
        {ok, Scope, PidList} ->
            L = [sets:from_list(cloudi_x_cpg:which_groups(Scope, Pid, Timeout))
                 || Pid <- PidList],
            {reply, {ok, lists:sort(sets:to_list(sets:union(L)))}, State};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_add, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:services_add(L, Config, infinity) of
        {ok, IDs, NewConfig} ->
            {reply, {ok, IDs}, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_remove, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:services_remove(L, Config, infinity) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_restart, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:services_restart(L, Config, infinity) of
        {ok, NewConfig} ->
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_update, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:services_update(L, Config, infinity) of
        {ok, Result, NewConfig} ->
            {reply, Result, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({services_search, Scope, ServiceName, Timeout}, _,
            #state{configuration = Config} = State) ->
    try cloudi_x_cpg:get_local_members(Scope, ServiceName, Timeout) of
        {ok, _, PidList} ->
            case cloudi_core_i_services_monitor:search(PidList, Timeout) of
                {ok, []} ->
                    {reply, {ok, []}, State};
                {ok, L} ->
                    {reply, 
                     {ok,
                      cloudi_core_i_configuration:services_search(L, Config)},
                     State};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, _} ->
            {reply, {ok, []}, State}
    catch
        exit:{timeout, _} ->
            {reply, {error, timeout}, State};
        exit:{noproc, _} ->
            {reply, {error, service_scope_invalid}, State}
    end;

handle_call(service_ids, _,
            #state{configuration = Config} = State) ->
    {reply, {ok, cloudi_core_i_configuration:service_ids(Config)}, State};

handle_call(services, _,
            #state{configuration = Config} = State) ->
    {reply, {ok, cloudi_core_i_configuration:services(Config)}, State};

handle_call({nodes_set, _, _} = Request, _, State) ->
    nodes_call(Request, State);

handle_call(nodes_get, _,
            #state{configuration = Config} = State) ->
    {reply, {ok, cloudi_core_i_configuration:nodes_get(Config)}, State};

handle_call({nodes_add, _, _} = Request, _, State) ->
    nodes_call(Request, State);

handle_call({nodes_remove, _, _} = Request, _, State) ->
    nodes_call(Request, State);

handle_call({logging_set, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:logging_set(L, Config) of
        {ok, #config{logging = LoggingConfig} = NewConfig} ->
            case cloudi_core_i_logger:set(LoggingConfig) of
                ok ->
                    {reply, ok, State#state{configuration = NewConfig}};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({logging_file_set, FilePath}, _,
            #state{configuration = Config} = State) ->
    #config{logging = LoggingConfig} = Config,
    case cloudi_core_i_logger:file_set(FilePath) of
        ok ->
            NewConfig = Config#config{
                            logging = LoggingConfig#config_logging{
                                file = FilePath}},
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({logging_stdout_set, Stdout}, _,
            #state{configuration = Config} = State) ->
    #config{logging = LoggingConfig} = Config,
    ok = cloudi_core_i_logger:stdout_set(Stdout),
    NewConfig = Config#config{
                    logging = LoggingConfig#config_logging{
                        stdout = Stdout}},
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({logging_level_set, Level}, _,
            #state{configuration = Config} = State) ->
    #config{logging = LoggingConfig} = Config,
    ok = cloudi_core_i_logger:level_set(Level),
    NewConfig = Config#config{
                    logging = LoggingConfig#config_logging{
                        level = Level}},
    {reply, ok, State#state{configuration = NewConfig}};

handle_call({logging_syslog_set, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:logging_syslog_set(L, Config) of
        {ok, #config{logging = #config_logging{
                         syslog = SyslogConfig}} = NewConfig} ->
            ok = cloudi_core_i_logger:syslog_set(SyslogConfig),
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({logging_formatters_set, L}, _,
            #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:logging_formatters_set(L, Config) of
        {ok, #config{logging = #config_logging{
                         formatters = FormattersConfig}} = NewConfig} ->
            ok = cloudi_core_i_logger:formatters_set(FormattersConfig),
            {reply, ok, State#state{configuration = NewConfig}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({logging_redirect_set, Node}, _,
            #state{configuration = Config} = State) ->
    #config{logging = LoggingConfig} = Config,
    ok = cloudi_core_i_logger:redirect_set(Node),
    NewConfig = Config#config{
                    logging = LoggingConfig#config_logging{
                        redirect = Node}},
    {reply, ok, State#state{configuration = NewConfig}};

handle_call(logging, _,
            #state{configuration = Config} = State) ->
    {reply, {ok, cloudi_core_i_configuration:logging(Config)}, State};

handle_call({code_status, SecondsStart, SecondsNow}, _,
            #state{configuration = Config} = State) ->
    #config{services = Services} = Config,
    {reply, code_status_files(Services, SecondsStart, SecondsNow), State};

handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~w\"", [Request]),
     error, State}.

handle_cast({service_dead, ID}, #state{configuration = Config} = State) ->
    #config{services = Services} = Config,
    NewServices = lists:filter(fun(Service) ->
        case Service of
            #config_service_internal{uuid = InternalID} ->
                InternalID /= ID;
            #config_service_external{uuid = ExternalID} ->
                ExternalID /= ID
        end
    end, Services),
    NewConfig = Config#config{services = NewServices},
    {noreply, State#state{configuration = NewConfig}};

handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~w\"", [Request]), State}.

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

handle_info({service_initialized_process, _}, State) ->
    % a service process has initialized due to count_process_dynamic
    % (nothing to do, handled by cloudi_core_i_services_monitor)
    {noreply, State};

handle_info({ReplyRef, _}, State) when is_reference(ReplyRef) ->
    % gen_server:call/3 had a timeout exception that was caught but the
    % reply arrived later and must be discarded
    {noreply, State};

handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

configure(#config{services = Services} = Config, Timeout) ->
    case configure_service(Services, Timeout) of
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
        {error, Reason} = Error ->
            % wait for logging statements to be logged before crashing
            ?LOG_FATAL_SYNC("configure failed: ~p~n~p",
                            [Reason, service_format(Service)]),
            Error
    end.

configure_service(Services, Timeout) ->
    configure_service(Services, [], Timeout).

service_start_find_internal(#config_service_internal{
                                module = Module,
                                options = #config_service_options{
                                    automatic_loading = AutomaticLoading,
                                    application_name = ApplicationNameForced
                                }} = Service, Timeout)
    when is_atom(Module) ->
    Application = if
        ApplicationNameForced =/= undefined ->
            ApplicationNameForced;
        ApplicationNameForced =:= undefined ->
            Module
    end,
    if
        AutomaticLoading =:= true ->
            % prefer application files to load internal services
            % (so that application dependencies can be clearly specified, etc.)
            case application:load(Application) of
                ok ->
                    service_start_find_internal_application(Application,
                                                            Module, Service,
                                                            Timeout);
                {error, {already_loaded, Application}} ->
                    service_start_find_internal_application(Application,
                                                            Module, Service,
                                                            Timeout);
                {error, _} when ApplicationNameForced =/= undefined ->
                    {error, {service_options_application_name_not_found,
                             ApplicationNameForced}};
                {error, _} ->
                    % if no application file can be loaded,
                    % load it as a simple module
                    service_start_find_internal_module(Module, Service)
            end;
        AutomaticLoading =:= false ->
            case code:is_loaded(Module) of
                false ->
                    {error, {service_internal_module_not_loaded, Module}};
                _ ->
                    case cloudi_x_reltool_util:
                         application_loaded(Application) of
                        {ok, _} ->
                            {ok, Application, Service};
                        {error, _} when ApplicationNameForced =/= undefined ->
                            {error, {service_options_application_name_not_found,
                                     ApplicationNameForced}};
                        {error, _} ->
                            {ok, undefined, Service}
                    end
            end
    end;
service_start_find_internal(#config_service_internal{
                                module = FilePath,
                                options = #config_service_options{
                                    automatic_loading = true,
                                    application_name = undefined
                                }} = Service, Timeout)
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
                            {error, {service_internal_module_compile, error}};
                        {error, Errors, Warnings} ->
                            {error,
                             {service_internal_module_compile,
                              {Errors, Warnings}}}
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
                            {error,
                             {service_internal_release_not_found, FilePath}};
                        FullFilePath ->
                            service_start_find_internal_script(FullFilePath,
                                                               Service)
                    end;
                _ ->
                    service_start_find_internal_script(FilePath, Service)
            end;
        ".boot" ->
            case filename:dirname(FilePath) of
                "." ->
                    case code:where_is_file(FilePath) of
                        non_existing ->
                            {error,
                             {service_internal_release_not_found, FilePath}};
                        FullFilePath ->
                            service_start_find_internal_boot(FullFilePath,
                                                             Service)
                    end;
                _ ->
                    service_start_find_internal_boot(FilePath, Service)
            end;
        Extension ->
            {error, {service_internal_file_extension_invalid, Extension}}
    end;
service_start_find_internal(#config_service_internal{
                                module = Module}, _Timeout) ->
    {error, {service_internal_module_invalid, Module}}.

compiler_options(FilePath) ->
    [{outdir, filename:dirname(FilePath)}].

service_start_find_internal_add_pathz(Path) ->
    CodePath = filename:dirname(Path),
    if
        CodePath == "." ->
            case code:where_is_file(Path) of
                non_existing ->
                    {error,
                     {service_internal_file_path_invalid, Path}};
                FullPath ->
                    {ok, FullPath}
            end;
        true ->
            case code:add_pathz(CodePath) of
                true ->
                    {ok, Path};
                {error, Reason} ->
                    {error,
                     {service_internal_file_path_invalid,
                      {Reason, CodePath}}}
            end
    end.

service_start_find_internal_module(Module, Service)
    when is_atom(Module) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_file(Module) of
                {module, Module} ->
                    {ok, undefined,
                     Service#config_service_internal{module = Module}};
                {error, Reason} ->
                    {error,
                     {service_internal_module_not_found,
                      {Reason, Module}}}
            end;
        _ ->
            {ok, undefined,
             Service#config_service_internal{module = Module}}
    end.

service_start_find_internal_application(Application, Module, Service, Timeout)
    when is_atom(Application), is_atom(Module) ->
    case cloudi_x_reltool_util:application_start(Application, [], Timeout) of
        ok ->
            {ok, Application,
             Service#config_service_internal{module = Module}};
        {error, Reason} ->
            {error, {service_internal_application_invalid, Reason}}
    end.

service_start_find_internal_script(ScriptPath, Service)
    when is_list(ScriptPath) ->
    case cloudi_x_reltool_util:script_start(ScriptPath) of
        {ok, [Application | _]} ->
            {ok, Application,
             Service#config_service_internal{module = Application}};
        {error, Reason} ->
            {error, {service_internal_release_invalid, Reason}}
    end.

service_start_find_internal_boot(BootPath, Service)
    when is_list(BootPath) ->
    case cloudi_x_reltool_util:boot_start(BootPath) of
        {ok, [Application | _]} ->
            {ok, Application,
             Service#config_service_internal{module = Application}};
        {error, Reason} ->
            {error, {service_internal_release_invalid, Reason}}
    end.

service_stop_remove_internal(#config_service_internal{
                                options = #config_service_options{
                                    automatic_loading = false}},
                             _Timeout) ->
    ignore;
service_stop_remove_internal(#config_service_internal{
                                 module = Module,
                                 file_path = FilePath},
                             Timeout)
    when is_atom(Module), is_list(FilePath) ->
    case filename:extension(FilePath) of
        ".erl" ->
            case cloudi_x_reltool_util:module_purged(Module, Timeout) of
                ok ->
                    {ok, module};
                {error, Reason} ->
                    {error, {service_internal_module_not_found, Reason}}
            end;
        ".beam" ->
            case cloudi_x_reltool_util:module_purged(Module, Timeout) of
                ok ->
                    {ok, module};
                {error, Reason} ->
                    {error, {service_internal_module_not_found, Reason}}
            end;
        ".app" ->
            case cloudi_x_reltool_util:application_remove(Module, Timeout,
                                                          [cloudi_core]) of
                ok ->
                    {ok, application};
                {error, Reason} ->
                    {error, {service_internal_application_not_found, Reason}}
            end;
        ".script" ->
            case cloudi_x_reltool_util:script_remove(FilePath, Timeout,
                                                     [cloudi_core]) of
                ok ->
                    {ok, release};
                {error, Reason} ->
                    {error, {service_internal_release_not_found, Reason}}
            end;
        ".boot" ->
            case cloudi_x_reltool_util:boot_remove(FilePath, Timeout,
                                                   [cloudi_core]) of
                ok ->
                    {ok, release};
                {error, Reason} ->
                    {error, {service_internal_release_not_found, Reason}}
            end
    end;
service_stop_remove_internal(#config_service_internal{
                                 module = Module,
                                 options = #config_service_options{
                                     application_name = undefined}},
                             Timeout)
    when is_atom(Module) ->
    case cloudi_x_reltool_util:application_running(Module, Timeout) of
        {ok, _} ->
            case cloudi_x_reltool_util:application_remove(Module, Timeout,
                                                          [cloudi_core]) of
                ok ->
                    {ok, application};
                {error, Reason} ->
                    {error, {service_internal_application_not_found, Reason}}
            end;
        {error, {not_found, Module}} ->
            case cloudi_x_reltool_util:module_purged(Module, Timeout) of
                ok ->
                    {ok, module};
                {error, Reason} ->
                    {error, {service_internal_module_not_found, Reason}}
            end;
        {error, Reason} ->
            {error, {service_internal_application_not_found, Reason}}
    end;
service_stop_remove_internal(#config_service_internal{
                                 module = Module,
                                 options = #config_service_options{
                                     application_name = Application}},
                             Timeout)
    when is_atom(Module) ->
    case cloudi_x_reltool_util:application_remove(Application, Timeout,
                                                  [cloudi_core]) of
        ok ->
            {ok, application};
        {error, Reason} ->
            {error, {service_internal_application_not_found, Reason}}
    end.

service_start_wait_pid([], Error) ->
    Error;
service_start_wait_pid([{MonitorRef, Pid} | M], Error) ->
    receive
        {service_initialized_process, Pid} ->
            erlang:demonitor(MonitorRef, [flush]),
            % at this point:
            % - if it is an internal service process:
            %   it has already completed executing cloudi_service_init/3 and
            %   any aspects_init_after functions
            % - if it is an external service process:
            %   it has already completed executing CloudI API functions called
            %   before the poll function and any aspects_init_after functions
            %   (so the external service is now executing the poll function)
            service_start_wait_pid(M, Error);
        {'DOWN', MonitorRef, process, Pid, Info} ->
            NewError = if
                Error =:= undefined ->
                    {error, {service_internal_start_failed, Info}};
                true ->
                    Error
            end,
            service_start_wait_pid(M, NewError)
    end.

service_start_wait_pids([], undefined, Service) ->
    {ok, Service};
service_start_wait_pids([], Error, _) ->
    Error;
service_start_wait_pids([M | MonitorPids], Error, Service) ->
    service_start_wait_pids(MonitorPids,
                            service_start_wait_pid(M, Error), Service).

service_start_wait(Pids, Service) ->
    % create the monitors on all the service's receiver pids
    MonitorPids = [[{erlang:monitor(process, Pid), Pid} ||
                    Pid <- P] || P <- Pids],
    % service processes block waiting for the configurator to get to this point
    % so that monitors here may catch a process crash due to user source code
    % and pass the error reason as a return value of the configure/2 function
    % or the cloudi_service_api:services_add/2 function when a service
    % starts for the first time
    % (restarts require cloudi_core_i_services_monitor to
    %  call cloudi_core_i_services_monitor:initialize/1)
    lists:foreach(fun(P) ->
        ok = cloudi_core_i_services_monitor:initialize(P)
    end, Pids),
    service_start_wait_pids(MonitorPids, undefined, Service).

service_start_internal(CountProcess, Pids, Service, _, CountProcess, _) ->
    service_start_wait(lists:reverse(Pids), Service);
service_start_internal(IndexProcess, Pids,
                       #config_service_internal{
                           module = Module,
                           args = Args,
                           timeout_init = TimeoutInit,
                           prefix = Prefix,
                           timeout_async = TimeoutAsync,
                           timeout_sync = TimeoutSync,
                           timeout_term = TimeoutTerm,
                           dest_refresh = DestRefresh,
                           dest_list_deny = DestListDeny,
                           dest_list_allow = DestListAllow,
                           options = #config_service_options{
                               restart_delay = RestartDelay,
                               scope = Scope} = Options,
                           max_r = MaxR,
                           max_t = MaxT,
                           uuid = ID} = Service, GroupLeader,
                       CountProcess, Timeout) ->
    case cloudi_core_i_services_monitor:
         monitor(cloudi_core_i_spawn, start_internal,
                 [GroupLeader,
                  Module, Args, TimeoutInit,
                  Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                  DestRefresh, DestListDeny,
                  DestListAllow, Options, ID],
                 IndexProcess, CountProcess, 1, Scope,
                 TimeoutTerm, RestartDelay, MaxR, MaxT, ID, Timeout) of
        {ok, P} ->
            service_format_log(Service, P),
            service_start_internal(IndexProcess + 1, [P | Pids], Service,
                                   GroupLeader, CountProcess, Timeout);
        {error, Reason} ->
            {error, {service_internal_start_failed, Reason}}
    end.

service_start_internal(Service, GroupLeader, CountProcess, Timeout) ->
    service_start_internal(0, [], Service, GroupLeader,
                           CountProcess, timeout_decr(Timeout)).

service_start_external(CountProcess, Pids, Service, _, CountProcess, _) ->
    service_start_wait(lists:reverse(Pids), Service);
service_start_external(IndexProcess, Pids,
                       #config_service_external{
                           file_path = FilePath,
                           args = Args,
                           env = Env,
                           protocol = Protocol,
                           buffer_size = BufferSize,
                           timeout_init = TimeoutInit,
                           prefix = Prefix,
                           timeout_async = TimeoutAsync,
                           timeout_sync = TimeoutSync,
                           timeout_term = TimeoutTerm,
                           dest_refresh = DestRefresh,
                           dest_list_deny = DestListDeny,
                           dest_list_allow = DestListAllow,
                           options = #config_service_options{
                               restart_delay = RestartDelay,
                               scope = Scope} = Options,
                           max_r = MaxR,
                           max_t = MaxT,
                           uuid = ID} = Service,
                       CountThread, CountProcess, Timeout) ->
    case cloudi_core_i_services_monitor:
         monitor(cloudi_core_i_spawn, start_external,
                 [CountThread,
                  FilePath, Args, Env,
                  Protocol, BufferSize, TimeoutInit,
                  Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                  DestRefresh, DestListDeny,
                  DestListAllow, Options, ID],
                 IndexProcess, CountProcess, CountThread, Scope,
                 TimeoutTerm, RestartDelay, MaxR, MaxT, ID, Timeout) of
        {ok, P} ->
            service_format_log(Service, P),
            service_start_external(IndexProcess + 1, [P | Pids], Service,
                                   CountThread, CountProcess, Timeout);
        {error, Reason} ->
            {error, {service_external_start_failed, Reason}}
    end.

service_start_external(Service, CountThread, CountProcess, Timeout) ->
    service_start_external(0, [], Service, CountThread,
                           CountProcess, timeout_decr(Timeout)).

service_stop_internal(#config_service_internal{
                          module = Module,
                          options = #config_service_options{
                              reload = Reload},
                          uuid = ID} = Service, Remove, Timeout) ->
    case cloudi_core_i_services_monitor:shutdown(ID, Timeout) of
        {ok, Pids} ->
            shutdown_wait(Pids),
            if
                Reload =:= true ->
                    ok = cloudi_core_i_services_internal_reload:
                         service_remove(Module);
                Reload =:= false ->
                    ok
            end,
            if
                Remove =:= true ->
                    % no service processes are using the service module
                    % so it is safe to remove the service module
                    % dependencies (applications, if they were used, or
                    % unload the service module)
                    case service_stop_remove_internal(Service, Timeout) of
                        ignore ->
                            ?LOG_INFO_SYNC("Service pids ~p stopped~n ~p",
                                           [Pids, service_id(ID)]),
                            ok;
                        {ok, RemoveType} ->
                            ?LOG_INFO_SYNC("Service pids ~p stopped ~p~n ~p",
                                           [Pids, RemoveType,
                                            service_id(ID)]),
                            ok;
                        {error, _} = Error ->
                            Error
                    end;
                Remove =:= false ->
                    ?LOG_INFO_SYNC("Service pids ~p stopped~n ~p",
                                   [Pids, service_id(ID)]),
                    ok
            end;
        {error, Reason} ->
            {error, {service_internal_stop_failed, Reason}}
    end.

service_stop_external(#config_service_external{
                          uuid = ID}, Timeout) ->
    case cloudi_core_i_services_monitor:shutdown(ID, Timeout) of
        {ok, Pids} ->
            shutdown_wait(Pids),
            ?LOG_INFO_SYNC("Service pids ~p stopped~n ~p",
                           [Pids, service_id(ID)]),
            ok;
        {error, Reason} ->
            {error, {service_external_stop_failed, Reason}}
    end.

service_restart_internal(#config_service_internal{
                             uuid = ID}, Timeout) ->
    case cloudi_core_i_services_monitor:restart(ID, Timeout) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {service_internal_restart_failed, Reason}}
    end.

service_restart_external(#config_service_external{
                             uuid = ID}, Timeout) ->
    case cloudi_core_i_services_monitor:restart(ID, Timeout) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {service_external_restart_failed, Reason}}
    end.

service_update_external(CountProcess, [], [], _, _, CountProcess) ->
    ok;
service_update_external(IndexProcess, Pids, Ports, Arguments,
                        CountThread, CountProcess) ->
    {ProcessPids, RemainingPids} = lists:split(CountThread, Pids),
    {ProcessPorts, RemainingPorts} = lists:split(CountThread, Ports),
    case cloudi_core_i_spawn:
         update_external(ProcessPids, ProcessPorts,
                         [IndexProcess, CountProcess | Arguments]) of
        ok ->
            service_update_external(IndexProcess + 1,
                                    RemainingPids, RemainingPorts, Arguments,
                                    CountThread, CountProcess);
        {error, Reason} ->
            {error, {service_external_update_failed, Reason}}
    end.

nodes_call_remote_result(aborted) ->
    {error, aborted};
nodes_call_remote_result({Replies, BadNodes}) ->
    % ignore bad nodes
    Errors = nodes_call_remote_result_replies(Replies, []) ++
             [{Node, bad_node} || Node <- BadNodes],
    if
        Errors == [] ->
            ok;
        true ->
            {error, {remote, Errors}}
    end.
    
nodes_call_remote_result_replies([], Output) ->
    lists:reverse(Output);
nodes_call_remote_result_replies([{_, ok} | Replies], Output) ->
    nodes_call_remote_result_replies(Replies, Output);
nodes_call_remote_result_replies([{_, _} = Error | Replies], Output) ->
    nodes_call_remote_result_replies(Replies, [Error | Output]).

nodes_call_remote({_, _, remote}, _) ->
    ok;
nodes_call_remote({F, L, local}, Connect) ->
    Nodes = if
        Connect =:= visible ->
            nodes();
        Connect =:= hidden ->
            nodes(connected)
    end,
    nodes_call_remote_result(global:trans({{?MODULE, L}, self()}, fun() ->
        gen_server:multi_call(Nodes, ?MODULE,
                              {F, L, remote}, infinity)
    end)).

nodes_call({F, L, _} = Request,
           #state{configuration = Config} = State) ->
    case cloudi_core_i_configuration:F(L, Config) of
        {ok, Config} ->
            {reply, ok, State};
        {ok, #config{nodes = #config_nodes{connect = Connect}} = NewConfig} ->
            Result = nodes_call_remote(Request, Connect),
            case cloudi_core_i_nodes:reconfigure(NewConfig, infinity) of
                ok ->
                    {reply, Result, State#state{configuration = NewConfig}};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        {error, _} = Error ->
            {reply, Error, State}
    end.

shutdown_wait_monitor([]) ->
    ok;
shutdown_wait_monitor(Monitors) ->
    receive
        {'DOWN', Monitor, process, _, _} ->
            shutdown_wait_monitor(lists:delete(Monitor, Monitors))
    end.
shutdown_wait(Pids) ->
    shutdown_wait_monitor([erlang:monitor(process, Pid) || Pid <- Pids]).

timeout_decr(infinity) ->
    infinity;
timeout_decr(Timeout)
    when is_integer(Timeout), Timeout >= ?TIMEOUT_DELTA ->
    Timeout - ?TIMEOUT_DELTA.

service_format_log(Service, Pids) ->
    ?LOG_INFO("~p ->~n    ~p", [service_format(Service), Pids]).

service_format(Service) ->
    {ID, ServiceConfig} = cloudi_core_i_configuration:service_format(Service),
    {service_id(ID), ServiceConfig}.

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).

code_status_files(Services, SecondsStart, SecondsNow) ->
    code_status_files(Services, [], SecondsStart, SecondsNow).

code_status_files([], ChangedFiles, _, _) ->
    {ok,
     [[{type, Type},
       {file_age, cloudi_timestamp:seconds_to_string(SecondsElapsed)},
       {file_path, FilePath}]
      || {SecondsElapsed, Type, FilePath} <- ChangedFiles]};
code_status_files([Service | Services], ChangedFiles,
                  SecondsStart, SecondsNow) ->
    {Type, FilePath} = case Service of
        #config_service_internal{file_path = FilePathValue}
        when FilePathValue /= undefined ->
            {internal, FilePathValue};
        #config_service_internal{module = Module} ->
            {file, FilePathValue} = code:is_loaded(Module),
            {internal, FilePathValue};
        #config_service_external{file_path = FilePathValue} ->
            {external, FilePathValue}
    end,
    case read_file_info(FilePath) of
        {ok, #file_info{mtime = MTime}} ->
            ServicesNew = code_status_files_filter(Services, Service),
            if
                MTime > SecondsStart ->
                    ChangedFile = {SecondsNow - MTime, Type, FilePath},
                    code_status_files(ServicesNew,
                                      lists:umerge(ChangedFiles,
                                                   [ChangedFile]),
                                      SecondsStart, SecondsNow);
                true ->
                    code_status_files(ServicesNew, ChangedFiles,
                                      SecondsStart, SecondsNow)
            end;
        {error, Reason} ->
            {error, {file, {FilePath, Reason}}}
    end.

code_status_files_filter([], _) ->
    [];
code_status_files_filter([#config_service_internal{module = Module,
                                                   file_path = FilePath} |
                          Services],
                         #config_service_internal{module = Module,
                                                  file_path = FilePath} = S) ->
    code_status_files_filter(Services, S);
code_status_files_filter([#config_service_external{file_path = FilePath} |
                          Services],
                         #config_service_external{file_path = FilePath} = S) ->
    code_status_files_filter(Services, S);
code_status_files_filter([Service | Services], S) ->
    [Service | code_status_files_filter(Services, S)].

read_file_info(FilePath) ->
    file:read_file_info(FilePath, [raw, {time, posix}]).

