%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Configuration==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2016 Michael Truog
%%% @version 1.5.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_configuration).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([load/1,
         acl_add/2,
         acl_remove/2,
         acl/1,
         services_add/3,
         services_remove/3,
         services_restart/3,
         services_update/3,
         services_search/2,
         services/1,
         service_format/1,
         services_format_options_internal/1,
         services_format_options_external/1,
         nodes_set/2,
         nodes_get/1,
         nodes_add/2,
         nodes_remove/2,
         logging_level_highest/1,
         logging_syslog_set/2,
         logging_formatters_set/2,
         logging/1]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").

-type error_reason_acl_add_configuration() ::
    {acl_invalid |
     acl_cyclic |
     acl_not_found, any()}.
-type error_reason_acl_remove_configuration() ::
    {acl_invalid, any()}.
-type error_reason_services_add_configuration() ::
    {acl_invalid |
     acl_not_found |
     service_invalid |
     service_internal_invalid |
     service_internal_prefix_invalid |
     service_internal_module_invalid |
     service_internal_args_invalid |
     service_internal_dest_refresh_invalid |
     service_internal_timeout_init_invalid |
     service_internal_timeout_async_invalid |
     service_internal_timeout_sync_invalid |
     service_internal_dest_list_deny_invalid |
     service_internal_dest_list_allow_invalid |
     service_internal_count_process_invalid |
     service_internal_max_r_invalid |
     service_internal_max_t_invalid |
     service_internal_max_t_increase |
     service_internal_options_invalid |
     service_external_invalid |
     service_external_prefix_invalid |
     service_external_file_path_invalid |
     service_external_args_invalid |
     service_external_env_invalid |
     service_external_dest_refresh_invalid |
     service_external_protocol_invalid |
     service_external_buffer_size_invalid |
     service_external_timeout_init_invalid |
     service_external_timeout_async_invalid |
     service_external_timeout_sync_invalid |
     service_external_dest_list_deny_invalid |
     service_external_dest_list_allow_invalid |
     service_external_count_process_invalid |
     service_external_count_thread_invalid |
     service_external_max_r_invalid |
     service_external_max_t_invalid |
     service_external_max_t_increase |
     service_external_options_invalid |
     service_options_priority_default_invalid |
     service_options_queue_limit_invalid |
     service_options_queue_size_invalid |
     service_options_rate_request_max_invalid |
     service_options_dest_refresh_start_invalid |
     service_options_dest_refresh_delay_invalid |
     service_options_request_name_lookup_invalid |
     service_options_request_timeout_adjustment_invalid |
     service_options_request_timeout_immediate_max_invalid |
     service_options_response_timeout_adjustment_invalid |
     service_options_response_timeout_immediate_max_invalid |
     service_options_count_process_dynamic_invalid |
     service_options_timeout_terminate_invalid |
     service_options_timeout_terminate_decrease |
     service_options_scope_invalid |
     service_options_monkey_latency_invalid |
     service_options_monkey_chaos_invalid |
     service_options_automatic_loading_invalid |
     service_options_aspects_init_invalid |
     service_options_aspects_request_invalid |
     service_options_aspects_info_invalid |
     service_options_aspects_terminate_invalid |
     service_options_limit_invalid |
     service_options_owner_invalid |
     service_options_application_name_invalid |
     service_options_request_pid_uses_invalid |
     service_options_request_pid_options_invalid |
     service_options_info_pid_uses_invalid |
     service_options_info_pid_options_invalid |
     service_options_duo_mode_invalid |
     service_options_hibernate_invalid |
     service_options_reload_invalid |
     service_options_invalid, any()}.
-type error_reason_services_remove_configuration() ::
    {service_invalid |
     service_not_found, any()}.
-type error_reason_services_restart_configuration() ::
    {service_invalid |
     service_not_found, any()}.
-type error_reason_services_update_configuration() ::
    {update_invalid |
     service_update_invalid |
     service_update_type_invalid |
     service_update_module_invalid |
     service_update_module_state_invalid |
     service_update_modules_load_invalid |
     service_update_modules_unload_invalid |
     service_update_code_paths_add_invalid |
     service_update_code_paths_remove_invalid, any()}.
-type error_reason_nodes_add_configuration() ::
    {node_invalid, any()}.
-type error_reason_nodes_remove_configuration() ::
    {node_invalid |
     node_not_found, any()}.
-type error_reason_nodes_set_configuration() ::
    {node_invalid |
     node_reconnect_start_invalid |
     node_reconnect_start_min |
     node_reconnect_delay_invalid |
     node_reconnect_delay_min |
     node_listen_invalid |
     node_connect_invalid |
     node_timestamp_type_invalid |
     node_discovery_invalid |
     node_discovery_ambiguous |
     node_discovery_multicast_invalid |
     node_discovery_multicast_interface_invalid |
     node_discovery_multicast_address_invalid |
     node_discovery_multicast_port_invalid |
     node_discovery_multicast_ttl_invalid |
     node_discovery_ec2_invalid |
     node_discovery_ec2_access_key_id_invalid |
     node_discovery_ec2_secret_access_key_invalid |
     node_discovery_ec2_host_invalid |
     node_discovery_ec2_tags_selection_null |
     node_discovery_ec2_groups_invalid |
     node_discovery_ec2_tags_invalid, any()}.
-type error_reason_logging_syslog_set_configuration() ::
    {logging_syslog_invalid |
     logging_syslog_identity_invalid |
     logging_syslog_facility_invalid |
     logging_syslog_level_invalid |
     logging_syslog_facility_invalid, any()}.
-type error_reason_logging_formatters_set_configuration() ::
    {logging_formatters_invalid |
     logging_formatter_modules_invalid |
     logging_formatter_level_invalid |
     logging_formatter_output_invalid |
     logging_formatter_output_args_invalid |
     logging_formatter_output_max_r_invalid |
     logging_formatter_output_max_t_invalid |
     logging_formatter_formatter_invalid |
     logging_formatter_formatter_config_invalid, any()}.
-type error_reason_acl_add() ::
    error_reason_acl_add_configuration().
-type error_reason_acl_remove() ::
    error_reason_acl_remove_configuration().
-type error_reason_services_add() ::
    error_reason_services_add_configuration() |
    cloudi_core_i_configurator:error_reason_service_start().
-type error_reason_services_remove() ::
    error_reason_services_remove_configuration() |
    cloudi_core_i_configurator:error_reason_service_stop().
-type error_reason_services_restart() ::
    error_reason_services_restart_configuration() |
    cloudi_core_i_configurator:error_reason_service_restart().
-type error_reason_services_update() ::
    error_reason_services_update_configuration() |
    cloudi_core_i_configurator:error_reason_service_update().
-type error_reason_nodes_add() ::
    error_reason_nodes_add_configuration().
-type error_reason_nodes_remove() ::
    error_reason_nodes_remove_configuration().
-type error_reason_nodes_set() ::
    error_reason_nodes_set_configuration().
-type error_reason_logging_syslog_set() ::
    error_reason_logging_syslog_set_configuration().
-type error_reason_logging_formatters_set() ::
    error_reason_logging_formatters_set_configuration().
-export_type([error_reason_acl_add/0,
              error_reason_acl_remove/0,
              error_reason_services_add/0,
              error_reason_services_remove/0,
              error_reason_services_restart/0,
              error_reason_services_update/0,
              error_reason_nodes_add/0,
              error_reason_nodes_remove/0,
              error_reason_nodes_set/0,
              error_reason_logging_syslog_set/0,
              error_reason_logging_formatters_set/0]).
-type error_reason_new() ::
    error_reason_acl_add_configuration() |
    error_reason_services_add_configuration() |
    error_reason_nodes_set_configuration() |
    error_reason_logging_syslog_set_configuration() |
    error_reason_logging_formatters_set_configuration() |
    {invalid |
     node_invalid |
     logging_invalid |
     logging_redirect_invalid |
     logging_file_invalid |
     logging_level_invalid, any()}.

% cloudi_service_api.hrl records without defaults or types set so
% dialyzer doesn't get confused
-record(internal,
    {
        prefix,
        module,
        args,
        dest_refresh,
        timeout_init,
        timeout_async,
        timeout_sync,
        dest_list_deny,
        dest_list_allow,
        count_process,
        max_r,
        max_t,
        options
    }).
-record(external,
    {
        prefix,
        file_path,
        args,
        env,
        dest_refresh,
        protocol,
        buffer_size,
        timeout_init,
        timeout_async,
        timeout_sync,
        dest_list_deny,
        dest_list_allow,
        count_process,
        count_thread,
        max_r,
        max_t,
        options
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Process the CloudI configuration data.===
%% ====logging:====
%%   `{logging, [{file, "path/to/log/file"}, {level, Level}]}'
%%
%%   The logging level is specified as an atom:
%%
%%   `off, fatal, error, warn, info, debug, trace'
%%
%% ====services:====
%%   `{services, [{internal, ServiceNamePrefix, ErlangModuleName, ModuleInitializationList, DestinationRefreshMethod, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, MaxR, MaxT, ServiceOptions}, {external, ServiceNamePrefix, ExecutableFilePath, ExecutableCommandLineArguments, ExecutableEnvironmentalVariables, DestinationRefreshMethod, Protocol, ProtocolBufferSize, InitializationTimeout, DefaultAsynchronousTimeout, DefaultSynchronousTimeout, DestinationDenyList, DestinationAllowList, ProcessCount, ThreadCount, MaxR, MaxT, ServiceOptions}]}'
%%
%%   Services configuration defines all the necessary information for the
%%   lifetime of running the service.
%%   Every service defines a name prefix which provides scope for the
%%   service (ServiceNamePrefix) and typically uses the forward slash ('/')
%%   character as a path delimiter (though this convention is not required
%%   for service functionality). An internal service is an Erlang application
%%   or module that exists in the code search path and is started with a list of
%%   initialization arguments (ErlangModuleName and ModuleInitializationList).
%%   An external service is an executable that has integrated with the
%%   CloudI API and is provided as the executable file path
%%   (ExecutableFilePath). An external service also specifies the command line
%%   arguments and the environmental variables
%%   (ExecutableCommandLineArguments and ExecutableEnvironmentalVariables)
%%   that are used when executing the service.
%%
%%   Each service configuration then defines the destination refresh method
%%   (DestinationRefreshMethod) which may be set to: lazy_closest,
%%   lazy_furthest, lazy_random, lazy_local, lazy_remote, lazy_newest,
%%   lazy_oldest, immediate_closest, immediate_furthest, immediate_random,
%%   immediate_local, immediate_remote, immediate_newest, immediate_oldest,
%%   or none. A "lazy" destination refresh
%%   method prefix is used by services that send messages to only
%%   long-lived services and will avoid contention for doing service name
%%   lookups (i.e., the most scalable choice).  An "immediate" destination
%%   refresh method prefix is used by services that send messages to
%%   short-lived services.  A "closest" destination refresh method suffix
%%   always prefers to send to a service on the local machine rather than send
%%   to a remote machine, to minimize latency.  A "furthest" destination
%%   refresh method suffix always prefers to send to a service on a remote
%%   machine, for fault-tolerance.  A "random" destination refresh method
%%   suffix always selects a service randomly, to load-balance the requests
%%   among both local and remote service instances,  A "local" destination
%%   refresh method will only send to local service instances, for minimal
%%   latency.  A "remote" destination refresh method will only send to remote
%%   service instances, to always provide a fault-tolerance guarantee.
%%
%%   The InitializationTimeout timeout specifies how long an internal service
%%   can spend in its cloudi_service_init/3 function or how long an external
%%   service may take to instantiate the CloudI API data structure (for all
%%   of the configured threads). The DefaultAsynchronousTimeout and the
%%   DefaultSynchronousTimeout provide timeouts for any service function calls
%%   that do not specify a timeout.  The DestinationDenyList and the
%%   DestinationAllowList both accept an Access Control List (ACL) which
%%   explicitly denies or allows sending service messages to destinations
%%   that match based on the service name prefix.  Both parameters may be
%%   either "undefined" or a list of service name prefixes (the service name
%%   prefixes may also be supplied as aliases defined in the ACL configuration).
%%
%%   The ProcessCount for an internal service determines how many services with
%%   the configuration will run as Erlang processes. The ProcessCount for an
%%   external service determines how many Operating System processes will be
%%   created with the configuration information. The ThreadCount determines
%%   how many external service threads will be expected to create CloudI API
%%   objects (i.e., to become initialized). The MaxR and MaxT are parameters
%%   to manage the fault-tolerance of the service in the same way as an
%%   Erlang OTP Supervisor manages Erlang processes. The MaxR parameters is the
%%   number of restarts.  The MaxT parameter is the amount of time in seconds
%%   the restarts must occur in, for the service to be considered failed.
%%
%% ====Access Control List (ACL):====
%%
%%   `{acl, [{alias1, ["/service/name/prefix1", "/service/name/prefix2", alias2]}]}'
%%
%%   The DestinationDenyList and DestinationAllowList are both lists that
%%   explicitly deny or allow sending messages from a service (respectively).
%%   The ACL configuration provides a simple way to condense service
%%   configuration based on common service name prefixes.  The ACL atoms
%%   provide short aliases for the literal service name prefixes and may be
%%   used to define other ACLs (in a way that is both acyclic and unordered).
%%
%%   The strings used are typically common service name prefixes, but can
%%   also be patterns with "*" where "**" is forbidden, similar to
%%   service subscriptions.
%%
%% ====nodes:====
%%   `{nodes, [cloudi@hostname1, cloudi@hostname2]}'
%%   `{nodes, automatic}'
%%   `{nodes, Options}'
%%
%%   Remote CloudI nodes that are started separately
%%   (CloudI operates as a master-less system).  Instead of providing the
%%   exact node names within a list, you can also provide "automatic"
%%   to let cloudi_x_nodefinder do automatic node discovery.
%%
%% @end
%%-------------------------------------------------------------------------

-spec load(Path :: string() | list(tuple())) ->
    {ok, #config{}} |
    {error,
     file:posix() |
     badarg |
     system_limit |
     terminated |
     {configuration_invalid |
      parse_error, any()} |
     error_reason_new()}.

load([I | _] = Path) when is_integer(I) ->
    case file:consult(Path) of
        {ok, Terms} ->
            case load_verify(Terms) of
                ok ->
                    new(Terms, #config{uuid_generator = uuid_generator()});
                {error, _} = Error ->
                    Error
            end;
        {error, enoent} = Error ->
            error_logger:error_msg("configuration file \"~s\" not found",
                                   [Path]),
            Error;
        {error, eacces} = Error ->
            error_logger:error_msg("configuration file \"~s\" not accessible",
                                   [Path]),
            Error;
        {error, {Line, Module, Term} = Reason}
            when is_integer(Line), is_atom(Module) ->
            error_logger:error_msg("configuration file \"~s\" parse error "
                                   "at line ~w: ~w ~p",
                                   [Path, Line, Module, Term]),
            {error, {parse_error, Reason}};
        {error, Reason} = Error ->
            error_logger:error_msg("configuration file \"~s\" invalid: ~p",
                                   [Path, Reason]),
            Error
    end;
load([T | _] = Terms) when is_tuple(T) ->
    case load_verify(Terms) of
        ok ->
            new(Terms, #config{uuid_generator = uuid_generator()});
        {error, _} = Error ->
            Error
    end;
load(Data) ->
    {error, {configuration_invalid, Data}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Add Access Control List (ACL) aliases (atom -> service name prefixes).===
%% @end
%%-------------------------------------------------------------------------

-spec acl_add(Value :: list({atom(), cloudi_service_api:acl()}),
              Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_acl_add()}.

acl_add([{A, [_ | _]} | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    case acl_lookup_add(Value, ACL) of
        {ok, NewACL} ->
            {ok, Config#config{acl = NewACL}};
        {error, _} = Error ->
            Error
    end;
acl_add(Value, _) ->
    {error, {acl_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove Access Control List (ACL) aliases.===
%% @end
%%-------------------------------------------------------------------------

-spec acl_remove(Value :: list(atom()),
                 Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_acl_remove()}.

acl_remove([A | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    NewACL = lists:foldl(fun(E, D) -> dict:erase(E, D) end, ACL, Value),
    {ok, Config#config{acl = NewACL}};
acl_remove(Value, _) ->
    {error, {acl_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===List all ACL entries.===
%% @end
%%-------------------------------------------------------------------------

-spec acl(#config{}) ->
    list({atom(), list(cloudi_service:service_name_pattern())}).

acl(#config{acl = ACL}) ->
    dict:to_list(ACL).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add services based on the configuration format.===
%% @end
%%-------------------------------------------------------------------------

-spec services_add(Value :: nonempty_list(#internal{} | #external{} |
                                          cloudi_service_api:
                                          service_proplist()),
                   Config :: #config{},
                   Timeout :: cloudi_service_api:timeout_milliseconds() |
                              infinity) ->
    {ok, list(cloudi_service_api:service_id()), #config{}} |
    {error, error_reason_services_add()}.

services_add([T | _] = Value,
             #config{uuid_generator = UUID,
                     services = Services,
                     acl = ACL} = Config, Timeout)
    when is_record(T, internal); is_record(T, external); is_list(T) ->
    case services_validate(Value, UUID) of
        {ok, ValidatedServices, IDs} ->
            case services_acl_update(ValidatedServices, ACL) of
                {ok, NextServices} ->
                    case services_add_service(NextServices, Timeout) of
                        {ok, NewServices} ->
                            {ok, IDs,
                             Config#config{services = Services ++
                                                      NewServices}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
services_add(Value, _, _) ->
    {error, {service_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_remove(Value :: nonempty_list(cloudi_service_api:service_id()),
                      Config :: #config{},
                      Timeout :: cloudi_service_api:timeout_milliseconds() |
                                 infinity) ->
    {ok, #config{}} |
    {error, error_reason_services_remove()}.

services_remove([ID | _] = Value,
                #config{services = Services} = Config, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_remove_uuid(Value, Services, Timeout) of
        {ok, NewServices} ->
            {ok, Config#config{services = NewServices}};
        {error, _} = Error ->
            Error
    end;
services_remove(Value, _, _) ->
    {error, {service_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_restart(Value :: nonempty_list(cloudi_service_api:service_id()),
                       Config :: #config{},
                       Timeout :: cloudi_service_api:timeout_milliseconds() |
                                  infinity) ->
    {ok, #config{}} |
    {error, error_reason_services_restart()}.

services_restart([ID | _] = Value,
                 #config{services = Services} = Config, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_restart_uuid(Value, Services, Timeout) of
        ok ->
            {ok, Config};
        {error, _} = Error ->
            Error
    end;
services_restart(Value, _, _) ->
    {error, {service_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update services after checking their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_update(Plan :: list(),
                      Config :: #config{},
                      Timeout :: cloudi_service_api:timeout_milliseconds() |
                                 infinity) ->
    {ok, ok | {error, any()}, #config{}} |
    {error, error_reason_services_update()}.

services_update([_ | _] = Plan,
                #config{services = Services} = Config, Timeout) ->
    case services_update_plan(Plan, Services, Timeout) of
        {ok, Result, NewServices} ->
            {ok, Result, Config#config{services = NewServices}};
        {error, _} = Error ->
            Error
    end;
services_update(Value, _, _) ->
    {error, {update_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Search services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_search(Value :: list(cloudi_service_api:service_id()),
                      Config :: #config{}) ->
    list({cloudi_service_api:service_id(), #internal{}} |
         {cloudi_service_api:service_id(), #external{}}).

services_search([ID | _] = Value, Config)
    when is_binary(ID), byte_size(ID) == 16 ->
    lists:filter(fun({CheckID, _}) ->
        lists:member(CheckID, Value)
    end, services(Config)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Display the currently running services (including their UUID).===
%% @end
%%-------------------------------------------------------------------------

-spec services(#config{}) ->
    list({cloudi_service_api:service_id(), #internal{}} |
         {cloudi_service_api:service_id(), #external{}}).

services(#config{services = Services}) ->
    [service_format(Service) || Service <- Services].

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the configuration format, as it was provided.===
%% Using the tuple format.  If necessary, the proplist format could be added
%% based on a separate function option parameter.
%% @end
%%-------------------------------------------------------------------------

-spec service_format(#config_service_internal{} |
                     #config_service_external{}) ->
    {cloudi_service_api:service_id(), #internal{}} |
    {cloudi_service_api:service_id(), #external{}}.

service_format(#config_service_internal{prefix = Prefix,
                                        module = Module,
                                        file_path = FilePath,
                                        args = Args,
                                        dest_refresh = DestRefresh,
                                        timeout_init = TimeoutInit,
                                        timeout_async = TimeoutAsync,
                                        timeout_sync = TimeoutSync,
                                        dest_list_deny = DestListDeny,
                                        dest_list_allow = DestListAllow,
                                        count_process = CountProcess,
                                        max_r = MaxR,
                                        max_t = MaxT,
                                        options = Options,
                                        uuid = ID}) ->
    ModuleEntry = if
        FilePath =:= undefined ->
            Module;
        is_list(FilePath) ->
            FilePath
    end,
    {ID,
     #internal{prefix = Prefix,
               module = ModuleEntry,
               args = Args,
               dest_refresh = DestRefresh,
               timeout_init = TimeoutInit,
               timeout_async = TimeoutAsync,
               timeout_sync = TimeoutSync,
               dest_list_deny = DestListDeny,
               dest_list_allow = DestListAllow,
               count_process = CountProcess,
               max_r = MaxR,
               max_t = MaxT,
               options = services_format_options_internal(Options)}};
service_format(#config_service_external{prefix = Prefix,
                                        file_path = FilePath,
                                        args = Args,
                                        env = Env,
                                        dest_refresh = DestRefresh,
                                        protocol = Protocol,
                                        buffer_size = BufferSize,
                                        timeout_init = TimeoutInit,
                                        timeout_async = TimeoutAsync,
                                        timeout_sync = TimeoutSync,
                                        dest_list_deny = DestListDeny,
                                        dest_list_allow = DestListAllow,
                                        count_process = CountProcess,
                                        count_thread = CountThread,
                                        max_r = MaxR,
                                        max_t = MaxT,
                                        options = Options,
                                        uuid = ID}) ->
    {ID,
     #external{prefix = Prefix,
               file_path = FilePath,
               args = Args,
               env = Env,
               dest_refresh = DestRefresh,
               protocol = Protocol,
               buffer_size = BufferSize,
               timeout_init = TimeoutInit,
               timeout_async = TimeoutAsync,
               timeout_sync = TimeoutSync,
               dest_list_deny = DestListDeny,
               dest_list_allow = DestListAllow,
               count_process = CountProcess,
               count_thread = CountThread,
               max_r = MaxR,
               max_t = MaxT,
               options = services_format_options_external(Options)}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the internal service configuration options in the configuration format with defaults ignored.===
%% @end
%%-------------------------------------------------------------------------

-spec services_format_options_internal(Options :: #config_service_options{}) ->
    cloudi_service_api:service_options_internal().

services_format_options_internal(Options) ->
    Defaults = #config_service_options{},
    OptionsList0 = lists:reverse(services_format_options_external(Options)),
    OptionsList1 = if
        Options#config_service_options.aspects_info_before /=
        Defaults#config_service_options.aspects_info_before ->
            [{aspects_info_before,
              Options#config_service_options.aspects_info_before} |
             OptionsList0];
        true ->
            OptionsList0
    end,
    OptionsList2 = if
        Options#config_service_options.aspects_info_after /=
        Defaults#config_service_options.aspects_info_after ->
            [{aspects_info_after,
              Options#config_service_options.aspects_info_after} |
             OptionsList1];
        true ->
            OptionsList1
    end,
    OptionsList3 = if
        Options#config_service_options.application_name /=
        Defaults#config_service_options.application_name ->
            [{application_name,
              Options#config_service_options.application_name} |
             OptionsList2];
        true ->
            OptionsList2
    end,
    OptionsList4 = if
        Options#config_service_options.request_pid_uses /=
        Defaults#config_service_options.request_pid_uses ->
            [{request_pid_uses,
              Options#config_service_options.request_pid_uses} |
             OptionsList3];
        true ->
            OptionsList3
    end,
    OptionsList5 = if
        Options#config_service_options.request_pid_options /= [link] ->
            [{request_pid_options, lists:delete(link,
              Options#config_service_options.request_pid_options)} |
             OptionsList4];
        true ->
            OptionsList4
    end,
    OptionsList6 = if
        Options#config_service_options.info_pid_uses /=
        Defaults#config_service_options.info_pid_uses ->
            [{info_pid_uses,
              Options#config_service_options.info_pid_uses} |
             OptionsList5];
        true ->
            OptionsList5
    end,
    OptionsList7 = if
        Options#config_service_options.info_pid_options /= [link] ->
            [{info_pid_options, lists:delete(link,
              Options#config_service_options.info_pid_options)} |
             OptionsList6];
        true ->
            OptionsList6
    end,
    OptionsList8 = if
        Options#config_service_options.duo_mode /=
        Defaults#config_service_options.duo_mode ->
            [{duo_mode,
              Options#config_service_options.duo_mode} |
             OptionsList7];
        true ->
            OptionsList7
    end,
    OptionsList9 = if
        Options#config_service_options.hibernate /=
        Defaults#config_service_options.hibernate ->
            [{hibernate,
              cloudi_core_i_rate_based_configuration:
              hibernate_format(
                  Options#config_service_options.hibernate)} |
             OptionsList8];
        true ->
            OptionsList8
    end,
    OptionsList10 = if
        Options#config_service_options.reload /=
        Defaults#config_service_options.reload ->
            [{reload,
              Options#config_service_options.reload} |
             OptionsList9];
        true ->
            OptionsList9
    end,
    lists:reverse(OptionsList10).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the external service configuration options in the configuration format with defaults ignored.===
%% @end
%%-------------------------------------------------------------------------

-spec services_format_options_external(Options :: #config_service_options{}) ->
    cloudi_service_api:service_options_external().

services_format_options_external(Options) ->
    Defaults = #config_service_options{},
    OptionsList0 = [],
    OptionsList1 = if
        Options#config_service_options.priority_default /=
        Defaults#config_service_options.priority_default ->
            [{priority_default,
              Options#config_service_options.priority_default} |
             OptionsList0];
        true ->
            OptionsList0
    end,
    OptionsList2 = if
        Options#config_service_options.queue_limit /=
        Defaults#config_service_options.queue_limit ->
            [{queue_limit,
              Options#config_service_options.queue_limit} |
             OptionsList1];
        true ->
            OptionsList1
    end,
    OptionsList3 = if
        Options#config_service_options.queue_size /=
        Defaults#config_service_options.queue_size ->
            [{queue_size,
              (Options#config_service_options.queue_size div 1024)} |
             OptionsList2];
        true ->
            OptionsList2
    end,
    OptionsList4 = if
        Options#config_service_options.rate_request_max /=
        Defaults#config_service_options.rate_request_max ->
            [{rate_request_max,
              cloudi_core_i_rate_based_configuration:
              rate_request_format(
                  Options#config_service_options.rate_request_max)} |
             OptionsList3];
        true ->
            OptionsList3
    end,
    OptionsList5 = if
        Options#config_service_options.dest_refresh_start /=
        Defaults#config_service_options.dest_refresh_start ->
            [{dest_refresh_start,
              Options#config_service_options.dest_refresh_start} |
             OptionsList4];
        true ->
            OptionsList4
    end,
    OptionsList6 = if
        Options#config_service_options.dest_refresh_delay /=
        Defaults#config_service_options.dest_refresh_delay ->
            [{dest_refresh_delay,
              Options#config_service_options.dest_refresh_delay} |
             OptionsList5];
        true ->
            OptionsList5
    end,
    OptionsList7 = if
        Options#config_service_options.request_name_lookup /=
        Defaults#config_service_options.request_name_lookup ->
            [{request_name_lookup,
              Options#config_service_options.request_name_lookup} |
             OptionsList6];
        true ->
            OptionsList6
    end,
    OptionsList8 = if
        Options#config_service_options.request_timeout_adjustment /=
        Defaults#config_service_options.request_timeout_adjustment ->
            [{request_timeout_adjustment,
              Options#config_service_options.request_timeout_adjustment} |
             OptionsList7];
        true ->
            OptionsList7
    end,
    OptionsList9 = if
        Options#config_service_options.request_timeout_immediate_max /=
        Defaults#config_service_options.request_timeout_immediate_max ->
            [{request_timeout_immediate_max,
              Options#config_service_options.request_timeout_immediate_max} |
             OptionsList8];
        true ->
            OptionsList8
    end,
    OptionsList10 = if
        Options#config_service_options.response_timeout_adjustment /=
        Defaults#config_service_options.response_timeout_adjustment ->
            [{response_timeout_adjustment,
              Options#config_service_options.response_timeout_adjustment} |
             OptionsList9];
        true ->
            OptionsList9
    end,
    OptionsList11 = if
        Options#config_service_options.response_timeout_immediate_max /=
        Defaults#config_service_options.response_timeout_immediate_max ->
            [{response_timeout_immediate_max,
              Options#config_service_options.response_timeout_immediate_max} |
             OptionsList10];
        true ->
            OptionsList10
    end,
    OptionsList12 = if
        Options#config_service_options.count_process_dynamic /=
        Defaults#config_service_options.count_process_dynamic ->
            [{count_process_dynamic,
              cloudi_core_i_rate_based_configuration:
              count_process_dynamic_format(
                  Options#config_service_options.count_process_dynamic)} |
             OptionsList11];
        true ->
            OptionsList11
    end,
    OptionsList13 = if
        Options#config_service_options.timeout_terminate /=
        Defaults#config_service_options.timeout_terminate ->
            [{timeout_terminate,
              Options#config_service_options.timeout_terminate} |
             OptionsList12];
        true ->
            OptionsList12
    end,
    OptionsList14 = if
        Options#config_service_options.scope /= ?SCOPE_DEFAULT ->
            [{scope,
              ?SCOPE_FORMAT(Options#config_service_options.scope)} |
             OptionsList13];
        true ->
            OptionsList13
    end,
    OptionsList15 = if
        Options#config_service_options.monkey_latency /=
        Defaults#config_service_options.monkey_latency ->
            [{monkey_latency,
              cloudi_core_i_runtime_testing:
              monkey_latency_format(
                  Options#config_service_options.monkey_latency)} |
             OptionsList14];
        true ->
            OptionsList14
    end,
    OptionsList16 = if
        Options#config_service_options.monkey_chaos /=
        Defaults#config_service_options.monkey_chaos ->
            [{monkey_chaos,
              cloudi_core_i_runtime_testing:
              monkey_chaos_format(
                  Options#config_service_options.monkey_chaos)} |
             OptionsList15];
        true ->
            OptionsList15
    end,
    OptionsList17 = if
        Options#config_service_options.automatic_loading /=
        Defaults#config_service_options.automatic_loading ->
            [{automatic_loading,
              Options#config_service_options.automatic_loading} |
             OptionsList16];
        true ->
            OptionsList16
    end,
    OptionsList18 = if
        Options#config_service_options.aspects_init_after /=
        Defaults#config_service_options.aspects_init_after ->
            [{aspects_init_after,
              Options#config_service_options.aspects_init_after} |
             OptionsList17];
        true ->
            OptionsList17
    end,
    OptionsList19 = if
        Options#config_service_options.aspects_request_before /=
        Defaults#config_service_options.aspects_request_before ->
            [{aspects_request_before,
              Options#config_service_options.aspects_request_before} |
             OptionsList18];
        true ->
            OptionsList18
    end,
    OptionsList20 = if
        Options#config_service_options.aspects_request_after /=
        Defaults#config_service_options.aspects_request_after ->
            [{aspects_request_after,
              Options#config_service_options.aspects_request_after} |
             OptionsList19];
        true ->
            OptionsList19
    end,
    OptionsList21 = if
        Options#config_service_options.aspects_terminate_before /=
        Defaults#config_service_options.aspects_terminate_before ->
            [{aspects_terminate_before,
              Options#config_service_options.aspects_terminate_before} |
             OptionsList20];
        true ->
            OptionsList20
    end,
    OptionsList22 = if
        Options#config_service_options.limit /=
        Defaults#config_service_options.limit ->
            [{limit,
              Options#config_service_options.limit} |
             OptionsList21];
        true ->
            OptionsList21
    end,
    OptionsList23 = if
        Options#config_service_options.owner /=
        Defaults#config_service_options.owner ->
            [{owner,
              Options#config_service_options.owner} |
             OptionsList22];
        true ->
            OptionsList22
    end,
    lists:reverse(OptionsList23).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set CloudI nodes configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_set(Value :: cloudi_service_api:nodes_proplist(),
                Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_nodes_set()}.

nodes_set([_ | _] = Value, #config{} = Config) ->
    case nodes_proplist(Value) of
        {ok, NodesConfig} ->
            {ok, Config#config{nodes = NodesConfig}};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get CloudI nodes configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_get(#config{}) ->
    cloudi_service_api:nodes_proplist().

nodes_get(#config{nodes = #config_nodes{nodes = Nodes,
                                        reconnect_start = ReconnectStart,
                                        reconnect_delay = ReconnectDelay,
                                        listen = Listen,
                                        connect = Connect,
                                        timestamp_type = TimestampType,
                                        discovery = Discovery}}) ->
    Defaults = #config_nodes{},
    NodesList0 = [],
    NodesList1 = if
        Nodes == Defaults#config_nodes.nodes ->
            NodesList0;
        true ->
            [{nodes, Nodes} | NodesList0]
    end,
    NodesList2 = if
        ReconnectStart == Defaults#config_nodes.reconnect_start ->
            NodesList1;
        true ->
            [{reconnect_start, ReconnectStart} | NodesList1]
    end,
    NodesList3 = if
        ReconnectDelay == Defaults#config_nodes.reconnect_delay ->
            NodesList2;
        true ->
            [{reconnect_delay, ReconnectDelay} | NodesList2]
    end,
    NodesList4 = if
        Listen == Defaults#config_nodes.listen ->
            NodesList3;
        true ->
            [{listen, Listen} | NodesList3]
    end,
    NodesList5 = if
        Connect == Defaults#config_nodes.connect ->
            NodesList4;
        true ->
            [{connect, Connect} | NodesList4]
    end,
    NodesList6 = if
        TimestampType == Defaults#config_nodes.timestamp_type ->
            NodesList5;
        true ->
            [{timestamp_type, TimestampType} | NodesList5]
    end,
    undefined = Defaults#config_nodes.discovery,
    NodesList7 = case Discovery of
        undefined ->
            NodesList6;
        #config_nodes_discovery{start_a = [MulticastInterface,
                                           MulticastAddress,
                                           MulticastPort,
                                           MulticastTTL, _],
                                discover_f = multicast_discover} ->
            [{discovery,
              [{multicast,
                [{interface, MulticastInterface},
                 {address, MulticastAddress},
                 {port, MulticastPort},
                 {ttl, MulticastTTL}]}]} | NodesList6];
        #config_nodes_discovery{start_a = [EC2AccessKeyId,
                                           EC2SecretAccessKey,
                                           EC2Host, EC2Groups, EC2Tags],
                                discover_f = ec2_discover} ->
                                
            [{discovery,
              [{ec2,
                [{access_key_id, EC2AccessKeyId},
                 {secret_access_key, EC2SecretAccessKey},
                 {host, EC2Host},
                 {groups, EC2Groups},
                 {tags, EC2Tags}]}]} | NodesList6]
    end,
    lists:reverse(NodesList7).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_add(Value :: list(node()),
                Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_nodes_add()}.

nodes_add([A | _] = Value, #config{nodes = NodesConfig} = Config)
    when is_atom(A) ->
    case nodes_elements_add(lists:delete(node(), Value), NodesConfig) of
        {ok, NewNodesConfig} ->
            {ok, Config#config{nodes = NewNodesConfig}};
        {error, _} = Error ->
            Error
    end;
nodes_add(Value, _) ->
    {error, {node_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_remove(Value :: list(node()),
                   Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_nodes_remove()}.

nodes_remove([A | _] = Value, #config{nodes = NodesConfig} = Config)
    when is_atom(A) ->
    case nodes_elements_remove(Value, NodesConfig) of
        {ok, NewNodesConfig} ->
            {ok, Config#config{nodes = NewNodesConfig}};
        {error, _} = Error ->
            Error
    end;
nodes_remove(Value, _) ->
    {error, {node_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the highest logging level in the list.===
%% @end
%%-------------------------------------------------------------------------

-spec logging_level_highest(list(cloudi_service_api:loglevel() | undefined)) ->
    cloudi_service_api:loglevel() | undefined.

logging_level_highest([_ | _] = Levels) ->
    [Level | _] = lists:dropwhile(fun(Highest) ->
        not lists:member(Highest, Levels)
    end, [trace, debug, info, warn, error, fatal, off, undefined]),
    Level.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set CloudI syslog configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec logging_syslog_set(Value :: cloudi_service_api:
                                  logging_syslog_set_proplist(),
                         Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_logging_syslog_set()}.

logging_syslog_set(Value, #config{logging = OldLogging} = Config)
    when is_list(Value) ->
    case logging_syslog_validate(Value) of
        {ok, SyslogConfig} ->
            {ok,
             Config#config{
                 logging = OldLogging#config_logging{
                     syslog = SyslogConfig}}};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the CloudI log formatters.===
%% @end
%%-------------------------------------------------------------------------

-spec logging_formatters_set(Value :: cloudi_service_api:
                                      logging_formatters_set_proplist(),
                             Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_logging_formatters_set()}.

logging_formatters_set(Value, #config{logging = OldLogging} = Config)
    when is_list(Value) ->
    case logging_formatters_validate(Value) of
        {ok, FormattersConfig} ->
            {ok,
             Config#config{
                 logging = OldLogging#config_logging{
                     formatters = FormattersConfig}}};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the current logging configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec logging(#config{}) ->
    cloudi_service_api:logging_proplist().

logging(#config{logging = #config_logging{file = File,
                                          level = Level,
                                          redirect = Redirect,
                                          syslog = Syslog,
                                          formatters = Formatters}}) ->
    Defaults = #config_logging{},
    LoggingList0 = [],
    LoggingList1 = if
        File == Defaults#config_logging.file ->
            LoggingList0;
        true ->
            [{file, File} | LoggingList0]
    end,
    LoggingList2 = if
        Level =:= Defaults#config_logging.level ->
            LoggingList1;
        true ->
            [{level, Level} | LoggingList1]
    end,
    LoggingList3 = if
        Redirect =:= Defaults#config_logging.redirect ->
            LoggingList2;
        true ->
            [{redirect, Redirect} | LoggingList2]
    end,
    undefined = Defaults#config_logging.syslog,
    LoggingList4 = case Syslog of
        undefined ->
            LoggingList3;
        #config_logging_syslog{identity = SyslogIdentity,
                               facility = SyslogFacility,
                               level = SyslogLevel} ->
            SyslogDefaults = #config_logging_syslog{},
            SyslogList0 = [],
            SyslogList1 = if
                SyslogIdentity ==
                SyslogDefaults#config_logging_syslog.identity ->
                    SyslogList0;
                true ->
                    [{identity, SyslogIdentity} | SyslogList0]
            end,
            SyslogList2 = if
                SyslogFacility =:=
                SyslogDefaults#config_logging_syslog.facility ->
                    SyslogList1;
                true ->
                    [{facility, SyslogFacility} | SyslogList1]
            end,
            SyslogList3 = if
                SyslogLevel =:=
                SyslogDefaults#config_logging_syslog.level ->
                    SyslogList2;
                true ->
                    [{level, SyslogLevel} | SyslogList2]
            end,
            [{syslog, lists:reverse(SyslogList3)} | LoggingList3]
    end,
    undefined = Defaults#config_logging.formatters,
    LoggingList5 = case Formatters of
        undefined ->
            LoggingList4;
        #config_logging_formatters{default = FormattersDefault,
                                   lookup = FormattersLookup} ->
            FormattersList0 = if
                FormattersDefault =:= undefined ->
                    cloudi_x_keys1value:to_list(FormattersLookup);
                true ->
                    [{any, FormattersDefault} |
                     cloudi_x_keys1value:to_list(FormattersLookup)]
            end,
            FormatterDefaults = #config_logging_formatter{},
            FormattersList1 = lists:map(fun
                ({FormatterKeys,
                  #config_logging_formatter{
                      level = FormatterLevel,
                      output = FormatterOutput,
                      output_args = FormatterOutputArgs,
                      output_max_r = FormatterOutputMaxR,
                      output_max_t = FormatterOutputMaxT,
                      formatter = Formatter,
                      formatter_config = FormatterConfig}}) ->
                FormatterValue0 = [],
                FormatterValue1 = if
                    FormatterLevel =:=
                    FormatterDefaults#config_logging_formatter.level ->
                        FormatterValue0;
                    true ->
                        [{level, FormatterLevel} | FormatterValue0]
                end,
                FormatterValue5 = if
                    FormatterOutput =:= undefined ->
                        FormatterValue1;
                    true ->
                        FormatterValue2 = [{output, FormatterOutput} |
                                           FormatterValue1],
                        FormatterValue3 = if
                            FormatterOutputArgs ==
                            FormatterDefaults
                            #config_logging_formatter.output_args ->
                                FormatterValue2;
                            true ->
                                [{output_args, FormatterOutputArgs} |
                                 FormatterValue2]
                        end,
                        FormatterValue4 = if
                            FormatterOutputMaxR ==
                            FormatterDefaults#config_logging_formatter.output_max_r ->
                                FormatterValue3;
                            true ->
                                [{output_max_r, FormatterOutputMaxR} |
                                 FormatterValue3]
                        end,
                        if
                            FormatterOutputMaxT ==
                            FormatterDefaults#config_logging_formatter.output_max_t ->
                                FormatterValue4;
                            true ->
                                [{output_max_t, FormatterOutputMaxT} |
                                 FormatterValue4]
                        end
                end,
                FormatterValue7 = if
                    Formatter =:= undefined ->
                        FormatterValue5;
                    true ->
                        FormatterValue6 = [{formatter, Formatter} |
                                           FormatterValue5],
                        if
                            FormatterConfig ==
                            FormatterDefaults#config_logging_formatter.formatter_config ->
                                FormatterValue6;
                            true ->
                                [{formatter_config, FormatterConfig} |
                                 FormatterValue6]
                        end
                end,
                {FormatterKeys, lists:reverse(FormatterValue7)}
            end, FormattersList0),
            [{formatters, FormattersList1} | LoggingList4]
           
    end,
    lists:reverse(LoggingList5).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

load_verify(Terms) ->
    UnknownTerms = lists:foldl(fun(ConfigSection, L) ->
        lists:keydelete(ConfigSection, 1, L)
    end, Terms, ['services', 'acl', 'nodes', 'logging']),
    if
        UnknownTerms == [] ->
            ok;
        true ->
            {error, {configuration_invalid, UnknownTerms}}
    end.

-spec new(Terms :: list({atom(), any()}),
          Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_new()}.

new([], #config{services = Services, acl = ACL} = Config) ->
    case services_acl_update(Services, ACL) of
        {ok, NewServices} ->
            {ok, Config#config{services = NewServices}};
        {error, _} = Error ->
            Error
    end;
new([{'services', []} | Terms], Config) ->
    new(Terms, Config);
new([{'services', [T | _] = Value} | Terms],
    #config{uuid_generator = UUID} = Config)
    when is_record(T, internal); is_record(T, external); is_list(T) ->
    case services_validate(Value, UUID) of
        {ok, NewServices, _IDs} ->
            new(Terms, Config#config{services = NewServices});
        {error, _} = Error ->
            Error
    end;
new([{'acl', []} | Terms], Config) ->
    new(Terms, Config);
new([{'acl', [{A, [_ | _]} | _] = Value} | Terms], Config)
    when is_atom(A) ->
    case acl_lookup_new(Value) of
        {ok, NewACL} ->
            new(Terms, Config#config{acl = NewACL});
        {error, _} = Error ->
            Error
    end;
new([{'nodes', automatic} | Terms], Config) ->
    {ok, NodesConfig} = nodes_options([], [{discovery, [{multicast, []}]}]),
    new(Terms, Config#config{nodes = NodesConfig});
new([{'nodes', []} | Terms], Config) ->
    new(Terms, Config);
new([{'nodes', [_ | _] = Value} | Terms], Config) ->
    case nodes_proplist(Value) of
        {ok, NodesConfig} ->
            new(Terms, Config#config{nodes = NodesConfig});
        {error, _} = Error ->
            Error
    end;
new([{'logging', []} | Terms], Config) ->
    new(Terms, Config);
new([{'logging', [T | _] = Value} | Terms], Config)
    when is_atom(element(1, T)) ->
    Defaults = [
        {level, (Config#config.logging)#config_logging.level},
        {file, (Config#config.logging)#config_logging.file},
        {redirect, (Config#config.logging)#config_logging.redirect},
        {syslog, (Config#config.logging)#config_logging.syslog},
        {formatters, (Config#config.logging)#config_logging.formatters}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [Level, _, _, _, _ | _]
            when not ((Level =:= fatal) orelse (Level =:= error) orelse
                      (Level =:= warn) orelse (Level =:= info) orelse
                      (Level =:= debug) orelse (Level =:= trace) orelse
                      (Level =:= off) orelse (Level =:= undefined)) ->
            {error, {logging_level_invalid, Level}};
        [_, File, _, _, _ | _]
            when not ((is_list(File) andalso
                       (length(File) > 0) andalso
                       is_integer(hd(File))) orelse
                      (File =:= undefined))->
            {error, {logging_file_invalid, File}};
        [_, _, Redirect, _, _ | _]
            when not is_atom(Redirect) ->
            {error, {logging_redirect_invalid, Redirect}};
        [_, _, _, Syslog, _ | _]
            when not ((Syslog =:= undefined) orelse
                      is_list(Syslog)) ->
            {error, {logging_syslog_invalid, Syslog}};
        [Level, File, Redirect, Syslog, Formatters] ->
            NewFile = if
                Level =:= undefined ->
                    undefined;
                true ->
                    File
            end,
            NewLevel = if
                File =:= undefined ->
                    undefined;
                true ->
                    Level
            end,
            case logging_syslog_validate(Syslog) of
                {ok, SyslogConfig} ->
                    case logging_formatters_validate(Formatters) of
                        {ok, FormattersConfig} ->
                            NewConfig = Config#config{
                                            logging = #config_logging{
                                                level = NewLevel,
                                                file = NewFile,
                                                redirect = Redirect,
                                                syslog = SyslogConfig,
                                                formatters = FormattersConfig}},
                            if
                                Redirect =:= undefined ->
                                    new(Terms, NewConfig);
                                true ->
                                    case node_validate(Redirect) of
                                        ok ->
                                            new(Terms, NewConfig);
                                        {error, _} = Error ->
                                            Error
                                    end
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _ | Extra] ->
            {error, {logging_invalid, Extra}}
    end;
new([Term | _], _) ->
    {error, {invalid, Term}}.

uuid_generator() ->
    {ok, MacAddress} = application:get_env(cloudi_core, mac_address),
    cloudi_x_uuid:new(self(), [{timestamp_type, erlang},
                               {mac_address, MacAddress}]).

services_add_service(NextServices, Timeout) ->
    services_add_service(NextServices, [], Timeout).

services_add_service([], Added, _) ->
    {ok, lists:reverse(Added)};
services_add_service([Service | Services], Added, Timeout) ->
    case cloudi_core_i_configurator:service_start(Service, Timeout) of
        {ok, NewService} ->
            services_add_service(Services, [NewService | Added], Timeout);
        {error, _} = Error ->
            Error
    end.

services_acl_update([], _) ->
    {ok, []};
services_acl_update([_ | _] = Services, Lookup) ->
    services_acl_update(Services, [], Lookup).

-define(CLOUDI_CORE_SUPPORT_INTERNAL_ACL,
services_acl_update([#config_service_internal{
                        dest_list_deny = Deny,
                        dest_list_allow = Allow} = Service | L],
                    Output, Lookup) ->
    case services_acl_update_list(Deny, [], Lookup) of
        {ok, NewDeny} ->
            case services_acl_update_list(Allow, [], Lookup) of
                {ok, NewAllow} ->
                    services_acl_update(L,
                        [Service#config_service_internal{
                            dest_list_deny = NewDeny,
                            dest_list_allow = NewAllow} | Output], Lookup);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    ).
-ifdef(CLOUDI_CORE_STANDALONE).
-define(CLOUDI_CORE_SUPPORT_EXTERNAL_ACL,
    end).
-else.
-define(CLOUDI_CORE_SUPPORT_EXTERNAL_ACL,
    end;
services_acl_update([#config_service_external{
                        dest_list_deny = Deny,
                        dest_list_allow = Allow} = Service | L],
                    Output, Lookup) ->
    case services_acl_update_list(Deny, [], Lookup) of
        {ok, NewDeny} ->
            case services_acl_update_list(Allow, [], Lookup) of
                {ok, NewAllow} ->
                    services_acl_update(L,
                        [Service#config_service_external{
                            dest_list_deny = NewDeny,
                            dest_list_allow = NewAllow} | Output], Lookup);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end).
-endif.
services_acl_update([], Output, _) ->
    {ok, lists:reverse(Output)};
?CLOUDI_CORE_SUPPORT_INTERNAL_ACL
?CLOUDI_CORE_SUPPORT_EXTERNAL_ACL
    .

services_acl_update_list(undefined, _, _) ->
    {ok, undefined};
services_acl_update_list([], Output, _) ->
    {ok, lists:reverse(Output)};
services_acl_update_list([E | L], Output, Lookup)
    when is_atom(E) ->
    case dict:find(E, Lookup) of
        {ok, Value} ->
            services_acl_update_list(L, Value ++ Output, Lookup);
        error ->
            {error, {acl_not_found, E}}
    end;
services_acl_update_list([E | L], Output, Lookup)
    when is_list(E), (length(E) > 0), is_integer(hd(E)) ->
    try cloudi_x_trie:is_pattern(E) of
        true ->
            services_acl_update_list(L, [E | Output], Lookup);
        false ->
            services_acl_update_list(L, [E ++ "*" | Output], Lookup)
    catch
        exit:badarg ->
            {error, {acl_invalid, E}}
    end.

-spec services_validate(Services :: list(#internal{} | #external{} |
                                         cloudi_service_api:service_proplist()),
                        UUID :: cloudi_x_uuid:state()) ->
    {ok,
     list(#config_service_internal{} | #config_service_external{}),
     list(cloudi_service_api:service_id())} |
    {error,
     {service_internal_invalid |
      service_internal_prefix_invalid |
      service_internal_module_invalid |
      service_internal_args_invalid |
      service_internal_dest_refresh_invalid |
      service_internal_timeout_init_invalid |
      service_internal_timeout_async_invalid |
      service_internal_timeout_sync_invalid |
      service_internal_dest_list_deny_invalid |
      service_internal_dest_list_allow_invalid |
      service_internal_count_process_invalid |
      service_internal_max_r_invalid |
      service_internal_max_t_invalid |
      service_internal_max_t_increase |
      service_internal_options_invalid |
      service_external_invalid |
      service_external_prefix_invalid |
      service_external_file_path_invalid |
      service_external_args_invalid |
      service_external_env_invalid |
      service_external_dest_refresh_invalid |
      service_external_protocol_invalid |
      service_external_buffer_size_invalid |
      service_external_timeout_init_invalid |
      service_external_timeout_async_invalid |
      service_external_timeout_sync_invalid |
      service_external_dest_list_deny_invalid |
      service_external_dest_list_allow_invalid |
      service_external_count_process_invalid |
      service_external_count_thread_invalid |
      service_external_max_r_invalid |
      service_external_max_t_invalid |
      service_external_max_t_increase |
      service_external_options_invalid |
      service_options_priority_default_invalid |
      service_options_queue_limit_invalid |
      service_options_queue_size_invalid |
      service_options_rate_request_max_invalid |
      service_options_dest_refresh_start_invalid |
      service_options_dest_refresh_delay_invalid |
      service_options_request_name_lookup_invalid |
      service_options_request_timeout_adjustment_invalid |
      service_options_request_timeout_immediate_max_invalid |
      service_options_response_timeout_adjustment_invalid |
      service_options_response_timeout_immediate_max_invalid |
      service_options_count_process_dynamic_invalid |
      service_options_timeout_terminate_invalid |
      service_options_timeout_terminate_decrease |
      service_options_scope_invalid |
      service_options_monkey_latency_invalid |
      service_options_monkey_chaos_invalid |
      service_options_automatic_loading_invalid |
      service_options_aspects_init_invalid |
      service_options_aspects_request_invalid |
      service_options_aspects_info_invalid |
      service_options_aspects_terminate_invalid |
      service_options_limit_invalid |
      service_options_owner_invalid |
      service_options_application_name_invalid |
      service_options_request_pid_uses_invalid |
      service_options_request_pid_options_invalid |
      service_options_info_pid_uses_invalid |
      service_options_info_pid_options_invalid |
      service_options_duo_mode_invalid |
      service_options_hibernate_invalid |
      service_options_reload_invalid |
      service_options_invalid, any()}}.

services_validate([_ | _] = Services, UUID) ->
    services_validate(Services, [], [], UUID).

-define(CLOUDI_CORE_SUPPORT_INTERNAL,
services_validate([#internal{prefix = Prefix} | _], _, _, _)
    when not (is_list(Prefix) andalso
              (length(Prefix) > 0) andalso
              is_integer(hd(Prefix))) ->
    {error, {service_internal_prefix_invalid, Prefix}};
services_validate([#internal{module = Module} | _], _, _, _)
    when not (is_atom(Module) or
              (is_list(Module) andalso
               (length(Module) > 0) andalso
               is_integer(hd(Module)))) ->
    {error, {service_internal_module_invalid, Module}};
services_validate([#internal{args = Args} | _], _, _, _)
    when not is_list(Args) ->
    {error, {service_internal_args_invalid, Args}};
services_validate([#internal{dest_refresh = DestRefresh} | _], _, _, _)
    when not (is_atom(DestRefresh) andalso
              ((DestRefresh =:= immediate_closest) orelse
               (DestRefresh =:= lazy_closest) orelse
               (DestRefresh =:= immediate_furthest) orelse
               (DestRefresh =:= lazy_furthest) orelse
               (DestRefresh =:= immediate_random) orelse
               (DestRefresh =:= lazy_random) orelse
               (DestRefresh =:= immediate_local) orelse
               (DestRefresh =:= lazy_local) orelse
               (DestRefresh =:= immediate_remote) orelse
               (DestRefresh =:= lazy_remote) orelse
               (DestRefresh =:= immediate_newest) orelse
               (DestRefresh =:= lazy_newest) orelse
               (DestRefresh =:= immediate_oldest) orelse
               (DestRefresh =:= lazy_oldest) orelse
               (DestRefresh =:= none))) ->
    {error, {service_internal_dest_refresh_invalid, DestRefresh}};
services_validate([#internal{timeout_init = TimeoutInit} | _], _, _, _)
    when not (is_integer(TimeoutInit) andalso
              (TimeoutInit > ?TIMEOUT_DELTA) andalso
              (TimeoutInit =< ?TIMEOUT_MAX)) ->
    {error, {service_internal_timeout_init_invalid, TimeoutInit}};
services_validate([#internal{timeout_async = TimeoutAsync} | _], _, _, _)
    when not (is_integer(TimeoutAsync) andalso
              (TimeoutAsync > ?TIMEOUT_DELTA) andalso
              (TimeoutAsync =< ?TIMEOUT_MAX)) ->
    {error, {service_internal_timeout_async_invalid, TimeoutAsync}};
services_validate([#internal{timeout_sync = TimeoutSync} | _], _, _, _)
    when not (is_integer(TimeoutSync) andalso
              (TimeoutSync > ?TIMEOUT_DELTA) andalso
              (TimeoutSync =< ?TIMEOUT_MAX)) ->
    {error, {service_internal_timeout_sync_invalid, TimeoutSync}};
services_validate([#internal{dest_refresh = DestRefresh,
                             dest_list_deny = DestListDeny} | _], _, _, _)
    when not (is_list(DestListDeny) orelse
              (DestListDeny =:= undefined)) orelse
         ((DestRefresh =:= none) andalso is_list(DestListDeny)) ->
    {error, {service_internal_dest_list_deny_invalid, DestListDeny}};
services_validate([#internal{dest_refresh = DestRefresh,
                             dest_list_allow = DestListAllow} | _], _, _, _)
    when not (is_list(DestListAllow) orelse
              (DestListAllow =:= undefined)) orelse
         ((DestRefresh =:= none) andalso is_list(DestListAllow)) ->
    {error, {service_internal_dest_list_allow_invalid, DestListAllow}};
services_validate([#internal{count_process = CountProcess} | _], _, _, _)
    when not (is_number(CountProcess) andalso CountProcess > 0) ->
    {error, {service_internal_count_process_invalid, CountProcess}};
services_validate([#internal{max_r = MaxR} | _], _, _, _)
    when not (is_integer(MaxR) andalso MaxR >= 0) ->
    {error, {service_internal_max_r_invalid, MaxR}};
services_validate([#internal{max_t = MaxT} | _], _, _, _)
    when not (is_integer(MaxT) andalso MaxT >= 0) ->
    {error, {service_internal_max_t_invalid, MaxT}};
services_validate([#internal{max_r = MaxR,
                             max_t = MaxT} | _], _, _, _)
    when (MaxT > 0) andalso
         (((MaxR == 0) andalso
           (?TIMEOUT_TERMINATE_CALC0(MaxT) < ?TIMEOUT_TERMINATE_MIN)) orelse
          ((MaxR > 0) andalso
           (?TIMEOUT_TERMINATE_CALC1(MaxR, MaxT) < ?TIMEOUT_TERMINATE_MIN))) ->
    {error, {service_internal_max_t_increase, MaxT}};
services_validate([#internal{options = Options} | _], _, _, _)
    when not is_list(Options) ->
    {error, {service_internal_options_invalid, Options}};
services_validate([#internal{
                       prefix = Prefix,
                       module = Module,
                       args = Args,
                       dest_refresh = DestRefresh,
                       timeout_init = TimeoutInit,
                       timeout_async = TimeoutAsync,
                       timeout_sync = TimeoutSync,
                       dest_list_deny = DestListDeny,
                       dest_list_allow = DestListAllow,
                       count_process = CountProcess,
                       max_r = MaxR,
                       max_t = MaxT,
                       options = Options} | L],
                  Output, IDs, UUID) ->
    FilePath = if
        is_atom(Module) ->
            undefined;
        is_list(Module) ->
            Module
    end,
    case service_name_valid(Prefix, service_internal_prefix_invalid) of
        ok ->
            case services_validate_options_internal(Options, CountProcess,
                                                    MaxR, MaxT) of
                {ok, TimeoutTerm, NewOptions} ->
                    {ID, NewUUID} = cloudi_x_uuid:get_v1(UUID),
                    services_validate(L,
                                      [#config_service_internal{
                                           prefix = Prefix,
                                           module = Module,
                                           file_path = FilePath,
                                           args = Args,
                                           dest_refresh = DestRefresh,
                                           timeout_init = TimeoutInit,
                                           timeout_async = TimeoutAsync,
                                           timeout_sync = TimeoutSync,
                                           timeout_term = TimeoutTerm,
                                           dest_list_deny = DestListDeny,
                                           dest_list_allow = DestListAllow,
                                           count_process = CountProcess,
                                           max_r = MaxR,
                                           max_t = MaxT,
                                           options = NewOptions,
                                           uuid = ID} | Output],
                                      [ID | IDs],
                                      NewUUID);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end).
-ifdef(CLOUDI_CORE_STANDALONE).
-compile({nowarn_unused_function,
          [{services_validate_options_external, 4},
           {services_validate_options_external_checks, 10},
           {services_validate_option_aspects_init_after_external, 2},
           {services_validate_option_aspects_request_before_external, 2},
           {services_validate_option_aspects_request_after_external, 2},
           {services_validate_option_aspects_external, 5}]}).
-define(CLOUDI_CORE_SUPPORT_EXTERNAL,
    ;).
-else.
-define(CLOUDI_CORE_SUPPORT_EXTERNAL,
    ;
services_validate([#external{prefix = Prefix} | _], _, _, _)
    when not (is_list(Prefix) andalso
              (length(Prefix) > 0) andalso
              is_integer(hd(Prefix))) ->
    {error, {service_external_prefix_invalid, Prefix}};
services_validate([#external{file_path = FilePath} | _], _, _, _)
    when not (is_list(FilePath) andalso
              (length(FilePath) > 0) andalso
              is_integer(hd(FilePath))) ->
    {error, {service_external_file_path_invalid, FilePath}};
services_validate([#external{args = Args} | _], _, _, _)
    when not (is_list(Args) andalso
              ((Args == "") orelse
               is_integer(hd(Args)))) ->
    {error, {service_external_args_invalid, Args}};
services_validate([#external{env = Env} | _], _, _, _)
    when not is_list(Env) ->
    {error, {service_external_env_invalid, Env}};
services_validate([#external{dest_refresh = DestRefresh} | _], _, _, _)
    when not (is_atom(DestRefresh) andalso
              ((DestRefresh =:= immediate_closest) orelse
               (DestRefresh =:= lazy_closest) orelse
               (DestRefresh =:= immediate_furthest) orelse
               (DestRefresh =:= lazy_furthest) orelse
               (DestRefresh =:= immediate_random) orelse
               (DestRefresh =:= lazy_random) orelse
               (DestRefresh =:= immediate_local) orelse
               (DestRefresh =:= lazy_local) orelse
               (DestRefresh =:= immediate_remote) orelse
               (DestRefresh =:= lazy_remote) orelse
               (DestRefresh =:= immediate_newest) orelse
               (DestRefresh =:= lazy_newest) orelse
               (DestRefresh =:= immediate_oldest) orelse
               (DestRefresh =:= lazy_oldest) orelse
               (DestRefresh =:= none))) ->
    {error, {service_external_dest_refresh_invalid, DestRefresh}};
services_validate([#external{protocol = Protocol} | _], _, _, _)
    when not ((Protocol =:= default) orelse
              (Protocol =:= tcp) orelse
              (Protocol =:= udp) orelse
              (Protocol =:= local)) ->
    {error, {service_external_protocol_invalid, Protocol}};
services_validate([#external{buffer_size = BufferSize} | _], _, _, _)
    when not ((BufferSize =:= default) orelse
              (is_integer(BufferSize) andalso (BufferSize >= 1024))) ->
    {error, {service_external_buffer_size_invalid, BufferSize}};
services_validate([#external{timeout_init = TimeoutInit} | _], _, _, _)
    when not (is_integer(TimeoutInit) andalso
              (TimeoutInit > ?TIMEOUT_DELTA) andalso
              (TimeoutInit =< ?TIMEOUT_MAX)) ->
    {error, {service_external_timeout_init_invalid, TimeoutInit}};
services_validate([#external{timeout_async = TimeoutAsync} | _], _, _, _)
    when not (is_integer(TimeoutAsync) andalso
              (TimeoutAsync > ?TIMEOUT_DELTA) andalso
              (TimeoutAsync =< ?TIMEOUT_MAX)) ->
    {error, {service_external_timeout_async_invalid, TimeoutAsync}};
services_validate([#external{timeout_sync = TimeoutSync} | _], _, _, _)
    when not (is_integer(TimeoutSync) andalso
              (TimeoutSync > ?TIMEOUT_DELTA) andalso
              (TimeoutSync =< ?TIMEOUT_MAX)) ->
    {error, {service_external_timeout_sync_invalid, TimeoutSync}};
services_validate([#external{dest_refresh = DestRefresh,
                             dest_list_deny = DestListDeny} | _], _, _, _)
    when not (is_list(DestListDeny) orelse
              (DestListDeny =:= undefined)) orelse
         ((DestRefresh =:= none) andalso is_list(DestListDeny)) ->
    {error, {service_external_dest_list_deny_invalid, DestListDeny}};
services_validate([#external{dest_refresh = DestRefresh,
                             dest_list_allow = DestListAllow} | _], _, _, _)
    when not (is_list(DestListAllow) orelse
              (DestListAllow =:= undefined)) orelse
         ((DestRefresh =:= none) andalso is_list(DestListAllow)) ->
    {error, {service_external_dest_list_allow_invalid, DestListAllow}};
services_validate([#external{count_process = CountProcess} | _], _, _, _)
    when not (is_number(CountProcess) andalso CountProcess > 0) ->
    {error, {service_external_count_process_invalid, CountProcess}};
services_validate([#external{count_thread = CountThread} | _], _, _, _)
    when not (is_number(CountThread) andalso CountThread > 0) ->
    {error, {service_external_count_thread_invalid, CountThread}};
services_validate([#external{max_r = MaxR} | _], _, _, _)
    when not (is_integer(MaxR) andalso MaxR >= 0) ->
    {error, {service_external_max_r_invalid, MaxR}};
services_validate([#external{max_t = MaxT} | _], _, _, _)
    when not (is_integer(MaxT) andalso MaxT >= 0) ->
    {error, {service_external_max_t_invalid, MaxT}};
services_validate([#external{max_r = MaxR,
                             max_t = MaxT} | _], _, _, _)
    when (MaxT > 0) andalso
         (((MaxR == 0) andalso
           (?TIMEOUT_TERMINATE_CALC0(MaxT) < ?TIMEOUT_TERMINATE_MIN)) orelse
          ((MaxR > 0) andalso
           (?TIMEOUT_TERMINATE_CALC1(MaxR, MaxT) < ?TIMEOUT_TERMINATE_MIN))) ->
    {error, {service_external_max_t_increase, MaxT}};
services_validate([#external{options = Options} | _], _, _, _)
    when not is_list(Options) ->
    {error, {service_external_options_invalid, Options}};
services_validate([#external{
                       prefix = Prefix,
                       file_path = FilePath,
                       args = Args,
                       env = Env,
                       dest_refresh = DestRefresh,
                       protocol = Protocol,
                       buffer_size = BufferSize,
                       timeout_init = TimeoutInit,
                       timeout_async = TimeoutAsync,
                       timeout_sync = TimeoutSync,
                       dest_list_deny = DestListDeny,
                       dest_list_allow = DestListAllow,
                       count_process = CountProcess,
                       count_thread = CountThread,
                       max_r = MaxR,
                       max_t = MaxT,
                       options = Options} | L],
                  Output, IDs, UUID) ->
    NewProtocol = if
        Protocol =:= default ->
            local;
        true ->
            Protocol
    end,
    NewBufferSize = if
        BufferSize =:= default ->
            if
                NewProtocol =:= tcp ->
                    16384; % Linux localhost (inet) MTU
                NewProtocol =:= udp ->
                    16384; % Linux localhost (inet) MTU
                NewProtocol =:= local ->
                    16384  % Linux localhost (inet) MTU for testing/comparison
            end;
        true ->
            BufferSize
    end,
    case service_name_valid(Prefix, service_external_prefix_invalid) of
        ok ->
            case services_validate_options_external(Options, CountProcess,
                                                    MaxR, MaxT) of
                {ok, TimeoutTerm, NewOptions} ->
                    {ID, NewUUID} = cloudi_x_uuid:get_v1(UUID),
                    services_validate(L,
                                      [#config_service_external{
                                           prefix = Prefix,
                                           file_path = FilePath,
                                           args = Args,
                                           env = Env,
                                           dest_refresh = DestRefresh,
                                           protocol = NewProtocol,
                                           buffer_size = NewBufferSize,
                                           timeout_init = TimeoutInit,
                                           timeout_async = TimeoutAsync,
                                           timeout_sync = TimeoutSync,
                                           timeout_term = TimeoutTerm,
                                           dest_list_deny = DestListDeny,
                                           dest_list_allow = DestListAllow,
                                           count_process = CountProcess,
                                           count_thread = CountThread,
                                           max_r = MaxR,
                                           max_t = MaxT,
                                           options = NewOptions,
                                           uuid = ID} | Output],
                                      [ID | IDs],
                                      NewUUID);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;).
-endif.

services_validate([], Output, IDs, _) ->
    {ok, lists:reverse(Output), lists:reverse(IDs)};
services_validate([[_| _] = ServicePropList | L], Output, IDs, UUID) ->
    Defaults = [
        {type,                 undefined},
        {prefix,               ?DEFAULT_SERVICE_PREFIX},
        {module,               undefined},
        {file_path,            undefined},
        {args,                 []},
        {env,                  []},
        {dest_refresh,         ?DEFAULT_DEST_REFRESH},
        {protocol,             default},
        {buffer_size,          default},
        {timeout_init,         ?DEFAULT_TIMEOUT_INIT},
        {timeout_async,        ?DEFAULT_TIMEOUT_ASYNC},
        {timeout_sync,         ?DEFAULT_TIMEOUT_SYNC},
        {dest_list_deny,       undefined},
        {dest_list_allow,      undefined},
        {count_process,        1},
        {count_thread,         1},
        {max_r,                ?DEFAULT_MAX_R},
        {max_t,                ?DEFAULT_MAX_T},
        {options,              []}],
    [Type, Prefix, Module,
     FilePath, Args, Env, DestRefresh, Protocol, BufferSize,
     TimeoutInit, TimeoutAsync, TimeoutSync, DestListDeny,
     DestListAllow, CountProcess, CountThread,
     MaxR, MaxT, ConfigurationOptions | Extra] =
        cloudi_proplists:take_values(Defaults, ServicePropList),
    ServiceType = if
        Type =:= undefined ->
            if
                Module /= undefined, FilePath =:= undefined ->
                    internal;
                FilePath /= undefined, Module =:= undefined ->
                    external;
                true ->
                    undefined
            end;
        Type =:= internal, Module /= undefined, FilePath =:= undefined ->
            internal;
        Type =:= external, FilePath /= undefined, Module =:= undefined ->
            external;
        true ->
            undefined
    end,
    if
        ServiceType =:= undefined ->
            {error, {service_invalid, ServicePropList}};
        Extra /= [] ->
            if
                ServiceType =:= internal ->
                    {error, {service_internal_invalid, Extra}};
                ServiceType =:= external ->
                    {error, {service_external_invalid, Extra}}
            end;
        ServiceType =:= internal ->
            Service = #internal{prefix = Prefix,
                                module = Module,
                                args = Args,
                                dest_refresh = DestRefresh,
                                timeout_init = TimeoutInit,
                                timeout_async = TimeoutAsync,
                                timeout_sync = TimeoutSync,
                                dest_list_deny = DestListDeny,
                                dest_list_allow = DestListAllow,
                                count_process = CountProcess,
                                max_r = MaxR,
                                max_t = MaxT,
                                options = ConfigurationOptions},
            services_validate([Service | L], Output, IDs, UUID);
        ServiceType =:= external ->
            Service = #external{prefix = Prefix,
                                file_path = FilePath,
                                args = Args,
                                env = Env,
                                dest_refresh = DestRefresh,
                                protocol = Protocol,
                                buffer_size = BufferSize,
                                timeout_init = TimeoutInit,
                                timeout_async = TimeoutAsync,
                                timeout_sync = TimeoutSync,
                                dest_list_deny = DestListDeny,
                                dest_list_allow = DestListAllow,
                                count_process = CountProcess,
                                count_thread = CountThread,
                                max_r = MaxR,
                                max_t = MaxT,
                                options = ConfigurationOptions},
            services_validate([Service | L], Output, IDs, UUID)
    end;
?CLOUDI_CORE_SUPPORT_INTERNAL
?CLOUDI_CORE_SUPPORT_EXTERNAL
services_validate([Service | _], _, _, _) ->
    {error, {service_invalid, Service}}.

timeout_terminate(TimeoutTerminate, _, 0) ->
    Default = ?TIMEOUT_TERMINATE_DEFAULT,
    if
        TimeoutTerminate =:= undefined ->
            {ok, Default};
        is_integer(TimeoutTerminate) ->
            {ok, TimeoutTerminate}
    end;
timeout_terminate(TimeoutTerminate, 0, MaxT) ->
    Default = erlang:min(?TIMEOUT_TERMINATE_CALC0(MaxT),
                         ?TIMEOUT_TERMINATE_MAX),
    if
        TimeoutTerminate =:= undefined ->
            {ok, Default};
        is_integer(TimeoutTerminate) ->
            {ok, TimeoutTerminate}
    end;
timeout_terminate(TimeoutTerminate, MaxR, MaxT) ->
    Default = erlang:min(?TIMEOUT_TERMINATE_CALC1(MaxR, MaxT),
                         ?TIMEOUT_TERMINATE_MAX),
    if
        TimeoutTerminate =:= undefined ->
            {ok, Default};
        is_integer(TimeoutTerminate) ->
            if
                TimeoutTerminate =< Default ->
                    {ok, TimeoutTerminate};
                true ->
                    {error,
                     {service_options_timeout_terminate_decrease,
                      TimeoutTerminate}}
            end
    end.

-spec services_validate_options_internal(OptionsList ::
                                             cloudi_service_api:
                                             service_options_internal(),
                                         CountProcess :: pos_integer(),
                                         MaxR :: non_neg_integer(),
                                         MaxT ::
                                             cloudi_service_api:seconds()) ->
    {ok, #config_service_options{}} |
    {error,
     {service_options_priority_default_invalid |
      service_options_queue_limit_invalid |
      service_options_queue_size_invalid |
      service_options_rate_request_max_invalid |
      service_options_dest_refresh_start_invalid |
      service_options_dest_refresh_delay_invalid |
      service_options_request_name_lookup_invalid |
      service_options_request_timeout_adjustment_invalid |
      service_options_request_timeout_immediate_max_invalid |
      service_options_response_timeout_adjustment_invalid |
      service_options_response_timeout_immediate_max_invalid |
      service_options_count_process_dynamic_invalid |
      service_options_timeout_terminate_invalid |
      service_options_timeout_terminate_decrease |
      service_options_scope_invalid |
      service_options_monkey_latency_invalid |
      service_options_monkey_chaos_invalid |
      service_options_automatic_loading_invalid |
      service_options_aspects_init_invalid |
      service_options_aspects_request_invalid |
      service_options_aspects_info_invalid |
      service_options_aspects_terminate_invalid |
      service_options_application_name_invalid |
      service_options_request_pid_uses_invalid |
      service_options_request_pid_options_invalid |
      service_options_info_pid_uses_invalid |
      service_options_info_pid_options_invalid |
      service_options_duo_mode_invalid |
      service_options_hibernate_invalid |
      service_options_reload_invalid |
      service_options_invalid, any()}}.

services_validate_options_internal(OptionsList, CountProcess, MaxR, MaxT) ->
    Options = #config_service_options{},
    Defaults = [
        {priority_default,
         Options#config_service_options.priority_default},
        {queue_limit,
         Options#config_service_options.queue_limit},
        {queue_size,
         Options#config_service_options.queue_size},
        {rate_request_max,
         Options#config_service_options.rate_request_max},
        {dest_refresh_start,
         Options#config_service_options.dest_refresh_start},
        {dest_refresh_delay,
         Options#config_service_options.dest_refresh_delay},
        {request_name_lookup,
         Options#config_service_options.request_name_lookup},
        {request_timeout_adjustment,
         Options#config_service_options.request_timeout_adjustment},
        {request_timeout_immediate_max,
         Options#config_service_options.request_timeout_immediate_max},
        {response_timeout_adjustment,
         Options#config_service_options.response_timeout_adjustment},
        {response_timeout_immediate_max,
         Options#config_service_options.response_timeout_immediate_max},
        {count_process_dynamic,
         Options#config_service_options.count_process_dynamic},
        {timeout_terminate,
         Options#config_service_options.timeout_terminate},
        {scope,
         Options#config_service_options.scope},
        {monkey_latency,
         Options#config_service_options.monkey_latency},
        {monkey_chaos,
         Options#config_service_options.monkey_chaos},
        {automatic_loading,
         Options#config_service_options.automatic_loading},
        {aspects_init_after,
         Options#config_service_options.aspects_init_after},
        {aspects_request_before,
         Options#config_service_options.aspects_request_before},
        {aspects_request_after,
         Options#config_service_options.aspects_request_after},
        {aspects_info_before,
         Options#config_service_options.aspects_info_before},
        {aspects_info_after,
         Options#config_service_options.aspects_info_after},
        {aspects_terminate_before,
         Options#config_service_options.aspects_terminate_before},
        {application_name,
         Options#config_service_options.application_name},
        {request_pid_uses,
         Options#config_service_options.request_pid_uses},
        {request_pid_options,
         Options#config_service_options.request_pid_options},
        {info_pid_uses,
         Options#config_service_options.info_pid_uses},
        {info_pid_options,
         Options#config_service_options.info_pid_options},
        {duo_mode,
         Options#config_service_options.duo_mode},
        {hibernate,
         Options#config_service_options.hibernate},
        {reload,
         Options#config_service_options.reload}],
    case cloudi_proplists:take_values(Defaults, OptionsList) of
        [PriorityDefault, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((PriorityDefault >= ?PRIORITY_HIGH) andalso
                  (PriorityDefault =< ?PRIORITY_LOW)) ->
            {error, {service_options_priority_default_invalid,
                     PriorityDefault}};
        [_, QueueLimit, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((QueueLimit =:= undefined) orelse
                  (is_integer(QueueLimit) andalso
                   (QueueLimit >= 0))) ->
            {error, {service_options_queue_limit_invalid,
                     QueueLimit}};
        [_, _, QueueSize, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((QueueSize =:= undefined) orelse
                  (is_integer(QueueSize) andalso
                   (QueueSize >= 1))) ->
            {error, {service_options_queue_size_invalid,
                     QueueSize}};
        [_, _, _, RateRequestMax, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((RateRequestMax =:= undefined) orelse
                  is_number(RateRequestMax) orelse
                  is_list(RateRequestMax)) ->
            {error, {service_options_rate_request_max_invalid,
                     RateRequestMax}};
        [_, _, _, _, DestRefreshStart, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshStart) andalso
                  (DestRefreshStart > ?TIMEOUT_DELTA) andalso
                  (DestRefreshStart =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_start_invalid,
                     DestRefreshStart}};
        [_, _, _, _, _, DestRefreshDelay, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshDelay) andalso
                  (DestRefreshDelay > ?TIMEOUT_DELTA) andalso
                  (DestRefreshDelay =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_delay_invalid,
                     DestRefreshDelay}};
        [_, _, _, _, _, _, RequestNameLookup, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((RequestNameLookup =:= sync) orelse
                  (RequestNameLookup =:= async)) ->
            {error, {service_options_request_name_lookup_invalid,
                     RequestNameLookup}};
        [_, _, _, _, _, _, _, RequestTimeoutAdjustment, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not is_boolean(RequestTimeoutAdjustment) ->
            {error, {service_options_request_timeout_adjustment_invalid,
                     RequestTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, RequestTimeoutImmediateMax, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not (is_integer(RequestTimeoutImmediateMax) andalso
                  (RequestTimeoutImmediateMax >= 0) andalso
                  (RequestTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_request_timeout_immediate_max_invalid,
                     RequestTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, ResponseTimeoutAdjustment, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not is_boolean(ResponseTimeoutAdjustment) ->
            {error, {service_options_response_timeout_adjustment_invalid,
                     ResponseTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, _, _, ResponseTimeoutImmediateMax, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not (is_integer(ResponseTimeoutImmediateMax) andalso
                  (ResponseTimeoutImmediateMax >= 0) andalso
                  (ResponseTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_response_timeout_immediate_max_invalid,
                     ResponseTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, _, _, CountProcessDynamic, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((CountProcessDynamic =:= false) orelse
                  is_list(CountProcessDynamic)) ->
            {error, {service_options_count_process_dynamic_invalid,
                     CountProcessDynamic}};
        [_, _, _, _, _, _, _, _, _, _, _, _, TimeoutTerminate, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((TimeoutTerminate =:= undefined) orelse
                  (is_integer(TimeoutTerminate) andalso
                   (TimeoutTerminate >= ?TIMEOUT_TERMINATE_MIN) andalso
                   (TimeoutTerminate =< ?TIMEOUT_TERMINATE_MAX))) ->
            {error, {service_options_timeout_terminate_invalid,
                     TimeoutTerminate}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, Scope,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not is_atom(Scope) ->
            {error, {service_options_scope_invalid,
                     Scope}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         MonkeyLatency, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((MonkeyLatency =:= false) orelse
                  (MonkeyLatency =:= system) orelse
                  is_list(MonkeyLatency)) ->
            {error, {service_options_monkey_latency_invalid,
                     MonkeyLatency}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, MonkeyChaos, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not ((MonkeyChaos =:= false) orelse
                  (MonkeyChaos =:= system) orelse
                  is_list(MonkeyChaos)) ->
            {error, {service_options_monkey_chaos_invalid,
                     MonkeyChaos}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, AutomaticLoading, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _]
        when not is_boolean(AutomaticLoading) ->
            {error, {service_options_automatic_loading_invalid,
                     AutomaticLoading}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         ApplicationName, _, _, _, _, _, _, _]
        when not is_atom(ApplicationName) ->
            {error, {service_options_application_name_invalid,
                     ApplicationName}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, RequestPidUses, _, _, _, _, _, _]
        when not ((RequestPidUses =:= infinity) orelse
                  (is_integer(RequestPidUses) andalso
                   (RequestPidUses >= 1))) ->
            {error, {service_options_request_pid_uses_invalid,
                     RequestPidUses}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, RequestPidOptions, _, _, _, _, _]
        when not is_list(RequestPidOptions) ->
            {error, {service_options_request_pid_options_invalid,
                     RequestPidOptions}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, InfoPidUses, _, _, _, _]
        when not ((InfoPidUses =:= infinity) orelse
                  (is_integer(InfoPidUses) andalso
                   (InfoPidUses >= 1))) ->
            {error, {service_options_info_pid_uses_invalid,
                     InfoPidUses}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, InfoPidOptions, _, _, _]
        when not is_list(InfoPidOptions) ->
            {error, {service_options_info_pid_options_invalid,
                     InfoPidOptions}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, DuoMode, _, _]
        when not is_boolean(DuoMode) ->
            {error, {service_options_duo_mode_invalid,
                     DuoMode}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, Hibernate, _]
        when not (is_boolean(Hibernate) orelse
                  is_list(Hibernate)) ->
            {error, {service_options_hibernate_invalid,
                     Hibernate}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, Reload]
        when not is_boolean(Reload) ->
            {error, {service_options_reload_invalid,
                     Reload}};
        [PriorityDefault, QueueLimit, QueueSize, RateRequestMax,
         DestRefreshStart, DestRefreshDelay, RequestNameLookup,
         RequestTimeoutAdjustment, RequestTimeoutImmediateMax,
         ResponseTimeoutAdjustment, ResponseTimeoutImmediateMax,
         CountProcessDynamic, TimeoutTerminate, Scope,
         MonkeyLatency, MonkeyChaos, AutomaticLoading,
         AspectsInitAfter, AspectsRequestBefore, AspectsRequestAfter,
         AspectsInfoBefore, AspectsInfoAfter, AspectsTerminateBefore,
         ApplicationName,
         RequestPidUses, RequestPidOptions, InfoPidUses, InfoPidOptions,
         DuoMode, Hibernate, Reload]
        when not ((DuoMode =:= true) andalso
                  (InfoPidUses =/= infinity)) ->
            NewQueueSize = if
                QueueSize =:= undefined ->
                    undefined;
                is_integer(QueueSize) ->
                    QueueSize * 1024
            end,
            case services_validate_options_internal_checks(
                RateRequestMax,
                CountProcessDynamic,
                TimeoutTerminate,
                MonkeyLatency,
                MonkeyChaos,
                RequestPidOptions,
                InfoPidOptions,
                Hibernate,
                AspectsInitAfter,
                AspectsRequestBefore,
                AspectsRequestAfter,
                AspectsInfoBefore,
                AspectsInfoAfter,
                AspectsTerminateBefore,
                CountProcess,
                MaxR,
                MaxT,
                AutomaticLoading) of
                {ok,
                 NewRateRequestMax,
                 NewCountProcessDynamic,
                 NewTimeoutTerminate,
                 NewMonkeyLatency,
                 NewMonkeyChaos,
                 NewRequestPidOptions,
                 NewInfoPidOptions,
                 NewHibernate,
                 NewAspectsInitAfter,
                 NewAspectsRequestBefore,
                 NewAspectsRequestAfter,
                 NewAspectsInfoBefore,
                 NewAspectsInfoAfter,
                 NewAspectsTerminateBefore} ->
                    {ok,
                     NewTimeoutTerminate,
                     Options#config_service_options{
                         priority_default =
                             PriorityDefault,
                         queue_limit =
                             QueueLimit,
                         queue_size =
                             NewQueueSize,
                         rate_request_max =
                             NewRateRequestMax,
                         dest_refresh_start =
                             DestRefreshStart,
                         dest_refresh_delay =
                             DestRefreshDelay,
                         request_name_lookup =
                             RequestNameLookup,
                         request_timeout_adjustment =
                             RequestTimeoutAdjustment,
                         request_timeout_immediate_max =
                             RequestTimeoutImmediateMax,
                         response_timeout_adjustment =
                             ResponseTimeoutAdjustment,
                         response_timeout_immediate_max =
                             ResponseTimeoutImmediateMax,
                         count_process_dynamic =
                             NewCountProcessDynamic,
                         scope =
                             ?SCOPE_ASSIGN(Scope),
                         monkey_latency =
                             NewMonkeyLatency,
                         monkey_chaos =
                             NewMonkeyChaos,
                         automatic_loading =
                             AutomaticLoading,
                         aspects_init_after =
                             NewAspectsInitAfter,
                         aspects_request_before =
                             NewAspectsRequestBefore,
                         aspects_request_after =
                             NewAspectsRequestAfter,
                         aspects_info_before =
                             NewAspectsInfoBefore,
                         aspects_info_after =
                             NewAspectsInfoAfter,
                         aspects_terminate_before =
                             NewAspectsTerminateBefore,
                         application_name =
                             ApplicationName,
                         request_pid_uses =
                             RequestPidUses,
                         request_pid_options =
                             NewRequestPidOptions,
                         info_pid_uses =
                             InfoPidUses,
                         info_pid_options =
                             NewInfoPidOptions,
                         duo_mode =
                             DuoMode,
                         hibernate =
                             NewHibernate,
                         reload =
                             Reload}};
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _] ->
            {error, {service_options_invalid, OptionsList}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _ | Extra] ->
            {error, {service_options_invalid, Extra}}
    end.

services_validate_options_internal_checks(RateRequestMax,
                                          CountProcessDynamic,
                                          TimeoutTerminate,
                                          MonkeyLatency,
                                          MonkeyChaos,
                                          RequestPidOptions,
                                          InfoPidOptions,
                                          Hibernate,
                                          AspectsInitAfter,
                                          AspectsRequestBefore,
                                          AspectsRequestAfter,
                                          AspectsInfoBefore,
                                          AspectsInfoAfter,
                                          AspectsTerminateBefore,
                                          CountProcess,
                                          MaxR,
                                          MaxT,
                                          AutomaticLoading) ->
    case services_validate_options_common_checks(RateRequestMax,
                                                 CountProcessDynamic,
                                                 TimeoutTerminate,
                                                 MonkeyLatency,
                                                 MonkeyChaos,
                                                 CountProcess,
                                                 MaxR,
                                                 MaxT) of
        {ok,
         NewRateRequestMax,
         NewCountProcessDynamic,
         NewTimeoutTerminate,
         NewMonkeyLatency,
         NewMonkeyChaos} ->
            case services_validate_option_pid_options(RequestPidOptions) of
                {ok, NewRequestPidOptions} ->
                    case services_validate_option_pid_options(InfoPidOptions) of
                        {ok, NewInfoPidOptions} ->
                            case cloudi_core_i_rate_based_configuration:
                                 hibernate_validate(Hibernate) of
                                {ok, NewHibernate} ->
                                    case services_validate_option_aspects_internal(
                                        AspectsInitAfter,
                                        AspectsRequestBefore,
                                        AspectsRequestAfter,
                                        AspectsInfoBefore,
                                        AspectsInfoAfter,
                                        AspectsTerminateBefore,
                                        AutomaticLoading) of
                                        {ok,
                                         NewAspectsInitAfter,
                                         NewAspectsRequestBefore,
                                         NewAspectsRequestAfter,
                                         NewAspectsInfoBefore,
                                         NewAspectsInfoAfter,
                                         NewAspectsTerminateBefore} ->
                                            {ok,
                                             NewRateRequestMax,
                                             NewCountProcessDynamic,
                                             NewTimeoutTerminate,
                                             NewMonkeyLatency,
                                             NewMonkeyChaos,
                                             NewRequestPidOptions,
                                             NewInfoPidOptions,
                                             NewHibernate,
                                             NewAspectsInitAfter,
                                             NewAspectsRequestBefore,
                                             NewAspectsRequestAfter,
                                             NewAspectsInfoBefore,
                                             NewAspectsInfoAfter,
                                             NewAspectsTerminateBefore};
                                        {error, _} = Error ->
                                            Error
                                    end;
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec services_validate_options_external(OptionsList ::
                                             cloudi_service_api:
                                             service_options_external(),
                                         CountProcess :: pos_integer(),
                                         MaxR :: non_neg_integer(),
                                         MaxT ::
                                             cloudi_service_api:seconds()) ->
    {ok, #config_service_options{}} |
    {error,
     {service_options_priority_default_invalid |
      service_options_queue_limit_invalid |
      service_options_queue_size_invalid |
      service_options_rate_request_max_invalid |
      service_options_dest_refresh_start_invalid |
      service_options_dest_refresh_delay_invalid |
      service_options_request_name_lookup_invalid |
      service_options_request_timeout_adjustment_invalid |
      service_options_request_timeout_immediate_max_invalid |
      service_options_response_timeout_adjustment_invalid |
      service_options_response_timeout_immediate_max_invalid |
      service_options_hibernate_invalid |
      service_options_count_process_dynamic_invalid |
      service_options_timeout_terminate_invalid |
      service_options_timeout_terminate_decrease |
      service_options_scope_invalid |
      service_options_monkey_latency_invalid |
      service_options_monkey_chaos_invalid |
      service_options_automatic_loading_invalid |
      service_options_aspects_init_invalid |
      service_options_aspects_request_invalid |
      service_options_aspects_terminate_invalid |
      service_options_limit_invalid |
      service_options_owner_invalid |
      service_options_invalid, any()}}.

services_validate_options_external(OptionsList, CountProcess, MaxR, MaxT) ->
    Options = #config_service_options{},
    Defaults = [
        {priority_default,
         Options#config_service_options.priority_default},
        {queue_limit,
         Options#config_service_options.queue_limit},
        {queue_size,
         Options#config_service_options.queue_size},
        {rate_request_max,
         Options#config_service_options.rate_request_max},
        {dest_refresh_start,
         Options#config_service_options.dest_refresh_start},
        {dest_refresh_delay,
         Options#config_service_options.dest_refresh_delay},
        {request_name_lookup,
         Options#config_service_options.request_name_lookup},
        {request_timeout_adjustment,
         Options#config_service_options.request_timeout_adjustment},
        {request_timeout_immediate_max,
         Options#config_service_options.request_timeout_immediate_max},
        {response_timeout_adjustment,
         Options#config_service_options.response_timeout_adjustment},
        {response_timeout_immediate_max,
         Options#config_service_options.response_timeout_immediate_max},
        {count_process_dynamic,
         Options#config_service_options.count_process_dynamic},
        {timeout_terminate,
         Options#config_service_options.timeout_terminate},
        {scope,
         Options#config_service_options.scope},
        {monkey_latency,
         Options#config_service_options.monkey_latency},
        {monkey_chaos,
         Options#config_service_options.monkey_chaos},
        {automatic_loading,
         Options#config_service_options.automatic_loading},
        {aspects_init_after,
         Options#config_service_options.aspects_init_after},
        {aspects_request_before,
         Options#config_service_options.aspects_request_before},
        {aspects_request_after,
         Options#config_service_options.aspects_request_after},
        {aspects_terminate_before,
         Options#config_service_options.aspects_terminate_before},
        {limit,
         Options#config_service_options.limit},
        {owner,
         Options#config_service_options.owner}],
    case cloudi_proplists:take_values(Defaults, OptionsList) of
        [PriorityDefault, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((PriorityDefault >= ?PRIORITY_HIGH) andalso
                  (PriorityDefault =< ?PRIORITY_LOW)) ->
            {error, {service_options_priority_default_invalid,
                     PriorityDefault}};
        [_, QueueLimit, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((QueueLimit =:= undefined) orelse
                  (is_integer(QueueLimit) andalso
                   (QueueLimit >= 0))) ->
            {error, {service_options_queue_limit_invalid,
                     QueueLimit}};
        [_, _, QueueSize, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((QueueSize =:= undefined) orelse
                  (is_integer(QueueSize) andalso
                   (QueueSize >= 1))) ->
            {error, {service_options_queue_size_invalid,
                     QueueSize}};
        [_, _, _, RateRequestMax, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((RateRequestMax =:= undefined) orelse
                  is_number(RateRequestMax) orelse
                  is_list(RateRequestMax)) ->
            {error, {service_options_rate_request_max_invalid,
                     RateRequestMax}};
        [_, _, _, _, DestRefreshStart, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshStart) andalso
                  (DestRefreshStart > ?TIMEOUT_DELTA) andalso
                  (DestRefreshStart =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_start_invalid,
                     DestRefreshStart}};
        [_, _, _, _, _, DestRefreshDelay, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshDelay) andalso
                  (DestRefreshDelay > ?TIMEOUT_DELTA) andalso
                  (DestRefreshDelay =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_delay_invalid,
                     DestRefreshDelay}};
        [_, _, _, _, _, _, RequestNameLookup, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((RequestNameLookup =:= sync) orelse
                  (RequestNameLookup =:= async)) ->
            {error, {service_options_request_name_lookup_invalid,
                     RequestNameLookup}};
        [_, _, _, _, _, _, _, RequestTimeoutAdjustment, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_boolean(RequestTimeoutAdjustment) ->
            {error, {service_options_request_timeout_adjustment_invalid,
                     RequestTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, RequestTimeoutImmediateMax, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not (is_integer(RequestTimeoutImmediateMax) andalso
                  (RequestTimeoutImmediateMax >= 0) andalso
                  (RequestTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_request_timeout_immediate_max_invalid,
                     RequestTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, ResponseTimeoutAdjustment, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_boolean(ResponseTimeoutAdjustment) ->
            {error, {service_options_response_timeout_adjustment_invalid,
                     ResponseTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, _, _, ResponseTimeoutImmediateMax, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not (is_integer(ResponseTimeoutImmediateMax) andalso
                  (ResponseTimeoutImmediateMax >= 0) andalso
                  (ResponseTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_response_timeout_immediate_max_invalid,
                     ResponseTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, _, _, CountProcessDynamic, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((CountProcessDynamic =:= false) orelse
                  is_list(CountProcessDynamic)) ->
            {error, {service_options_count_process_dynamic_invalid,
                     CountProcessDynamic}};
        [_, _, _, _, _, _, _, _, _, _, _, _, TimeoutTerminate, _,
         _, _, _, _, _, _, _, _, _]
        when not ((TimeoutTerminate =:= undefined) orelse
                  (is_integer(TimeoutTerminate) andalso
                   (TimeoutTerminate >= ?TIMEOUT_TERMINATE_MIN) andalso
                   (TimeoutTerminate =< ?TIMEOUT_TERMINATE_MAX))) ->
            {error, {service_options_timeout_terminate_invalid,
                     TimeoutTerminate}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, Scope,
         _, _, _, _, _, _, _, _, _]
        when not is_atom(Scope) ->
            {error, {service_options_scope_invalid,
                     Scope}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         MonkeyLatency, _, _, _, _, _, _, _, _]
        when not ((MonkeyLatency =:= false) orelse
                  (MonkeyLatency =:= system) orelse
                  is_list(MonkeyLatency)) ->
            {error, {service_options_monkey_latency_invalid,
                     MonkeyLatency}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, MonkeyChaos, _, _, _, _, _, _, _]
        when not ((MonkeyChaos =:= false) orelse
                  (MonkeyChaos =:= system) orelse
                  is_list(MonkeyChaos)) ->
            {error, {service_options_monkey_chaos_invalid,
                     MonkeyChaos}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, AutomaticLoading, _, _, _, _, _, _]
        when not is_boolean(AutomaticLoading) ->
            {error, {service_options_automatic_loading_invalid,
                     AutomaticLoading}};
        [PriorityDefault, QueueLimit, QueueSize, RateRequestMax,
         DestRefreshStart, DestRefreshDelay, RequestNameLookup,
         RequestTimeoutAdjustment, RequestTimeoutImmediateMax,
         ResponseTimeoutAdjustment, ResponseTimeoutImmediateMax,
         CountProcessDynamic, TimeoutTerminate, Scope,
         MonkeyLatency, MonkeyChaos,
         AutomaticLoading, AspectsInitAfter, AspectsRequestBefore,
         AspectsRequestAfter, AspectsTerminateBefore, Limit, Owner] ->
            NewQueueSize = if
                QueueSize =:= undefined ->
                    undefined;
                is_integer(QueueSize) ->
                    QueueSize * 1024
            end,
            case services_validate_options_external_checks(RateRequestMax,
                                                           CountProcessDynamic,
                                                           TimeoutTerminate,
                                                           MonkeyLatency,
                                                           MonkeyChaos,
                                                           CountProcess,
                                                           MaxR,
                                                           MaxT,
                                                           Limit,
                                                           Owner) of
                {ok,
                 NewRateRequestMax,
                 NewCountProcessDynamic,
                 NewTimeoutTerminate,
                 NewMonkeyLatency,
                 NewMonkeyChaos,
                 NewLimit,
                 NewOwner} ->
                    case services_validate_option_aspects_external(
                        AspectsInitAfter,
                        AspectsRequestBefore,
                        AspectsRequestAfter,
                        AspectsTerminateBefore,
                        AutomaticLoading) of
                        {ok,
                         NewAspectsInitAfter,
                         NewAspectsRequestBefore,
                         NewAspectsRequestAfter,
                         NewAspectsTerminateBefore} ->
                            {ok,
                             NewTimeoutTerminate,
                             Options#config_service_options{
                                 priority_default =
                                     PriorityDefault,
                                 queue_limit =
                                     QueueLimit,
                                 queue_size =
                                     NewQueueSize,
                                 rate_request_max =
                                     NewRateRequestMax,
                                 dest_refresh_start =
                                     DestRefreshStart,
                                 dest_refresh_delay =
                                     DestRefreshDelay,
                                 request_name_lookup =
                                     RequestNameLookup,
                                 request_timeout_adjustment =
                                     RequestTimeoutAdjustment,
                                 request_timeout_immediate_max =
                                     RequestTimeoutImmediateMax,
                                 response_timeout_adjustment =
                                     ResponseTimeoutAdjustment,
                                 response_timeout_immediate_max =
                                     ResponseTimeoutImmediateMax,
                                 count_process_dynamic =
                                     NewCountProcessDynamic,
                                 scope =
                                     ?SCOPE_ASSIGN(Scope),
                                 monkey_latency =
                                     NewMonkeyLatency,
                                 monkey_chaos =
                                     NewMonkeyChaos,
                                 automatic_loading =
                                     AutomaticLoading,
                                 aspects_init_after =
                                     NewAspectsInitAfter,
                                 aspects_request_before =
                                     NewAspectsRequestBefore,
                                 aspects_request_after =
                                     NewAspectsRequestAfter,
                                 aspects_terminate_before =
                                     NewAspectsTerminateBefore,
                                 limit =
                                     NewLimit,
                                 owner =
                                     NewOwner}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _ | Extra] ->
            {error, {service_options_invalid, Extra}}
    end.

services_validate_options_external_checks(RateRequestMax,
                                          CountProcessDynamic,
                                          TimeoutTerminate,
                                          MonkeyLatency,
                                          MonkeyChaos,
                                          CountProcess,
                                          MaxR,
                                          MaxT,
                                          Limit,
                                          Owner) ->
    case services_validate_options_common_checks(RateRequestMax,
                                                 CountProcessDynamic,
                                                 TimeoutTerminate,
                                                 MonkeyLatency,
                                                 MonkeyChaos,
                                                 CountProcess,
                                                 MaxR,
                                                 MaxT) of
        {ok,
         NewRateRequestMax,
         NewCountProcessDynamic,
         NewTimeoutTerminate,
         NewMonkeyLatency,
         NewMonkeyChaos} ->
            case cloudi_core_i_os_process:limit_validate(Limit) of
                {ok, NewLimit} ->
                    case cloudi_core_i_os_process:owner_validate(Owner) of
                        {ok, NewOwner} ->
                            {ok,
                             NewRateRequestMax,
                             NewCountProcessDynamic,
                             NewTimeoutTerminate,
                             NewMonkeyLatency,
                             NewMonkeyChaos,
                             NewLimit,
                             NewOwner};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

services_validate_options_common_checks(RateRequestMax,
                                        CountProcessDynamic,
                                        TimeoutTerminate,
                                        MonkeyLatency,
                                        MonkeyChaos,
                                        CountProcess,
                                        MaxR,
                                        MaxT) ->
    case cloudi_core_i_rate_based_configuration:
         rate_request_validate(RateRequestMax) of
        {ok, NewRateRequestMax} ->
            case cloudi_core_i_rate_based_configuration:
                 count_process_dynamic_validate(CountProcessDynamic,
                                                CountProcess) of
                {ok, NewCountProcessDynamic} ->
                    case timeout_terminate(TimeoutTerminate, MaxR, MaxT) of
                        {ok, NewTimeoutTerminate} ->
                            case cloudi_core_i_runtime_testing:
                                 monkey_latency_validate(MonkeyLatency) of
                                {ok, NewMonkeyLatency} ->
                                    case cloudi_core_i_runtime_testing:
                                         monkey_chaos_validate(MonkeyChaos) of
                                        {ok, NewMonkeyChaos} ->
                                            {ok,
                                             NewRateRequestMax,
                                             NewCountProcessDynamic,
                                             NewTimeoutTerminate,
                                             NewMonkeyLatency,
                                             NewMonkeyChaos};
                                        {error, _} = Error ->
                                            Error
                                    end;
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

services_validate_option_pid_options(OptionsList) ->
    services_validate_option_pid_options(OptionsList, [link]).

-ifdef(ERLANG_OTP_VERSION_19_FEATURES).
-define(PID_OPTIONS_ERLANG_OTP_VERSION_19,
    ;
services_validate_option_pid_options([{max_heap_size, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V) andalso V >= 0; is_map(V) ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{message_queue_data, V} = PidOption |
                                      OptionsList], Output)
    when (V =:= off_heap) orelse (V =:= on_heap) orelse (V =:= mixed) ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);).
-else.
-define(PID_OPTIONS_ERLANG_OTP_VERSION_19,
    ;).
-endif.

services_validate_option_pid_options([], Output) ->
    {ok, lists:reverse(Output)};
services_validate_option_pid_options([{sensitive, V} = PidOption |
                                      OptionsList], Output)
    when is_boolean(V) ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{fullsweep_after, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V), V >= 0 ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{min_heap_size, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V), V >= 0 ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{min_bin_vheap_size, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V), V >= 0 ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output])
?PID_OPTIONS_ERLANG_OTP_VERSION_19
services_validate_option_pid_options([{priority, V} = PidOption |
                                      OptionsList], Output)
    when (V =:= high) orelse (V =:= low) orelse (V =:= normal) ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([PidOption | _], _) ->
    {error, {service_options_pid_invalid, PidOption}}.

services_validate_option_aspects_f(Aspects, Arity, AutomaticLoading) ->
    services_validate_option_aspects_f(Aspects, [], Arity, AutomaticLoading).

services_validate_option_aspects_f([], AspectsNew, _, _) ->
    {ok, lists:reverse(AspectsNew)};
services_validate_option_aspects_f([F | AspectsOld], AspectsNew,
                                   Arity, AutomaticLoading)
    when is_function(F, Arity) ->
    services_validate_option_aspects_f(AspectsOld, [F | AspectsNew],
                                       Arity, AutomaticLoading);
services_validate_option_aspects_f([{M, F} = Entry | AspectsOld], AspectsNew,
                                   Arity, AutomaticLoading)
    when is_atom(M), is_atom(F) ->
    % check if a function is exported
    % if the module is not currently loaded,
    % only load the module for the check
    Exported = case code:is_loaded(M) of
        {file, _} ->
            erlang:function_exported(M, F, Arity);
        false ->
            if
                AutomaticLoading =:= true ->
                    case code:load_file(M) of
                        {module, _} ->
                            V = erlang:function_exported(M, F, Arity),
                            true = code:delete(M),
                            false = code:purge(M),
                            V;
                        {error, _} ->
                            false
                    end;
                AutomaticLoading =:= false ->
                    false
            end
    end,
    if
        Exported =:= true ->
            services_validate_option_aspects_f(AspectsOld,
                                               [Entry | AspectsNew],
                                               Arity, AutomaticLoading);
        Exported =:= false ->
            {error, Entry}
    end;
services_validate_option_aspects_f([{{M, F}} = Entry | AspectsOld], AspectsNew,
                                   Arity, AutomaticLoading)
    when is_atom(M), is_atom(F) ->
    % check if a function is exported
    % if the module is not currently loaded,
    % only load the module to make a function
    {Exported, Function} = case code:is_loaded(M) of
        {file, _} ->
            case erlang:function_exported(M, F, 0) of
                true ->
                    {true, M:F()};
                false ->
                    {false, undefined}
            end;
        false ->
            if
                AutomaticLoading =:= true ->
                    case code:load_file(M) of
                        {module, _} ->
                            V = case erlang:function_exported(M, F, 0) of
                                true ->
                                    {true, M:F()};
                                false ->
                                    {false, undefined}
                            end,
                            true = code:delete(M),
                            false = code:purge(M),
                            V;
                        {error, _} ->
                            {false, undefined}
                    end;
                AutomaticLoading =:= false ->
                    {false, undefined}
            end
    end,
    if
        Exported =:= true, is_function(Function, Arity) ->
            services_validate_option_aspects_f(AspectsOld,
                                               [Function | AspectsNew],
                                               Arity, AutomaticLoading);
        Exported =:= false ->
            {error, Entry}
    end;
services_validate_option_aspects_f([Entry | _], _, _, _) ->
    {error, Entry}.

services_validate_option_aspects_terminate_before(AspectsTerminate,
                                                  AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsTerminate, 3,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_terminate_before_invalid, Entry}}
    end.

services_validate_option_aspects_init_after_internal(AspectsInit,
                                                     AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsInit, 5,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_init_after_invalid, Entry}}
    end.

services_validate_option_aspects_request_before_internal(AspectsRequest,
                                                         AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsRequest, 11,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_request_before_invalid, Entry}}
    end.

services_validate_option_aspects_request_after_internal(AspectsRequest,
                                                        AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsRequest, 12,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_request_after_invalid, Entry}}
    end.

services_validate_option_aspects_info_before_internal(AspectsInfo,
                                                      AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsInfo, 3,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_info_before_invalid, Entry}}
    end.

services_validate_option_aspects_info_after_internal(AspectsInfo,
                                                      AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsInfo, 3,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_info_after_invalid, Entry}}
    end.

services_validate_option_aspects_internal(AspectsInitAfter,
                                          AspectsRequestBefore,
                                          AspectsRequestAfter,
                                          AspectsInfoBefore,
                                          AspectsInfoAfter,
                                          AspectsTerminateBefore,
                                          AutomaticLoading) ->
    case services_validate_option_aspects_init_after_internal(
        AspectsInitAfter,
        AutomaticLoading) of
        {ok, NewAspectsInitAfter} ->
            case services_validate_option_aspects_request_before_internal(
                AspectsRequestBefore,
                AutomaticLoading) of
                {ok, NewAspectsRequestBefore} ->
                    case services_validate_option_aspects_request_after_internal(
                        AspectsRequestAfter,
                        AutomaticLoading) of
                        {ok, NewAspectsRequestAfter} ->
                            case services_validate_option_aspects_info_before_internal(
                                AspectsInfoBefore,
                                AutomaticLoading) of
                                {ok, NewAspectsInfoBefore} ->
                                    case services_validate_option_aspects_info_after_internal(
                                        AspectsInfoAfter,
                                        AutomaticLoading) of
                                        {ok, NewAspectsInfoAfter} ->
                                            case services_validate_option_aspects_terminate_before(
                                                AspectsTerminateBefore,
                                                AutomaticLoading) of
                                                {ok, NewAspectsTerminateBefore} ->
                                                    {ok,
                                                     NewAspectsInitAfter,
                                                     NewAspectsRequestBefore,
                                                     NewAspectsRequestAfter,
                                                     NewAspectsInfoBefore,
                                                     NewAspectsInfoAfter,
                                                     NewAspectsTerminateBefore};
                                                {error, _} = Error ->
                                                    Error
                                            end;
                                        {error, _} = Error ->
                                            Error
                                    end;
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

services_validate_option_aspects_init_after_external(AspectsInit,
                                                     AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsInit, 4,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_init_after_invalid, Entry}}
    end.

services_validate_option_aspects_request_before_external(AspectsRequest,
                                                         AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsRequest, 10,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_request_before_invalid, Entry}}
    end.

services_validate_option_aspects_request_after_external(AspectsRequest,
                                                        AutomaticLoading) ->
    case services_validate_option_aspects_f(AspectsRequest, 11,
                                            AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_request_after_invalid, Entry}}
    end.

services_validate_option_aspects_external(AspectsInitAfter,
                                          AspectsRequestBefore,
                                          AspectsRequestAfter,
                                          AspectsTerminateBefore,
                                          AutomaticLoading) ->
    case services_validate_option_aspects_init_after_external(
        AspectsInitAfter,
        AutomaticLoading) of
        {ok, NewAspectsInitAfter} ->
            case services_validate_option_aspects_request_before_external(
                AspectsRequestBefore,
                AutomaticLoading) of
                {ok, NewAspectsRequestBefore} ->
                    case services_validate_option_aspects_request_after_external(
                        AspectsRequestAfter,
                        AutomaticLoading) of
                        {ok, NewAspectsRequestAfter} ->
                            case services_validate_option_aspects_terminate_before(
                                AspectsTerminateBefore,
                                AutomaticLoading) of
                                {ok, NewAspectsTerminateBefore} ->
                                    {ok,
                                     NewAspectsInitAfter,
                                     NewAspectsRequestBefore,
                                     NewAspectsRequestAfter,
                                     NewAspectsTerminateBefore};
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

acl_lookup_new(L) ->
    acl_lookup_add(L, dict:new()).

acl_lookup_add(L, OldLookup) ->
    case acl_store(L, OldLookup) of
        {ok, NewLookup} ->
            acl_expand(L, OldLookup, NewLookup);
        {error, _} = Error ->
            Error
    end.

acl_store([], Lookup) ->
    {ok, Lookup};
acl_store([{Key, [E | _] = Value} | L], Lookup)
    when is_atom(E);
         (is_list(E) andalso (length(E) > 0) andalso is_integer(hd(E))) ->
    acl_store(L, dict:store(Key, Value, Lookup));
acl_store([H | _], _) ->
    {error, {acl_invalid, H}}.

acl_expand([], LookupFinal, _) ->
    {ok, LookupFinal};
acl_expand([{Key, Value} | L], LookupFinal, LookupConfig) ->
    case acl_expand_values(Value, [], [], Key, LookupConfig) of
        {ok, NewValue} ->
            acl_expand(L, dict:store(Key, NewValue, LookupFinal),
                       LookupConfig);
        {error, _} = Error ->
            Error
    end.

acl_expand_values([], Output, _Path, _Key, _Lookup) ->
    {ok, lists:reverse(Output)};
acl_expand_values([E | L], Output, Path, Key, Lookup)
    when is_atom(E) ->
    case lists:member(E, Path) of
        true ->
            {error, {acl_cyclic, Key, E}};
        false ->
            case dict:find(E, Lookup) of
                error ->
                    {error, {acl_not_found, E}};
                {ok, OtherL} ->
                    case acl_expand_values(OtherL, Output,
                                           [E | Path], Key, Lookup) of
                        {ok, NewOutput} ->
                            acl_expand_values(L, NewOutput, Path, Key, Lookup);
                        {error, _} = Error ->
                            Error
                    end
            end
    end;
acl_expand_values([E | L], Output, Path, Key, Lookup)
    when is_list(E), (length(E) > 0), is_integer(hd(E)) ->
    try cloudi_x_trie:is_pattern(E) of
        true ->
            acl_expand_values(L, [E | Output], Path, Key, Lookup);
        false ->
            acl_expand_values(L, [E ++ "*" | Output], Path, Key, Lookup)
    catch
        exit:badarg ->
            {error, {acl_invalid, E}}
    end;
acl_expand_values([E | _], _, _, _, _) ->
    {error, {acl_invalid, E}}.

services_remove_uuid(Value, Services, Timeout) ->
    services_remove_uuid(Value, [], Services, Timeout).

services_remove_uuid([], RemoveServices, Services, Timeout) ->
    case services_remove_all(lists:reverse(RemoveServices),
                             Services, Timeout) of
        {ok, _} = Success ->
            Success;
        {error, _} = Error ->
            Error
    end;
services_remove_uuid([ID | IDs], RemoveServices, Services, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    {ServiceList, NextServices} = lists:partition(fun(S) ->
        (is_record(S, config_service_internal) andalso
         (S#config_service_internal.uuid == ID)) orelse
        (is_record(S, config_service_external) andalso
         (S#config_service_external.uuid == ID))
    end, Services),
    case ServiceList of
        [] ->
            {error, {service_not_found, ID}};
        [Service] ->
            services_remove_uuid(IDs, [Service | RemoveServices],
                                 NextServices, Timeout)
    end;
services_remove_uuid([ID | _], _, _, _) ->
    {error, {service_invalid, ID}}.

services_remove_all_internal(_, #config_service_external{}) ->
    false;
services_remove_all_internal([], #config_service_internal{}) ->
    true;
services_remove_all_internal([#config_service_internal{
                                  module = Module} | _],
                             #config_service_internal{
                                  module = Module}) ->
    false;
services_remove_all_internal([#config_service_internal{
                                  options = #config_service_options{
                                      application_name = Application}} | _],
                             #config_service_internal{
                                  options = #config_service_options{
                                      application_name = Application}})
    when Application =/= undefined ->
    false;
services_remove_all_internal([_ | Services],
                             #config_service_internal{} = RemoveService) ->
    services_remove_all_internal(Services, RemoveService).

services_remove_all([], Services, _) ->
    {ok, Services};
services_remove_all([Service | RemoveServices], Services, Timeout) ->
    Remove = services_remove_all_internal(RemoveServices ++ Services, Service),
    case cloudi_core_i_configurator:service_stop(Service, Remove, Timeout) of
        ok ->
            services_remove_all(RemoveServices, Services, Timeout);
        {error, _} = Error ->
            Error
    end.

services_restart_uuid(Value, Services, Timeout) ->
    services_restart_uuid(Value, [], Services, Timeout).

services_restart_uuid([], RestartServices, _, Timeout) ->
    case services_restart_all(lists:reverse(RestartServices), Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end;
services_restart_uuid([ID | IDs], RestartServices, Services, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    ServiceList = lists:filter(fun(S) ->
        (is_record(S, config_service_internal) andalso
         (S#config_service_internal.uuid == ID)) orelse
        (is_record(S, config_service_external) andalso
         (S#config_service_external.uuid == ID))
    end, Services),
    case ServiceList of
        [] ->
            {error, {service_not_found, ID}};
        [Service] ->
            services_restart_uuid(IDs, [Service | RestartServices],
                                  Services, Timeout)
    end;
services_restart_uuid([ID | _], _, _, _) ->
    {error, {service_invalid, ID}}.

services_restart_all([], _) ->
    ok;
services_restart_all([Service | RestartServices], Timeout) ->
    case cloudi_core_i_configurator:service_restart(Service, Timeout) of
        ok ->
            services_restart_all(RestartServices, Timeout);
        {error, _} = Error ->
            Error
    end.

services_update_plan(Value, Services, Timeout) ->
    services_update_plan(Value, [], Services, Timeout).

services_update_plan([], UpdatePlans, Services, Timeout) ->
    services_update_all(lists:reverse(UpdatePlans), Services, Timeout);
services_update_plan([{ID, Plan} | L], UpdatePlans, Services, Timeout)
    when is_binary(ID), (byte_size(ID) == 16) orelse (byte_size(ID) == 0) ->
    UpdatePlan = #config_service_update{},
    Defaults = [
        {type,
         UpdatePlan#config_service_update.type},
        {module,
         UpdatePlan#config_service_update.module},
        {module_state,
         UpdatePlan#config_service_update.module_state},
        {modules_load,
         UpdatePlan#config_service_update.modules_load},
        {modules_unload,
         UpdatePlan#config_service_update.modules_unload},
        {code_paths_add,
         UpdatePlan#config_service_update.code_paths_add},
        {code_paths_remove,
         UpdatePlan#config_service_update.code_paths_remove}],
    case cloudi_proplists:take_values(Defaults, Plan) of
        [Type, _, _,
         _, _, _, _]
        when not ((Type =:= undefined) orelse
                  (Type =:= internal) orelse (Type =:= external)) ->
            {error, {service_update_type_invalid,
                     Type}};
        [_, Module, _,
         _, _, _, _]
        when not is_atom(Module) ->
            {error, {service_update_module_invalid,
                     Module}};
        [_, _, ModuleState,
         _, _, _, _]
        when not ((ModuleState =:= undefined) orelse
                  is_tuple(ModuleState) orelse
                  is_function(ModuleState, 3)) ->
            {error, {service_update_module_state_invalid,
                     ModuleState}};
        [_, _, _,
         ModulesLoad, _, _, _]
        when not (is_list(ModulesLoad) andalso
                  is_atom(hd(ModulesLoad))) ->
            {error, {service_update_modules_load_invalid,
                     ModulesLoad}};
        [_, _, _,
         _, ModulesUnload, _, _]
        when not (is_list(ModulesUnload) andalso
                  is_atom(hd(ModulesUnload))) ->
            {error, {service_update_modules_unload_invalid,
                     ModulesUnload}};
        [_, _, _,
         _, _, CodePathsAdd, _]
        when not (is_list(CodePathsAdd) andalso
                  is_list(hd(CodePathsAdd)) andalso
                  is_integer(hd(hd(CodePathsAdd)))) ->
            {error, {service_update_code_paths_add_invalid,
                     CodePathsAdd}};
        [_, _, _,
         _, _, _, CodePathsRemove]
        when not (is_list(CodePathsRemove) andalso
                  is_list(hd(CodePathsRemove)) andalso
                  is_integer(hd(hd(CodePathsRemove)))) ->
            {error, {service_update_code_paths_remove_invalid,
                     CodePathsRemove}};
        [Type, Module, _,
         _, _, _, _]
        when not (((Type =:= undefined) orelse (Type =:= internal)) andalso
                  (Module =/= undefined)) ->
            {error, {service_update_type_invalid,
                     Type}};
        [_, Module, ModuleState,
         ModulesLoad, ModulesUnload, _, _]
        when Module =/= undefined, ModuleState =:= undefined,
             ModulesLoad == [], ModulesUnload == [] ->
            {error, {service_update_invalid,
                     no_update}};
        [_, Module, ModuleState,
         ModulesLoad, ModulesUnload, CodePathsAdd, CodePathsRemove]
        when Module =/= undefined ->
            ModuleIDs = lists:foldr(fun(S, IDs) ->
                if
                    is_record(S, config_service_internal),
                    S#config_service_internal.module =:= Module ->
                        [S#config_service_internal.uuid | IDs];
                    true ->
                        IDs
                end
            end, [], Services),
            UpdateValid = case ModuleIDs of
                [_ | _] when ID == <<>> ->
                    true;
                [ID] ->
                    true;
                _ ->
                    false
            end,
            if
                UpdateValid =:= true ->
                    case services_update_plan_module_state(ModuleState) of
                        {ok, NewModuleState} ->
                            ModuleVersion = cloudi_x_reltool_util:
                                            module_version(Module),
                            NewUpdatePlans =
                                [UpdatePlan#config_service_update{
                                     type = internal,
                                     module = Module,
                                     module_version_old = ModuleVersion,
                                     module_state = NewModuleState,
                                     modules_load = ModulesLoad,
                                     modules_unload = ModulesUnload,
                                     code_paths_add = CodePathsAdd,
                                     code_paths_remove = CodePathsRemove,
                                     uuids = ModuleIDs} |
                                 UpdatePlans],
                            services_update_plan(L, NewUpdatePlans,
                                                 Services, Timeout);
                        {error, _} = Error ->
                            Error
                    end;
                UpdateValid =:= false ->
                    {error, {update_invalid, ID}}
            end;
        [_, _, _,
         _, _, _, _ | Invalid] ->
            {error, {service_update_invalid,
                     Invalid}}
    end;
services_update_plan([{ID, _} | _], _, _, _) ->
    {error, {update_invalid, ID}}.

services_update_all([], Services, _) ->
    {ok, ok, Services};
services_update_all([UpdatePlan | UpdatePlans], Services, Timeout) ->
    case cloudi_core_i_configurator:service_update(UpdatePlan, Timeout) of
        ok ->
            % XXX modify Services as required
            services_update_all(UpdatePlans, Services, Timeout);
        {error, _} = Error ->
            {ok, Error, Services}
    end.

services_update_plan_module_state(undefined) ->
    {ok, undefined};
services_update_plan_module_state(ModuleState)
    when is_function(ModuleState, 3) ->
    {ok, ModuleState};
services_update_plan_module_state({M, F} = ModuleState) ->
    case erlang:function_exported(M, F, 3) of
        true ->
            {ok, fun M:F/3};
        false ->
            {error, {service_update_module_state_invalid,
                     ModuleState}}
    end;
services_update_plan_module_state({{M, F}} = ModuleState) ->
    Function = case erlang:function_exported(M, F, 0) of
        true ->
            M:F();
        false ->
            undefined
    end,
    if
        is_function(Function, 3) ->
            {ok, Function};
        true ->
            {error, {service_update_module_state_invalid,
                     ModuleState}}
    end;
services_update_plan_module_state(ModuleState) ->
    {error, {service_update_module_state_invalid,
             ModuleState}}.

service_name_valid(Name, ErrorReason) ->
    try cloudi_x_trie:is_pattern(Name) of
        _ ->
            ok
    catch
        exit:badarg ->
            {error, {ErrorReason, Name}}
    end.

node_validate(A) ->
    case lists:member($@, erlang:atom_to_list(A)) of
        true ->
            ok;
        false ->
            {error, {node_invalid, A}}
    end.

nodes_validate([]) ->
    ok;
nodes_validate([A | As])
    when is_atom(A) ->
    case node_validate(A) of
        ok ->
            nodes_validate(As);
        {error, _} = Error ->
            Error
    end;
nodes_validate([A | _]) ->
    {error, {node_invalid, A}}.

nodes_elements_add([], NodesConfig) ->
    {ok, NodesConfig};
nodes_elements_add([A | As], NodesConfig)
    when A =:= node() ->
    nodes_elements_add(As, NodesConfig);
nodes_elements_add([A | As], #config_nodes{nodes = Nodes} = NodesConfig)
    when is_atom(A) ->
    case node_validate(A) of
        ok ->
            NewNodes = lists:umerge(Nodes, [A]),
            nodes_elements_add(As,
                               NodesConfig#config_nodes{nodes = NewNodes});
        {error, _} = Error ->
            Error
    end;
nodes_elements_add([A | _], _) ->
    {error, {node_invalid, A}}.

nodes_elements_remove([], NodesConfig) ->
    {ok, NodesConfig};
nodes_elements_remove([A | As], NodesConfig)
    when A =:= node() ->
    nodes_elements_remove(As, NodesConfig);
nodes_elements_remove([A | As], #config_nodes{nodes = Nodes} = NodesConfig)
    when is_atom(A) ->
    case cloudi_lists:delete_checked(A, Nodes) of
        false ->
            {error, {node_not_found, A}};
        NewNodes ->
            nodes_elements_remove(As,
                                  NodesConfig#config_nodes{nodes = NewNodes})
    end;
nodes_elements_remove([A | _], _) ->
    {error, {node_invalid, A}}.

nodes_discovery_ec2_validate(Groups, Tags) ->
    case cloudi_x_nodefinder_ec2:validate_groups(Groups) of
        ok ->
            case cloudi_x_nodefinder_ec2:validate_tags(Tags) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {node_discovery_ec2_invalid, Reason}}
            end;
        {error, Reason} ->
            {error, {node_discovery_ec2_invalid, Reason}}
    end.

nodes_discovery_ec2_options(Value, NodesConfig) ->
    #config_nodes{reconnect_delay = TimeoutSeconds} = NodesConfig,
    Defaults = [
        {access_key_id, undefined},
        {secret_access_key, undefined},
        {host, "ec2.amazonaws.com"},
        {groups, []},
        {tags, []}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [AccessKeyId, _, _, _, _ | _]
            when not (is_list(AccessKeyId) orelse
                      is_integer(hd(AccessKeyId))) ->
            {error, {node_discovery_ec2_access_key_id_invalid,
                     AccessKeyId}};
        [_, SecretAccessKey, _, _, _ | _]
            when not (is_list(SecretAccessKey) orelse
                      is_integer(hd(SecretAccessKey))) ->
            {error, {node_discovery_ec2_secret_access_key_invalid,
                     SecretAccessKey}};
        [_, _, Host, _, _ | _]
            when not (is_list(Host) orelse
                      is_integer(hd(Host))) ->
            {error, {node_discovery_ec2_host_invalid, Host}};
        [_, _, _, [], []] ->
            {error, {node_discovery_ec2_tags_selection_null, []}};
        [_, _, _, Groups, _ | _]
            when not is_list(Groups) ->
            {error, {node_discovery_ec2_groups_invalid, Groups}};
        [_, _, _, _, Tags | _]
            when not is_list(Tags) ->
            {error, {node_discovery_ec2_tags_invalid, Tags}};
        [AccessKeyId, SecretAccessKey, Host, Groups, Tags] ->
            case nodes_discovery_ec2_validate(Groups, Tags) of
                ok ->
                    Discovery = #config_nodes_discovery{
                        mode = ec2,
                        module = cloudi_x_nodefinder,
                        start_f = ec2_start,
                        start_a = [AccessKeyId, SecretAccessKey,
                                   Host, Groups, Tags],
                        discover_f = ec2_discover,
                        discover_a = [TimeoutSeconds * 1000 + ?TIMEOUT_DELTA],
                        stop_f = ec2_stop,
                        stop_a = []},
                    {ok, NodesConfig#config_nodes{discovery = Discovery}};
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _ | Extra] ->
            {error, {node_discovery_ec2_invalid, Extra}}
    end.

nodes_discovery_multicast_options(Value, NodesConfig) ->
    #config_nodes{reconnect_delay = TimeoutSeconds} = NodesConfig,
    Defaults = [
        {interface, {0,0,0,0}},
        {address, {224,0,0,1}},
        {port, 4475},
        {ttl, 1}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [Interface, _, _, _ | _]
            when not is_tuple(Interface) ->
            {error, {node_discovery_multicast_interface_invalid, Interface}};
        [_, Address, _, _ | _]
            when not is_tuple(Address) ->
            {error, {node_discovery_multicast_address_invalid, Address}};
        [_, _, Port, _ | _]
            when not (is_integer(Port) andalso
                      (Port > 0)) ->
            {error, {node_discovery_multicast_port_invalid, Port}};
        [_, _, _, TTL | _]
            when not (is_integer(TTL) andalso
                      (TTL >= 0)) ->
            {error, {node_discovery_multicast_ttl_invalid, TTL}};
        [Interface, Address, Port, TTL] ->
            Discovery = #config_nodes_discovery{
                mode = multicast,
                module = cloudi_x_nodefinder,
                start_f = multicast_start,
                start_a = [Interface, Address, Port, TTL, TimeoutSeconds],
                discover_f = multicast_discover,
                discover_a = [TimeoutSeconds * 1000 + ?TIMEOUT_DELTA],
                stop_f = multicast_stop,
                stop_a = []},
            {ok, NodesConfig#config_nodes{discovery = Discovery}};
        [_, _, _, _ | Extra] ->
            {error, {node_discovery_multicast_invalid, Extra}}
    end.

nodes_discovery_options(undefined, NodesConfig) ->
    {ok, NodesConfig};
nodes_discovery_options(Value, NodesConfig) ->
    Defaults = [
        {multicast, []},
        {ec2, []}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [MulticastOptions, _ | _]
            when not is_list(MulticastOptions) ->
            {error, {node_discovery_multicast_invalid, MulticastOptions}};
        [_, EC2Options | _]
            when not is_list(EC2Options) ->
            {error, {node_discovery_ec2_invalid, EC2Options}};
        [[_ | _], [_ | _]] ->
            {error, {node_discovery_ambiguous, Value}};
        [MulticastOptions, []] ->
            nodes_discovery_multicast_options(MulticastOptions, NodesConfig);
        [[], EC2Options] ->
            nodes_discovery_ec2_options(EC2Options, NodesConfig);
        [_, _ | Extra] ->
            {error, {node_discovery_invalid, Extra}}
    end.

nodes_options(Nodes0, Value) ->
    NodesConfig = #config_nodes{},
    Defaults = [
        {nodes,
         NodesConfig#config_nodes.nodes},
        {reconnect_start,
         NodesConfig#config_nodes.reconnect_start},
        {reconnect_delay,
         NodesConfig#config_nodes.reconnect_delay},
        {listen,
         NodesConfig#config_nodes.listen},
        {connect,
         NodesConfig#config_nodes.connect},
        {timestamp_type,
         NodesConfig#config_nodes.timestamp_type},
        {discovery,
         NodesConfig#config_nodes.discovery}],
    ConnectTimeSeconds = (cloudi_x_nodefinder:timeout_min() + 500) div 1000,
    case cloudi_proplists:take_values(Defaults, Value) of
        [Nodes1, _, _, _, _, _, _ | _]
            when not is_list(Nodes1) ->
            {error, {node_invalid, Nodes1}};
        [_, ReconnectStart, _, _, _, _, _ | _]
            when not (is_integer(ReconnectStart) andalso
                      (ReconnectStart > 0) andalso
                      (ReconnectStart =< ?TIMEOUT_MAX_ERLANG div 1000)) ->
            {error, {node_reconnect_start_invalid, ReconnectStart}};
        [_, ReconnectStart, _, _, _, _, _ | _]
            when not (ReconnectStart >= ConnectTimeSeconds) ->
            {error, {node_reconnect_start_min, ConnectTimeSeconds}};
        [_, _, ReconnectDelay, _, _, _, _ | _]
            when not (is_integer(ReconnectDelay) andalso
                      (ReconnectDelay > 0) andalso
                      (ReconnectDelay =< ?TIMEOUT_MAX_ERLANG div 1000)) ->
            {error, {node_reconnect_delay_invalid, ReconnectDelay}};
        [_, _, ReconnectDelay, _, _, _, _ | _]
            when not (ReconnectDelay >= ConnectTimeSeconds) ->
            {error, {node_reconnect_delay_min, ConnectTimeSeconds}};
        [_, _, _, Listen, _, _, _ | _]
            when not ((Listen =:= visible) orelse
                      (Listen =:= all)) ->
            {error, {node_listen_invalid, Listen}};
        [_, _, _, _, Connect, _, _ | _]
            when not ((Connect =:= visible) orelse
                      (Connect =:= hidden)) ->
            {error, {node_connect_invalid, Connect}};
        [_, _, _, _, _, TimestampType, _ | _]
            when not ((TimestampType =:= erlang) orelse
                      (TimestampType =:= os) orelse
                      (TimestampType =:= warp)) ->
            {error, {node_timestamp_type_invalid, TimestampType}};
        [_, _, _, _, _, _, Discovery | _]
            when not ((Discovery =:= undefined) orelse
                      is_list(Discovery)) ->
            {error, {node_discovery_invalid, Discovery}};
        [Nodes1, ReconnectStart, ReconnectDelay,
         Listen, Connect, TimestampType, Discovery] ->
            case nodes_elements_add(lists:delete(node(), Nodes1),
                                    NodesConfig#config_nodes{
                                        nodes = Nodes0,
                                        reconnect_start = ReconnectStart,
                                        reconnect_delay = ReconnectDelay,
                                        listen = Listen,
                                        connect = Connect,
                                        timestamp_type = TimestampType}) of
                {ok, NextNodesConfig} ->
                    case nodes_discovery_options(Discovery, NextNodesConfig) of
                        {ok, NewNodesConfig} ->
                            {ok, NewNodesConfig};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _ | Extra] ->
            {error, {node_invalid, Extra}}
    end.

nodes_proplist(Value) ->
    {NodeLiterals, Options} = lists:partition(fun erlang:is_atom/1, Value),
    Nodes = lists:delete(node(), lists:usort(NodeLiterals)),
    case nodes_validate(Nodes) of
        ok ->
            nodes_options(Nodes, Options);
        {error, _} = Error ->
            Error
    end.

logging_syslog_validate(undefined) ->
    {ok, undefined};
logging_syslog_validate([]) ->
    {ok, #config_logging_syslog{}};
logging_syslog_validate([_ | _] = Value) ->
    SyslogConfig = #config_logging_syslog{},
    Defaults = [
        {identity,
         SyslogConfig#config_logging_syslog.identity},
        {facility,
         SyslogConfig#config_logging_syslog.facility},
        {level,
         SyslogConfig#config_logging_syslog.level}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [Identity, _, _ | _]
            when not (is_list(Identity) andalso
                      (length(Identity) > 0) andalso
                      is_integer(hd(Identity))) ->
            {error, {logging_syslog_identity_invalid, Identity}};
        [_, Facility, _ | _]
            when not (is_atom(Facility) orelse
                      (is_integer(Facility) andalso
                       (Facility >= 0))) ->
            {error, {logging_syslog_facility_invalid, Facility}};
        [_, _, Level | _]
            when not ((Level =:= fatal) orelse (Level =:= error) orelse
                      (Level =:= warn) orelse (Level =:= info) orelse
                      (Level =:= debug) orelse (Level =:= trace) orelse
                      (Level =:= off) orelse (Level =:= undefined)) ->
            {error, {logging_syslog_level_invalid, Level}};
        [Identity, Facility, Level] ->
            try cloudi_x_syslog:facility(Facility) of
                _ when (Level =:= undefined) ->
                    {ok, undefined};
                _ ->
                    {ok,
                     SyslogConfig#config_logging_syslog{
                        identity = Identity,
                        facility = Facility,
                        level = Level}}
            catch
                error:badarg ->
                    {error, {logging_syslog_facility_invalid, Facility}}
            end;
        [_, _, _ | Extra] ->
            {error, {logging_syslog_invalid, Extra}}
    end.

logging_formatters_validate(undefined) ->
    {ok, undefined};
logging_formatters_validate([]) ->
    {ok, undefined};
logging_formatters_validate([_ | _] = Value) ->
    logging_formatters_validate(Value, [undefined],
                                #config_logging_formatters{}).

logging_formatters_validate([], Levels, FormattersConfig) ->
    {ok,
     FormattersConfig#config_logging_formatters{
         level = logging_level_highest(Levels)}};
logging_formatters_validate([{any, Options} | L], Levels,
                            #config_logging_formatters{
                                default = undefined} = FormattersConfig)
    when is_list(Options) ->
    case logging_formatter_validate(any, Options) of
        {ok, #config_logging_formatter{level = Level,
                                       output = Output} = Formatter} ->
            NewLevels = if
                Output =:= undefined ->
                    Levels;
                true ->
                    [Level | Levels]
            end,
            logging_formatters_validate(L, NewLevels,
                FormattersConfig#config_logging_formatters{
                    default = Formatter});
        {error, _} = Error ->
            Error
    end;
logging_formatters_validate([{[_ | _] = Modules, Options} | L], Levels,
                            #config_logging_formatters{
                                lookup = Lookup} = FormattersConfig)
    when is_list(Options) ->
    case (lists:all(fun is_atom/1, Modules) andalso
          (not cloudi_x_keys1value:is_key(Modules, Lookup))) of
        true ->
            case logging_formatter_validate(Modules, Options) of
                {ok, #config_logging_formatter{level = Level,
                                               output = Output} = Formatter} ->
                    NewLookup = cloudi_x_keys1value:
                                store(Modules, Formatter, Lookup),
                    NewLevels = if
                        Output =:= undefined ->
                            Levels;
                        true ->
                            [Level | Levels]
                    end,
                    logging_formatters_validate(L, NewLevels,
                        FormattersConfig#config_logging_formatters{
                            lookup = NewLookup});
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {logging_formatter_modules_invalid, Modules}}
    end;
logging_formatters_validate([Entry | _], _, _) ->
    {error, {logging_formatters_invalid, Entry}}.

% handle a conversion from lager log levels to CloudI log levels, if necessary
logging_formatter_level(fatal) ->
    {ok, fatal};
logging_formatter_level(emergency) ->
    {ok, fatal};
logging_formatter_level(alert) ->
    {ok, fatal};
logging_formatter_level(critical) ->
    {ok, fatal};
logging_formatter_level(error) ->
    {ok, error};
logging_formatter_level(warn) ->
    {ok, warn};
logging_formatter_level(warning) ->
    {ok, warn};
logging_formatter_level(notice) ->
    {ok, warn};
logging_formatter_level(info) ->
    {ok, info};
logging_formatter_level(debug) ->
    {ok, debug};
logging_formatter_level(trace) ->
    {ok, trace};
logging_formatter_level(off) ->
    {ok, off};
logging_formatter_level(none) ->
    {ok, off};
logging_formatter_level(undefined) ->
    {ok, off};
logging_formatter_level(Invalid) ->
    {error, {logging_formatter_level_invalid, Invalid}}.

logging_formatter_validate(Key, Value) ->
    NewValue = lists:map(fun(A) ->
        % handle lager logging level atoms in the proplist
        if
            is_atom(A) ->
                {level, A};
            true ->
                A
        end
    end, Value),
    FormatterConfig = #config_logging_formatter{},
    Defaults = [
        {level,
         FormatterConfig#config_logging_formatter.level},
        {output,
         FormatterConfig#config_logging_formatter.output},
        {output_args,
         FormatterConfig#config_logging_formatter.output_args},
        {output_max_r,
         FormatterConfig#config_logging_formatter.output_max_r},
        {output_max_t,
         FormatterConfig#config_logging_formatter.output_max_t},
        {formatter,
         FormatterConfig#config_logging_formatter.formatter},
        {formatter_config,
         FormatterConfig#config_logging_formatter.formatter_config}],
    case cloudi_proplists:take_values(Defaults, NewValue) of
        [Level, _, _, _, _, _, _ | _]
            when not is_atom(Level) ->
            {error, {logging_formatter_level_invalid, Level}};
        [_, Output, _, _, _, _, _ | _]
            when not is_atom(Output) ->
            {error, {logging_formatter_output_invalid, Output}};
        [_, _, OutputArgs, _, _, _, _ | _]
            when not is_list(OutputArgs) ->
            {error, {logging_formatter_output_args_invalid, OutputArgs}};
        [_, _, _, OutputMaxR, _, _, _ | _]
            when not (is_integer(OutputMaxR) andalso (OutputMaxR >= 0)) ->
            {error, {logging_formatter_output_max_r_invalid, OutputMaxR}};
        [_, _, _, _, OutputMaxT, _, _ | _]
            when not (is_integer(OutputMaxT) andalso (OutputMaxT >= 0)) ->
            {error, {logging_formatter_output_max_t_invalid, OutputMaxT}};
        [_, Output, _, _, _, Formatter, _ | _]
            when not (is_atom(Formatter) andalso
                      (not ((Output =:= undefined) andalso
                            (Formatter =:= undefined)))) ->
            {error, {logging_formatter_formatter_invalid, Formatter}};
        [_, _, _, _, _, _, Config | _]
            when not is_list(Config) ->
            {error, {logging_formatter_formatter_config_invalid, Config}};
        [Level, Output, OutputArgs, OutputMaxR, OutputMaxT,
         Formatter, Config] ->
            case logging_formatter_level(Level) of
                {ok, NewLevel} ->
                    NewOutputArgs = if
                        Output =:= undefined ->
                            OutputArgs;
                        true ->
                            LagerLevel = if
                                NewLevel =:= fatal ->
                                    emergency;
                                NewLevel =:= error ->
                                    error;
                                NewLevel =:= warn ->
                                    warning;
                                NewLevel =:= info ->
                                    info;
                                NewLevel =:= debug ->
                                    debug;
                                NewLevel =:= trace ->
                                    debug;
                                NewLevel =:= off ->
                                    none
                            end,
                            [{level, LagerLevel} | OutputArgs]
                    end,
                    OutputName = if
                        Output =:= undefined ->
                            undefined;
                        true ->
                            Instance = erlang:phash2({Key,
                                                      NewOutputArgs,
                                                      OutputMaxR, OutputMaxT,
                                                      Formatter, Config}),
                            ?LOGGING_FORMATTER_OUTPUT_ASSIGN(Output, Instance)
                    end,
                    {ok,
                     FormatterConfig#config_logging_formatter{
                         level = NewLevel,
                         output = Output,
                         output_name = OutputName,
                         output_args = NewOutputArgs,
                         output_max_r = OutputMaxR,
                         output_max_t = OutputMaxT,
                         formatter = Formatter,
                         formatter_config = Config}};
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _ | Extra] ->
            {error, {logging_formatter_invalid, Extra}}
    end.

