%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Configuration==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_configuration).
-author('mjtruog at protonmail dot com').

%% external interface
-export([load/1,
         acl_add/2,
         acl_remove/2,
         acl/1,
         services_add/3,
         services_remove/3,
         services_restart/3,
         services_suspend/3,
         services_resume/3,
         services_update/3,
         services_search/2,
         service_ids/1,
         services/1,
         service_format/1,
         services_format_options_internal/1,
         services_format_options_external/1,
         service_options_copy/3,
         nodes_set/2,
         nodes_get/1,
         nodes_add/2,
         nodes_remove/2,
         logging_level_highest/1,
         logging_set/2,
         logging_syslog_set/2,
         logging_formatters_set/2,
         logging/1,
         code_path_add/2,
         code_path_remove/2,
         code_path/1]).

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
     service_options_restart_all_invalid |
     service_options_restart_delay_invalid |
     service_options_scope_invalid |
     service_options_monkey_latency_invalid |
     service_options_monkey_chaos_invalid |
     service_options_automatic_loading_invalid |
     service_options_dispatcher_pid_options_invalid |
     service_options_aspects_init_invalid |
     service_options_aspects_request_invalid |
     service_options_aspects_info_invalid |
     service_options_aspects_terminate_invalid |
     service_options_limit_invalid |
     service_options_owner_invalid |
     service_options_nice_invalid |
     service_options_cgroup_invalid |
     service_options_chroot_invalid |
     service_options_syscall_lock_invalid |
     service_options_directory_invalid |
     service_options_application_name_invalid |
     service_options_init_pid_options_invalid |
     service_options_request_pid_uses_invalid |
     service_options_request_pid_options_invalid |
     service_options_info_pid_uses_invalid |
     service_options_info_pid_options_invalid |
     service_options_pid_invalid |
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
-type error_reason_services_suspend_configuration() ::
    {service_invalid |
     service_not_found, any()}.
-type error_reason_services_resume_configuration() ::
    {service_invalid |
     service_not_found, any()}.
-type error_reason_services_update_configuration() ::
    {update_invalid |
     service_update_invalid |
     service_update_type_invalid |
     service_update_module_invalid |
     service_update_module_state_invalid |
     service_update_file_path_invalid |
     service_update_args_invalid |
     service_update_env_invalid |
     service_update_sync_invalid |
     service_update_modules_load_invalid |
     service_update_modules_unload_invalid |
     service_update_code_paths_add_invalid |
     service_update_code_paths_remove_invalid |
     service_update_dest_refresh_invalid |
     service_update_timeout_init_invalid |
     service_update_timeout_async_invalid |
     service_update_timeout_sync_invalid |
     service_update_dest_list_deny_invalid |
     service_update_dest_list_allow_invalid |
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
     service_options_monkey_latency_invalid |
     service_options_monkey_chaos_invalid |
     service_options_dispatcher_pid_options_invalid |
     service_options_aspects_init_invalid |
     service_options_aspects_request_invalid |
     service_options_aspects_info_invalid |
     service_options_aspects_terminate_invalid |
     service_options_init_pid_options_invalid |
     service_options_request_pid_uses_invalid |
     service_options_request_pid_options_invalid |
     service_options_info_pid_uses_invalid |
     service_options_info_pid_options_invalid |
     service_options_pid_invalid |
     service_options_hibernate_invalid |
     service_options_reload_invalid |
     service_options_limit_invalid |
     service_update_options_invalid, any()}.
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
     node_discovery_ec2_tags_invalid |
     node_cost_invalid |
     node_cost_value |
     node_cost_precision_invalid |
     node_log_reconnect_invalid, any()}.
-type error_reason_logging_set_configuration() ::
    {node_invalid |
     logging_invalid |
     logging_redirect_invalid |
     logging_file_invalid |
     logging_stdout_invalid |
     logging_level_invalid |
     logging_log_time_offset_invalid, any()} |
    error_reason_logging_syslog_set_configuration() |
    error_reason_logging_formatters_set_configuration().
-type error_reason_logging_syslog_set_configuration() ::
    {logging_syslog_invalid |
     logging_syslog_identity_invalid |
     logging_syslog_facility_invalid |
     logging_syslog_level_invalid |
     logging_syslog_transport_invalid |
     logging_syslog_transport_options_invalid |
     logging_syslog_protocol_invalid |
     logging_syslog_path_invalid |
     logging_syslog_host_invalid |
     logging_syslog_port_invalid, any()}.
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
-type error_reason_code_path_add_configuration() ::
    already_exists |
    bad_directory.
-type error_reason_code_path_remove_configuration() ::
    does_not_exist |
    bad_name.
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
-type error_reason_services_suspend() ::
    error_reason_services_suspend_configuration() |
    cloudi_core_i_configurator:error_reason_service_suspend().
-type error_reason_services_resume() ::
    error_reason_services_resume_configuration() |
    cloudi_core_i_configurator:error_reason_service_resume().
-type error_reason_services_update() ::
    error_reason_services_update_configuration().
-type error_reason_services_search() ::
    cloudi_core_i_configurator:error_reason_services_search().
-type error_reason_nodes_add() ::
    error_reason_nodes_add_configuration().
-type error_reason_nodes_remove() ::
    error_reason_nodes_remove_configuration().
-type error_reason_nodes_set() ::
    error_reason_nodes_set_configuration() |
    cloudi_core_i_configurator:error_reason_nodes_set().
-type error_reason_logging_set() ::
    error_reason_logging_set_configuration().
-type error_reason_logging_syslog_set() ::
    error_reason_logging_syslog_set_configuration().
-type error_reason_logging_formatters_set() ::
    error_reason_logging_formatters_set_configuration().
-type error_reason_code_path_add() ::
    error_reason_code_path_add_configuration().
-type error_reason_code_path_remove() ::
    error_reason_code_path_remove_configuration().
-type error_reason_code_status() ::
    cloudi_core_i_configurator:error_reason_code_status().
-export_type([error_reason_acl_add/0,
              error_reason_acl_remove/0,
              error_reason_services_add/0,
              error_reason_services_remove/0,
              error_reason_services_restart/0,
              error_reason_services_suspend/0,
              error_reason_services_resume/0,
              error_reason_services_update/0,
              error_reason_services_search/0,
              error_reason_nodes_add/0,
              error_reason_nodes_remove/0,
              error_reason_nodes_set/0,
              error_reason_logging_set/0,
              error_reason_logging_syslog_set/0,
              error_reason_logging_formatters_set/0,
              error_reason_code_path_add/0,
              error_reason_code_path_remove/0,
              error_reason_code_status/0]).
-type error_reason_code_configuration() ::
    {code_invalid |
     code_paths_invalid |
     code_modules_invalid |
     code_applications_invalid |
     code_releases_invalid, any()}.
-type error_reason_new() ::
    error_reason_acl_add_configuration() |
    error_reason_services_add_configuration() |
    error_reason_nodes_set_configuration() |
    error_reason_logging_set_configuration() |
    error_reason_code_configuration() |
    {invalid, any()}.

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

-define(PID_OPTIONS_DEFAULT, [link,{message_queue_data,on_heap}]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

% timeout for the functions below based on
% cloudi_service_api and the timeout_decr/1 use in cloudi_core_i_configurator
-type api_timeout_milliseconds() ::
    1..?TIMEOUT_MAX | infinity.

%%-------------------------------------------------------------------------
%% @doc
%% ===Process the CloudI configuration data.===
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
            new(Terms);
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
load(Terms) when is_list(Terms) ->
    new(Terms);
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
    {error, error_reason_acl_add_configuration()}.

acl_add([{A, [_ | _]} | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    case acl_lookup_add(Value, ACL) of
        {ok, ACLNew} ->
            {ok, Config#config{acl = ACLNew}};
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
    {error, error_reason_acl_remove_configuration()}.

acl_remove([A | _] = Value, #config{acl = ACL} = Config)
    when is_atom(A) ->
    ACLNew = lists:foldl(fun(E, D) -> maps:remove(E, D) end, ACL, Value),
    {ok, Config#config{acl = ACLNew}};
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
    maps:to_list(ACL).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add services based on the configuration format.===
%% @end
%%-------------------------------------------------------------------------

-spec services_add(Value :: nonempty_list(#internal{} | #external{} |
                                          cloudi_service_api:
                                          service_proplist()),
                   Config :: #config{},
                   Timeout :: api_timeout_milliseconds()) ->
    {ok, nonempty_list(cloudi_service_api:service_id()), #config{}} |
    {error, error_reason_services_add(), #config{}}.

services_add([T | _] = Value,
             #config{uuid_generator = UUID,
                     services = Services,
                     acl = ACL} = Config, Timeout)
    when is_record(T, internal); is_record(T, external); is_list(T) ->
    case services_validate(Value, UUID) of
        {ok, ValidatedServices, IDs} ->
            case services_acl_expand(ValidatedServices, ACL) of
                {ok, ServicesNext} ->
                    case services_add_service(ServicesNext, Timeout) of
                        {ok, ServicesNew} ->
                            {ok, IDs,
                             Config#config{services = Services ++
                                                      ServicesNew}};
                        {error, Reason, ServicesNew} ->
                            {error, Reason,
                             Config#config{services = Services ++
                                                      ServicesNew}}
                    end;
                {error, Reason} ->
                    {error, Reason, Config}
            end;
        {error, Reason} ->
            {error, Reason, Config}
    end;
services_add(Value, Config, _) ->
    {error, {service_invalid, Value}, Config}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_remove(Value :: nonempty_list(cloudi_service_api:service_id()),
                      Config :: #config{},
                      Timeout :: api_timeout_milliseconds()) ->
    {ok, #config{}} |
    {error, error_reason_services_remove()}.

services_remove([ID | _] = Value,
                #config{services = Services} = Config, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_remove_uuid(Value, Services, Timeout) of
        {ok, ServicesNew} ->
            {ok, Config#config{services = ServicesNew}};
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
                       Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, error_reason_services_restart()}.

services_restart([ID | _] = Value, #config{services = Services}, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_change_uuid(Value, Services, service_restart, Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end;
services_restart(Value, _, _) ->
    {error, {service_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Suspend services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_suspend(Value :: nonempty_list(cloudi_service_api:service_id()),
                       Config :: #config{},
                       Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, error_reason_services_suspend()}.

services_suspend([ID | _] = Value, #config{services = Services}, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_change_uuid(Value, Services, service_suspend, Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end;
services_suspend(Value, _, _) ->
    {error, {service_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Resume services based on their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_resume(Value :: nonempty_list(cloudi_service_api:service_id()),
                      Config :: #config{},
                      Timeout :: api_timeout_milliseconds()) ->
    ok |
    {error, error_reason_services_resume()}.

services_resume([ID | _] = Value, #config{services = Services}, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    case services_change_uuid(Value, Services, service_resume, Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end;
services_resume(Value, _, _) ->
    {error, {service_invalid, Value}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update services after checking their UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services_update(Plan :: list(),
                      Config :: #config{},
                      Timeout :: api_timeout_milliseconds()) ->
    {ok,
     {ok, nonempty_list(nonempty_list(cloudi_service_api:service_id()))} |
     {error,
      {nonempty_list(cloudi_service_api:service_id()),
       cloudi_core_i_configurator:error_reason_service_update()},
      nonempty_list(nonempty_list(cloudi_service_api:service_id()))},
     #config{}} |
    {error, error_reason_services_update_configuration()}.

services_update([_ | _] = Plan, Config, Timeout) ->
    services_update_plan(Plan, Config, Timeout);
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
%% ===Return a list of all the configured service UUIDs.===
%% @end
%%-------------------------------------------------------------------------

-spec service_ids(#config{}) ->
    list(cloudi_service_api:service_id()).

service_ids(#config{services = Services}) ->
    [begin
        case Service of
            #config_service_internal{uuid = ID} -> ID;
            #config_service_external{uuid = ID} -> ID
        end
     end || Service <- Services].

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
               timeout_init = ?TIMEOUT_INITIALIZE_FORMAT(TimeoutInit),
               timeout_async = ?TIMEOUT_SEND_ASYNC_FORMAT(TimeoutAsync),
               timeout_sync = ?TIMEOUT_SEND_SYNC_FORMAT(TimeoutSync),
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
               timeout_init = ?TIMEOUT_INITIALIZE_FORMAT(TimeoutInit),
               timeout_async = ?TIMEOUT_SEND_ASYNC_FORMAT(TimeoutAsync),
               timeout_sync = ?TIMEOUT_SEND_SYNC_FORMAT(TimeoutSync),
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
        Options#config_service_options.init_pid_options /=
        ?PID_OPTIONS_DEFAULT ->
            [{init_pid_options,
              pid_options_format(
                  Options#config_service_options.init_pid_options)} |
             OptionsList3];
        true ->
            OptionsList3
    end,
    OptionsList5 = if
        Options#config_service_options.request_pid_uses /=
        Defaults#config_service_options.request_pid_uses ->
            [{request_pid_uses,
              Options#config_service_options.request_pid_uses} |
             OptionsList4];
        true ->
            OptionsList4
    end,
    OptionsList6 = if
        Options#config_service_options.request_pid_options /=
        ?PID_OPTIONS_DEFAULT ->
            [{request_pid_options,
              pid_options_format(
                  Options#config_service_options.request_pid_options)} |
             OptionsList5];
        true ->
            OptionsList5
    end,
    OptionsList7 = if
        Options#config_service_options.info_pid_uses /=
        Defaults#config_service_options.info_pid_uses ->
            [{info_pid_uses,
              Options#config_service_options.info_pid_uses} |
             OptionsList6];
        true ->
            OptionsList6
    end,
    OptionsList8 = if
        Options#config_service_options.info_pid_options /=
        ?PID_OPTIONS_DEFAULT ->
            [{info_pid_options,
              pid_options_format(
                  Options#config_service_options.info_pid_options)} |
             OptionsList7];
        true ->
            OptionsList7
    end,
    OptionsList9 = if
        Options#config_service_options.duo_mode /=
        Defaults#config_service_options.duo_mode ->
            [{duo_mode,
              Options#config_service_options.duo_mode} |
             OptionsList8];
        true ->
            OptionsList8
    end,
    OptionsList10 = if
        Options#config_service_options.hibernate /=
        Defaults#config_service_options.hibernate ->
            [{hibernate,
              cloudi_core_i_rate_based_configuration:
              hibernate_format(
                  Options#config_service_options.hibernate)} |
             OptionsList9];
        true ->
            OptionsList9
    end,
    OptionsList11 = if
        Options#config_service_options.reload /=
        Defaults#config_service_options.reload ->
            [{reload,
              Options#config_service_options.reload} |
             OptionsList10];
        true ->
            OptionsList10
    end,
    lists:reverse(OptionsList11).

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
              ?LIMIT_FORMAT(Options#config_service_options
                            .request_timeout_immediate_max,
                            0, ?TIMEOUT_MAX_ERLANG)} |
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
              ?LIMIT_FORMAT(Options#config_service_options
                            .response_timeout_immediate_max,
                            0, ?TIMEOUT_MAX_ERLANG)} |
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
              ?LIMIT_FORMAT(Options#config_service_options.timeout_terminate,
                            ?TIMEOUT_TERMINATE_MIN, ?TIMEOUT_TERMINATE_MAX)} |
             OptionsList12];
        true ->
            OptionsList12
    end,
    OptionsList14 = if
        Options#config_service_options.restart_all /=
        Defaults#config_service_options.restart_all ->
            [{restart_all,
              Options#config_service_options.restart_all} |
             OptionsList13];
        true ->
            OptionsList13
    end,
    OptionsList15 = if
        Options#config_service_options.restart_delay /=
        Defaults#config_service_options.restart_delay ->
            [{restart_delay,
              cloudi_core_i_rate_based_configuration:
              restart_delay_format(
                  Options#config_service_options.restart_delay)} |
             OptionsList14];
        true ->
            OptionsList14
    end,
    OptionsList16 = if
        Options#config_service_options.scope /= ?SCOPE_DEFAULT ->
            [{scope,
              ?SCOPE_FORMAT(Options#config_service_options.scope)} |
             OptionsList15];
        true ->
            OptionsList15
    end,
    OptionsList17 = if
        Options#config_service_options.monkey_latency /=
        Defaults#config_service_options.monkey_latency ->
            [{monkey_latency,
              cloudi_core_i_runtime_testing:
              monkey_latency_format(
                  Options#config_service_options.monkey_latency)} |
             OptionsList16];
        true ->
            OptionsList16
    end,
    OptionsList18 = if
        Options#config_service_options.monkey_chaos /=
        Defaults#config_service_options.monkey_chaos ->
            [{monkey_chaos,
              cloudi_core_i_runtime_testing:
              monkey_chaos_format(
                  Options#config_service_options.monkey_chaos)} |
             OptionsList17];
        true ->
            OptionsList17
    end,
    OptionsList19 = if
        Options#config_service_options.automatic_loading /=
        Defaults#config_service_options.automatic_loading ->
            [{automatic_loading,
              Options#config_service_options.automatic_loading} |
             OptionsList18];
        true ->
            OptionsList18
    end,
    OptionsList20 = if
        Options#config_service_options.dispatcher_pid_options /=
        ?PID_OPTIONS_DEFAULT ->
            [{dispatcher_pid_options,
              pid_options_format(
                  Options#config_service_options.dispatcher_pid_options)} |
             OptionsList19];
        true ->
            OptionsList19
    end,
    OptionsList21 = if
        Options#config_service_options.aspects_init_after /=
        Defaults#config_service_options.aspects_init_after ->
            [{aspects_init_after,
              Options#config_service_options.aspects_init_after} |
             OptionsList20];
        true ->
            OptionsList20
    end,
    OptionsList22 = if
        Options#config_service_options.aspects_request_before /=
        Defaults#config_service_options.aspects_request_before ->
            [{aspects_request_before,
              Options#config_service_options.aspects_request_before} |
             OptionsList21];
        true ->
            OptionsList21
    end,
    OptionsList23 = if
        Options#config_service_options.aspects_request_after /=
        Defaults#config_service_options.aspects_request_after ->
            [{aspects_request_after,
              Options#config_service_options.aspects_request_after} |
             OptionsList22];
        true ->
            OptionsList22
    end,
    OptionsList24 = if
        Options#config_service_options.aspects_terminate_before /=
        Defaults#config_service_options.aspects_terminate_before ->
            [{aspects_terminate_before,
              Options#config_service_options.aspects_terminate_before} |
             OptionsList23];
        true ->
            OptionsList23
    end,
    OptionsList25 = if
        Options#config_service_options.limit /=
        Defaults#config_service_options.limit ->
            [{limit,
              Options#config_service_options.limit} |
             OptionsList24];
        true ->
            OptionsList24
    end,
    OptionsList26 = if
        Options#config_service_options.owner /=
        Defaults#config_service_options.owner ->
            [{owner,
              Options#config_service_options.owner} |
             OptionsList25];
        true ->
            OptionsList25
    end,
    OptionsList27 = if
        Options#config_service_options.nice /=
        Defaults#config_service_options.nice ->
            [{nice,
              Options#config_service_options.nice} |
             OptionsList26];
        true ->
            OptionsList26
    end,
    OptionsList28 = if
        Options#config_service_options.cgroup /=
        Defaults#config_service_options.cgroup ->
            [{cgroup,
              Options#config_service_options.cgroup} |
             OptionsList27];
        true ->
            OptionsList27
    end,
    OptionsList29 = if
        Options#config_service_options.chroot /=
        Defaults#config_service_options.chroot ->
            [{chroot,
              Options#config_service_options.chroot} |
             OptionsList28];
        true ->
            OptionsList28
    end,
    OptionsList30 = if
        Options#config_service_options.syscall_lock /=
        Defaults#config_service_options.syscall_lock ->
            [{syscall_lock,
              Options#config_service_options.syscall_lock} |
             OptionsList29];
        true ->
            OptionsList29
    end,
    OptionsList31 = if
        Options#config_service_options.directory /=
        Defaults#config_service_options.directory ->
            [{directory,
              Options#config_service_options.directory} |
             OptionsList30];
        true ->
            OptionsList30
    end,
    lists:reverse(OptionsList31).

%%-------------------------------------------------------------------------
%% @doc
%% ===Copy specific fields from validated service configuration options.===
%% @end
%%-------------------------------------------------------------------------

-spec service_options_copy(OptionsKeys :: list(atom()),
                           OptionsOld0 :: #config_service_options{},
                           OptionsNew :: #config_service_options{}) ->
    #config_service_options{}.

service_options_copy(OptionsKeys,
                     #config_service_options{} = OptionsOld0,
                     #config_service_options{} = OptionsNew) ->
    Fields = [undefined | record_info(fields, config_service_options)],
    OptionsOldN = lists:foldl(fun(OptionsKey, OptionsOld1) ->
        Index = cloudi_lists:index(OptionsKey, Fields),
        erlang:setelement(Index, OptionsOld1, element(Index, OptionsNew))
    end, OptionsOld0, OptionsKeys),
    OptionsOldN.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set CloudI nodes configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_set(Value :: cloudi_service_api:nodes_proplist(),
                Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_nodes_set_configuration()}.

nodes_set(Value, #config{} = Config) ->
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
                                        discovery = Discovery,
                                        cost = Cost,
                                        cost_precision = CostPrecision,
                                        log_reconnect = LogReconnect}}) ->
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
        (Listen =:= visible) andalso (Connect =:= visible);
        (Listen =:= all) andalso (Connect =:= hidden) ->
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
    NodesList8 = if
        Cost == Defaults#config_nodes.cost ->
            NodesList7;
        true ->
            [{cost, Cost} | NodesList7]
    end,
    NodesList9 = if
        CostPrecision == Defaults#config_nodes.cost_precision ->
            NodesList8;
        true ->
            [{cost_precision, CostPrecision} | NodesList8]
    end,
    NodesList10 = if
        LogReconnect == Defaults#config_nodes.log_reconnect ->
            NodesList9;
        true ->
            [{log_reconnect, LogReconnect} | NodesList9]
    end,
    lists:reverse(NodesList10).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_add(Value :: list(node()),
                Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_nodes_add_configuration()}.

nodes_add([A | _] = Value, #config{nodes = NodesConfig} = Config)
    when is_atom(A) ->
    case nodes_elements_add(Value, NodesConfig) of
        {ok, NodesConfigNew} ->
            {ok, Config#config{nodes = NodesConfigNew}};
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
    {error, error_reason_nodes_remove_configuration()}.

nodes_remove([A | _] = Value, #config{nodes = NodesConfig} = Config)
    when is_atom(A) ->
    case nodes_elements_remove(Value, NodesConfig) of
        {ok, NodesConfigNew} ->
            {ok, Config#config{nodes = NodesConfigNew}};
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
%% ===Set CloudI logging configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec logging_set(Value :: cloudi_service_api:logging_proplist(),
                  Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_logging_set_configuration()}.

logging_set([_ | _] = Value, #config{} = Config) ->
    case logging_proplist(Value) of
        {ok, LoggingConfig} ->
            {ok, Config#config{logging = LoggingConfig}};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set CloudI syslog configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec logging_syslog_set(Value :: cloudi_service_api:
                                  logging_syslog_set_proplist() |
                                  undefined,
                         Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_logging_syslog_set_configuration()}.

logging_syslog_set(Value, #config{logging = LoggingOld} = Config)
    when is_list(Value) orelse (Value =:= undefined) ->
    case logging_validate_syslog(Value) of
        {ok, SyslogConfig} ->
            {ok,
             Config#config{
                 logging = LoggingOld#config_logging{
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
                                      logging_formatters_set_proplist() |
                                      undefined,
                             Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_logging_formatters_set_configuration()}.

logging_formatters_set(Value, #config{logging = LoggingOld} = Config)
    when is_list(Value) orelse (Value =:= undefined) ->
    case logging_validate_formatters(Value) of
        {ok, FormattersConfig} ->
            {ok,
             Config#config{
                 logging = LoggingOld#config_logging{
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

logging(#config{logging = #config_logging{
                              file = File,
                              stdout = Stdout,
                              level = Level,
                              redirect = Redirect,
                              syslog = Syslog,
                              formatters = Formatters,
                              log_time_offset = LogTimeOffset,
                              aspects_log_before = AspectsLogBefore,
                              aspects_log_after = AspectsLogAfter}}) ->
    Defaults = #config_logging{},
    LoggingList0 = [],
    LoggingList1 = if
        File == Defaults#config_logging.file ->
            LoggingList0;
        true ->
            [{file, File} | LoggingList0]
    end,
    LoggingList2 = if
        Stdout == Defaults#config_logging.stdout ->
            LoggingList1;
        true ->
            [{stdout, Stdout} | LoggingList1]
    end,
    LoggingList3 = if
        Level =:= Defaults#config_logging.level ->
            LoggingList2;
        true ->
            [{level, Level} | LoggingList2]
    end,
    LoggingList4 = if
        Redirect =:= Defaults#config_logging.redirect ->
            LoggingList3;
        true ->
            [{redirect, Redirect} | LoggingList3]
    end,
    undefined = Defaults#config_logging.syslog,
    LoggingList5 = case Syslog of
        undefined ->
            LoggingList4;
        #config_logging_syslog{identity = SyslogIdentity,
                               facility = SyslogFacility,
                               level = SyslogLevel,
                               transport = SyslogTransport,
                               transport_options = SyslogTransportOptions,
                               protocol = SyslogProtocol,
                               path = SyslogPath,
                               host = SyslogHost,
                               port = SyslogPort} ->
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
            SyslogList4 = if
                SyslogTransport =:=
                SyslogDefaults#config_logging_syslog.transport ->
                    SyslogList3;
                true ->
                    [{transport, SyslogTransport} | SyslogList3]
            end,
            SyslogList5 = if
                SyslogTransportOptions =:=
                SyslogDefaults#config_logging_syslog.transport_options ->
                    SyslogList4;
                true ->
                    [{transport_options, SyslogTransportOptions} | SyslogList4]
            end,
            SyslogList6 = if
                SyslogProtocol =:=
                SyslogDefaults#config_logging_syslog.protocol ->
                    SyslogList5;
                true ->
                    [{protocol, SyslogProtocol} | SyslogList5]
            end,
            SyslogList7 = if
                SyslogPath =:=
                SyslogDefaults#config_logging_syslog.path ->
                    SyslogList6;
                true ->
                    [{path, SyslogPath} | SyslogList6]
            end,
            SyslogList8 = if
                SyslogHost =:=
                SyslogDefaults#config_logging_syslog.host ->
                    SyslogList7;
                true ->
                    [{host, SyslogHost} | SyslogList7]
            end,
            SyslogList9 = if
                SyslogPort =:=
                SyslogDefaults#config_logging_syslog.port ->
                    SyslogList8;
                true ->
                    [{port, SyslogPort} | SyslogList8]
            end,
            [{syslog, lists:reverse(SyslogList9)} | LoggingList4]
    end,
    undefined = Defaults#config_logging.formatters,
    LoggingList6 = case Formatters of
        undefined ->
            LoggingList5;
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
            [{formatters, FormattersList1} | LoggingList5]
           
    end,
    LoggingList7 = if
        LogTimeOffset =:= Defaults#config_logging.log_time_offset ->
            LoggingList6;
        true ->
            [{log_time_offset, LogTimeOffset} | LoggingList6]
    end,
    LoggingList8 = if
        AspectsLogBefore =:= Defaults#config_logging.aspects_log_before ->
            LoggingList7;
        true ->
            [{aspects_log_before, AspectsLogBefore} | LoggingList7]
    end,
    LoggingList9 = if
        AspectsLogAfter =:= Defaults#config_logging.aspects_log_after ->
            LoggingList8;
        true ->
            [{aspects_log_after, AspectsLogAfter} | LoggingList8]
    end,
    lists:reverse(LoggingList9).

-spec code_path_add(Path :: string(),
                    Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_code_path_add_configuration()}.

code_path_add(Path, #config{code = ConfigCode} = Config) ->
    #config_code{paths = Paths} = ConfigCode,
    case code_load_path_add(Path, Paths) of
        {ok, PathNormalized} ->
            {ok,
             Config#config{
                 code = ConfigCode#config_code{
                     paths = Paths ++ [PathNormalized]}}};
        {error, Reason, _} ->
            {error, Reason}
    end.

-spec code_path_remove(Path :: string(),
                       Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_code_path_remove_configuration()}.

code_path_remove(Path, #config{code = ConfigCode} = Config) ->
    #config_code{paths = Paths} = ConfigCode,
    case code_load_path_remove(Path, Paths) of
        {ok, PathsNew} ->
            {ok,
             Config#config{
                 code = ConfigCode#config_code{
                     paths = PathsNew}}};
        {error, _} = Error ->
            Error
    end.

-spec code_path(#config{}) ->
    list(string()).

code_path(#config{code = #config_code{paths = Paths}}) ->
    Paths.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec new(Terms :: list({atom(), any()})) ->
    {ok, #config{}} |
    {error,
     {configuration_invalid, any()} |
     error_reason_new()}.

new(Terms) ->
    Defaults = [
        {'code',               []},
        {'logging',            []},
        {'acl',                []},
        {'services',           []},
        {'nodes',              []}],
    case cloudi_proplists:take_values(Defaults, Terms) of
        [Code, Logging, ACL, Services, Nodes] ->
            case code_proplist(Code) of
                {ok, CodeConfig} ->
                    new([{'logging', Logging},
                         {'acl', ACL},
                         {'services', Services},
                         {'nodes', Nodes}],
                        #config{uuid_generator = uuid_generator(),
                                code = CodeConfig});
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _ | UnknownTerms] ->
            {error, {configuration_invalid, UnknownTerms}}
    end.

-spec new(Terms :: list({atom(), any()}),
          Config :: #config{}) ->
    {ok, #config{}} |
    {error, error_reason_new()}.

new([], #config{services = Services, acl = ACL} = Config) ->
    case services_acl_expand(Services, ACL) of
        {ok, ServicesNew} ->
            {ok, Config#config{services = ServicesNew}};
        {error, _} = Error ->
            Error
    end;
new([{ConfigSection, []} | Terms], Config)
    when ConfigSection =:= 'acl';
         ConfigSection =:= 'services';
         ConfigSection =:= 'nodes';
         ConfigSection =:= 'logging' ->
    new(Terms, Config);
new([{'acl', [_ | _] = Value} | Terms], Config) ->
    case acl_lookup_new(Value) of
        {ok, ACLNew} ->
            new(Terms, Config#config{acl = ACLNew});
        {error, _} = Error ->
            Error
    end;
new([{'services', [T | _] = Value} | Terms],
    #config{uuid_generator = UUID} = Config)
    when is_record(T, internal); is_record(T, external); is_list(T) ->
    case services_validate(Value, UUID) of
        {ok, ServicesNew, _IDs} ->
            new(Terms, Config#config{services = ServicesNew});
        {error, _} = Error ->
            Error
    end;
new([{'nodes', automatic} | Terms], Config) ->
    {ok, NodesConfig} = nodes_options([], [{discovery, [{multicast, []}]}]),
    new(Terms, Config#config{nodes = NodesConfig});
new([{'nodes', [_ | _] = Value} | Terms], Config) ->
    case nodes_proplist(Value) of
        {ok, NodesConfig} ->
            new(Terms, Config#config{nodes = NodesConfig});
        {error, _} = Error ->
            Error
    end;
new([{'logging', [_ | _] = Value} | Terms], Config) ->
    case logging_proplist(Value) of
        {ok, LoggingConfig} ->
            new(Terms, Config#config{logging = LoggingConfig});
        {error, _} = Error ->
            Error
    end;
new([Term | _], _) ->
    {error, {invalid, Term}}.

services_add_service(ServicesNext, Timeout) ->
    services_add_service(ServicesNext, [], Timeout).

services_add_service([], Added, _) ->
    {ok, lists:reverse(Added)};
services_add_service([Service | Services], Added, Timeout) ->
    case cloudi_core_i_configurator:service_start(Service, Timeout) of
        {ok, ServiceNew} ->
            services_add_service(Services, [ServiceNew | Added], Timeout);
        {error, Reason} ->
            {error, Reason, lists:reverse(Added)}
    end.

-spec services_validate(Services :: nonempty_list(#internal{} | #external{} |
                                                  cloudi_service_api:
                                                  service_proplist()),
                        UUID :: cloudi_x_uuid:state()) ->
    {ok,
     nonempty_list(#config_service_internal{} | #config_service_external{}),
     nonempty_list(cloudi_service_api:service_id())} |
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
      service_options_restart_all_invalid |
      service_options_restart_delay_invalid |
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
      service_options_nice_invalid |
      service_options_cgroup_invalid |
      service_options_chroot_invalid |
      service_options_syscall_lock_invalid |
      service_options_directory_invalid |
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
              is_integer(hd(Prefix))) ->
    {error, {service_internal_prefix_invalid, Prefix}};
services_validate([#internal{module = Module} | _], _, _, _)
    when not (is_atom(Module) orelse
              (is_list(Module) andalso
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
    when not ((is_integer(TimeoutInit) andalso
               (TimeoutInit >= ?TIMEOUT_INITIALIZE_MIN) andalso
               (TimeoutInit =< ?TIMEOUT_INITIALIZE_MAX)) orelse
              (TimeoutInit =:= limit_min) orelse
              (TimeoutInit =:= limit_max)) ->
    {error, {service_internal_timeout_init_invalid, TimeoutInit}};
services_validate([#internal{timeout_async = TimeoutAsync} | _], _, _, _)
    when not ((is_integer(TimeoutAsync) andalso
               (TimeoutAsync >= ?TIMEOUT_SEND_ASYNC_MIN) andalso
               (TimeoutAsync =< ?TIMEOUT_SEND_ASYNC_MAX)) orelse
              (TimeoutAsync =:= limit_min) orelse
              (TimeoutAsync =:= limit_max)) ->
    {error, {service_internal_timeout_async_invalid, TimeoutAsync}};
services_validate([#internal{timeout_sync = TimeoutSync} | _], _, _, _)
    when not ((is_integer(TimeoutSync) andalso
               (TimeoutSync >= ?TIMEOUT_SEND_SYNC_MIN) andalso
               (TimeoutSync =< ?TIMEOUT_SEND_SYNC_MAX)) orelse
              (TimeoutSync =:= limit_min) orelse
              (TimeoutSync =:= limit_max)) ->
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
    TimeoutInitValue = ?TIMEOUT_INITIALIZE_ASSIGN(TimeoutInit),
    TimeoutAsyncValue = ?TIMEOUT_SEND_ASYNC_ASSIGN(TimeoutAsync),
    TimeoutSyncValue = ?TIMEOUT_SEND_SYNC_ASSIGN(TimeoutSync),
    case service_name_valid(Prefix, service_internal_prefix_invalid) of
        ok ->
            case services_validate_options_internal(Options, CountProcess,
                                                    MaxR, MaxT) of
                {ok, TimeoutTermValue, OptionsNew} ->
                    {ID, UUIDNew} = cloudi_x_uuid:get_v1(UUID),
                    services_validate(L,
                                      [#config_service_internal{
                                           prefix = Prefix,
                                           module = Module,
                                           file_path = FilePath,
                                           args = Args,
                                           dest_refresh = DestRefresh,
                                           timeout_init = TimeoutInitValue,
                                           timeout_async = TimeoutAsyncValue,
                                           timeout_sync = TimeoutSyncValue,
                                           timeout_term = TimeoutTermValue,
                                           dest_list_deny = DestListDeny,
                                           dest_list_allow = DestListAllow,
                                           count_process = CountProcess,
                                           max_r = MaxR,
                                           max_t = MaxT,
                                           options = OptionsNew,
                                           uuid = ID} | Output],
                                      [ID | IDs],
                                      UUIDNew);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end).
-ifdef(CLOUDI_CORE_STANDALONE).
-compile({nowarn_unused_function,
          [{services_validate_options_external, 4},
           {services_validate_options_external_checks, 13},
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
              is_integer(hd(Prefix))) ->
    {error, {service_external_prefix_invalid, Prefix}};
services_validate([#external{file_path = FilePath} | _], _, _, _)
    when not (is_list(FilePath) andalso
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
    when not ((is_integer(TimeoutInit) andalso
               (TimeoutInit >= ?TIMEOUT_INITIALIZE_MIN) andalso
               (TimeoutInit =< ?TIMEOUT_INITIALIZE_MAX)) orelse
              (TimeoutInit =:= limit_min) orelse
              (TimeoutInit =:= limit_max)) ->
    {error, {service_external_timeout_init_invalid, TimeoutInit}};
services_validate([#external{timeout_async = TimeoutAsync} | _], _, _, _)
    when not ((is_integer(TimeoutAsync) andalso
               (TimeoutAsync >= ?TIMEOUT_SEND_ASYNC_MIN) andalso
               (TimeoutAsync =< ?TIMEOUT_SEND_ASYNC_MAX)) orelse
              (TimeoutAsync =:= limit_min) orelse
              (TimeoutAsync =:= limit_max)) ->
    {error, {service_external_timeout_async_invalid, TimeoutAsync}};
services_validate([#external{timeout_sync = TimeoutSync} | _], _, _, _)
    when not ((is_integer(TimeoutSync) andalso
               (TimeoutSync >= ?TIMEOUT_SEND_SYNC_MIN) andalso
               (TimeoutSync =< ?TIMEOUT_SEND_SYNC_MAX)) orelse
              (TimeoutSync =:= limit_min) orelse
              (TimeoutSync =:= limit_max)) ->
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
    ProtocolNew = if
        Protocol =:= default ->
            local;
        true ->
            Protocol
    end,
    BufferSizeNew = if
        BufferSize =:= default ->
            if
                ProtocolNew =:= tcp ->
                    ?DEFAULT_BUFFER_SIZE_TCP;
                ProtocolNew =:= udp ->
                    ?DEFAULT_BUFFER_SIZE_UDP;
                ProtocolNew =:= local ->
                    ?DEFAULT_BUFFER_SIZE_LOCAL
            end;
        true ->
            BufferSize
    end,
    TimeoutInitValue = ?TIMEOUT_INITIALIZE_ASSIGN(TimeoutInit),
    TimeoutAsyncValue = ?TIMEOUT_SEND_ASYNC_ASSIGN(TimeoutAsync),
    TimeoutSyncValue = ?TIMEOUT_SEND_SYNC_ASSIGN(TimeoutSync),
    case service_name_valid(Prefix, service_external_prefix_invalid) of
        ok ->
            case services_validate_options_external(Options, CountProcess,
                                                    MaxR, MaxT) of
                {ok, TimeoutTermValue, OptionsNew} ->
                    {ID, UUIDNew} = cloudi_x_uuid:get_v1(UUID),
                    services_validate(L,
                                      [#config_service_external{
                                           prefix = Prefix,
                                           file_path = FilePath,
                                           args = Args,
                                           env = Env,
                                           dest_refresh = DestRefresh,
                                           protocol = ProtocolNew,
                                           buffer_size = BufferSizeNew,
                                           timeout_init = TimeoutInitValue,
                                           timeout_async = TimeoutAsyncValue,
                                           timeout_sync = TimeoutSyncValue,
                                           timeout_term = TimeoutTermValue,
                                           dest_list_deny = DestListDeny,
                                           dest_list_allow = DestListAllow,
                                           count_process = CountProcess,
                                           count_thread = CountThread,
                                           max_r = MaxR,
                                           max_t = MaxT,
                                           options = OptionsNew,
                                           uuid = ID} | Output],
                                      [ID | IDs],
                                      UUIDNew);
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

timeout_terminate(undefined, undefined, undefined) ->
    {ok, undefined};
timeout_terminate(TimeoutTerminate, _, 0) ->
    Default = ?TIMEOUT_TERMINATE_DEFAULT,
    if
        TimeoutTerminate =:= undefined ->
            {ok, Default};
        TimeoutTerminate =:= limit_min ->
            {ok, ?TIMEOUT_TERMINATE_MIN};
        TimeoutTerminate =:= limit_max ->
            {ok, ?TIMEOUT_TERMINATE_MAX};
        is_integer(TimeoutTerminate) ->
            {ok, TimeoutTerminate}
    end;
timeout_terminate(TimeoutTerminate, 0, MaxT)
    when is_integer(MaxT) ->
    Default = erlang:min(?TIMEOUT_TERMINATE_CALC0(MaxT),
                         ?TIMEOUT_TERMINATE_MAX),
    if
        TimeoutTerminate =:= undefined ->
            {ok, Default};
        TimeoutTerminate =:= limit_min ->
            {ok, ?TIMEOUT_TERMINATE_MIN};
        TimeoutTerminate =:= limit_max ->
            {ok, ?TIMEOUT_TERMINATE_MAX};
        is_integer(TimeoutTerminate) ->
            {ok, TimeoutTerminate}
    end;
timeout_terminate(TimeoutTerminate, MaxR, MaxT)
    when is_integer(MaxR), is_integer(MaxT) ->
    Default = erlang:min(?TIMEOUT_TERMINATE_CALC1(MaxR, MaxT),
                         ?TIMEOUT_TERMINATE_MAX),
    if
        TimeoutTerminate =:= undefined ->
            {ok, Default};
        TimeoutTerminate =:= limit_min ->
            {ok, ?TIMEOUT_TERMINATE_MIN};
        TimeoutTerminate =:= limit_max ->
            {ok, ?TIMEOUT_TERMINATE_MAX};
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

pid_options_format(OptionsList0) ->
    OptionsList1 = lists:delete(link, OptionsList0),
    OptionsListN = case lists:keyfind(message_queue_data, 1, OptionsList1) of
        {message_queue_data, on_heap} ->
            lists:keydelete(message_queue_data, 1, OptionsList1);
        {message_queue_data, _} ->
            OptionsList1
    end,
    OptionsListN.

-spec services_validate_options_internal(OptionsList ::
                                             cloudi_service_api:
                                             service_options_internal(),
                                         CountProcess :: pos_integer() |
                                                         undefined,
                                         MaxR :: non_neg_integer() | undefined,
                                         MaxT :: cloudi_service_api:seconds() |
                                                 undefined) ->
    {ok,
     TimeoutTerminateNew ::
         cloudi_service_api:timeout_terminate_value_milliseconds(),
     #config_service_options{}} |
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
      service_options_restart_all_invalid |
      service_options_restart_delay_invalid |
      service_options_scope_invalid |
      service_options_monkey_latency_invalid |
      service_options_monkey_chaos_invalid |
      service_options_automatic_loading_invalid |
      service_options_dispatcher_pid_options_invalid |
      service_options_aspects_init_invalid |
      service_options_aspects_request_invalid |
      service_options_aspects_info_invalid |
      service_options_aspects_terminate_invalid |
      service_options_application_name_invalid |
      service_options_init_pid_options_invalid |
      service_options_request_pid_uses_invalid |
      service_options_request_pid_options_invalid |
      service_options_info_pid_uses_invalid |
      service_options_info_pid_options_invalid |
      service_options_pid_invalid |
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
        {restart_all,
         Options#config_service_options.restart_all},
        {restart_delay,
         Options#config_service_options.restart_delay},
        {scope,
         Options#config_service_options.scope},
        {monkey_latency,
         Options#config_service_options.monkey_latency},
        {monkey_chaos,
         Options#config_service_options.monkey_chaos},
        {automatic_loading,
         Options#config_service_options.automatic_loading},
        {dispatcher_pid_options,
         Options#config_service_options.dispatcher_pid_options},
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
        {init_pid_options,
         Options#config_service_options.init_pid_options},
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
        [PriorityDefault, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((PriorityDefault >= ?PRIORITY_HIGH) andalso
                  (PriorityDefault =< ?PRIORITY_LOW)) ->
            {error, {service_options_priority_default_invalid,
                     PriorityDefault}};
        [_, QueueLimit, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((QueueLimit =:= undefined) orelse
                  (is_integer(QueueLimit) andalso
                   (QueueLimit >= 0))) ->
            {error, {service_options_queue_limit_invalid,
                     QueueLimit}};
        [_, _, QueueSize, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((QueueSize =:= undefined) orelse
                  (is_integer(QueueSize) andalso
                   (QueueSize >= 1))) ->
            {error, {service_options_queue_size_invalid,
                     QueueSize}};
        [_, _, _, RateRequestMax, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((RateRequestMax =:= undefined) orelse
                  is_number(RateRequestMax) orelse
                  is_list(RateRequestMax)) ->
            {error, {service_options_rate_request_max_invalid,
                     RateRequestMax}};
        [_, _, _, _, DestRefreshStart, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshStart) andalso
                  (DestRefreshStart > ?TIMEOUT_DELTA) andalso
                  (DestRefreshStart =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_start_invalid,
                     DestRefreshStart}};
        [_, _, _, _, _, DestRefreshDelay, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshDelay) andalso
                  (DestRefreshDelay > ?TIMEOUT_DELTA) andalso
                  (DestRefreshDelay =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_delay_invalid,
                     DestRefreshDelay}};
        [_, _, _, _, _, _, RequestNameLookup, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((RequestNameLookup =:= sync) orelse
                  (RequestNameLookup =:= async)) ->
            {error, {service_options_request_name_lookup_invalid,
                     RequestNameLookup}};
        [_, _, _, _, _, _, _, RequestTimeoutAdjustment, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_boolean(RequestTimeoutAdjustment) ->
            {error, {service_options_request_timeout_adjustment_invalid,
                     RequestTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, RequestTimeoutImmediateMax, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((is_integer(RequestTimeoutImmediateMax) andalso
                   (RequestTimeoutImmediateMax >= 0) andalso
                   (RequestTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) orelse
                  (RequestTimeoutImmediateMax =:= limit_min) orelse
                  (RequestTimeoutImmediateMax =:= limit_max)) ->
            {error, {service_options_request_timeout_immediate_max_invalid,
                     RequestTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, ResponseTimeoutAdjustment, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_boolean(ResponseTimeoutAdjustment) ->
            {error, {service_options_response_timeout_adjustment_invalid,
                     ResponseTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, _, _, ResponseTimeoutImmediateMax, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((is_integer(ResponseTimeoutImmediateMax) andalso
                   (ResponseTimeoutImmediateMax >= 0) andalso
                   (ResponseTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) orelse
                  (ResponseTimeoutImmediateMax =:= limit_min) orelse
                  (ResponseTimeoutImmediateMax =:= limit_max)) ->
            {error, {service_options_response_timeout_immediate_max_invalid,
                     ResponseTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, _, _, CountProcessDynamic, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((CountProcessDynamic =:= false) orelse
                  is_list(CountProcessDynamic)) ->
            {error, {service_options_count_process_dynamic_invalid,
                     CountProcessDynamic}};
        [_, _, _, _, _, _, _, _, _, _, _, _, TimeoutTerminate,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((TimeoutTerminate =:= undefined) orelse
                  (is_integer(TimeoutTerminate) andalso
                   (TimeoutTerminate >= ?TIMEOUT_TERMINATE_MIN) andalso
                   (TimeoutTerminate =< ?TIMEOUT_TERMINATE_MAX)) orelse
                  (TimeoutTerminate =:= limit_min) orelse
                  (TimeoutTerminate =:= limit_max)) ->
            {error, {service_options_timeout_terminate_invalid,
                     TimeoutTerminate}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         RestartAll, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_boolean(RestartAll) ->
            {error, {service_options_restart_all_invalid,
                     RestartAll}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, RestartDelay, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((RestartDelay =:= false) orelse
                  is_list(RestartDelay)) ->
            {error, {service_options_restart_delay_invalid,
                     RestartDelay}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, Scope, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_atom(Scope) ->
            {error, {service_options_scope_invalid,
                     Scope}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, MonkeyLatency, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((MonkeyLatency =:= false) orelse
                  (MonkeyLatency =:= system) orelse
                  is_list(MonkeyLatency)) ->
            {error, {service_options_monkey_latency_invalid,
                     MonkeyLatency}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, MonkeyChaos, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not ((MonkeyChaos =:= false) orelse
                  (MonkeyChaos =:= system) orelse
                  is_list(MonkeyChaos)) ->
            {error, {service_options_monkey_chaos_invalid,
                     MonkeyChaos}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, AutomaticLoading, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_boolean(AutomaticLoading) ->
            {error, {service_options_automatic_loading_invalid,
                     AutomaticLoading}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, DispatcherPidOptions, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _]
        when not is_list(DispatcherPidOptions) ->
            {error, {service_options_dispatcher_pid_options_invalid,
                     DispatcherPidOptions}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         ApplicationName, _, _, _, _, _, _, _, _]
        when not is_atom(ApplicationName) ->
            {error, {service_options_application_name_invalid,
                     ApplicationName}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, InitPidOptions, _, _, _, _, _, _, _]
        when not is_list(InitPidOptions) ->
            {error, {service_options_init_pid_options_invalid,
                     InitPidOptions}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, RequestPidUses, _, _, _, _, _, _]
        when not ((RequestPidUses =:= infinity) orelse
                  (is_integer(RequestPidUses) andalso
                   (RequestPidUses >= 1))) ->
            {error, {service_options_request_pid_uses_invalid,
                     RequestPidUses}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, RequestPidOptions, _, _, _, _, _]
        when not is_list(RequestPidOptions) ->
            {error, {service_options_request_pid_options_invalid,
                     RequestPidOptions}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, InfoPidUses, _, _, _, _]
        when not ((InfoPidUses =:= infinity) orelse
                  (is_integer(InfoPidUses) andalso
                   (InfoPidUses >= 1))) ->
            {error, {service_options_info_pid_uses_invalid,
                     InfoPidUses}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, InfoPidOptions, _, _, _]
        when not is_list(InfoPidOptions) ->
            {error, {service_options_info_pid_options_invalid,
                     InfoPidOptions}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, DuoMode, _, _]
        when not is_boolean(DuoMode) ->
            {error, {service_options_duo_mode_invalid,
                     DuoMode}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, Hibernate, _]
        when not (is_boolean(Hibernate) orelse
                  is_list(Hibernate)) ->
            {error, {service_options_hibernate_invalid,
                     Hibernate}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, Reload]
        when not is_boolean(Reload) ->
            {error, {service_options_reload_invalid,
                     Reload}};
        [PriorityDefault, QueueLimit, QueueSize, RateRequestMax,
         DestRefreshStart, DestRefreshDelay, RequestNameLookup,
         RequestTimeoutAdjustment, RequestTimeoutImmediateMax,
         ResponseTimeoutAdjustment, ResponseTimeoutImmediateMax,
         CountProcessDynamic, TimeoutTerminate,
         RestartAll, RestartDelay, Scope,
         MonkeyLatency, MonkeyChaos, AutomaticLoading, DispatcherPidOptions,
         AspectsInitAfter, AspectsRequestBefore, AspectsRequestAfter,
         AspectsInfoBefore, AspectsInfoAfter, AspectsTerminateBefore,
         ApplicationName, InitPidOptions,
         RequestPidUses, RequestPidOptions, InfoPidUses, InfoPidOptions,
         DuoMode, Hibernate, Reload]
        when not ((DuoMode =:= true) andalso
                  (InfoPidUses =/= infinity)) ->
            QueueSizeNew = if
                QueueSize =:= undefined ->
                    undefined;
                is_integer(QueueSize) ->
                    QueueSize * 1024
            end,
            case services_validate_options_internal_checks(
                RateRequestMax,
                CountProcessDynamic,
                TimeoutTerminate,
                RestartDelay,
                MonkeyLatency,
                MonkeyChaos,
                DispatcherPidOptions,
                InitPidOptions,
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
                 RateRequestMaxNew,
                 CountProcessDynamicNew,
                 TimeoutTerminateNew,
                 RestartDelayNew,
                 MonkeyLatencyNew,
                 MonkeyChaosNew,
                 DispatcherPidOptionsNew,
                 InitPidOptionsNew,
                 RequestPidOptionsNew,
                 InfoPidOptionsNew,
                 HibernateNew,
                 AspectsInitAfterNew,
                 AspectsRequestBeforeNew,
                 AspectsRequestAfterNew,
                 AspectsInfoBeforeNew,
                 AspectsInfoAfterNew,
                 AspectsTerminateBeforeNew} ->
                    {ok,
                     TimeoutTerminateNew,
                     Options#config_service_options{
                         priority_default =
                             PriorityDefault,
                         queue_limit =
                             QueueLimit,
                         queue_size =
                             QueueSizeNew,
                         rate_request_max =
                             RateRequestMaxNew,
                         dest_refresh_start =
                             DestRefreshStart,
                         dest_refresh_delay =
                             DestRefreshDelay,
                         request_name_lookup =
                             RequestNameLookup,
                         request_timeout_adjustment =
                             RequestTimeoutAdjustment,
                         request_timeout_immediate_max =
                             ?LIMIT_ASSIGN(RequestTimeoutImmediateMax,
                                           0, ?TIMEOUT_MAX_ERLANG),
                         response_timeout_adjustment =
                             ResponseTimeoutAdjustment,
                         response_timeout_immediate_max =
                             ?LIMIT_ASSIGN(ResponseTimeoutImmediateMax,
                                           0, ?TIMEOUT_MAX_ERLANG),
                         count_process_dynamic =
                             CountProcessDynamicNew,
                         timeout_terminate =
                             TimeoutTerminate,
                         restart_all =
                             RestartAll,
                         restart_delay =
                             RestartDelayNew,
                         scope =
                             ?SCOPE_ASSIGN(Scope),
                         monkey_latency =
                             MonkeyLatencyNew,
                         monkey_chaos =
                             MonkeyChaosNew,
                         automatic_loading =
                             AutomaticLoading,
                         dispatcher_pid_options =
                             DispatcherPidOptionsNew,
                         aspects_init_after =
                             AspectsInitAfterNew,
                         aspects_request_before =
                             AspectsRequestBeforeNew,
                         aspects_request_after =
                             AspectsRequestAfterNew,
                         aspects_info_before =
                             AspectsInfoBeforeNew,
                         aspects_info_after =
                             AspectsInfoAfterNew,
                         aspects_terminate_before =
                             AspectsTerminateBeforeNew,
                         application_name =
                             ApplicationName,
                         init_pid_options =
                             InitPidOptionsNew,
                         request_pid_uses =
                             RequestPidUses,
                         request_pid_options =
                             RequestPidOptionsNew,
                         info_pid_uses =
                             InfoPidUses,
                         info_pid_options =
                             InfoPidOptionsNew,
                         duo_mode =
                             DuoMode,
                         hibernate =
                             HibernateNew,
                         reload =
                             Reload}};
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _] ->
            {error, {service_options_invalid, OptionsList}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _ | Extra] ->
            {error, {service_options_invalid, Extra}}
    end.

services_validate_options_internal_checks(RateRequestMax,
                                          CountProcessDynamic,
                                          TimeoutTerminate,
                                          RestartDelay,
                                          MonkeyLatency,
                                          MonkeyChaos,
                                          DispatcherPidOptions,
                                          InitPidOptions,
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
                                                 RestartDelay,
                                                 MonkeyLatency,
                                                 MonkeyChaos,
                                                 CountProcess,
                                                 MaxR,
                                                 MaxT) of
        {ok,
         RateRequestMaxNew,
         CountProcessDynamicNew,
         TimeoutTerminateNew,
         RestartDelayNew,
         MonkeyLatencyNew,
         MonkeyChaosNew} ->
            case services_validate_option_pid_options(DispatcherPidOptions,
                                                      InitPidOptions,
                                                      RequestPidOptions,
                                                      InfoPidOptions) of
                {ok, DispatcherPidOptionsNew,
                     InitPidOptionsNew,
                     RequestPidOptionsNew,
                     InfoPidOptionsNew} ->
                    case cloudi_core_i_rate_based_configuration:
                         hibernate_validate(Hibernate) of
                        {ok, HibernateNew} ->
                            case services_validate_option_aspects_internal(
                                AspectsInitAfter,
                                AspectsRequestBefore,
                                AspectsRequestAfter,
                                AspectsInfoBefore,
                                AspectsInfoAfter,
                                AspectsTerminateBefore,
                                AutomaticLoading) of
                                {ok,
                                 AspectsInitAfterNew,
                                 AspectsRequestBeforeNew,
                                 AspectsRequestAfterNew,
                                 AspectsInfoBeforeNew,
                                 AspectsInfoAfterNew,
                                 AspectsTerminateBeforeNew} ->
                                    {ok,
                                     RateRequestMaxNew,
                                     CountProcessDynamicNew,
                                     TimeoutTerminateNew,
                                     RestartDelayNew,
                                     MonkeyLatencyNew,
                                     MonkeyChaosNew,
                                     DispatcherPidOptionsNew,
                                     InitPidOptionsNew,
                                     RequestPidOptionsNew,
                                     InfoPidOptionsNew,
                                     HibernateNew,
                                     AspectsInitAfterNew,
                                     AspectsRequestBeforeNew,
                                     AspectsRequestAfterNew,
                                     AspectsInfoBeforeNew,
                                     AspectsInfoAfterNew,
                                     AspectsTerminateBeforeNew};
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
                                         CountProcess :: pos_integer() |
                                                         undefined,
                                         MaxR :: non_neg_integer() | undefined,
                                         MaxT :: cloudi_service_api:seconds() |
                                                 undefined) ->
    {ok,
     TimeoutTerminateNew ::
         cloudi_service_api:timeout_terminate_value_milliseconds(),
     #config_service_options{}} |
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
      service_options_restart_all_invalid |
      service_options_restart_delay_invalid |
      service_options_scope_invalid |
      service_options_monkey_latency_invalid |
      service_options_monkey_chaos_invalid |
      service_options_automatic_loading_invalid |
      service_options_dispatcher_pid_options_invalid |
      service_options_pid_invalid |
      service_options_aspects_init_invalid |
      service_options_aspects_request_invalid |
      service_options_aspects_terminate_invalid |
      service_options_limit_invalid |
      service_options_owner_invalid |
      service_options_nice_invalid |
      service_options_cgroup_invalid |
      service_options_chroot_invalid |
      service_options_syscall_lock_invalid |
      service_options_directory_invalid |
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
        {restart_all,
         Options#config_service_options.restart_all},
        {restart_delay,
         Options#config_service_options.restart_delay},
        {scope,
         Options#config_service_options.scope},
        {monkey_latency,
         Options#config_service_options.monkey_latency},
        {monkey_chaos,
         Options#config_service_options.monkey_chaos},
        {automatic_loading,
         Options#config_service_options.automatic_loading},
        {dispatcher_pid_options,
         Options#config_service_options.dispatcher_pid_options},
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
         Options#config_service_options.owner},
        {nice,
         Options#config_service_options.nice},
        {cgroup,
         Options#config_service_options.cgroup},
        {chroot,
         Options#config_service_options.chroot},
        {syscall_lock,
         Options#config_service_options.syscall_lock},
        {directory,
         Options#config_service_options.directory}],
    case cloudi_proplists:take_values(Defaults, OptionsList) of
        [PriorityDefault, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((PriorityDefault >= ?PRIORITY_HIGH) andalso
                  (PriorityDefault =< ?PRIORITY_LOW)) ->
            {error, {service_options_priority_default_invalid,
                     PriorityDefault}};
        [_, QueueLimit, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((QueueLimit =:= undefined) orelse
                  (is_integer(QueueLimit) andalso
                   (QueueLimit >= 0))) ->
            {error, {service_options_queue_limit_invalid,
                     QueueLimit}};
        [_, _, QueueSize, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((QueueSize =:= undefined) orelse
                  (is_integer(QueueSize) andalso
                   (QueueSize >= 1))) ->
            {error, {service_options_queue_size_invalid,
                     QueueSize}};
        [_, _, _, RateRequestMax, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((RateRequestMax =:= undefined) orelse
                  is_number(RateRequestMax) orelse
                  is_list(RateRequestMax)) ->
            {error, {service_options_rate_request_max_invalid,
                     RateRequestMax}};
        [_, _, _, _, DestRefreshStart, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshStart) andalso
                  (DestRefreshStart > ?TIMEOUT_DELTA) andalso
                  (DestRefreshStart =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_start_invalid,
                     DestRefreshStart}};
        [_, _, _, _, _, DestRefreshDelay, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not (is_integer(DestRefreshDelay) andalso
                  (DestRefreshDelay > ?TIMEOUT_DELTA) andalso
                  (DestRefreshDelay =< ?TIMEOUT_MAX_ERLANG)) ->
            {error, {service_options_dest_refresh_delay_invalid,
                     DestRefreshDelay}};
        [_, _, _, _, _, _, RequestNameLookup, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((RequestNameLookup =:= sync) orelse
                  (RequestNameLookup =:= async)) ->
            {error, {service_options_request_name_lookup_invalid,
                     RequestNameLookup}};
        [_, _, _, _, _, _, _, RequestTimeoutAdjustment, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not is_boolean(RequestTimeoutAdjustment) ->
            {error, {service_options_request_timeout_adjustment_invalid,
                     RequestTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, RequestTimeoutImmediateMax, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((is_integer(RequestTimeoutImmediateMax) andalso
                   (RequestTimeoutImmediateMax >= 0) andalso
                   (RequestTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) orelse
                  (RequestTimeoutImmediateMax =:= limit_min) orelse
                  (RequestTimeoutImmediateMax =:= limit_max)) ->
            {error, {service_options_request_timeout_immediate_max_invalid,
                     RequestTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, ResponseTimeoutAdjustment, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not is_boolean(ResponseTimeoutAdjustment) ->
            {error, {service_options_response_timeout_adjustment_invalid,
                     ResponseTimeoutAdjustment}};
        [_, _, _, _, _, _, _, _, _, _, ResponseTimeoutImmediateMax, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((is_integer(ResponseTimeoutImmediateMax) andalso
                   (ResponseTimeoutImmediateMax >= 0) andalso
                   (ResponseTimeoutImmediateMax =< ?TIMEOUT_MAX_ERLANG)) orelse
                  (ResponseTimeoutImmediateMax =:= limit_min) orelse
                  (ResponseTimeoutImmediateMax =:= limit_max)) ->
            {error, {service_options_response_timeout_immediate_max_invalid,
                     ResponseTimeoutImmediateMax}};
        [_, _, _, _, _, _, _, _, _, _, _, CountProcessDynamic, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((CountProcessDynamic =:= false) orelse
                  is_list(CountProcessDynamic)) ->
            {error, {service_options_count_process_dynamic_invalid,
                     CountProcessDynamic}};
        [_, _, _, _, _, _, _, _, _, _, _, _, TimeoutTerminate, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((TimeoutTerminate =:= undefined) orelse
                  (is_integer(TimeoutTerminate) andalso
                   (TimeoutTerminate >= ?TIMEOUT_TERMINATE_MIN) andalso
                   (TimeoutTerminate =< ?TIMEOUT_TERMINATE_MAX))) ->
            {error, {service_options_timeout_terminate_invalid,
                     TimeoutTerminate}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, RestartAll, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not is_boolean(RestartAll) ->
            {error, {service_options_restart_all_invalid,
                     RestartAll}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, RestartDelay,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((RestartDelay =:= false) orelse
                  is_list(RestartDelay)) ->
            {error, {service_options_restart_delay_invalid,
                     RestartDelay}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         Scope, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not is_atom(Scope) ->
            {error, {service_options_scope_invalid,
                     Scope}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, MonkeyLatency, _, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((MonkeyLatency =:= false) orelse
                  (MonkeyLatency =:= system) orelse
                  is_list(MonkeyLatency)) ->
            {error, {service_options_monkey_latency_invalid,
                     MonkeyLatency}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, MonkeyChaos, _, _, _, _, _, _, _, _, _, _, _, _, _]
        when not ((MonkeyChaos =:= false) orelse
                  (MonkeyChaos =:= system) orelse
                  is_list(MonkeyChaos)) ->
            {error, {service_options_monkey_chaos_invalid,
                     MonkeyChaos}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, AutomaticLoading, _, _, _, _, _, _, _, _, _, _, _, _]
        when not is_boolean(AutomaticLoading) ->
            {error, {service_options_automatic_loading_invalid,
                     AutomaticLoading}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, DispatcherPidOptions, _, _, _, _, _, _, _, _, _, _, _]
        when not is_list(DispatcherPidOptions) ->
            {error, {service_options_dispatcher_pid_options_invalid,
                     DispatcherPidOptions}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, Nice, _, _, _, _]
        when not (is_integer(Nice) andalso
                  (Nice >= -20) andalso (Nice =< 20)) ->
            {error, {service_options_nice_invalid,
                     Nice}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, Chroot, _, _]
        when not ((Chroot =:= undefined) orelse
                  (is_list(Chroot) andalso
                   is_integer(hd(Chroot)))) ->
            {error, {service_options_chroot_invalid,
                     Chroot}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, SyscallLock, _]
        when not ((SyscallLock =:= undefined) orelse
                  (is_list(SyscallLock) andalso
                   is_tuple(hd(SyscallLock)))) ->
            {error, {service_options_syscall_lock_invalid,
                     SyscallLock}};
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Directory]
        when not ((Directory =:= undefined) orelse
                  (is_list(Directory) andalso
                   is_integer(hd(Directory)))) ->
            {error, {service_options_directory_invalid,
                     Directory}};
        [PriorityDefault, QueueLimit, QueueSize, RateRequestMax,
         DestRefreshStart, DestRefreshDelay, RequestNameLookup,
         RequestTimeoutAdjustment, RequestTimeoutImmediateMax,
         ResponseTimeoutAdjustment, ResponseTimeoutImmediateMax,
         CountProcessDynamic, TimeoutTerminate,
         RestartAll, RestartDelay, Scope,
         MonkeyLatency, MonkeyChaos,
         AutomaticLoading, DispatcherPidOptions,
         AspectsInitAfter, AspectsRequestBefore,
         AspectsRequestAfter, AspectsTerminateBefore,
         Limit, Owner, Nice, CGroup, Chroot, SyscallLock, Directory] ->
            QueueSizeNew = if
                QueueSize =:= undefined ->
                    undefined;
                is_integer(QueueSize) ->
                    QueueSize * 1024
            end,
            case services_validate_options_external_checks(RateRequestMax,
                                                           CountProcessDynamic,
                                                           TimeoutTerminate,
                                                           RestartDelay,
                                                           MonkeyLatency,
                                                           MonkeyChaos,
                                                           DispatcherPidOptions,
                                                           CountProcess,
                                                           MaxR,
                                                           MaxT,
                                                           Limit,
                                                           Owner,
                                                           CGroup,
                                                           SyscallLock) of
                {ok,
                 RateRequestMaxNew,
                 CountProcessDynamicNew,
                 TimeoutTerminateNew,
                 RestartDelayNew,
                 MonkeyLatencyNew,
                 MonkeyChaosNew,
                 DispatcherPidOptionsNew,
                 LimitNew,
                 OwnerNew,
                 CGroupNew,
                 SyscallLockNew} ->
                    case services_validate_option_aspects_external(
                        AspectsInitAfter,
                        AspectsRequestBefore,
                        AspectsRequestAfter,
                        AspectsTerminateBefore,
                        AutomaticLoading) of
                        {ok,
                         AspectsInitAfterNew,
                         AspectsRequestBeforeNew,
                         AspectsRequestAfterNew,
                         AspectsTerminateBeforeNew} ->
                            {ok,
                             TimeoutTerminateNew,
                             Options#config_service_options{
                                 priority_default =
                                     PriorityDefault,
                                 queue_limit =
                                     QueueLimit,
                                 queue_size =
                                     QueueSizeNew,
                                 rate_request_max =
                                     RateRequestMaxNew,
                                 dest_refresh_start =
                                     DestRefreshStart,
                                 dest_refresh_delay =
                                     DestRefreshDelay,
                                 request_name_lookup =
                                     RequestNameLookup,
                                 request_timeout_adjustment =
                                     RequestTimeoutAdjustment,
                                 request_timeout_immediate_max =
                                     ?LIMIT_ASSIGN(RequestTimeoutImmediateMax,
                                                   0, ?TIMEOUT_MAX_ERLANG),
                                 response_timeout_adjustment =
                                     ResponseTimeoutAdjustment,
                                 response_timeout_immediate_max =
                                     ?LIMIT_ASSIGN(ResponseTimeoutImmediateMax,
                                                   0, ?TIMEOUT_MAX_ERLANG),
                                 count_process_dynamic =
                                     CountProcessDynamicNew,
                                 timeout_terminate =
                                     TimeoutTerminate,
                                 restart_all =
                                     RestartAll,
                                 restart_delay =
                                     RestartDelayNew,
                                 scope =
                                     ?SCOPE_ASSIGN(Scope),
                                 monkey_latency =
                                     MonkeyLatencyNew,
                                 monkey_chaos =
                                     MonkeyChaosNew,
                                 automatic_loading =
                                     AutomaticLoading,
                                 dispatcher_pid_options =
                                     DispatcherPidOptionsNew,
                                 aspects_init_after =
                                     AspectsInitAfterNew,
                                 aspects_request_before =
                                     AspectsRequestBeforeNew,
                                 aspects_request_after =
                                     AspectsRequestAfterNew,
                                 aspects_terminate_before =
                                     AspectsTerminateBeforeNew,
                                 limit =
                                     LimitNew,
                                 owner =
                                     OwnerNew,
                                 nice =
                                     Nice,
                                 cgroup =
                                     CGroupNew,
                                 chroot =
                                     Chroot,
                                 syscall_lock =
                                     SyscallLockNew,
                                 directory =
                                     Directory}};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _,
         _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _ | Extra] ->
            {error, {service_options_invalid, Extra}}
    end.

services_validate_options_external_checks(RateRequestMax,
                                          CountProcessDynamic,
                                          TimeoutTerminate,
                                          RestartDelay,
                                          MonkeyLatency,
                                          MonkeyChaos,
                                          DispatcherPidOptions,
                                          CountProcess,
                                          MaxR,
                                          MaxT,
                                          Limit,
                                          Owner,
                                          CGroup,
                                          SyscallLock) ->
    case services_validate_options_common_checks(RateRequestMax,
                                                 CountProcessDynamic,
                                                 TimeoutTerminate,
                                                 RestartDelay,
                                                 MonkeyLatency,
                                                 MonkeyChaos,
                                                 CountProcess,
                                                 MaxR,
                                                 MaxT) of
        {ok,
         RateRequestMaxNew,
         CountProcessDynamicNew,
         TimeoutTerminateNew,
         RestartDelayNew,
         MonkeyLatencyNew,
         MonkeyChaosNew} ->
            case eval([{DispatcherPidOptions,
                        fun services_validate_option_pid_options/1},
                       {Limit,
                        fun cloudi_core_i_os_process:
                            limit_validate/1},
                       {Owner,
                        fun cloudi_core_i_os_process:
                            owner_validate/1},
                       {CGroup,
                        fun cloudi_core_i_os_process:
                            cgroup_validate/1},
                       {SyscallLock,
                        fun cloudi_core_i_os_process:
                            syscall_lock_validate/1}]) of
                {ok,
                 DispatcherPidOptionsNew,
                 LimitNew,
                 OwnerNew,
                 CGroupNew,
                 SyscallLockNew} ->
                    {ok,
                     RateRequestMaxNew,
                     CountProcessDynamicNew,
                     TimeoutTerminateNew,
                     RestartDelayNew,
                     MonkeyLatencyNew,
                     MonkeyChaosNew,
                     DispatcherPidOptionsNew,
                     LimitNew,
                     OwnerNew,
                     CGroupNew,
                     SyscallLockNew};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

services_validate_options_common_checks(RateRequestMax,
                                        CountProcessDynamic,
                                        TimeoutTerminate,
                                        RestartDelay,
                                        MonkeyLatency,
                                        MonkeyChaos,
                                        CountProcess,
                                        MaxR,
                                        MaxT) ->
    eval([{RateRequestMax,
           fun cloudi_core_i_rate_based_configuration:
               rate_request_validate/1},
          {CountProcessDynamic,
           fun(Value) ->
               cloudi_core_i_rate_based_configuration:
               count_process_dynamic_validate(Value, CountProcess)
           end},
          {TimeoutTerminate,
           fun(Value) ->
               timeout_terminate(Value, MaxR, MaxT)
           end},
          {RestartDelay,
           fun cloudi_core_i_rate_based_configuration:
               restart_delay_validate/1},
          {MonkeyLatency,
           fun cloudi_core_i_runtime_testing:
               monkey_latency_validate/1},
          {MonkeyChaos,
           fun cloudi_core_i_runtime_testing:
               monkey_chaos_validate/1}]).

services_validate_option_pid_options(OptionsList0,
                                     OptionsList1,
                                     OptionsList2,
                                     OptionsList3) ->
    F = fun services_validate_option_pid_options/1,
    eval([{OptionsList0, F},
          {OptionsList1, F},
          {OptionsList2, F},
          {OptionsList3, F}]).

services_validate_option_pid_options(OptionsList) ->
    services_validate_option_pid_options(OptionsList, [link]).

services_validate_option_pid_options([], Output0) ->
    OutputN = case lists:keyfind(message_queue_data, 1, Output0) of
        {message_queue_data, _} ->
            Output0;
        false ->
            % based on testing, the best default option for
            % CloudI service processes is on_heap
            [{message_queue_data, on_heap} | Output0]
    end,
    {ok, lists:reverse(OutputN)};
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
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{max_heap_size, V} = PidOption |
                                      OptionsList], Output)
    when is_integer(V) andalso V >= 0; is_map(V) ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{message_queue_data, V} = PidOption |
                                      OptionsList], Output)
    when (V =:= off_heap) orelse (V =:= on_heap) ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([{priority, V} = PidOption |
                                      OptionsList], Output)
    when (V =:= high) orelse (V =:= low) orelse (V =:= normal) ->
    services_validate_option_pid_options(OptionsList, [PidOption | Output]);
services_validate_option_pid_options([PidOption | _], _) ->
    {error, {service_options_pid_invalid, PidOption}}.

services_validate_option_aspects_terminate_before(AspectsTerminate,
                                                  AutomaticLoading) ->
    case validate_aspects_f(AspectsTerminate, 3, AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_terminate_before_invalid, Entry}}
    end.

services_validate_option_aspects_init_after_internal(AspectsInit,
                                                     AutomaticLoading) ->
    case validate_aspects_f(AspectsInit, 5, AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_init_after_invalid, Entry}}
    end.

services_validate_option_aspects_request_before_internal(AspectsRequest,
                                                         AutomaticLoading) ->
    case validate_aspects_f(AspectsRequest, 11, AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_request_before_invalid, Entry}}
    end.

services_validate_option_aspects_request_after_internal(AspectsRequest,
                                                        AutomaticLoading) ->
    case validate_aspects_f(AspectsRequest, 12, AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_request_after_invalid, Entry}}
    end.

services_validate_option_aspects_info_before_internal(AspectsInfo,
                                                      AutomaticLoading) ->
    case validate_aspects_f(AspectsInfo, 3, AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_info_before_invalid, Entry}}
    end.

services_validate_option_aspects_info_after_internal(AspectsInfo,
                                                      AutomaticLoading) ->
    case validate_aspects_f(AspectsInfo, 3, AutomaticLoading) of
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
    eval([{AspectsInitAfter,
           fun(Value) ->
               services_validate_option_aspects_init_after_internal(
                   Value, AutomaticLoading)
           end},
          {AspectsRequestBefore,
           fun(Value) ->
               services_validate_option_aspects_request_before_internal(
                   Value, AutomaticLoading)
           end},
          {AspectsRequestAfter,
           fun(Value) ->
               services_validate_option_aspects_request_after_internal(
                   Value, AutomaticLoading)
           end},
          {AspectsInfoBefore,
           fun(Value) ->
               services_validate_option_aspects_info_before_internal(
                   Value, AutomaticLoading)
           end},
          {AspectsInfoAfter,
           fun(Value) ->
               services_validate_option_aspects_info_after_internal(
                   Value, AutomaticLoading)
           end},
          {AspectsTerminateBefore,
           fun(Value) ->
               services_validate_option_aspects_terminate_before(
                   Value, AutomaticLoading)
           end}]).

services_validate_option_aspects_init_after_external(AspectsInit,
                                                     AutomaticLoading) ->
    case validate_aspects_f(AspectsInit, 4, AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_init_after_invalid, Entry}}
    end.

services_validate_option_aspects_request_before_external(AspectsRequest,
                                                         AutomaticLoading) ->
    case validate_aspects_f(AspectsRequest, 10, AutomaticLoading) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {service_options_aspects_request_before_invalid, Entry}}
    end.

services_validate_option_aspects_request_after_external(AspectsRequest,
                                                        AutomaticLoading) ->
    case validate_aspects_f(AspectsRequest, 11, AutomaticLoading) of
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
    eval([{AspectsInitAfter,
           fun(Value) ->
               services_validate_option_aspects_init_after_external(
                   Value, AutomaticLoading)
           end},
          {AspectsRequestBefore,
           fun(Value) ->
               services_validate_option_aspects_request_before_external(
                   Value, AutomaticLoading)
           end},
          {AspectsRequestAfter,
           fun(Value) ->
               services_validate_option_aspects_request_after_external(
                   Value, AutomaticLoading)
           end},
          {AspectsTerminateBefore,
           fun(Value) ->
               services_validate_option_aspects_terminate_before(
                   Value, AutomaticLoading)
           end}]).

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
    {ServiceList, ServicesNext} = lists:partition(fun(S) ->
        case S of
            #config_service_internal{uuid = ID} ->
                true;
            #config_service_external{uuid = ID} ->
                true;
            _ ->
                false
        end
    end, Services),
    case ServiceList of
        [] ->
            {error, {service_not_found, ID}};
        [Service] ->
            services_remove_uuid(IDs, [Service | RemoveServices],
                                 ServicesNext, Timeout)
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

services_change_uuid(Value, Services, Function, Timeout) ->
    services_change_uuid(Value, [], Services, Function, Timeout).

services_change_uuid([], ChangeServices, _, Function, Timeout) ->
    case services_change_all(lists:reverse(ChangeServices),
                             Function, Timeout) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end;
services_change_uuid([ID | IDs], ChangeServices, Services, Function, Timeout)
    when is_binary(ID), byte_size(ID) == 16 ->
    ServiceList = lists:filter(fun(S) ->
        case S of
            #config_service_internal{uuid = ID} ->
                true;
            #config_service_external{uuid = ID} ->
                true;
            _ ->
                false
        end
    end, Services),
    case ServiceList of
        [] ->
            {error, {service_not_found, ID}};
        [Service] ->
            services_change_uuid(IDs, [Service | ChangeServices],
                                 Services, Function, Timeout)
    end;
services_change_uuid([ID | _], _, _, _, _) ->
    {error, {service_invalid, ID}}.

services_change_all([], _, _) ->
    ok;
services_change_all([Service | ChangeServices], Function, Timeout) ->
    case cloudi_core_i_configurator:Function(Service, Timeout) of
        ok ->
            services_change_all(ChangeServices, Function, Timeout);
        {error, _} = Error ->
            Error
    end.

services_update_plan(Value, Config, Timeout) ->
    services_update_plan(Value, [], Config, Timeout).

services_update_plan([], UpdatePlans, Config, Timeout) ->
    services_update_all(lists:reverse(UpdatePlans), Config, Timeout);
services_update_plan([{ID, Plan} | L], UpdatePlans,
                     #config{services = Services, acl = ACL} = Config, Timeout)
    when is_binary(ID), (byte_size(ID) == 16) orelse (byte_size(ID) == 0) ->
    UpdatePlan = #config_service_update{},
    Defaults = [
        {type,
         UpdatePlan#config_service_update.type},
        {module,
         UpdatePlan#config_service_update.module},
        {module_state,
         UpdatePlan#config_service_update.module_state},
        {file_path,
         UpdatePlan#config_service_update.file_path},
        {args,
         UpdatePlan#config_service_update.args},
        {env,
         UpdatePlan#config_service_update.env},
        {sync,
         UpdatePlan#config_service_update.sync},
        {modules_load,
         UpdatePlan#config_service_update.modules_load},
        {modules_unload,
         UpdatePlan#config_service_update.modules_unload},
        {code_paths_add,
         UpdatePlan#config_service_update.code_paths_add},
        {code_paths_remove,
         UpdatePlan#config_service_update.code_paths_remove},
        {dest_refresh,
         UpdatePlan#config_service_update.dest_refresh},
        {timeout_init,
         UpdatePlan#config_service_update.timeout_init},
        {timeout_async,
         UpdatePlan#config_service_update.timeout_async},
        {timeout_sync,
         UpdatePlan#config_service_update.timeout_sync},
        {dest_list_deny,
         UpdatePlan#config_service_update.dest_list_deny},
        {dest_list_allow,
         UpdatePlan#config_service_update.dest_list_allow},
        {options,
         UpdatePlan#config_service_update.options}],
    case cloudi_proplists:take_values(Defaults, Plan) of
        [Type, _, _,
         _, _, _,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when not ((Type =:= undefined) orelse
                  (Type =:= internal) orelse (Type =:= external)) ->
            {error, {service_update_type_invalid,
                     Type}};
        [Type, Module, _,
         _, _, _,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when not ((Module =:= undefined) orelse
                  (((Type =:= undefined) orelse (Type =:= internal)) andalso
                   is_atom(Module))) ->
            {error, {service_update_module_invalid,
                     Module}};
        [Type, _, ModuleState,
         _, _, _,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when not ((ModuleState =:= undefined) orelse
                  (((Type =:= undefined) orelse (Type =:= internal)) andalso
                   (is_tuple(ModuleState) orelse
                    is_function(ModuleState, 3)))) ->
            {error, {service_update_module_state_invalid,
                     ModuleState}};
        [Type, _, _,
         FilePath, _, _,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when not ((FilePath =:= undefined) orelse
                  (((Type =:= undefined) orelse (Type =:= external)) andalso
                   (is_list(FilePath) andalso
                    is_integer(hd(FilePath))))) ->
            {error, {service_update_file_path_invalid,
                     FilePath}};
        [Type, _, _,
         _, Args, _,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when not ((Args =:= undefined) orelse
                  (((Type =:= undefined) orelse (Type =:= external)) andalso
                   (is_list(Args) andalso
                    ((Args == "") orelse
                     is_integer(hd(Args)))))) ->
            {error, {service_update_args_invalid,
                     Args}};
        [Type, _, _,
         _, _, Env,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when not ((Env =:= undefined) orelse
                  (((Type =:= undefined) orelse (Type =:= external)) andalso
                   is_list(Env))) ->
            {error, {service_update_env_invalid,
                     Env}};
        [_, _, _,
         _, _, _,
         Sync, _, _, _, _,
         _, _, _, _, _, _, _]
        when not is_boolean(Sync) ->
            {error, {service_update_sync_invalid,
                     Sync}};
        [_, _, _,
         _, _, _,
         _, ModulesLoad, _, _, _,
         _, _, _, _, _, _, _]
        when not (is_list(ModulesLoad) andalso
                  is_atom(hd(ModulesLoad))) ->
            {error, {service_update_modules_load_invalid,
                     ModulesLoad}};
        [_, _, _,
         _, _, _,
         _, _, ModulesUnload, _, _,
         _, _, _, _, _, _, _]
        when not (is_list(ModulesUnload) andalso
                  is_atom(hd(ModulesUnload))) ->
            {error, {service_update_modules_unload_invalid,
                     ModulesUnload}};
        [_, _, _,
         _, _, _,
         _, _, _, CodePathsAdd, _,
         _, _, _, _, _, _, _]
        when not (is_list(CodePathsAdd) andalso
                  is_list(hd(CodePathsAdd)) andalso
                  is_integer(hd(hd(CodePathsAdd)))) ->
            {error, {service_update_code_paths_add_invalid,
                     CodePathsAdd}};
        [_, _, _,
         _, _, _,
         _, _, _, _, CodePathsRemove,
         _, _, _, _, _, _, _]
        when not (is_list(CodePathsRemove) andalso
                  is_list(hd(CodePathsRemove)) andalso
                  is_integer(hd(hd(CodePathsRemove)))) ->
            {error, {service_update_code_paths_remove_invalid,
                     CodePathsRemove}};
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         DestRefresh, _, _, _, _, _, _]
        when not ((DestRefresh =:= undefined) orelse
                  (is_atom(DestRefresh) andalso
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
                    (DestRefresh =:= none)))) ->
            {error, {service_update_dest_refresh_invalid,
                     DestRefresh}};
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         _, TimeoutInit, _, _, _, _, _]
        when not ((TimeoutInit =:= undefined) orelse
                  (is_integer(TimeoutInit) andalso
                   (TimeoutInit >= ?TIMEOUT_INITIALIZE_MIN) andalso
                   (TimeoutInit =< ?TIMEOUT_INITIALIZE_MAX)) orelse
                  (TimeoutInit =:= limit_min) orelse
                  (TimeoutInit =:= limit_max)) ->
            {error, {service_update_timeout_init_invalid,
                     TimeoutInit}};
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         _, _, TimeoutAsync, _, _, _, _]
        when not ((TimeoutAsync =:= undefined) orelse
                  (is_integer(TimeoutAsync) andalso
                   (TimeoutAsync >= ?TIMEOUT_SEND_ASYNC_MIN) andalso
                   (TimeoutAsync =< ?TIMEOUT_SEND_ASYNC_MAX)) orelse
                  (TimeoutAsync =:= limit_min) orelse
                  (TimeoutAsync =:= limit_max)) ->
            {error, {service_update_timeout_async_invalid,
                     TimeoutAsync}};
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         _, _, _, TimeoutSync, _, _, _]
        when not ((TimeoutSync =:= undefined) orelse
                  (is_integer(TimeoutSync) andalso
                   (TimeoutSync >= ?TIMEOUT_SEND_SYNC_MIN) andalso
                   (TimeoutSync =< ?TIMEOUT_SEND_SYNC_MAX)) orelse
                  (TimeoutSync =:= limit_min) orelse
                  (TimeoutSync =:= limit_max)) ->
            {error, {service_update_timeout_sync_invalid,
                     TimeoutSync}};
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         DestRefresh, _, _, _, DestListDeny, _, _]
        when not ((DestListDeny =:= invalid) orelse
                  is_list(DestListDeny) orelse
                  (DestListDeny =:= undefined)) orelse
             ((DestRefresh =:= none) andalso is_list(DestListDeny)) ->
            {error, {service_update_dest_list_deny_invalid,
                     DestListDeny}};
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         DestRefresh, _, _, _, _, DestListAllow, _]
        when not ((DestListAllow =:= invalid) orelse
                  is_list(DestListAllow) orelse
                  (DestListAllow =:= undefined)) orelse
             ((DestRefresh =:= none) andalso is_list(DestListAllow)) ->
            {error, {service_update_dest_list_allow_invalid,
                     DestListAllow}};
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         _, _, _, _, _, _, Options]
        when not is_list(Options) ->
            {error, {service_update_options_invalid,
                     Options}};
        [Type, Module, _,
         FilePath, Args, Env,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when not (((Type =:= internal) andalso
                   (Module =:= undefined) andalso
                   (ID /= <<>>)) orelse
                  (((Type =:= internal) orelse
                    (Type =:= undefined)) andalso
                   (Module =/= undefined)) orelse
                  (((Type =:= external) orelse
                    (Type =:= undefined)) andalso 
                   ((FilePath =/= undefined) orelse
                    (Args =/= undefined) orelse
                    (Env =/= undefined)))) ->
            {error, {service_update_type_invalid,
                     Type}};
        [Type, Module, ModuleState,
         FilePath, Args, Env,
         _, _, _, _, _,
         _, _, _, _, _, _, _]
        when ((Module =/= undefined) orelse
              (ModuleState =/= undefined)) andalso
             ((FilePath =/= undefined) orelse
              (Args =/= undefined) orelse
              (Env =/= undefined)) ->
            {error, {service_update_type_invalid,
                     Type}};
        [Type, Module, ModuleState,
         _, _, _,
         _, ModulesLoad, ModulesUnload, _, _,
         DestRefresh, TimeoutInit, TimeoutAsync, TimeoutSync,
         DestListDeny, DestListAllow, Options]
        when ((Type =:= internal) orelse
              (Module =/= undefined)) andalso
             (ModuleState =:= undefined) andalso
             (ModulesLoad == []) andalso (ModulesUnload == []) andalso
             (DestRefresh =:= undefined) andalso
             (TimeoutInit =:= undefined) andalso
             (TimeoutAsync =:= undefined) andalso
             (TimeoutSync =:= undefined) andalso
             (DestListDeny =:= invalid) andalso
             (DestListAllow =:= invalid) andalso
             (Options == []) ->
            {error, {service_update_invalid,
                     no_update}};
        [Type, _, _,
         FilePath, Args, Env,
         _, ModulesLoad, ModulesUnload, _, _,
         DestRefresh, TimeoutInit, TimeoutAsync, TimeoutSync,
         DestListDeny, DestListAllow, Options]
        when (Type =:= external) andalso
             (FilePath =:= undefined) andalso
             (Args =:= undefined) andalso
             (Env =:= undefined) andalso
             (ModulesLoad == []) andalso (ModulesUnload == []) andalso
             (DestRefresh =:= undefined) andalso
             (TimeoutInit =:= undefined) andalso
             (TimeoutAsync =:= undefined) andalso
             (TimeoutSync =:= undefined) andalso
             (DestListDeny =:= invalid) andalso
             (DestListAllow =:= invalid) andalso
             (Options == []) ->
            {error, {service_update_invalid,
                     no_update}};
        [Type, Module, ModuleState,
         _, _, _,
         Sync, ModulesLoad, ModulesUnload, CodePathsAdd, CodePathsRemove,
         DestRefresh, TimeoutInit, TimeoutAsync, TimeoutSync,
         DestListDeny, DestListAllow, Options]
        when (Type =:= internal) orelse
             (Module =/= undefined) ->
            TimeoutInitValue = ?TIMEOUT_INITIALIZE_ASSIGN(TimeoutInit),
            TimeoutAsyncValue = ?TIMEOUT_SEND_ASYNC_ASSIGN(TimeoutAsync),
            TimeoutSyncValue = ?TIMEOUT_SEND_SYNC_ASSIGN(TimeoutSync),
            case services_update_plan_internal(Module, ModuleState,
                                               DestListDeny, DestListAllow,
                                               Options, ID, Services, ACL) of
                {ok, UpdateModule, IDs, ModuleStateNew,
                 DestListDenyNew, DestListAllowNew,
                 OptionsKeys, OptionsNew, ModuleVersion, ReloadStop} ->
                    UpdatePlansNew =
                        [UpdatePlan#config_service_update{
                             type = internal,
                             module = UpdateModule,
                             module_state = ModuleStateNew,
                             sync = Sync,
                             modules_load = ModulesLoad,
                             modules_unload = ModulesUnload,
                             code_paths_add = CodePathsAdd,
                             code_paths_remove = CodePathsRemove,
                             dest_refresh = DestRefresh,
                             timeout_init = TimeoutInitValue,
                             timeout_async = TimeoutAsyncValue,
                             timeout_sync = TimeoutSyncValue,
                             dest_list_deny = DestListDenyNew,
                             dest_list_allow = DestListAllowNew,
                             options_keys = OptionsKeys,
                             options = OptionsNew,
                             uuids = IDs,
                             module_version_old = ModuleVersion,
                             reload_stop = ReloadStop} |
                         UpdatePlans],
                    services_update_plan(L, UpdatePlansNew, Config, Timeout);
                {error, _} = Error ->
                    Error
            end;
        [Type, _, _,
         FilePath, Args, Env,
         Sync, ModulesLoad, ModulesUnload, CodePathsAdd, CodePathsRemove,
         DestRefresh, TimeoutInit, TimeoutAsync, TimeoutSync,
         DestListDeny, DestListAllow, Options]
        when (Type =:= external) orelse
             ((FilePath =/= undefined) orelse
              (Args =/= undefined) orelse
              (Env =/= undefined)) ->
            TimeoutInitValue = ?TIMEOUT_INITIALIZE_ASSIGN(TimeoutInit),
            TimeoutAsyncValue = ?TIMEOUT_SEND_ASYNC_ASSIGN(TimeoutAsync),
            TimeoutSyncValue = ?TIMEOUT_SEND_SYNC_ASSIGN(TimeoutSync),
            case services_update_plan_external(FilePath, Args, Env,
                                               DestListDeny, DestListAllow,
                                               Options, ID, Services, ACL) of
                {ok, DestListDenyNew, DestListAllowNew,
                 OptionsKeys, OptionsNew, SpawnOsProcess} ->
                    UpdatePlansNew =
                        [UpdatePlan#config_service_update{
                             type = external,
                             file_path = FilePath,
                             args = Args,
                             env = Env,
                             sync = Sync,
                             modules_load = ModulesLoad,
                             modules_unload = ModulesUnload,
                             code_paths_add = CodePathsAdd,
                             code_paths_remove = CodePathsRemove,
                             dest_refresh = DestRefresh,
                             timeout_init = TimeoutInitValue,
                             timeout_async = TimeoutAsyncValue,
                             timeout_sync = TimeoutSyncValue,
                             dest_list_deny = DestListDenyNew,
                             dest_list_allow = DestListAllowNew,
                             options_keys = OptionsKeys,
                             options = OptionsNew,
                             uuids = [ID],
                             spawn_os_process = SpawnOsProcess} |
                         UpdatePlans],
                    services_update_plan(L, UpdatePlansNew, Config, Timeout);
                {error, _} = Error ->
                    Error
            end;
        [_, _, _,
         _, _, _,
         _, _, _, _, _,
         _, _, _, _, _, _, _ | Invalid] ->
            {error, {service_update_invalid,
                     Invalid}}
    end;
services_update_plan([{ID, _} | _], _, _, _) ->
    {error, {update_invalid, ID}}.

services_update_plan_internal(Module, ModuleState,
                              DestListDeny, DestListAllow,
                              Options, UpdateID, Services, ACL) ->
    UpdateModule = if
        Module =:= undefined ->
            case lists:keyfind(UpdateID, #config_service_internal.uuid,
                               Services) of
                #config_service_internal{module = UpdateIDModule} ->
                    UpdateIDModule;
                false ->
                    undefined
            end;
        true ->
            Module
    end,
    {ModuleIDs,
     ReloadStop} = lists:foldr(fun(S, {IDs, ReloadModule} = Next) ->
        case S of
            #config_service_internal{
                module = UpdateModule,
                options = #config_service_options{
                    reload = Reload},
                uuid = ID} ->
                {[ID | IDs], ReloadModule orelse Reload};
            _ ->
                Next
        end
    end, {[], false}, Services),
    {UpdateValid, UpdateIDs} = case ModuleIDs of
        [_ | _] when UpdateID == <<>> ->
            {true, ModuleIDs};
        [UpdateID] ->
            {true, ModuleIDs};
        [_ | _] when ModuleState =:= undefined ->
            {lists:member(UpdateID, ModuleIDs), [UpdateID]};
        _ ->
            {false, []}
    end,
    if
        UpdateValid =:= true ->
            case services_update_plan_module_state(ModuleState) of
                {ok, ModuleStateNew} ->
                    case service_acl_expand_lists(DestListDeny,
                                                  DestListAllow,
                                                  ACL) of
                        {ok, DestListDenyNew, DestListAllowNew} ->
                            case services_update_plan_options_internal(Options) of
                                {ok, OptionsNew} ->
                                    OptionsKeys = [Key || {Key, _} <- Options],
                                    ModuleVersion =
                                        cloudi_x_reltool_util:
                                        module_version(UpdateModule),
                                    {ok, UpdateModule, UpdateIDs,
                                     ModuleStateNew,
                                     DestListDenyNew, DestListAllowNew,
                                     OptionsKeys, OptionsNew,
                                     ModuleVersion, ReloadStop};
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        UpdateValid =:= false ->
            {error, {update_invalid, UpdateID}}
    end.

services_update_plan_external(FilePath, Args, Env,
                              DestListDeny, DestListAllow,
                              Options, UpdateID, Services, ACL) ->
    UpdateValid = if
        UpdateID == <<>> ->
            false;
        true ->
            case lists:keyfind(UpdateID, #config_service_external.uuid,
                               Services) of
                #config_service_external{} ->
                    true;
                false ->
                    false
            end
    end,
    if
        UpdateValid =:= true ->
            case service_acl_expand_lists(DestListDeny,
                                          DestListAllow,
                                          ACL) of
                {ok, DestListDenyNew, DestListAllowNew} ->
                    case services_update_plan_options_external(Options) of
                        {ok, OptionsNew} ->
                            OptionsKeys = [Key || {Key, _} <- Options],
                            SpawnOsProcess =
                                not ((FilePath =:= undefined) andalso
                                     (Args =:= undefined) andalso
                                     (Env =:= undefined)),
                            {ok, DestListDenyNew, DestListAllowNew,
                             OptionsKeys, OptionsNew, SpawnOsProcess};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        UpdateValid =:= false ->
            {error, {update_invalid, UpdateID}}
    end.

services_update_plan_options_internal(OptionsList) ->
    ValidKeys = [priority_default, queue_limit, queue_size,
                 rate_request_max, dest_refresh_start, dest_refresh_delay,
                 request_name_lookup,
                 request_timeout_adjustment, request_timeout_immediate_max,
                 response_timeout_adjustment, response_timeout_immediate_max,
                 monkey_latency, monkey_chaos,
                 dispatcher_pid_options,
                 aspects_init_after,
                 aspects_request_before, aspects_request_after,
                 aspects_info_before, aspects_info_after,
                 aspects_terminate_before,
                 init_pid_options,
                 request_pid_uses, request_pid_options,
                 info_pid_uses, info_pid_options,
                 hibernate, reload],
    case cloudi_proplists:delete_all(ValidKeys, OptionsList) of
        [] ->
            case services_validate_options_internal(OptionsList,
                                                    undefined,
                                                    undefined, undefined) of
                {ok, _, Options} ->
                    {ok, Options};
                {error, _} = Error ->
                    Error
            end;
        InvalidOptions ->
            {error, {service_update_options_invalid,
                     InvalidOptions}}
    end.

services_update_plan_options_external(OptionsList) ->
    ValidKeys = [priority_default, queue_limit, queue_size,
                 rate_request_max, dest_refresh_start, dest_refresh_delay,
                 request_name_lookup,
                 request_timeout_adjustment, request_timeout_immediate_max,
                 response_timeout_adjustment, response_timeout_immediate_max,
                 monkey_latency, monkey_chaos,
                 dispatcher_pid_options,
                 aspects_init_after,
                 aspects_request_before, aspects_request_after,
                 aspects_terminate_before,
                 limit],
    case cloudi_proplists:delete_all(ValidKeys, OptionsList) of
        [] ->
            case services_validate_options_external(OptionsList,
                                                    undefined,
                                                    undefined, undefined) of
                {ok, _, Options} ->
                    {ok, Options};
                {error, _} = Error ->
                    Error
            end;
        InvalidOptions ->
            {error, {service_update_options_invalid,
                     InvalidOptions}}
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

services_update_all(UpdatePlans, Config, Timeout) ->
    services_update_all(UpdatePlans, [], Config, Timeout).

services_update_all([], ServiceIdLists, Config, _) ->
    {ok, {ok, lists:reverse(ServiceIdLists)}, Config};
services_update_all([UpdatePlan | UpdatePlans], ServiceIdLists,
                    Config, Timeout) ->
    case cloudi_core_i_configurator:service_update(UpdatePlan, Timeout) of
        {ok, ServiceIdList} ->
            ConfigNew = service_update_success(ServiceIdList,
                                               UpdatePlan, Config),
            services_update_all(UpdatePlans, [ServiceIdList | ServiceIdLists],
                                service_update_done(UpdatePlan, ConfigNew),
                                Timeout);
        {error, ServiceIdList, Reason} ->
            Error = {error,
                     {ServiceIdList, Reason},
                     lists:reverse(ServiceIdLists)},
            {ok, Error, service_update_done(UpdatePlan, Config)}
    end.

service_update_done(#config_service_update{
                        code_paths_add = PathsAdd,
                        code_paths_remove = PathsRemove},
                    #config{code = Code} = Config) ->
    % only configuration changes that always occur based on the update plan
    % (for both a successful update and one with an error)
    #config_code{paths = CodePaths0} = Code,
    % code_paths_add uses code:add_patha/1
    % code_paths_remove uses code:del_path/1 (path may not exist in config)
    PathsAddNormalized = [path_normalize(Path) || Path <- PathsAdd],
    PathsRemoveNormalized = [path_normalize(Path) || Path <- PathsRemove],
    CodePaths1 = lists:reverse(PathsAddNormalized, CodePaths0),
    CodePathsN = CodePaths1 -- PathsRemoveNormalized,
    CodeNew = Code#config_code{paths = CodePathsN},
    Config#config{code = CodeNew}.

service_update_success(IDs,
                       #config_service_update{
                           type = internal} = UpdatePlan,
                       #config{services = Services0} = Config) ->
    ServicesN = lists:foldl(fun(ID, Services1) ->
        #config_service_internal{} = Service0 =
            lists:keyfind(ID, #config_service_internal.uuid, Services1),
        ServiceN = service_update_success_common(Service0, UpdatePlan),
        lists:keystore(ID, #config_service_internal.uuid, Services1, ServiceN)
    end, Services0, IDs),
    Config#config{services = ServicesN};
service_update_success([ID],
                       #config_service_update{
                           type = external,
                           file_path = FilePath,
                           args = Args,
                           env = Env} = UpdatePlan,
                       #config{services = Services} = Config) ->
    #config_service_external{} = Service0 =
        lists:keyfind(ID, #config_service_external.uuid, Services),
    Service1 = if
        is_list(FilePath) ->
            Service0#config_service_external{
                file_path = FilePath};
        FilePath =:= undefined ->
            Service0
    end,
    Service2 = if
        is_list(Args) ->
            Service1#config_service_external{
                args = Args};
        Args =:= undefined ->
            Service1
    end,
    Service3 = if
        is_list(Env) ->
            Service2#config_service_external{
                env = Env};
        Env =:= undefined ->
            Service2
    end,
    ServiceN = service_update_success_common(Service3, UpdatePlan),
    ServicesNew = lists:keystore(ID, #config_service_external.uuid,
                                 Services, ServiceN),
    Config#config{services = ServicesNew}.

service_update_success_common(Service0,
                              #config_service_update{
                                  type = Type,
                                  dest_refresh = DestRefresh,
                                  timeout_init = TimeoutInit,
                                  timeout_async = TimeoutAsync,
                                  timeout_sync = TimeoutSync,
                                  dest_list_deny = DestListDeny,
                                  dest_list_allow = DestListAllow,
                                  options_keys = OptionsKeys,
                                  options = OptionsNew}) ->
    Service1 = if
        DestRefresh =:= undefined ->
            Service0;
        Type =:= internal ->
            Service0#config_service_internal{
                dest_refresh = DestRefresh};
        Type =:= external ->
            Service0#config_service_external{
                dest_refresh = DestRefresh}
    end,
    Service2 = if
        TimeoutInit =:= undefined ->
            Service1;
        Type =:= internal ->
            true = is_integer(TimeoutInit),
            Service1#config_service_internal{
                timeout_init = TimeoutInit};
        Type =:= external ->
            true = is_integer(TimeoutInit),
            Service1#config_service_external{
                timeout_init = TimeoutInit}
    end,
    Service3 = if
        TimeoutAsync =:= undefined ->
            Service2;
        Type =:= internal ->
            true = is_integer(TimeoutAsync),
            Service2#config_service_internal{
                timeout_async = TimeoutAsync};
        Type =:= external ->
            true = is_integer(TimeoutAsync),
            Service2#config_service_external{
                timeout_async = TimeoutAsync}
    end,
    Service4 = if
        TimeoutSync =:= undefined ->
            Service3;
        Type =:= internal ->
            true = is_integer(TimeoutSync),
            Service3#config_service_internal{
                timeout_sync = TimeoutSync};
        Type =:= external ->
            true = is_integer(TimeoutSync),
            Service3#config_service_external{
                timeout_sync = TimeoutSync}
    end,
    Service5 = if
        DestListDeny =:= invalid ->
            Service4;
        Type =:= internal ->
            Service4#config_service_internal{
                dest_list_deny = DestListDeny};
        Type =:= external ->
            Service4#config_service_external{
                dest_list_deny = DestListDeny}
    end,
    Service6 = if
        DestListAllow =:= invalid ->
            Service5;
        Type =:= internal ->
            Service5#config_service_internal{
                dest_list_allow = DestListAllow};
        Type =:= external ->
            Service5#config_service_external{
                dest_list_allow = DestListAllow}
    end,
    ServiceN = if
        Type =:= internal ->
            #config_service_internal{
                options = OptionsOld} = Service6,
            Service6#config_service_internal{
                options = service_options_copy(OptionsKeys,
                                               OptionsOld,
                                               OptionsNew)};
        Type =:= external ->
            #config_service_external{
                options = OptionsOld} = Service6,
            Service6#config_service_external{
                options = service_options_copy(OptionsKeys,
                                               OptionsOld,
                                               OptionsNew)}
    end,
    ServiceN.

services_acl_expand([], _) ->
    {ok, []};
services_acl_expand([_ | _] = Services, ACL) ->
    services_acl_expand(Services, [], ACL).

services_acl_expand([], Output, _) ->
    {ok, lists:reverse(Output)};
services_acl_expand([Service | Services], Output, ACL) ->
    case service_acl_expand(Service, ACL) of
        {ok, ServiceNew} ->
            services_acl_expand(Services, [ServiceNew | Output], ACL);
        {error, _} = Error ->
            Error
    end.

-define(CLOUDI_CORE_SUPPORT_INTERNAL_ACL,
service_acl_expand(#config_service_internal{
                       dest_list_deny = DestListDeny,
                       dest_list_allow = DestListAllow} = Service,
                   ACL) ->
    case service_acl_expand_lists(DestListDeny, DestListAllow, ACL) of
        {ok, DestListDenyNew, DestListAllowNew} ->
            {ok, Service#config_service_internal{
                     dest_list_deny = DestListDenyNew,
                     dest_list_allow = DestListAllowNew}};
        {error, _} = Error ->
            Error
    ).
-ifdef(CLOUDI_CORE_STANDALONE).
-define(CLOUDI_CORE_SUPPORT_EXTERNAL_ACL,
    end).
-else.
-define(CLOUDI_CORE_SUPPORT_EXTERNAL_ACL,
    end;
service_acl_expand(#config_service_external{
                       dest_list_deny = DestListDeny,
                       dest_list_allow = DestListAllow} = Service,
                   ACL) ->
    case service_acl_expand_lists(DestListDeny, DestListAllow, ACL) of
        {ok, DestListDenyNew, DestListAllowNew} ->
            {ok, Service#config_service_external{
                     dest_list_deny = DestListDenyNew,
                     dest_list_allow = DestListAllowNew}};
        {error, _} = Error ->
            Error
    end).
-endif.
?CLOUDI_CORE_SUPPORT_INTERNAL_ACL
?CLOUDI_CORE_SUPPORT_EXTERNAL_ACL
    .

service_acl_expand_lists(DestListDeny, DestListAllow, ACL) ->
    case service_acl_expand_list(DestListDeny, [], ACL) of
        {ok, DestListDenyNew} ->
            case service_acl_expand_list(DestListAllow, [], ACL) of
                {ok, DestListAllowNew} ->
                    {ok, DestListDenyNew, DestListAllowNew};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

service_acl_expand_list(invalid, _, _) ->
    {ok, invalid};
service_acl_expand_list(undefined, _, _) ->
    {ok, undefined};
service_acl_expand_list([], Output, _) ->
    {ok, lists:reverse(Output)};
service_acl_expand_list([E | L], Output, ACL)
    when is_atom(E) ->
    case maps:find(E, ACL) of
        {ok, Value} ->
            service_acl_expand_list(L, Value ++ Output, ACL);
        error ->
            {error, {acl_not_found, E}}
    end;
service_acl_expand_list([E | L], Output, ACL)
    when is_list(E), is_integer(hd(E)) ->
    try cloudi_x_trie:is_pattern2(E) of
        _ ->
            service_acl_expand_list(L, [E | Output], ACL)
    catch
        exit:badarg ->
            {error, {acl_invalid, E}}
    end.

acl_lookup_new(L) ->
    acl_lookup_add(L, #{}).

acl_lookup_add(L, ACLOld) ->
    case acl_store(L, ACLOld) of
        {ok, ACLNew} ->
            acl_update(L, ACLOld, ACLNew);
        {error, _} = Error ->
            Error
    end.

acl_store([], ACL) ->
    {ok, ACL};
acl_store([{Key, [_ | _] = Value} | L], ACL)
    when is_atom(Key) ->
    acl_store(L, maps:put(Key, Value, ACL));
acl_store([H | _], _) ->
    {error, {acl_invalid, H}}.

acl_update([], ACLFinal, _) ->
    {ok, ACLFinal};
acl_update([{Key, Value} | L], ACLFinal, ACLConfig) ->
    case acl_update_values(Value, [], [], Key, ACLConfig) of
        {ok, ValueNew} ->
            acl_update(L, maps:put(Key, ValueNew, ACLFinal),
                       ACLConfig);
        {error, _} = Error ->
            Error
    end.

acl_update_values([], Output, _Path, _Key, _ACL) ->
    {ok, lists:reverse(Output)};
acl_update_values([E | L], Output, Path, Key, ACL)
    when is_atom(E) ->
    case lists:member(E, Path) of
        true ->
            {error, {acl_cyclic, Key, E}};
        false ->
            case maps:find(E, ACL) of
                error ->
                    {error, {acl_not_found, E}};
                {ok, OtherL} ->
                    case acl_update_values(OtherL, Output,
                                           [E | Path], Key, ACL) of
                        {ok, OutputNew} ->
                            acl_update_values(L, OutputNew, Path, Key, ACL);
                        {error, _} = Error ->
                            Error
                    end
            end
    end;
acl_update_values([E | L], Output, Path, Key, ACL)
    when is_list(E), is_integer(hd(E)) ->
    try cloudi_x_trie:is_pattern2(E) of
        _ ->
            acl_update_values(L, [E | Output], Path, Key, ACL)
    catch
        exit:badarg ->
            {error, {acl_invalid, E}}
    end;
acl_update_values([E | _], _, _, _, _) ->
    {error, {acl_invalid, E}}.

service_name_valid(Name, ErrorReason) ->
    try cloudi_x_trie:is_pattern2(Name) of
        _ ->
            ok
    catch
        exit:badarg ->
            {error, {ErrorReason, Name}}
    end.

nodes_validate([]) ->
    ok;
nodes_validate([A | As])
    when is_atom(A) ->
    case validate_node(A) of
        ok ->
            nodes_validate(As);
        {error, _} = Error ->
            Error
    end;
nodes_validate([A | _]) ->
    {error, {node_invalid, A}}.

nodes_listen(undefined, #config_nodes{connect = Connect} = NodesConfig) ->
    Listen = if
        Connect =:= visible ->
            visible;
        Connect =:= hidden ->
            all
    end,
    {ok, NodesConfig#config_nodes{listen = Listen}};
nodes_listen(visible, #config_nodes{connect = hidden}) ->
    {error, {node_listen_invalid, visible}};
nodes_listen(Listen, NodesConfig) ->
    true = (Listen =:= visible) orelse (Listen =:= all),
    {ok, NodesConfig#config_nodes{listen = Listen}}.

nodes_elements_add([], NodesConfig) ->
    {ok, NodesConfig};
nodes_elements_add([A | As], NodesConfig)
    when A =:= node() ->
    nodes_elements_add(As, NodesConfig);
nodes_elements_add([A | As], #config_nodes{nodes = Nodes} = NodesConfig)
    when is_atom(A) ->
    case validate_node(A) of
        ok ->
            NodesNew = lists:umerge(Nodes, [A]),
            nodes_elements_add(As,
                               NodesConfig#config_nodes{nodes = NodesNew});
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
        NodesNew ->
            nodes_elements_remove(As,
                                  NodesConfig#config_nodes{nodes = NodesNew})
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
        [AccessKeyId, _, _, _, _]
            when not (is_list(AccessKeyId) orelse
                      is_integer(hd(AccessKeyId))) ->
            {error, {node_discovery_ec2_access_key_id_invalid,
                     AccessKeyId}};
        [_, SecretAccessKey, _, _, _]
            when not (is_list(SecretAccessKey) orelse
                      is_integer(hd(SecretAccessKey))) ->
            {error, {node_discovery_ec2_secret_access_key_invalid,
                     SecretAccessKey}};
        [_, _, Host, _, _]
            when not (is_list(Host) orelse
                      is_integer(hd(Host))) ->
            {error, {node_discovery_ec2_host_invalid,
                     Host}};
        [_, _, _, [], []] ->
            {error, {node_discovery_ec2_tags_selection_null,
                     []}};
        [_, _, _, Groups, _]
            when not is_list(Groups) ->
            {error, {node_discovery_ec2_groups_invalid,
                     Groups}};
        [_, _, _, _, Tags]
            when not is_list(Tags) ->
            {error, {node_discovery_ec2_tags_invalid,
                     Tags}};
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
        [Interface, _, _, _]
            when not is_tuple(Interface) ->
            {error, {node_discovery_multicast_interface_invalid,
                     Interface}};
        [_, Address, _, _]
            when not is_tuple(Address) ->
            {error, {node_discovery_multicast_address_invalid,
                     Address}};
        [_, _, Port, _]
            when not (is_integer(Port) andalso
                      (Port > 0)) ->
            {error, {node_discovery_multicast_port_invalid,
                     Port}};
        [_, _, _, TTL]
            when not (is_integer(TTL) andalso
                      (TTL >= 0)) ->
            {error, {node_discovery_multicast_ttl_invalid,
                     TTL}};
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
        [MulticastOptions, _]
            when not is_list(MulticastOptions) ->
            {error, {node_discovery_multicast_invalid,
                     MulticastOptions}};
        [_, EC2Options]
            when not is_list(EC2Options) ->
            {error, {node_discovery_ec2_invalid,
                     EC2Options}};
        [[_ | _], [_ | _]] ->
            {error, {node_discovery_ambiguous,
                     Value}};
        [MulticastOptions, []] ->
            nodes_discovery_multicast_options(MulticastOptions, NodesConfig);
        [[], EC2Options] ->
            nodes_discovery_ec2_options(EC2Options, NodesConfig);
        [_, _ | Extra] ->
            {error, {node_discovery_invalid, Extra}}
    end.

nodes_cost([]) ->
    ok;
nodes_cost([{Node, Value} | Cost]) ->
    if
        not is_atom(Node) ->
            {error, {node_cost_invalid, Node}};
        not (is_float(Value) andalso (Value > 0.0)) ->
            {error, {node_cost_value, Value}};
        Node =:= default ->
            nodes_cost(Cost);
        true ->
            case validate_node(Node) of
                ok ->
                    nodes_cost(Cost);
                {error, {node_invalid, Node}} ->
                    {error, {node_cost_invalid, Node}}
            end
    end.

nodes_cost(Cost, NodesConfig) ->
    case nodes_cost(Cost) of
        ok ->
            {ok, NodesConfig#config_nodes{cost = Cost}};
        {error, _} = Error ->
            Error
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
         undefined},
        {connect,
         NodesConfig#config_nodes.connect},
        {timestamp_type,
         NodesConfig#config_nodes.timestamp_type},
        {discovery,
         NodesConfig#config_nodes.discovery},
        {cost,
         NodesConfig#config_nodes.cost},
        {cost_precision,
         NodesConfig#config_nodes.cost_precision},
        {log_reconnect,
         NodesConfig#config_nodes.log_reconnect}],
    ConnectTimeSeconds = (cloudi_x_nodefinder:timeout_min() + 500) div 1000,
    case cloudi_proplists:take_values(Defaults, Value) of
        [Nodes1, _, _, _, _, _, _, _, _, _]
            when not is_list(Nodes1) ->
            {error, {node_invalid,
                     Nodes1}};
        [_, ReconnectStart, _, _, _, _, _, _, _, _]
            when not (is_integer(ReconnectStart) andalso
                      (ReconnectStart > 0) andalso
                      (ReconnectStart =< ?TIMEOUT_MAX_ERLANG div 1000)) ->
            {error, {node_reconnect_start_invalid,
                     ReconnectStart}};
        [_, ReconnectStart, _, _, _, _, _, _, _, _]
            when not (ReconnectStart >= ConnectTimeSeconds) ->
            {error, {node_reconnect_start_min,
                     ConnectTimeSeconds}};
        [_, _, ReconnectDelay, _, _, _, _, _, _, _]
            when not (is_integer(ReconnectDelay) andalso
                      (ReconnectDelay > 0) andalso
                      (ReconnectDelay =< ?TIMEOUT_MAX_ERLANG div 1000)) ->
            {error, {node_reconnect_delay_invalid,
                     ReconnectDelay}};
        [_, _, ReconnectDelay, _, _, _, _, _, _, _]
            when not (ReconnectDelay >= ConnectTimeSeconds) ->
            {error, {node_reconnect_delay_min,
                     ConnectTimeSeconds}};
        [_, _, _, Listen, _, _, _, _, _, _]
            when not ((Listen =:= visible) orelse
                      (Listen =:= all) orelse
                      (Listen =:= undefined)) ->
            {error, {node_listen_invalid,
                     Listen}};
        [_, _, _, _, Connect, _, _, _, _, _]
            when not ((Connect =:= visible) orelse
                      (Connect =:= hidden)) ->
            {error, {node_connect_invalid,
                     Connect}};
        [_, _, _, _, _, TimestampType, _, _, _, _]
            when not ((TimestampType =:= erlang) orelse
                      (TimestampType =:= os) orelse
                      (TimestampType =:= warp)) ->
            {error, {node_timestamp_type_invalid,
                     TimestampType}};
        [_, _, _, _, _, _, Discovery, _, _, _]
            when not ((Discovery =:= undefined) orelse
                      is_list(Discovery)) ->
            {error, {node_discovery_invalid,
                     Discovery}};
        [_, _, _, _, _, _, _, Cost, _, _]
            when not is_list(Cost) ->
            {error, {node_cost_invalid,
                     Cost}};
        [_, _, _, _, _, _, _, _, CostPrecision, _]
            when not (is_integer(CostPrecision) andalso
                      (CostPrecision >= 0) andalso
                      (CostPrecision =< 253)) ->
            {error, {node_cost_precision_invalid,
                     CostPrecision}};
        [_, _, _, _, _, _, _, _, _, LogReconnect]
            when not ((LogReconnect =:= fatal) orelse
                      (LogReconnect =:= error) orelse
                      (LogReconnect =:= warn) orelse
                      (LogReconnect =:= info) orelse
                      (LogReconnect =:= debug) orelse
                      (LogReconnect =:= trace) orelse
                      (LogReconnect =:= off)) ->
            {error, {node_log_reconnect_invalid,
                     LogReconnect}};
        [Nodes1, ReconnectStart, ReconnectDelay,
         Listen, Connect, TimestampType, Discovery, Cost, CostPrecision,
         LogReconnect] ->
            accum([{Listen,
                    fun nodes_listen/2},
                   {Nodes1,
                    fun nodes_elements_add/2},
                   {Discovery,
                    fun nodes_discovery_options/2},
                   {Cost,
                    fun nodes_cost/2}],
                  NodesConfig#config_nodes{
                      nodes = Nodes0,
                      reconnect_start = ReconnectStart,
                      reconnect_delay = ReconnectDelay,
                      connect = Connect,
                      timestamp_type = TimestampType,
                      cost_precision = CostPrecision,
                      log_reconnect = LogReconnect});
        [_, _, _, _, _, _, _, _, _, _ | Extra] ->
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

logging_proplist(Value) ->
    Logging = #config_logging{},
    Defaults = [
        {level, Logging#config_logging.level},
        {file, Logging#config_logging.file},
        {stdout, Logging#config_logging.stdout},
        {redirect, Logging#config_logging.redirect},
        {syslog, Logging#config_logging.syslog},
        {formatters, Logging#config_logging.formatters},
        {log_time_offset, Logging#config_logging.log_time_offset},
        {aspects_log_before, Logging#config_logging.aspects_log_before},
        {aspects_log_after, Logging#config_logging.aspects_log_after}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [Level, _, _, _, _, _, _, _, _]
            when not ((Level =:= fatal) orelse (Level =:= error) orelse
                      (Level =:= warn) orelse (Level =:= info) orelse
                      (Level =:= debug) orelse (Level =:= trace) orelse
                      (Level =:= off) orelse (Level =:= undefined)) ->
            {error, {logging_level_invalid,
                     Level}};
        [_, File, _, _, _, _, _, _, _]
            when not ((is_list(File) andalso
                       is_integer(hd(File))) orelse
                      (File =:= undefined))->
            {error, {logging_file_invalid,
                     File}};
        [_, _, Stdout, _, _, _, _, _, _]
            when not is_boolean(Stdout) ->
            {error, {logging_stdout_invalid,
                     Stdout}};
        [_, _, _, Redirect, _, _, _, _, _]
            when not is_atom(Redirect) ->
            {error, {logging_redirect_invalid,
                     Redirect}};
        [_, _, _, _, Syslog, _, _, _, _]
            when not ((Syslog =:= undefined) orelse
                      is_list(Syslog)) ->
            {error, {logging_syslog_invalid,
                     Syslog}};
        [_, _, _, _, _, _, LogTimeOffset, _, _]
            when not ((LogTimeOffset =:= fatal) orelse
                      (LogTimeOffset =:= error) orelse
                      (LogTimeOffset =:= warn) orelse
                      (LogTimeOffset =:= info) orelse
                      (LogTimeOffset =:= debug) orelse
                      (LogTimeOffset =:= trace) orelse
                      (LogTimeOffset =:= off)) ->
            {error, {logging_log_time_offset_invalid,
                     LogTimeOffset}};
        [Level, File, Stdout, Redirect, Syslog, Formatters,
         LogTimeOffset, AspectsLogBefore, AspectsLogAfter] ->
            FileNew = if
                Level =:= undefined ->
                    undefined;
                true ->
                    File
            end,
            LevelNew = if
                File =:= undefined ->
                    undefined;
                true ->
                    Level
            end,
            case logging_validate(Redirect, Syslog, Formatters,
                                  AspectsLogBefore, AspectsLogAfter) of
                {ok, Redirect, SyslogConfig, FormattersConfig,
                     AspectsLogBeforeNew, AspectsLogAfterNew} ->
                    LoggingNew = Logging#config_logging{
                                     level = LevelNew,
                                     file = FileNew,
                                     stdout = Stdout,
                                     redirect = Redirect,
                                     syslog = SyslogConfig,
                                     formatters = FormattersConfig,
                                     log_time_offset = LogTimeOffset,
                                     aspects_log_before = AspectsLogBeforeNew,
                                     aspects_log_after = AspectsLogAfterNew},
                    {ok, LoggingNew};
                {error, _} = Error ->
                    Error
            end;
        [_, _, _, _, _, _, _, _, _ | Extra] ->
            {error, {logging_invalid, Extra}}
    end.

logging_validate(Redirect, Syslog, Formatters,
                 AspectsLogBefore, AspectsLogAfter) ->
    eval([{Redirect,
           fun logging_validate_redirect/1},
          {Syslog,
           fun logging_validate_syslog/1},
          {Formatters,
           fun logging_validate_formatters/1},
          {AspectsLogBefore,
           fun logging_validate_aspects_log_before/1},
          {AspectsLogAfter,
           fun logging_validate_aspects_log_after/1}]).

logging_validate_redirect(undefined) ->
    {ok, undefined};
logging_validate_redirect(Redirect) ->
    case validate_node(Redirect) of
        ok ->
            {ok, Redirect};
        {error, _} = Error ->
            Error
    end.

logging_validate_syslog(undefined) ->
    {ok, undefined};
logging_validate_syslog([]) ->
    {ok, #config_logging_syslog{}};
logging_validate_syslog([_ | _] = Value) ->
    SyslogConfig = #config_logging_syslog{},
    Defaults = [
        {identity,
         SyslogConfig#config_logging_syslog.identity},
        {facility,
         SyslogConfig#config_logging_syslog.facility},
        {level,
         SyslogConfig#config_logging_syslog.level},
        {transport,
         SyslogConfig#config_logging_syslog.transport},
        {transport_options,
         SyslogConfig#config_logging_syslog.transport_options},
        {protocol,
         SyslogConfig#config_logging_syslog.protocol},
        {path,
         SyslogConfig#config_logging_syslog.path},
        {host,
         SyslogConfig#config_logging_syslog.host},
        {port,
         SyslogConfig#config_logging_syslog.port}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [Identity, _, _, _, _, _, _, _, _]
            when not (is_list(Identity) andalso
                      is_integer(hd(Identity))) ->
            {error, {logging_syslog_identity_invalid,
                     Identity}};
        [_, Facility, _, _, _, _, _, _, _]
            when not (is_atom(Facility) orelse
                      (is_integer(Facility) andalso
                       (Facility >= 0))) ->
            {error, {logging_syslog_facility_invalid,
                     Facility}};
        [_, _, Level, _, _, _, _, _, _]
            when not ((Level =:= fatal) orelse (Level =:= error) orelse
                      (Level =:= warn) orelse (Level =:= info) orelse
                      (Level =:= debug) orelse (Level =:= trace) orelse
                      (Level =:= off) orelse (Level =:= undefined)) ->
            {error, {logging_syslog_level_invalid, Level}};
        [_, _, Level, _, _, _, _, _, _]
            when (Level =:= undefined) ->
            {ok, undefined};
        [_, _, _, Transport, _, _, _, _, _]
            when not ((Transport =:= local) orelse (Transport =:= udp) orelse
                      (Transport =:= tcp) orelse (Transport =:= tls)) ->
            {error, {logging_syslog_transport_invalid,
                     Transport}};
        [_, _, _, _, TransportOptions, _, _, _, _]
            when not is_list(TransportOptions) ->
            {error, {logging_syslog_transport_options_invalid,
                     TransportOptions}};
        [_, _, _, _, _, Protocol, _, _, _]
            when not ((Protocol =:= rfc3164) orelse (Protocol =:= rfc5424)) ->
            {error, {logging_syslog_protocol_invalid,
                     Protocol}};
        [_, _, _, _, _, _, Path, _, _]
            when not (is_list(Path) andalso is_integer(hd(Path))) ->
            {error, {logging_syslog_path_invalid,
                     Path}};
        [_, _, _, _, _, _, _, Host, _]
            when not ((is_list(Host) andalso is_integer(hd(Host))) orelse
                      (is_tuple(Host) andalso
                       ((tuple_size(Host) == 4) orelse
                        (tuple_size(Host) == 8)))) ->
            {error, {logging_syslog_host_invalid,
                     Host}};
        [_, _, _, _, _, _, _, _, Port]
            when not ((Port =:= undefined) orelse
                      (is_integer(Port) andalso
                       (Port > 0) andalso (Port =< 65535))) ->
            {error, {logging_syslog_port_invalid,
                     Port}};
        [Identity, Facility, Level, Transport, TransportOptions,
         Protocol, Path, Host, Port] ->
            case cloudi_x_syslog_socket:facility_valid(Facility) of
                true ->
                    {ok,
                     SyslogConfig#config_logging_syslog{
                        identity = Identity,
                        facility = Facility,
                        level = Level,
                        transport = Transport,
                        transport_options = TransportOptions,
                        protocol = Protocol,
                        path = Path,
                        host = Host,
                        port = Port}};
                false ->
                    {error, {logging_syslog_facility_invalid,
                             Facility}}
            end;
        [_, _, _, _, _, _, _, _, _ | Extra] ->
            {error, {logging_syslog_invalid, Extra}}
    end.

logging_validate_formatters(undefined) ->
    {ok, undefined};
logging_validate_formatters([]) ->
    {ok, undefined};
logging_validate_formatters([_ | _] = Value) ->
    logging_validate_formatters(Value, [undefined],
                                #config_logging_formatters{}).

logging_validate_formatters([], Levels, FormattersConfig) ->
    {ok,
     FormattersConfig#config_logging_formatters{
         level = logging_level_highest(Levels)}};
logging_validate_formatters([{any, Options} | L], Levels,
                            #config_logging_formatters{
                                default = undefined} = FormattersConfig)
    when is_list(Options) ->
    case logging_validate_formatter(any, Options) of
        {ok, #config_logging_formatter{level = Level,
                                       output = Output} = Formatter} ->
            LevelsNew = if
                Output =:= undefined ->
                    Levels;
                true ->
                    [Level | Levels]
            end,
            logging_validate_formatters(L, LevelsNew,
                FormattersConfig#config_logging_formatters{
                    default = Formatter});
        {error, _} = Error ->
            Error
    end;
logging_validate_formatters([{[_ | _] = Modules, Options} | L], Levels,
                            #config_logging_formatters{
                                lookup = Lookup} = FormattersConfig)
    when is_list(Options) ->
    case (lists:all(fun is_atom/1, Modules) andalso
          (not cloudi_x_keys1value:is_key(Modules, Lookup))) of
        true ->
            case logging_validate_formatter(Modules, Options) of
                {ok, #config_logging_formatter{level = Level,
                                               output = Output} = Formatter} ->
                    LookupNew = cloudi_x_keys1value:
                                store(Modules, Formatter, Lookup),
                    LevelsNew = if
                        Output =:= undefined ->
                            Levels;
                        true ->
                            [Level | Levels]
                    end,
                    logging_validate_formatters(L, LevelsNew,
                        FormattersConfig#config_logging_formatters{
                            lookup = LookupNew});
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {logging_formatter_modules_invalid, Modules}}
    end;
logging_validate_formatters([Entry | _], _, _) ->
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

logging_validate_formatter(Key, Value) ->
    ValueNew = lists:map(fun(A) ->
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
    case cloudi_proplists:take_values(Defaults, ValueNew) of
        [Level, _, _, _, _, _, _ ]
            when not is_atom(Level) ->
            {error, {logging_formatter_level_invalid,
                     Level}};
        [_, Output, _, _, _, _, _]
            when not is_atom(Output) ->
            {error, {logging_formatter_output_invalid,
                     Output}};
        [_, _, OutputArgs, _, _, _, _]
            when not is_list(OutputArgs) ->
            {error, {logging_formatter_output_args_invalid,
                     OutputArgs}};
        [_, _, _, OutputMaxR, _, _, _]
            when not (is_integer(OutputMaxR) andalso (OutputMaxR >= 0)) ->
            {error, {logging_formatter_output_max_r_invalid,
                     OutputMaxR}};
        [_, _, _, _, OutputMaxT, _, _]
            when not (is_integer(OutputMaxT) andalso (OutputMaxT >= 0)) ->
            {error, {logging_formatter_output_max_t_invalid,
                     OutputMaxT}};
        [_, Output, _, _, _, Formatter, _]
            when not (is_atom(Formatter) andalso
                      (not ((Output =:= undefined) andalso
                            (Formatter =:= undefined)))) ->
            {error, {logging_formatter_formatter_invalid,
                     Formatter}};
        [_, _, _, _, _, _, Config]
            when not is_list(Config) ->
            {error, {logging_formatter_formatter_config_invalid,
                     Config}};
        [Level, Output, OutputArgs, OutputMaxR, OutputMaxT,
         Formatter, Config] ->
            case logging_formatter_level(Level) of
                {ok, LevelNew} ->
                    OutputArgsNew = if
                        Output =:= undefined ->
                            OutputArgs;
                        true ->
                            LagerLevel = if
                                LevelNew =:= fatal ->
                                    emergency;
                                LevelNew =:= error ->
                                    error;
                                LevelNew =:= warn ->
                                    warning;
                                LevelNew =:= info ->
                                    info;
                                LevelNew =:= debug ->
                                    debug;
                                LevelNew =:= trace ->
                                    debug;
                                LevelNew =:= off ->
                                    none
                            end,
                            [{level, LagerLevel} | OutputArgs]
                    end,
                    OutputName = if
                        Output =:= undefined ->
                            undefined;
                        true ->
                            Instance = erlang:phash2({Key,
                                                      OutputArgsNew,
                                                      OutputMaxR, OutputMaxT,
                                                      Formatter, Config}),
                            ?LOGGING_FORMATTER_OUTPUT_ASSIGN(Output, Instance)
                    end,
                    {ok,
                     FormatterConfig#config_logging_formatter{
                         level = LevelNew,
                         output = Output,
                         output_name = OutputName,
                         output_args = OutputArgsNew,
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

logging_validate_aspects_log_before(AspectsLogBefore) ->
    case validate_aspects_f(AspectsLogBefore, 10, true) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {logging_aspects_log_before_invalid, Entry}}
    end.

logging_validate_aspects_log_after(AspectsLogAfter) ->
    case validate_aspects_f(AspectsLogAfter, 10, true) of
        {ok, _} = Success ->
            Success;
        {error, Entry} ->
            {error, {logging_aspects_log_after_invalid, Entry}}
    end.

validate_aspects_f(Aspects, Arity, AutomaticLoading) ->
    validate_aspects_f(Aspects, [], Arity, AutomaticLoading).

validate_aspects_f([], AspectsNew, _, _) ->
    {ok, lists:reverse(AspectsNew)};
validate_aspects_f([F | AspectsOld], AspectsNew,
                   Arity, AutomaticLoading)
    when is_function(F, Arity) ->
    validate_aspects_f(AspectsOld, [F | AspectsNew],
                       Arity, AutomaticLoading);
validate_aspects_f([{M, F} = Entry | AspectsOld], AspectsNew,
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
            validate_aspects_f(AspectsOld, [Entry | AspectsNew],
                               Arity, AutomaticLoading);
        Exported =:= false ->
            {error, Entry}
    end;
validate_aspects_f([{{M, F}} = Entry | AspectsOld], AspectsNew,
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
            validate_aspects_f(AspectsOld, [Function | AspectsNew],
                               Arity, AutomaticLoading);
        Exported =:= false ->
            {error, Entry}
    end;
validate_aspects_f([Entry | _], _, _, _) ->
    {error, Entry}.

validate_node(Node) ->
    Valid = cloudi_x_cpg:valid_node(Node),
    if
        Valid =:= true ->
            ok;
        Valid =:= false ->
            {error, {node_invalid, Node}}
    end.

code_proplist(Value) ->
    CodeConfig = #config_code{},
    Defaults = [
        {paths,
         CodeConfig#config_code.paths},
        {modules,
         CodeConfig#config_code.modules},
        {applications,
         CodeConfig#config_code.applications},
        {releases,
         CodeConfig#config_code.releases}],
    case cloudi_proplists:take_values(Defaults, Value) of
        [Paths, _, _, _]
            when not is_list(Paths) ->
            {error, {code_paths_invalid, Paths}};
        [_, Modules, _, _]
            when not is_list(Modules) ->
            {error, {code_modules_invalid, Modules}};
        [_, _, Applications, _]
            when not is_list(Applications) ->
            {error, {code_applications_invalid, Applications}};
        [_, _, _, Releases]
            when not is_list(Releases) ->
            {error, {code_releases_invalid, Releases}};
        [Paths, Modules, Applications, Releases] ->
            accum([{Paths,
                    fun code_load_paths/2},
                   {Modules,
                    fun code_load_modules/2},
                   {Applications,
                    fun code_load_applications/2},
                   {Releases,
                    fun code_load_releases/2}],
                  CodeConfig);
        [_, _, _, _ | Extra] ->
            {error, {code_invalid, Extra}}
    end.

code_load_path_add(Path, PathsNormalized) ->
    PathNormalized = path_normalize(Path),
    case lists:member(PathNormalized, PathsNormalized) of
        true ->
            {error, already_exists, PathNormalized};
        false ->
            case code:add_pathz(PathNormalized) of
                true ->
                    {ok, PathNormalized};
                {error, Reason} ->
                    {error, Reason, PathNormalized}
            end
    end.

code_load_path_remove(Path, PathsNormalized) ->
    PathNormalized = path_normalize(Path),
    case cloudi_lists:delete_checked(PathNormalized, PathsNormalized) of
        false ->
            {error, does_not_exist};
        PathsNormalizedNew ->
            case code:del_path(PathNormalized) of
                true ->
                    {ok, PathsNormalizedNew};
                false ->
                    {error, does_not_exist};
                {error, _} = Error ->
                    Error
            end
    end.

code_load_path([], PathsNormalized) ->
    {ok, lists:reverse(PathsNormalized)};
code_load_path([Path | _], _)
    when not is_integer(hd(Path)) ->
    {error, {code_paths_invalid, Path}};
code_load_path([Path | Paths], PathsNormalized) ->
    case code_load_path_add(Path, PathsNormalized) of
        {ok, PathNormalized} ->
            code_load_path(Paths, [PathNormalized | PathsNormalized]);
        {error, Reason, PathNormalized} ->
            {error, {code_paths_invalid, {Reason, PathNormalized}}}
    end.

code_load_paths(Paths, CodeConfig) ->
    case code_load_path(Paths, []) of
        {ok, PathsNormalized} ->
            {ok, CodeConfig#config_code{paths = PathsNormalized}};
        {error, _} = Error ->
            Error
    end.

code_load_module([]) ->
    ok;
code_load_module([Module | _])
    when not is_atom(Module) ->
    {error, {code_modules_invalid, Module}};
code_load_module([Module | Modules]) ->
    case code:is_loaded(Module) of
        false ->
            case code:load_file(Module) of
                {module, Module} ->
                    code_load_module(Modules);
                {error, Reason} ->
                    {error, {code_modules_invalid, {Reason, Module}}}
            end;
        _ ->
            {error, {code_modules_invalid, {already_loaded, Module}}}
    end.

code_load_modules([], #config_code{modules = []} = CodeConfig) ->
    {ok, CodeConfig};
code_load_modules(Modules, CodeConfig) ->
    case code_load_module(Modules) of
        ok ->
            error_logger:info_msg("code modules loaded~n  ~tp~n",
                                  [Modules]),
            {ok, CodeConfig#config_code{modules = Modules}};
        {error, _} = Error ->
            Error
    end.

code_load_application([]) ->
    ok;
code_load_application([Application | _])
    when not is_atom(Application) ->
    {error, {code_applications_invalid, Application}};
code_load_application([Application | Applications]) ->
    case cloudi_x_reltool_util:application_start(Application, [], infinity) of
        ok ->
            code_load_application(Applications);
        {error, Reason} ->
            {error, {code_applications_invalid, {Reason, Application}}}
    end.

code_load_applications([], #config_code{applications = []} = CodeConfig) ->
    {ok, CodeConfig};
code_load_applications(Applications, CodeConfig) ->
    case code_load_application(Applications) of
        ok ->
            error_logger:info_msg("code applications loaded~n  ~tp~n",
                                  [Applications]),
            {ok, CodeConfig#config_code{applications = Applications}};
        {error, _} = Error ->
            Error
    end.

code_load_release([]) ->
    ok;
code_load_release([Release | _])
    when not is_list(Release) ->
    {error, {code_releases_invalid, Release}};
code_load_release([Release | Releases]) ->
    if
        hd(Release) == $/ ->
            case filename:extension(Release) of
                ".script" ->
                    case cloudi_x_reltool_util:script_start(Release) of
                        {ok, _} ->
                            code_load_release(Releases);
                        {error, Reason} ->
                            {error, {code_releases_invalid, {Reason, Release}}}
                    end;
                ".boot" ->
                    case cloudi_x_reltool_util:boot_start(Release) of
                        {ok, _} ->
                            code_load_release(Releases);
                        {error, Reason} ->
                            {error, {code_releases_invalid, {Reason, Release}}}
                    end;
                _ ->
                    {error, {code_releases_invalid, Release}}
            end;
        true ->
            {error, {code_releases_invalid, Release}}
    end.

code_load_releases([], #config_code{releases = []} = CodeConfig) ->
    {ok, CodeConfig};
code_load_releases(Releases, CodeConfig) ->
    case code_load_release(Releases) of
        ok ->
            error_logger:info_msg("code releases loaded~n  ~tp~n",
                                  [Releases]),
            {ok, CodeConfig#config_code{releases = Releases}};
        {error, _} = Error ->
            Error
    end.

path_normalize(Path) ->
    filename:join([Path]).

uuid_generator() ->
    Variant = application:get_env(cloudi_core, uuid_v1_variant,
                                  ?UUID_V1_VARIANT_DEFAULT),
    {ok, MacAddress} = application:get_env(cloudi_core, mac_address),
    cloudi_x_uuid:new(self(), [{timestamp_type, erlang},
                               {mac_address, MacAddress},
                               {variant, Variant}]).

-type eval_value() :: number() | atom() | list().
-spec eval(L :: list({eval_value(),
                      fun((eval_value()) -> {ok, any()} | {error, any()})})) ->
    tuple().

eval(L) ->
    eval(L, []).

eval([], Output) ->
    erlang:list_to_tuple([ok | lists:reverse(Output)]);
eval([{Value, F} | L], Output)
    when is_number(Value) orelse is_atom(Value) orelse is_list(Value) ->
    case F(Value) of
        {ok, ValueNew} ->
            eval(L, [ValueNew | Output]);
        {error, _} = Error ->
            Error
    end.

-type accum_value() :: any().
-spec accum(list({accum_value(),
                  fun((accum_value(), any()) -> {ok, any()} | {error, any()})}),
            State :: any()) ->
    {ok, StateNew :: any()} | {error, any()}.

accum([], State) ->
    {ok, State};
accum([{Value, F} | L], State) ->
    case F(Value, State) of
        {ok, StateNew} ->
            accum(L, StateNew);
        {error, _} = Error ->
            Error
    end.
