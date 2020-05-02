%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
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
%%%------------------------------------------------------------------------

-include("cloudi_core_i_configuration_defaults.hrl").

-record(config_logging_syslog,
    {
        identity = "CloudI"
            :: cloudi_service_api:logging_syslog_identity(),
        facility = local0
            :: cloudi_service_api:logging_syslog_facility(),
        % The mapping for CloudI levels to syslog priorities is:
        % fatal  -> critical       (2)
        % error  -> error          (3)
        % warn   -> warning        (4)
        % info   -> notice         (5)
        % debug  -> informational  (6)
        % trace  -> debug          (7)
        level = trace
            :: cloudi_service_api:loglevel(),
        transport = local
            :: cloudi_service_api:logging_syslog_transport(),
        transport_options = []
            :: cloudi_service_api:logging_syslog_transport_options(),
        protocol = rfc3164
            :: cloudi_service_api:logging_syslog_protocol(),
        path = "/dev/log"
            :: cloudi_service_api:logging_syslog_path(),
        host = {127,0,0,1}
            :: cloudi_service_api:logging_syslog_host(),
        port = undefined
            :: cloudi_service_api:logging_syslog_port()
    }).

-record(config_logging_formatter,
    {
        % The mapping for lager levels to CloudI levels is:
        % (use CloudI levels to avoid the mapping)
        % emergency                     -> fatal
        % alert     (becomes emergency) -> fatal
        % critical  (becomes emergency) -> fatal
        % error                         -> error
        % warning                       -> warn
        % notice    (becomes warning)   -> warn
        % info                          -> info
        % debug                         -> debug
        %                               -> trace
        % none                          -> off
        level = trace
            :: cloudi_service_api:loglevel(),
        output = undefined
            :: undefined | module(),
        output_name
            :: atom(),
        output_args = [] % provided to output module with formatter args
            :: list(),
        output_max_r = ?DEFAULT_MAX_R
            :: non_neg_integer(),
        output_max_t = ?DEFAULT_MAX_T
            :: cloudi_service_api:seconds(),
        formatter = undefined
            :: undefined | module(),
        formatter_config = []
            :: list()
    }).

-record(config_logging_formatters,
    {
        % 'any' formatter entry
        default = undefined
            :: undefined | #config_logging_formatter{},
        % nonempty_list(module()) -> #config_logging_formatter{} lookup
        lookup = cloudi_x_keys1value:new(maps)
            :: cloudi_x_keys1value:
               cloudi_x_keys1value(module(), #config_logging_formatter{}),
        level = undefined
            :: undefined | cloudi_service_api:loglevel()
    }).

-record(config_logging,
    {
        % file path to write log output to while allowing the file to rotate
        file = "cloudi.log"
            :: undefined | string(),
        % write log output to stdout
        stdout = false
            :: boolean(),
        % controls both file and stdout
        level = trace
            :: undefined | cloudi_service_api:loglevel(),
        % redirect log output to a different CloudI node
        redirect = undefined
            :: undefined | node(),
        % send log output to syslog
        syslog = undefined
            :: undefined | #config_logging_syslog{},
        % use custom formatters for log output
        formatters = undefined
            :: undefined | #config_logging_formatters{},
        % log when Erlang system time has changed with the amount
        log_time_offset = off
            :: cloudi_service_api:loglevel(),
        % aspect functions to execute before logging
        aspects_log_before = []
            :: list(cloudi_service_api:aspect_log_before()),
        % aspect functions to execute after logging
        aspects_log_after = []
            :: list(cloudi_service_api:aspect_log_after())
    }).

-record(config_service_options,
    {
        % DEFAULT VALUES ASSIGNED BELOW

        % Relevant for both Internal and External Services in the same way:

        % -128 (high) <= priority_default <= 127 (low)
        priority_default = ?DEFAULT_PRIORITY
            :: cloudi_service:priority(),
        % a limit on the total number of incoming service requests that
        % are queued while the service is busy (limits memory consumption)
        queue_limit = undefined
            :: undefined | non_neg_integer(),
        % a limit on the total amount of memory incoming service requests may
        % consume within the queue while the service is busy,
        % (configured in kilobytes and stored as bytes)
        queue_size = undefined
            :: undefined | pos_integer(),
        % service reqs/second maximum
        rate_request_max = undefined
            :: undefined | number() |
               list({period, cloudi_service_api:period_seconds()} |
                    {value, number()}) |
               tuple(),
        % delay after startup before requesting the initial service
        % group membership (when using a lazy destination refresh method)
        dest_refresh_start = ?DEFAULT_DEST_REFRESH_START
            :: cloudi_service_api:dest_refresh_delay_milliseconds(),
        % maximum possible time for a service death to remove service
        % group membership when using a lazy destination refresh method
        % (not an immediate destination refresh method).
        % a lazy destination refresh method is used when a
        % service is mainly communicating with long-lived services
        % (and an immediate destination refresh method is used when
        %  a service is mainly communicating with short-lived services).
        dest_refresh_delay = ?DEFAULT_DEST_REFRESH_DELAY
            :: cloudi_service_api:dest_refresh_delay_milliseconds(),
        % should the service request name lookup be a synchronous or
        % an asynchronous operation (the default is to be synchronous and
        % keep the service name lookup result decoupled from the
        % destination service's lifetime during the service request
        % timeout period, to anticipate any number of delays a service 
        % request may encounter, including node-splits and forwards,
        % to provide service failure isolation)
        request_name_lookup = ?DEFAULT_REQUEST_NAME_LOOKUP
            :: sync | async,
        % should the service request handler execution time decrement the
        % request timeout to reduce the timeout of a forwarded request or
        % the timeout of a returned response
        % (if the request timeout is equal to the forward or return timeout,
        %  n.b., doesn't adjust the timeout of a cloudi_service:return_nothrow)
        request_timeout_adjustment = false
            :: boolean(),
        % defines the max request timeout considered to be "immediate":
        % max timeout value of sent service requests whose destination
        % Erlang pid will not be monitored because the rate at which
        % sent service requests are being sent to unresponsive
        % destination Erlang pids will not cause excessive timer
        % (erlang:send_after/3) memory consumption during the time period
        % specified by this value.  sent service requests with timeouts
        % that are greater than or equal to this value will have their
        % destination Erlang pid monitored so that timer memory consumption
        % is cleaned up quicker than the timeout value specified within the
        % service request.  as the rate of sent service requests increases
        % to unresponsive services, this value will need to decrease,
        % to affect service requests of shorter duration.
        request_timeout_immediate_max = 20001 % milliseconds
            :: cloudi_service_api:request_timeout_immediate_max_milliseconds(),
        % should the service use internal timeout information to provide a
        % more accurate timeout value within the response provided
        % (n.b., this only affects the response timeout of a successful
        %  send_async request)
        response_timeout_adjustment = false
            :: boolean(),
        % defines the max response timeout considered to be "immediate":
        % max timeout value of a returned null response which is discarded
        % rather than being returned, so that the associated service request
        % timeout is caused by the sending service's service request timer
        % instead of a returned null response message.  service request
        % null responses with timeouts that are greater than or equal to this
        % value will always be sent to help the sending-side avoid excessive
        % delays during the timeout period.
        response_timeout_immediate_max = 20001 % milliseconds
            :: cloudi_service_api:response_timeout_immediate_max_milliseconds(),
        % should the process count be varied automatically based on the
        % rate of service processing within a specific time period.
        % the count max/min specify limits for the count_process changes
        % as either floating point percentages (the result is rounded) or
        % as integer absolutes.
        count_process_dynamic = false
            :: false |
               list({period, cloudi_service_api:period_seconds()} |
                    {rate_request_max, number()} | % service reqs/second
                    {rate_request_min, number()} | % service reqs/second
                    {count_max, number()} | % float multiplier or
                    {count_min, number()}) | % integer absolute
               tuple(),
        % when undefined, timeout_terminate defaults to
        % (1000 * MaxT) / MaxR - 100 to ensure the service lifetime is finite
        % when errors occur. timeout_terminate can be set manually to
        % enforce a greater uptime constraint on the service execution.
        timeout_terminate = undefined
            :: undefined |
               cloudi_service_api:timeout_terminate_milliseconds(),
        % should all processes be restarted when one process restarts
        % (after one of its execution threads crashes)
        restart_all = false
            :: boolean(),
        % delay to wait after a service terminate but before the service
        % initialization of the new service instance, during a service restart
        restart_delay = false
            :: list({time_exponential_min,
                     cloudi_service_api:restart_delay_milliseconds()} |
                    {time_exponential_max,
                     cloudi_service_api:restart_delay_milliseconds()} |
                    {time_absolute,
                     cloudi_service_api:restart_delay_milliseconds()}) |
               false |
               tuple(),
        % provide a scope for all subscribe/unsubscribe and messaging
        % (i.e., all service name usage is within the scope).  Using a
        % different scope can help avoid contention when using an immediate
        % destination refresh method.
        scope = ?DEFAULT_SCOPE
            :: atom(),
        % add latency to all service requests and info messages received
        % based on the parameters specified.  If "system" is set, the
        % cloudi_core Erlang application env value is used after being
        % checked during service startup (e.g., after service restarts).
        % (all time parameters are specified in milliseconds)
        monkey_latency = false
            :: list({time_uniform_min,
                     cloudi_service_api:latency_min_time_milliseconds()} |
                    {time_uniform_max,
                     cloudi_service_api:latency_max_time_milliseconds()} |
                    {time_gaussian_mean,
                     cloudi_service_api:latency_mean_time_milliseconds()} |
                    {time_gaussian_stddev, float() | pos_integer()} |
                    {time_absolute,
                     cloudi_service_api:latency_time_milliseconds()}) |
               system | false |
               tuple(),
        % cause service termination based on the probability parameter
        % (checked for each service request and info message, if necessary).
        % If "system" is set, the cloudi_core Erlang application env value
        % is used after being checked during service startup
        % (e.g., after service restarts).  The probability_day method
        % replicates the Netflix chaos monkey usage.
        monkey_chaos = false
            :: list({probability_request, float()} |
                    {probability_day, float()}) |
               system | false |
               tuple(),
        % should the service be automatically loaded and unloaded at
        % service start and stop, respectively?
        % (or the aspect module be automatically loaded)
        automatic_loading = true
            :: boolean(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % service's long-lived service request sending Erlang process
        dispatcher_pid_options = []
            :: list(link |
                    {priority, low | normal | high} |
                    {fullsweep_after, non_neg_integer()} |
                    {min_heap_size, non_neg_integer()} |
                    {min_bin_vheap_size, non_neg_integer()} |
                    {max_heap_size,
                     cloudi_service_api:max_heap_size_options()} |
                    {sensitive, boolean()} |
                    {message_queue_data, off_heap | on_heap | mixed}),

        % Relevant for both Internal and External Services, different values:

        % aspects are functions provided to be processed before or after
        % the service callback is executed (Aspect-Oriented Programming (AOP))
        aspects_init_after = []
            :: list(cloudi_service_api:aspect_init_after_internal() |
                    cloudi_service_api:aspect_init_after_external()),
        aspects_request_before = []
            :: list(cloudi_service_api:aspect_request_before_internal() |
                    cloudi_service_api:aspect_request_before_external()),
        aspects_request_after = []
            :: list(cloudi_service_api:aspect_request_after_internal() |
                    cloudi_service_api:aspect_request_after_external()),
        aspects_info_before = []
            :: list(cloudi_service_api:aspect_info_before_internal()),
        aspects_info_after = []
            :: list(cloudi_service_api:aspect_info_after_internal()),
        aspects_terminate_before = []
            :: list(cloudi_service_api:aspect_terminate_before_internal() |
                    cloudi_service_api:aspect_terminate_before_external()),

        % Only Relevant for External Services:

        % set resource limits for each OS process spawned
        limit = []
            :: cloudi_service_api:limit_external(),
        % use a specific user and/or group for spawning the OS processes
        owner = []
            :: cloudi_service_api:owner_external(),
        % set a nice value for each OS process spawned
        nice = 0
            :: cloudi_service_api:nice_external(),
        % put each OS process spawned into a specific cgroup
        cgroup = undefined
            :: cloudi_service_api:cgroup_external(),
        % set the root directory for spawning the OS process
        chroot = undefined
            :: cloudi_service_api:chroot_external(),
        % set the current directory for spawning the OS process
        directory = undefined
            :: cloudi_service_api:directory_external(),

        % Only Relevant for Internal Services:

        % specify an Erlang application name, so it can be different from
        % the CloudI service module name
        application_name = undefined
            :: atom(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % service init handling Erlang process
        init_pid_options = []
            :: list(link |
                    {priority, low | normal | high} |
                    {fullsweep_after, non_neg_integer()} |
                    {min_heap_size, non_neg_integer()} |
                    {min_bin_vheap_size, non_neg_integer()} |
                    {max_heap_size,
                     cloudi_service_api:max_heap_size_options()} |
                    {sensitive, boolean()} |
                    {message_queue_data, off_heap | on_heap | mixed}),
        % how many service requests should restart the Erlang process used for
        % handling the service requests
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        request_pid_uses = 1
            :: infinity | pos_integer(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % service request handling Erlang process
        request_pid_options = []
            :: list(link |
                    {priority, low | normal | high} |
                    {fullsweep_after, non_neg_integer()} |
                    {min_heap_size, non_neg_integer()} |
                    {min_bin_vheap_size, non_neg_integer()} |
                    {max_heap_size,
                     cloudi_service_api:max_heap_size_options()} |
                    {sensitive, boolean()} |
                    {message_queue_data, off_heap | on_heap | mixed}),
        % how many info messages should restart the Erlang process used for
        % handling the info message
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        info_pid_uses = infinity
            :: infinity | pos_integer(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % info message handling Erlang process
        info_pid_options = []
            :: list(link |
                    {priority, low | normal | high} |
                    {fullsweep_after, non_neg_integer()} |
                    {min_heap_size, non_neg_integer()} |
                    {min_bin_vheap_size, non_neg_integer()} |
                    {max_heap_size,
                     cloudi_service_api:max_heap_size_options()} |
                    {sensitive, boolean()} |
                    {message_queue_data, off_heap | on_heap | mixed}),
        % use two Erlang processes instead of one for an internal service to
        % keep send operations separate from receive operations.  better
        % throughput can be achieved with duo_mode, especially when sending to
        % external services (the mean request latency should become half
        % due to the Erlang processes splitting the processing).  duo_mode was
        % the default for pre-v1.2.0 internal services.  the second process
        % is used in place of the info_pid and the process' message queue
        % is used directly (so info_pid_uses must be set to infinity when
        % duo_mode is true).  cloudi_service_handle_info/3 must not contain
        % a call to cloudi_service:send_sync or cloudi_service:recv_async
        % if duo_mode is true.  Instead, use cloudi_service:send_async_active.
        duo_mode = false
            :: boolean(),
        % should a mostly idle service hibernate automatically to conserve
        % memory at the expense of extra garbage collections and an empty
        % stack trace.  if a list is provided, hibernate will occur when
        % the rate of service processing drops below the minimum specified.
        hibernate = false
            :: boolean() |
               list({period, cloudi_service_api:period_seconds()} |
                    {rate_request_min, number()}) | % service reqs/second
               tuple(),
        % should the service be reloaded automatically when an Erlang module
        % file changes?  should only be used during service development.
        reload = false
            :: boolean()
    }).

% internal service parameters
-record(config_service_internal,
    {
        prefix
            :: string(),
        module
            :: atom() | file:filename(),
        file_path
            :: undefined | file:filename(), % if module a path
        args
            :: list(),
        dest_refresh
            :: cloudi_service_api:dest_refresh(),
        timeout_init
            :: cloudi_service_api:timeout_initialize_value_milliseconds(),
        timeout_async
            :: cloudi_service_api:timeout_send_async_value_milliseconds(),
        timeout_sync
            :: cloudi_service_api:timeout_send_sync_value_milliseconds(),
        timeout_term
            :: cloudi_service_api:timeout_terminate_value_milliseconds(),
        dest_list_deny
            :: cloudi_service_api:dest_list(),
        dest_list_allow
            :: cloudi_service_api:dest_list(),
        count_process
            :: pos_integer(),
        max_r
            :: non_neg_integer(),
        max_t
            :: cloudi_service_api:seconds(),
        options
            :: #config_service_options{},
        uuid
            :: cloudi_service_api:service_id()
    }).

% external service parameters
-record(config_service_external,
    {
        prefix
            :: string(),
        file_path
            :: file:filename(),
        args
            :: string(),
        env
            :: list({string(), string()}),
        dest_refresh
            :: cloudi_service_api:dest_refresh(),
        protocol
            :: 'local' | 'tcp' | 'udp',
        buffer_size
            :: pos_integer(),
        timeout_init
            :: cloudi_service_api:timeout_initialize_value_milliseconds(),
        timeout_async
            :: cloudi_service_api:timeout_send_async_value_milliseconds(),
        timeout_sync
            :: cloudi_service_api:timeout_send_sync_value_milliseconds(),
        timeout_term
            :: cloudi_service_api:timeout_terminate_value_milliseconds(),
        dest_list_deny
            :: cloudi_service_api:dest_list(),
        dest_list_allow
            :: cloudi_service_api:dest_list(),
        count_process
            :: pos_integer(),
        count_thread
            :: pos_integer(),
        max_r
            :: non_neg_integer(),
        max_t
            :: cloudi_service_api:seconds(),
        options
            :: #config_service_options{},
        uuid
            :: cloudi_service_api:service_id()
    }).

% service update plan
-record(config_service_update,
    {
        % service update plan configuration

        type = undefined
            :: undefined | internal | external,

        % internal service update configuration

        % internal service module to update
        module = undefined
            :: atom(),
        % internal service state update
        % equivalent to Module:code_change/3 with an Erlang/OTP behaviour
        % but with the addition of the new module version and without the
        % Extra variable (due to the function existing only for the upgrade).
        module_state = undefined
            :: undefined |
               fun((ModuleVersonOld :: cloudi_service_api:module_version(),
                    ModuleVersonNew :: cloudi_service_api:module_version(),
                    StateOld :: any()) ->
                   {ok, StateNew :: any()} | {error, Reason :: any()} | any()),

        % external service update configuration

        % external service executable to use after the update
        file_path = undefined
            :: undefined | file:filename(),
        % external service executable command-line arguments for the update
        args = undefined
            :: undefined | string(),
        % external service executable environment variables for the update
        env = undefined
            :: undefined | list({string(), string()}),

        % common update configuration

        % should the update be done synchronously among all service processes
        % (i.e., should the update operation be coordinated to not occur
        %  while a service request is being handled)
        sync = true
            :: boolean(),
        % additional modules to load before module_state is called, if provided
        modules_load = []
            :: list(atom()),
        % modules to unload after module_state is called, if provided
        modules_unload = []
            :: list(atom()),
        % code paths to add before modules are loaded
        code_paths_add = []
            :: list(string()),
        % code paths to remove after the update is successful
        code_paths_remove = []
            :: list(string()),
        % destination refresh method
        dest_refresh = undefined
            :: undefined | cloudi_service_api:dest_refresh(),
        % service initialization timeout
        timeout_init = undefined
            :: undefined |
               cloudi_service_api:timeout_initialize_value_milliseconds(),
        % default async timeout
        timeout_async = undefined
            :: undefined |
               cloudi_service_api:timeout_send_async_value_milliseconds(),
        % default sync timeout
        timeout_sync = undefined
            :: undefined |
               cloudi_service_api:timeout_send_sync_value_milliseconds(),
        % destinations denied
        dest_list_deny = invalid
            :: invalid | cloudi_service_api:dest_list(),
        % destinations allowed
        dest_list_allow = invalid
            :: invalid | cloudi_service_api:dest_list(),
        % service configuration option keys to update
        options_keys = []
            :: list(atom()),
        % service configuration options to update
        options = []
            :: [] | #config_service_options{},

        % service update plan state

        % service ids of affected services
        % (internal services must have all usage of the
        %  service module represented in this list while
        %  external services always have a list of length 1)
        uuids = []
            :: list(cloudi_service_api:service_id()),
        % old module version passed to module_state function
        % (internal services only)
        module_version_old = undefined
            :: undefined | cloudi_service_api:module_version(),
        % should auto-reloading of the module be stopped during the update?
        % (internal services only)
        reload_stop = false
            :: boolean(),
        % should a new OS process be created?
        % (external services only)
        spawn_os_process = false
            :: boolean(),
        % is the service busy handling a service request?
        % (when sync == true, delay module loading until after
        %  all service requests currently being handled are done)
        update_pending = undefined
            :: undefined | pid(),
        % should the update occur now
        % (when sync == false, delay the module_state update until after
        %  a service request currently being handled is done in the process)
        update_now = undefined
            :: undefined | pid(),
        % can the update start as anticipated?
        % (false if processes died during the update attempt)
        update_start = true
            :: boolean(),
        % is a service request currently being handled?
        process_busy = undefined
            :: undefined | boolean()
    }).

-record(config_nodes_discovery,
    {
        % nodefinder interface
        mode         :: multicast | ec2,
        module       :: module(),
        start_f      :: atom(),
        start_a      :: list(),
        discover_f   :: atom(),
        discover_a   :: list(),
        stop_f       :: atom(),
        stop_a       :: list()
    }).

-record(config_nodes,
    {
        nodes = []
            :: list(node()),
        % time to wait before the first reconnect is attempted with a node
        reconnect_start = ?DEFAULT_NODE_RECONNECT_START
            :: cloudi_service_api:node_reconnect_delay_seconds(),
        % maximum wait time before a reconnect is attempted with a node
        reconnect_delay = ?DEFAULT_NODE_RECONNECT_DELAY
            :: cloudi_service_api:node_reconnect_delay_seconds(),
        % how should node connections be monitored
        % (inferred from connect value if it is not set,
        %  if connect is visible
        %  then listen is visible unless set to all for hidden node connections
        %  if connect is hidden
        %  then listen needs to be all)
        listen = visible
            :: visible | all,
        % how should node connections be created
        connect = visible
            :: visible | hidden,
        % what timestamp is used for generating service request transaction ids
        timestamp_type = erlang
            :: erlang | os | warp,
        % discovery format (with defaults) is:
        %
        % [{multicast,
        %   [{address, {224,0,0,1}},
        %    {port, 4475},
        %    {ttl, 1}]}]
        %
        % (or)
        %
        % [{ec2,
        %   [{access_key_id, undefined},
        %    {secret_access_key, undefined},
        %    {ec2_host, "ec2.amazonaws.com"},
        %    {groups, []},
        %    {tags, []}]}]
        %
        discovery = undefined
            :: #config_nodes_discovery{} | undefined,
        % cost as currency per hour
        % (average kilowatts * currency per kilowatt-hour (kWh))
        cost = []
            :: list({node() | default, float()}),
        % cost currency decimal places
        cost_precision = 2
            :: 0..253,
        % loglevel for logging disconnected nodes before
        % each reconnect attempt occurs
        log_reconnect = info
            :: cloudi_service_api:loglevel()
    }).

-record(config,
    {
        uuid_generator
            :: cloudi_x_uuid:state(),
        logging = #config_logging{}
            :: #config_logging{},
        acl = #{}
            :: #{atom() := nonempty_list(cloudi:service_name_pattern())},
        services = []
            :: list(#config_service_internal{} |
                    #config_service_external{}),
        nodes = #config_nodes{}
            :: #config_nodes{}
    }).

