%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
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
%%%------------------------------------------------------------------------

-include("cloudi_configuration_defaults.hrl").

-record(config_logging_syslog,
    {
        identity = "CloudI"
            :: string(),
        facility = local0
            :: syslog:facility(),
        % The mapping for CloudI levels to syslog priorities is:
        % fatal  -> emerg    (0)
        % error  -> err      (3)
        % warn   -> warning  (4)
        % info   -> info     (6)
        % debug  -> debug    (7)
        % trace  -> 8
        level = trace
            :: cloudi_service_api:loglevel()
    }).

-record(config_logging,
    {
        file = "logs/cloudi.log"
            :: undefined | file:filename(),
        level = trace
            :: cloudi_service_api:loglevel(),
        redirect = undefined
            :: undefined | node(),
        syslog = undefined
            :: undefined | #config_logging_syslog{}
    }).

-record(config_service_options,
    {
        % DEFAULT VALUES ASSIGNED BELOW

        % -128 (high) <= priority_default <= 127 (low)
        priority_default = ?DEFAULT_PRIORITY
            :: cloudi_service:priority(),
        % a limit on the total number of incoming service requests that
        % are queued while the service is busy (limits memory consumption)
        queue_limit = undefined
            :: undefined | pos_integer(),
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
        % should the service request handler execution time decrement the
        % request timeout to reduce the timeout of a forwarded request or
        % the timeout of a returned response
        % (if the request timeout is equal to the forward or return timeout,
        %  n.b., doesn't adjust the timeout of a cloudi_service:return_nothrow)
        request_timeout_adjustment = false
            :: boolean(),
        % max request timeout considered to be "immediate":
        % max timeout value of sent service requests whose destination
        % Erlang pid will not be monitored because the rate at which
        % sent service requests are being sent to unresponsive
        % destination Erlang pids will not cause excessive timer
        % (erlang:send_after/3) memory consumption during the time period
        % specified by this value.  sent service requests with timeouts
        % that are greater than this value will have their destination
        % Erlang pid monitored so that timer memory consumption is cleaned up
        % quicker than the timeout value specified within the service request.
        % as the rate of sent service requests increases to unresponsive
        % services, this value will need to decrease, to affect service
        % requests of shorter duration.
        request_timeout_immediate_max = 20000 % milliseconds
            :: cloudi_service_api:request_timeout_immediate_max_milliseconds(),
        % should the service use internal timeout information to provide a
        % more accurate timeout value within the response provided
        % (n.b., this only affects the response timeout of a successful
        %  send_async request)
        response_timeout_adjustment = false
            :: boolean(),
        % max response timeout considered to be "immediate":
        % max timeout value of a returned null response which is discarded
        % rather than being returned, so that the associated service request
        % timeout is caused by the sending service's service request timer
        % instead of a returned null response message
        response_timeout_immediate_max = 20000 % milliseconds
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
                     cloudi_service_api:latency_time_milliseconds()} |
                    {time_uniform_max,
                     cloudi_service_api:latency_time_milliseconds()} |
                    {time_gaussian_mean,
                     cloudi_service_api:latency_time_milliseconds()} |
                    {time_gaussian_stddev, float()} |
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

        % Only Relevant For Internal Services:

        % specify an Erlang application name, so it can be different from
        % the CloudI service module name
        application_name = undefined
            :: atom(),
        % how many service requests should restart the Erlang process used for
        % handling the service requests
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        request_pid_uses = 1
            :: infinity | pos_integer(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % service request handling Erlang process
        request_pid_options = []
            :: list({fullsweep_after, non_neg_integer()} |
                    {min_heap_size, non_neg_integer()} |
                    {min_bin_vheap_size, non_neg_integer()}),
        % how many info messages should restart the Erlang process used for
        % handling the info message
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        info_pid_uses = infinity
            :: infinity | pos_integer(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % info message handling Erlang process
        info_pid_options = []
            :: list({fullsweep_after, non_neg_integer()} |
                    {min_heap_size, non_neg_integer()} |
                    {min_bin_vheap_size, non_neg_integer()}),
        % use two Erlang processes instead of one for an internal service to
        % keep send operations separate from receive operations.  better
        % throughput can be achieved with duo_mode, especially when sending to
        % external services (the mean request latency should become half
        % due to the Erlang processes splitting the processing).  duo_mode was
        % the default for pre-v1.2.0 internal services.  the second process
        % is used in place of the info_pid and the process' message queue
        % is used directly (so info_pid_uses must be set to infinity when
        % duo_mode is true).
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
            :: boolean(),
        % should the service be automatically loaded and unloaded at
        % service start and stop, respectively?
        automatic_loading = true
            :: boolean()
    }).

% internal service parameters
-record(config_service_internal,
    {
        prefix             :: string(),
        module             :: atom() | file:filename(),
        file_path          :: undefined | file:filename(), % if module a path
        args               :: list(),
        dest_refresh       :: cloudi_service_api:dest_refresh(),
        timeout_init       :: cloudi_service_api:timeout_milliseconds(),
        timeout_async      :: cloudi_service_api:timeout_milliseconds(),
        timeout_sync       :: cloudi_service_api:timeout_milliseconds(),
        dest_list_deny     :: cloudi_service_api:dest_list(),
        dest_list_allow    :: cloudi_service_api:dest_list(),
        count_process      :: pos_integer(),
        max_r              :: non_neg_integer(),
        max_t              :: cloudi_service_api:seconds(),
        options            :: #config_service_options{},
        uuid               :: cloudi_service_api:service_id()
    }).

% external service parameters
-record(config_service_external,
    {
        prefix             :: string(),
        file_path          :: file:filename(),
        args               :: string(),
        env                :: list({string(), string()}),
        dest_refresh       :: cloudi_service_api:dest_refresh(),
        protocol           :: 'local' | 'tcp' | 'udp',
        buffer_size        :: pos_integer(),
        timeout_init       :: cloudi_service_api:timeout_milliseconds(),
        timeout_async      :: cloudi_service_api:timeout_milliseconds(),
        timeout_sync       :: cloudi_service_api:timeout_milliseconds(),
        dest_list_deny     :: cloudi_service_api:dest_list(),
        dest_list_allow    :: cloudi_service_api:dest_list(),
        count_process      :: pos_integer(),
        count_thread       :: pos_integer(),
        max_r              :: non_neg_integer(),
        max_t              :: cloudi_service_api:seconds(),
        options            :: #config_service_options{},
        uuid               :: cloudi_service_api:service_id()
    }).

-record(config_nodes_discovery,
    {
        % nodefinder interface
        mode :: multicast | ec2,
        module :: module(),
        start_f :: atom(),
        start_a :: list(),
        discover_f :: atom(),
        discover_a :: list(),
        stop_f :: atom(),
        stop_a :: list()
    }).

-record(config_nodes,
    {
        nodes = [] :: list(node()),
        % time to wait before the first reconnect is attempted with a node
        reconnect_start = ?DEFAULT_NODE_RECONNECT_START
            :: cloudi_service_api:node_reconnect_delay_seconds(),
        % maximum wait time before a reconnect is attempted with a node
        reconnect_delay = ?DEFAULT_NODE_RECONNECT_DELAY
            :: cloudi_service_api:node_reconnect_delay_seconds(),
        % how should node connections be monitored
        listen = visible
            :: visible | all,
        % how should node connections be created
        connect = visible
            :: visible | hidden,
        % what timestamp is used for generating service request transaction ids
        timestamp_type = erlang
            :: erlang | os,
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
            :: #config_nodes_discovery{} | undefined
    }).

-record(config,
    {
        uuid_generator :: cloudi_x_uuid:state(),
        logging = #config_logging{} :: #config_logging{},
        acl = dict:new(),
        services = [] :: list(#config_service_internal{} |
                              #config_service_external{}),
        nodes = #config_nodes{} :: #config_nodes{}
    }).

