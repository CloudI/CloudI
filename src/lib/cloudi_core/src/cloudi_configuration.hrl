%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2013, Michael Truog <mjtruog at gmail dot com>
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

-record(config_logging,
    {
        level = trace :: cloudi_service_api:loglevel(),
        file = "logs/cloudi.log" :: file:filename(),
        redirect = undefined :: undefined | node()
    }).

-record(config_service_options,
    {
        % DEFAULT VALUES ASSIGNED BELOW

        % -128 (high) <= priority_default <= 127 (low)
        priority_default = 0 :: cloudi_service:priority(),
        % a limit on the total number of incoming service requests that
        % are queued while the service is busy (limits memory consumption)
        queue_limit = undefined :: undefined | pos_integer(),
        % delay after startup before requesting the initial service
        % group membership (when using a lazy destination refresh method)
        dest_refresh_start = 500 % 0.5 seconds
            :: cloudi_service_api:dest_refresh_delay_milliseconds(),
        % maximum possible time for a service death to remove service
        % group membership when using a lazy destination refresh method
        % (not an immediate destination refresh method).
        % a lazy destination refresh method is used when a
        % service is mainly communicating with long-lived services
        % (and an immediate destination refresh method is used when
        %  a service is mainly communicating with short-lived services).
        dest_refresh_delay = 300000 % 5 minutes
            :: cloudi_service_api:dest_refresh_delay_milliseconds(),
        % should the service request handler execution time decrement the
        % request timeout to reduce the timeout of a forwarded request or
        % the timeout of a returned response
        % (if the request timeout is equal to the forward or return timeout,
        %  n.b., doesn't adjust the timeout of a cloudi_service:return_nothrow)
        request_timeout_adjustment = false :: boolean(),
        % should the service use internal timeout information to provide a
        % more accurate timeout value within the response provided
        % (n.b., this only affects the response timeout of a successful
        %  send_async request)
        response_timeout_adjustment = false :: boolean(),
        % provide a scope for all subscribe/unsubscribe and messaging
        % (i.e., all service name usage is within the scope).  Using a
        % different scope can help avoid contention when using an immediate
        % destination refresh method.
        scope = default :: atom(),

        % Only Relevant For Internal Services:

        % how many service requests should restart the Erlang process used for
        % handling the service requests
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        request_pid_uses = 1 :: infinity | pos_integer(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % service request handling Erlang process
        request_pid_options = [] ::
            list({fullsweep_after, non_neg_integer()} |
                 {min_heap_size, non_neg_integer()} |
                 {min_bin_vheap_size, non_neg_integer()}),
        % how many info messages should restart the Erlang process used for
        % handling the info message
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        info_pid_uses = infinity :: infinity | pos_integer(),
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % info message handling Erlang process
        info_pid_options = [] ::
            list({fullsweep_after, non_neg_integer()} |
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
        duo_mode = false :: boolean(),
        % should a mostly idle service hibernate automatically to conserve
        % memory at the expense of extra garbage collections and an empty
        % stack trace.
        hibernate = false :: boolean(),
        % should the service be reloaded automatically when an Erlang module
        % file changes?  should only be used during service development.
        reload = false :: boolean()
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

-record(config,
    {
        uuid_generator :: cloudi_x_uuid:state(),
        logging = #config_logging{} :: #config_logging{},
        acl = dict:new(),
        services = [] :: list(#config_service_internal{} |
                              #config_service_external{}),
        nodes = [] :: dynamic | list(node())
    }).

