%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
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
        level = trace,
        file = "logs/cloudi.log",
        redirect = undefined
    }).

-record(config_service_options,
    {
        % -128 (high) <= priority_default <= 127 (low)
        priority_default = 0,
        % a limit on the total number of incoming service requests that
        % are queued while the service is busy (limits memory consumption)
        queue_limit = undefined,
        % delay after startup before requesting the initial service
        % group membership (when using a lazy destination refresh method)
        dest_refresh_start = 500, % milliseconds
        % maximum possible time for a service death to remove service
        % group membership when using a lazy destination refresh method
        % (not an immediate destination refresh method).
        % a lazy destination refresh method is used when a
        % service is mainly communicating with long-lived services
        % (and an immediate destination refresh method is used when
        %  a service is mainly communicating with short-lived services).
        dest_refresh_delay = 300000, % milliseconds (5 minutes)
        % should the service request handler execution time decrement the
        % request timeout to reduce the timeout of a forwarded request or
        % the timeout of a returned response
        % (if the request timeout is equal to the forward or return timeout,
        %  n.b., doesn't adjust the timeout of a cloudi_service:return_nothrow)
        request_timeout_adjustment = false,
        % should the service use internal timeout information to provide a
        % more accurate timeout value within the response provided
        % (n.b., this only affects the response timeout of a successful
        %  send_async request)
        response_timeout_adjustment = false,

        % Only Relevant For Internal Services:

        % how many service requests should restart the Erlang process used for
        % handling the service requests
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        request_pid_uses = 1,
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % service request handling Erlang process
        request_pid_options = [],
        % how many info messages should restart the Erlang process used for
        % handling the info message
        % (an integer greater than 0 or the atom 'infinity' are valid values)
        info_pid_uses = infinity,
        % what erlang:spawn_opt/2 options should be used, if any, by the
        % info message handling Erlang process
        info_pid_options = [],
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
    }).

% internal service parameters
-record(config_service_internal,
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
        options,
        uuid
    }).

% external service parameters
-record(config_service_external,
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
        options,
        uuid
    }).

-record(config,
    {
        uuid_generator,
        logging = #config_logging{},
        acl = dict:new(),
        services = [],
        nodes = []
    }).

