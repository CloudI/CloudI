%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Safe to tune without causing major internal problems                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% recv_async null UUID strategy
-define(RECV_ASYNC_STRATEGY, recv_async_select_oldest).
%-define(RECV_ASYNC_STRATEGY, recv_async_select_random). % fastest

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reasonable constants that are unlikely to need modification.               %
% Possibly, in different environments, tuning may be beneficial, though      %
% it has not yet been necessary to modify these settings during testing.     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% interval at which asynchronous messages are checked
-define(RECV_ASYNC_INTERVAL, 500). % milliseconds

% interval at which asynchronous messages are sent
-define(SEND_ASYNC_INTERVAL, 500). % milliseconds

% interval at which synchronous messages are sent
-define(SEND_SYNC_INTERVAL, 500). % milliseconds

% interval at which multicast asynchronous messages are sent
-define(MCAST_ASYNC_INTERVAL, 500). % milliseconds

% interval at which synchronous forwarded messages are sent
-define(FORWARD_SYNC_INTERVAL, 500). % milliseconds

% interval at which asynchronous forwarded messages are sent
-define(FORWARD_ASYNC_INTERVAL, 500). % milliseconds

% decrement the timeout of each successful forward, to prevent infinite messages
% (i.e., this is the timeout penalty a request takes when forwarding a request)
-define(FORWARD_DELTA, 100). % milliseconds

% blocking operations must decrement the timeout to make sure timeouts
% have time to unravel all synchronous calls
% (should be less than all INTERVAL constants)
-define(TIMEOUT_DELTA, 100). % milliseconds

% interval to reload all internal services which have been configured to
% reload their modules automatically
-define(SERVICE_INTERNAL_RELOAD, 1000). % milliseconds

% maximum average time inbetween CloudI logger calls during 10 seconds
% to trigger logger flooding prevention, so that logging messages are discarded
% since they are coming from source code that is misbehaving that has already
% logged enough
-define(LOGGER_FLOODING_DELTA, 10). % microseconds

% message queue size that causes the logger to use synchronous messaging
% to avoid excessive memory consumption and system death
% (i.e., when the logger is not being flooded quickly by an individual
%  process, but is simply overloaded by all processes)
-define(LOGGER_MSG_QUEUE_SYNC, 1000).

% message queue size that causes the logger to switch back to
% asynchronous messaging after using synchronous messaging
-define(LOGGER_MSG_QUEUE_ASYNC, (?LOGGER_MSG_QUEUE_SYNC - 250)).

% periodic connection checks to determine if the udp connection is still active
% must be a short time since this impacts MaxR and MaxT.  However, this time
% becomes a hard maximum (minus a delta for overhead) for a task time target
% used in a service (i.e., the maximum amount of time spent not responding
% to incoming API calls).
-define(KEEPALIVE_UDP, 5000). % milliseconds

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constants that should never be changed                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for using cloudi_core as an isolated Erlang application
% outside of the CloudI repository
% (only internal services are supported,
%  due to the extra compilation required for external services support)
%-define(CLOUDI_CORE_STANDALONE, true).

% cloudi_x_pqueue4 usage limited by the signed byte integer storage
-define(PRIORITY_HIGH, -128).
-define(PRIORITY_LOW, 127).

-define(SCOPE_DEFAULT, cpg_default_scope).
-define(SCOPE_ASSIGN(Scope),
        if
            Scope =:= default ->
                % DEFAULT_SCOPE in cpg application
                ?SCOPE_DEFAULT;
            true ->
                erlang:list_to_atom("cloudi_x_cpg_x_" ++
                                    erlang:atom_to_list(Scope))
        end).

% maximum timeout value for erlang:send_after/3 and gen_server:call
-define(TIMEOUT_MAX_ERLANG, 4294967295).
% maximum timeout value for a service request
% (limitation for internal service requests, external service requests
%  should have a maximum of TIMEOUT_MAX_ERLANG)
-define(TIMEOUT_MAX, ?TIMEOUT_MAX_ERLANG - ?TIMEOUT_DELTA).

