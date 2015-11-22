%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Safe to tune without causing major internal problems                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% recv_async null UUID strategy
-define(RECV_ASYNC_STRATEGY, recv_async_select_oldest).
%-define(RECV_ASYNC_STRATEGY, recv_async_select_random). % fastest

% have errors report the service Erlang state as-is without simplification
% (to aid with debugging, should not normally be necessary)
%-define(VERBOSE_STATE, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reasonable constants that are unlikely to need modification.               %
% Possibly, in different environments, tuning may be beneficial, though      %
% it has not yet been necessary to modify these settings during testing.     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% termination timeout when MaxT == 0
% (if MaxR == 0, take MaxT as a terminate timeout value, i.e., as if MaxR == 1)
-define(TIMEOUT_TERMINATE_DEFAULT,  2000). % milliseconds
% absolute bounds for the terminate function execution time
% when a service stops or restarts
-define(TIMEOUT_TERMINATE_MIN,    10). % milliseconds
% fail-fast is somewhat arbitrary but failure occurs in 1 minute or less
-define(TIMEOUT_TERMINATE_MAX, 60000). % milliseconds

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

% interval at which count_process_dynamic checks the service's incoming queue
% before terminating a service process when reducing the number of service
% processes due to an incoming service request rate lower than required
-define(COUNT_PROCESS_DYNAMIC_INTERVAL, 500). % milliseconds

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

% handle the dict type change
-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.

% handle the queue type change
-ifdef(ERLANG_OTP_VERSION_16).
-type queue_proxy(_Value) :: queue().
-else.
-type queue_proxy(Value) :: queue:queue(Value).
-endif.

% for features specific to Erlang/OTP version 18.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_16).
-else.
-ifdef(ERLANG_OTP_VERSION_17).
-else.
-define(ERLANG_OTP_VERSION_18_FEATURES, true).
-endif.
-endif.

% provide a simple way of switching from the dict module to the maps module
-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
-type maps_proxy(_Key, _Value) :: any().
-define(MAP_NEW(),           maps:new()).
-define(MAP_FIND(K, M),      maps:find(K, M)).
-define(MAP_FETCH(K, M),     maps:get(K, M)).
-define(MAP_STORE(K, V, M),  maps:put(K, V, M)).
-define(MAP_ERASE(K, M),     maps:remove(K, M)).
-define(MAP_TO_LIST(M),      maps:to_list(M)).
-define(MSGPACK_MAP, map).
-else.
-type maps_proxy(Key, Value) :: dict_proxy(Key, Value).
-define(MAP_NEW(),           dict:new()).
-define(MAP_FIND(K, M),      dict:find(K, M)).
-define(MAP_FETCH(K, M),     dict:fetch(K, M)).
-define(MAP_STORE(K, V, M),  dict:store(K, V, M)).
-define(MAP_ERASE(K, M),     dict:erase(K, M)).
-define(MAP_TO_LIST(M),      dict:to_list(M)).
-define(MSGPACK_MAP, jsx).
-endif.

% used to calculate the timeout_terminate based on MaxT / MaxR
-define(TIMEOUT_TERMINATE_CALC0(MaxT),
        ((1000 * MaxT) - ?TIMEOUT_DELTA)).
-define(TIMEOUT_TERMINATE_CALC1(MaxR, MaxT),
        ((1000 * MaxT) div MaxR - ?TIMEOUT_DELTA)).

% cloudi_x_pqueue4 usage limited by the signed byte integer storage
-define(PRIORITY_HIGH, -128).
-define(PRIORITY_LOW, 127).

% create the locally registered name for a cpg scope
-define(SCOPE_DEFAULT, cpg_default_scope).
-define(SCOPE_CUSTOM_PREFIX, "cloudi_x_cpg_x_").
-define(SCOPE_ASSIGN(Scope),
        if
            Scope =:= default ->
                % DEFAULT_SCOPE in cpg application
                ?SCOPE_DEFAULT;
            true ->
                erlang:list_to_atom(?SCOPE_CUSTOM_PREFIX ++
                                    erlang:atom_to_list(Scope))
        end).
-define(SCOPE_FORMAT(Name),
        if
            Name =:= ?SCOPE_DEFAULT ->
                default;
            true ->
                ?SCOPE_CUSTOM_PREFIX ++ L = erlang:atom_to_list(Name),
                erlang:list_to_atom(L)
        end).

% create the locally registered name for a cloudi_core_i_logger
% formatter output gen_event module
-define(LOGGING_FORMATTER_OUTPUT_CUSTOM_PREFIX,
        "cloudi_core_i_logger_output_sup_").
-define(LOGGING_FORMATTER_OUTPUT_ASSIGN(Output, Instance),
        if
            Output =:= undefined ->
                undefined;
            true ->
                erlang:list_to_atom(?LOGGING_FORMATTER_OUTPUT_CUSTOM_PREFIX ++
                                    erlang:atom_to_list(Output) ++ "_" ++
                                    erlang:integer_to_list(Instance))
        end).

% maximum timeout value for erlang:send_after/3 and gen_server:call
-define(TIMEOUT_MAX_ERLANG, 4294967295).
% maximum timeout value for a service request
% (limitation for internal service requests, external service requests
%  should have a maximum of TIMEOUT_MAX_ERLANG)
-define(TIMEOUT_MAX, ?TIMEOUT_MAX_ERLANG - ?TIMEOUT_DELTA).

