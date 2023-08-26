%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Health Check CloudI Service==
%%% Each interval, do a DNS lookup of a hostname (if a hostname was provided)
%%% and check the health of each IP address.
%%%
%%% Any DNS failure can have custom functions for the event with the
%%% dns_failure function and/or the dns_restored function.  The DNS failure
%%% will only occur once for each hostname provided and old IP address
%%% information will still be used for the health check.
%%%
%%% The health check uses either a TCP port number or a single ping request
%%% to each of the IP addresses associated with the hostname.
%%% If any of the IP addresses had a failure the host's health_failed boolean
%%% will be set to true.  The availability and downtime of a host is based
%%% on whether all IP addresses pass the health check.
%%%
%%% Any TCP health check failure can have custom functions for the event with
%%% the tcp_failure function and/or the tcp_restored function.
%%% A tcp_test function custom function can be provided if the TCP socket
%%% needs additional health check criteria.
%%%
%%% Any ping health check failure can have custom functions for the event with
%%% the ping failure function and/or the ping_restored function.
%%%
%%% All _failure custom functions are provided with the failure Reason
%%% as a parameter.  All _restored custom functions are provided with
%%% the native monotonic time when the failure occurred (TimeFailure) and
%%% when the restore occurred (TimeRestored) to provide the event duration.
%%%
%%% A dns_ip_added function can be provided for execution when an IP address
%%% is added after a successful DNS lookup response.
%%% A dns_ip_removed_healthy function and/or a dns_ip_removed_failed function
%%% can be provided for execution when an IP address is removed due to a
%%% successful DNS lookup response.  DNS-based load balancing may cause
%%% excessive execution of these functions.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2022-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2022-2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_health_check).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([tcp_test_http/3,
         tcp_test_https/3]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_availability.hrl").
-include_lib("cloudi_core/include/cloudi_constants.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_HOSTS,                         []).
-define(DEFAULT_INTERVAL,                       5). % seconds
        % May be set for each host too.
-define(DEFAULT_IPV4,                        true).
        % May be set for each host too.
-define(DEFAULT_IPV6,                        true).
        % May be set for each host too.
-define(DEFAULT_ERROR_LEVEL,                error).
-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                debug).

% hosts configuration arguments
-define(DEFAULT_HEALTH,                       tcp).
        % Set to ping if no TCP port is available for the
        % health check.
-define(DEFAULT_PORT,                          80).
        % TCP port used for the host's TCP health check.
-define(DEFAULT_DNS_FAILURE,            undefined).
-define(DEFAULT_DNS_RESTORED,           undefined).
-define(DEFAULT_DNS_IP_ADDED,           undefined).
-define(DEFAULT_DNS_IP_REMOVED_HEALTHY, undefined).
-define(DEFAULT_DNS_IP_REMOVED_FAILED,  undefined).
-define(DEFAULT_TCP_FAILURE,            undefined).
-define(DEFAULT_TCP_RESTORED,           undefined).
-define(DEFAULT_TCP_TEST,               undefined).
        % If the TCP connection needs a request/response to test the
        % server's liveness, this function can be provided to test
        % the socket.  Otherwise, only the ability of the server to
        % accept a TCP connection for the configured port is tested.
        % If the function needs to create the socket, the function arity
        % can be 4 instead of 2.
-define(DEFAULT_PING_FAILURE,           undefined).
-define(DEFAULT_PING_RESTORED,          undefined).

% ensure timeouts do not delay the health check interval
-define(TIMEOUT_DELTA, 100). % milliseconds

-type hostname() ::
    nonempty_string(). % dns name or ip address
-type tcp_port() ::
    1..65535.
-type interval() ::
    1..(?TIMEOUT_MAX_ERLANG div 1000).
-type dns_failure() ::
    fun((Name :: hostname(),
         Reason :: atom()) ->
        ok).
-type dns_restored() ::
    fun((Name :: hostname(),
         TimeFailure :: cloudi_timestamp:native_monotonic(),
         TimeRestored :: cloudi_timestamp:native_monotonic()) ->
        ok).
-type dns_ip_added() ::
    fun((Name :: hostname(),
         IP :: inet:ip_address()) ->
        ok).
-type dns_ip_removed_healthy() ::
    fun((Name :: hostname(),
         IP :: inet:ip_address()) ->
        ok).
-type dns_ip_removed_failed() ::
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         TimeFailure :: cloudi_timestamp:native_monotonic(),
         TimeRemoved :: cloudi_timestamp:native_monotonic()) ->
        ok).
-type tcp_failure() ::
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         Port :: tcp_port(),
         Reason :: any()) ->
        ok).
-type tcp_restored() ::
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         Port :: tcp_port(),
         TimeFailure :: cloudi_timestamp:native_monotonic(),
         TimeRestored :: cloudi_timestamp:native_monotonic()) ->
        ok).
-type tcp_test() ::
    fun((Socket :: gen_tcp:socket(),
         Timeout :: 1..?TIMEOUT_MAX_ERLANG) ->
        ok | {error, any()}) |
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         Port :: tcp_port(),
         Timeout :: 1..?TIMEOUT_MAX_ERLANG) ->
        ok | {error, any()}).
-type ping_failure() ::
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         Reason :: string()) ->
        ok).
-type ping_restored() ::
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         TimeFailure :: cloudi_timestamp:native_monotonic(),
         TimeRestored :: cloudi_timestamp:native_monotonic()) ->
        ok).
-export_type([hostname/0,
              tcp_port/0,
              interval/0,
              dns_failure/0,
              dns_restored/0,
              dns_ip_added/0,
              dns_ip_removed_healthy/0,
              dns_ip_removed_failed/0,
              tcp_failure/0,
              tcp_restored/0,
              tcp_test/0,
              ping_failure/0,
              ping_restored/0]).

-record(host,
    {
        name
            :: hostname(),
        ipv4 = []
            :: list(inet:ip4_address()),
        ipv4_allowed
            :: boolean(),
        ipv6 = []
            :: list(inet:ip6_address()),
        ipv6_allowed
            :: boolean(),
        health
            :: tcp | ping,
        port
            :: tcp_port(),
        interval
            :: interval(),
        start_time
            :: cloudi_timestamp:native_monotonic(),
        dns_disabled = false
            :: boolean(), % name is an IP address
        dns_failed = false
            :: boolean(),
        dns_failed_time = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        dns_failure
            :: undefined | dns_failure(),
        dns_restored
            :: undefined | dns_restored(),
        dns_ip_added
            :: undefined | dns_ip_added(),
        dns_ip_removed_healthy
            :: undefined | dns_ip_removed_healthy(),
        dns_ip_removed_failed
            :: undefined | dns_ip_removed_failed(),
        health_failed = false
            :: boolean(),
        health_failed_count = 0
            :: non_neg_integer(),
        health_failed_time = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        health_failed_time_ip = #{}
            :: #{inet:ip_address() := cloudi_timestamp:native_monotonic()},
        tcp_failure
            :: undefined | tcp_failure(),
        tcp_restored
            :: undefined | tcp_restored(),
        tcp_test
            :: undefined | tcp_test(),
        ping_failure
            :: undefined | ping_failure(),
        ping_restored
            :: undefined | ping_restored()
    }).

-record(state,
    {
        service
            :: cloudi_service:source(),
        process_index
            :: non_neg_integer(),
        process_count
            :: pos_integer(),
        prefix
            :: cloudi:service_name_pattern(),
        hosts
            :: cloudi_x_trie:cloudi_x_trie(), % hostname() -> #host{}
        durations_down = cloudi_availability:durations_new()
            :: cloudi_availability:durations(hostname()),
        error_level
            :: trace | debug | info | warn | error | fatal,
        debug_level
            :: off | trace | debug | info | warn | error | fatal
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% tcp_test HTTP/HTTPS functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===HTTP tcp_test function.===
%% Add as
%% {{cloudi_service_health_check, tcp_test_http, [Method, Path, StatusCode]}}
%% with values provided for Method, Path and StatusCode.
%% @end
%%-------------------------------------------------------------------------

-spec tcp_test_http(Method :: head | get,
                    Path :: nonempty_string(),
                    StatusCode :: 200..399) ->
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         Port :: tcp_port(),
         Timeout :: 1..?TIMEOUT_MAX_ERLANG) ->
        ok | {error, any()}).

tcp_test_http(Method, [_ | _] = Path, StatusCode)
    when is_atom(Method), is_integer(StatusCode) ->
    Profile = default,
    fun(Name, IP, Port, Timeout) ->
        SocketAddress = case IP of
            {_, _, _, _} ->
                inet:ntoa(IP) ++ ":" ++ erlang:integer_to_list(Port);
            {_, _, _, _, _, _, _, _} ->
                "[" ++ inet:ntoa(IP) ++ "]:" ++ erlang:integer_to_list(Port)
        end,
        URL = "http://" ++ SocketAddress ++ Path,
        RequestHeaders = [{<<"host">>, erlang:list_to_binary(Name)}],
        case cloudi_x_hackney:request(Method, URL, RequestHeaders, <<>>,
                                      [with_body,
                                       {connect_timeout, Timeout},
                                       {recv_timeout, Timeout},
                                       {pool, Profile}]) of
            {ok, ResponseStatusCode, ResponseHeaders, Response} ->
                if
                    ResponseStatusCode == StatusCode ->
                        ok;
                    true ->
                        {error,
                         {http_response_mismatch,
                          ResponseStatusCode, ResponseHeaders, Response}}
                end;
            {error, _} = Error ->
                Error
        end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===HTTPS tcp_test function.===
%% Add as
%% {{cloudi_service_health_check, tcp_test_https, [Method, Path, StatusCode]}}
%% with values provided for Method, Path and StatusCode.
%% @end
%%-------------------------------------------------------------------------

-spec tcp_test_https(Method :: head | get,
                     Path :: nonempty_string(),
                     StatusCode :: 200..399) ->
    fun((Name :: hostname(),
         IP :: inet:ip_address(),
         Port :: tcp_port(),
         Timeout :: 1..?TIMEOUT_MAX_ERLANG) ->
        ok | {error, any()}).

tcp_test_https(Method, [_ | _] = Path, StatusCode)
    when is_atom(Method), is_integer(StatusCode) ->
    Profile = default,
    fun(Name, IP, Port, Timeout) ->
        SocketAddress = case IP of
            {_, _, _, _} ->
                inet:ntoa(IP) ++ ":" ++ erlang:integer_to_list(Port);
            {_, _, _, _, _, _, _, _} ->
                "[" ++ inet:ntoa(IP) ++ "]:" ++ erlang:integer_to_list(Port)
        end,
        URL = "https://" ++ SocketAddress ++ Path,
        RequestHeaders = [{<<"host">>, erlang:list_to_binary(Name)}],
        SSLOptions = [{server_name_indication, Name}] ++
                     cloudi_x_hackney_ssl:check_hostname_opts(Name) ++
                     cloudi_x_hackney_ssl:cipher_opts(),
        case cloudi_x_hackney:request(Method, URL, RequestHeaders, <<>>,
                                      [with_body,
                                       {ssl_options, SSLOptions},
                                       {connect_timeout, Timeout},
                                       {recv_timeout, Timeout},
                                       {pool, Profile}]) of
            {ok, ResponseStatusCode, ResponseHeaders, Response} ->
                if
                    ResponseStatusCode == StatusCode ->
                        ok;
                    true ->
                        {error,
                         {https_response_mismatch,
                          ResponseStatusCode, ResponseHeaders, Response}}
                end;
            {error, _} = Error ->
                Error
        end
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {hosts,                    ?DEFAULT_HOSTS},
        {interval,                 ?DEFAULT_INTERVAL},
        {ipv4,                     ?DEFAULT_IPV4},
        {ipv6,                     ?DEFAULT_IPV6},
        {error_level,              ?DEFAULT_ERROR_LEVEL},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Hosts, IntervalDefault, IPv4Allowed, IPv6Allowed, ErrorLogLevel,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
    true = is_list(Hosts) andalso (Hosts /= []),
    true = is_integer(IntervalDefault) andalso
           (IntervalDefault > 0) andalso
           (IntervalDefault =< ?TIMEOUT_MAX_ERLANG div 1000),
    true = is_boolean(IPv4Allowed),
    true = is_boolean(IPv6Allowed),
    true = (IPv4Allowed /= false) orelse
           (IPv6Allowed /= false),
    Service = cloudi_service:self(Dispatcher),
    ProcessIndex = cloudi_service:process_index(Dispatcher),
    ProcessCount = cloudi_service:process_count(Dispatcher),
    ProcessCountMin = cloudi_service:process_count_min(Dispatcher),
    ProcessCountMax = cloudi_service:process_count_max(Dispatcher),
    true = (ProcessCountMin =:= ProcessCountMax) andalso
           (ProcessCountMin =:= ProcessCount),
    HostsLoaded = hosts_load(Hosts, ProcessIndex, ProcessCount, Service,
                             IntervalDefault, IPv4Allowed, IPv6Allowed),
    true = ((ErrorLogLevel =:= trace) orelse
            (ErrorLogLevel =:= debug) orelse
            (ErrorLogLevel =:= info) orelse
            (ErrorLogLevel =:= warn) orelse
            (ErrorLogLevel =:= error) orelse
            (ErrorLogLevel =:= fatal)),
    true = is_boolean(Debug),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    DebugLogLevel = if
        Debug =:= false ->
            off;
        Debug =:= true ->
            DebugLevel
    end,
    ok = cloudi_service:subscribe(Dispatcher, "hosts.erl"),
    ok = cloudi_service:subscribe(Dispatcher, "hosts.erl/get"),
    ok = cloudi_service:subscribe(Dispatcher, "hosts.json/get"),
    ok = cloudi_service:subscribe(Dispatcher,
                                  "hosts/" ++
                                  erlang:integer_to_list(ProcessIndex)),
    {ok, #state{service = Service,
                process_index = ProcessIndex,
                process_count = ProcessCount,
                prefix = Prefix,
                hosts = HostsLoaded,
                error_level = ErrorLogLevel,
                debug_level = DebugLogLevel}}.

cloudi_service_handle_request(_RequestType, Name, _Pattern,
                              RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Source,
                              #state{process_index = ProcessIndex,
                                     process_count = ProcessCount,
                                     prefix = Prefix,
                                     hosts = Hosts,
                                     durations_down = DurationsDown} = State,
                              _Dispatcher) ->
    KeyValues0 = cloudi_request_info:key_value_parse(RequestInfo),
    ProcessIndexLastKey = <<"process-index-last">>,
    {ProcessIndexLast,
     KeyValues1} = case cloudi_key_value:find(ProcessIndexLastKey,
                                              KeyValues0) of
        {ok, ProcessIndexLastBin} ->
            {erlang:binary_to_integer(ProcessIndexLastBin),
             KeyValues0};
        error ->
            ProcessIndexLastValue = if
                ProcessIndex == 0 ->
                    ProcessCount - 1;
                true ->
                    ProcessIndex - 1
            end,
            {ProcessIndexLastValue,
             cloudi_key_value:
             store(ProcessIndexLastKey,
                   erlang:integer_to_binary(ProcessIndexLastValue),
                   KeyValues0)}
    end,
    ProcessIndexLastStr = erlang:integer_to_list(ProcessIndexLast),
    TextFormatKey = <<"text-format">>,
    {Last,
     HostsInfo,
     TextFormatValue} = case cloudi_service_name:suffix(Prefix, Name) of
        "hosts.erl" ->
            % request from an internal service
            {ProcessIndexLast == ProcessIndex,
             hosts_list(Hosts, DurationsDown),
             <<"none">>};
        "hosts.erl/get" ->
            {ProcessIndexLast == ProcessIndex,
             hosts_list(Hosts, DurationsDown),
             <<"erl">>};
        "hosts.json/get" ->
            {ProcessIndexLast == ProcessIndex,
             hosts_list(Hosts, DurationsDown),
             <<"json">>};
        "hosts/" ++ Index ->
            {(ProcessIndexLastStr == Index) orelse
             (ProcessIndexLast >= ProcessCount),
             lists:keymerge(1, Request, hosts_list(Hosts, DurationsDown)),
             element(2, cloudi_key_value:find(TextFormatKey, KeyValues0))}
    end,
    if
        Last =:= true ->
            Response = if
                TextFormatValue == <<"none">> ->
                    HostsInfo;
                TextFormatValue == <<"erl">> ->
                    convert_term_to_erlang(HostsInfo);
                TextFormatValue == <<"json">> ->
                    convert_term_to_json(HostsInfo, hosts)
            end,
            {reply, Response, State};
        Last =:= false ->
            ProcessIndexNext = (ProcessIndex + 1) rem ProcessCount,
            KeyValuesN = cloudi_key_value:store(TextFormatKey,
                                                TextFormatValue, KeyValues1),
            {forward,
             Prefix ++ "hosts/" ++ erlang:integer_to_list(ProcessIndexNext),
             cloudi_request_info:key_value_new(KeyValuesN, text_pairs),
             HostsInfo,
             State}
    end.

cloudi_service_handle_info({host, Hostname}, State, _Dispatcher) ->
    {noreply, health_check(Hostname, State)};
cloudi_service_handle_info({_, inet_getaddrs, _}, State, _Dispatcher) ->
    % old inet_getaddrs/3 message
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

hosts_load(Hosts, ProcessIndex, ProcessCount, Service,
           IntervalDefault, IPv4Allowed, IPv6Allowed) ->
    hosts_load(Hosts, cloudi_x_trie:new(),
               0, ProcessIndex, ProcessCount, Service,
               IntervalDefault, IPv4Allowed, IPv6Allowed,
               cloudi_environment:lookup()).

hosts_load([], HostsLoaded, _, _, _, _, _, _, _, _) ->
    HostsLoaded;
hosts_load([{HostnameRaw, Args} | Hosts], HostsLoaded,
           ProcessIndex, ProcessIndex, ProcessCount, Service,
           IntervalDefault, IPv4Allowed, IPv6Allowed, Environment) ->
    true = is_list(HostnameRaw),
    Hostname = hostname_to_ascii(cloudi_environment:transform(HostnameRaw,
                                                              Environment)),
    false = cloudi_x_trie:is_key(Hostname, HostsLoaded),
    Defaults = [
        {health,                   ?DEFAULT_HEALTH},
        {port,                     ?DEFAULT_PORT},
        {interval,                 IntervalDefault},
        {ipv4,                     IPv4Allowed},
        {ipv6,                     IPv6Allowed},
        {dns_failure,              ?DEFAULT_DNS_FAILURE},
        {dns_restored,             ?DEFAULT_DNS_RESTORED},
        {dns_ip_added,             ?DEFAULT_DNS_IP_ADDED},
        {dns_ip_removed_healthy,   ?DEFAULT_DNS_IP_REMOVED_HEALTHY},
        {dns_ip_removed_failed,    ?DEFAULT_DNS_IP_REMOVED_FAILED},
        {tcp_failure,              ?DEFAULT_TCP_FAILURE},
        {tcp_restored,             ?DEFAULT_TCP_RESTORED},
        {tcp_test,                 ?DEFAULT_TCP_TEST},
        {ping_failure,             ?DEFAULT_PING_FAILURE},
        {ping_restored,            ?DEFAULT_PING_RESTORED}],
    [Health, Port, Interval, IPv4AllowedHost, IPv6AllowedHost,
     DNSFailure0, DNSRestored0, DNSIPAdded0, DNSIPRemovedHealthy0,
     DNSIPRemovedFailed0, TCPFailure0, TCPRestored0, TCPTest0, PingFailure0,
     PingRestored0] = cloudi_proplists:take_values(Defaults, Args),
    true = is_integer(Port) andalso (Port > 0),
    true = is_integer(Interval) andalso
           (Interval > 0) andalso
           (Interval =< ?TIMEOUT_MAX_ERLANG div 1000),
    true = is_boolean(IPv4AllowedHost),
    true = is_boolean(IPv6AllowedHost),
    true = (IPv4AllowedHost /= false) orelse
           (IPv6AllowedHost /= false),
    DNSFailureN = cloudi_args_type:
                  function_optional(DNSFailure0, 2),
    DNSRestoredN = cloudi_args_type:
                   function_optional(DNSRestored0, 3),
    DNSIPAddedN = cloudi_args_type:
                  function_optional(DNSIPAdded0, 2),
    DNSIPRemovedHealthyN = cloudi_args_type:
                           function_optional(DNSIPRemovedHealthy0, 2),
    DNSIPRemovedFailedN = cloudi_args_type:
                          function_optional(DNSIPRemovedFailed0, 4),
    TCPFailureN = cloudi_args_type:
                  function_optional(TCPFailure0, 4),
    TCPRestoredN = cloudi_args_type:
                   function_optional(TCPRestored0, 5),
    TCPTestN = cloudi_args_type:
               function_optional_pick_any(TCPTest0, [2, 4]),
    PingFailureN = cloudi_args_type:
                   function_optional(PingFailure0, 3),
    PingRestoredN = cloudi_args_type:
                    function_optional(PingRestored0, 4),
    if
        Health =:= tcp ->
            undefined = PingFailureN,
            undefined = PingRestoredN;
        Health =:= ping ->
            undefined = TCPFailureN,
            undefined = TCPRestoredN,
            undefined = TCPTestN
    end,
    TimeStart = cloudi_timestamp:native_monotonic(),
    erlang:send_after(Interval * 1000, Service, {host, Hostname}),
    HostLoaded0 = #host{name = Hostname,
                        ipv4_allowed = IPv4AllowedHost,
                        ipv6_allowed = IPv6AllowedHost,
                        health = Health,
                        port = Port,
                        interval = Interval,
                        start_time = TimeStart,
                        dns_failure = DNSFailureN,
                        dns_restored = DNSRestoredN,
                        dns_ip_added = DNSIPAddedN,
                        dns_ip_removed_healthy = DNSIPRemovedHealthyN,
                        dns_ip_removed_failed = DNSIPRemovedFailedN,
                        tcp_failure = TCPFailureN,
                        tcp_restored = TCPRestoredN,
                        tcp_test = TCPTestN,
                        ping_failure = PingFailureN,
                        ping_restored = PingRestoredN},
    HostLoadedN = case inet:parse_ipv4_address(Hostname) of
        {ok, IPv4} ->
            true = IPv4AllowedHost,
            HostLoaded0#host{ipv4 = [IPv4],
                             dns_disabled = true};
        {error, einval} ->
            case inet:parse_ipv6_address(Hostname) of
                {ok, IPv6} ->
                    true = IPv6AllowedHost,
                    HostLoaded0#host{ipv6 = [IPv6],
                                     dns_disabled = true};
                {error, einval} ->
                    HostLoaded0
            end
    end,
    hosts_load(Hosts, cloudi_x_trie:store(Hostname, HostLoadedN, HostsLoaded),
               (ProcessIndex + 1) rem ProcessCount,
               ProcessIndex, ProcessCount, Service,
               IntervalDefault, IPv4Allowed, IPv6Allowed, Environment);
hosts_load([{HostnameRaw, _} | Hosts], HostsLoaded,
           Index, ProcessIndex, ProcessCount, Service,
           IntervalDefault, IPv4Allowed, IPv6Allowed, Environment) ->
    true = is_list(HostnameRaw),
    Hostname = cloudi_environment:transform(HostnameRaw, Environment),
    false = cloudi_x_trie:is_key(Hostname, HostsLoaded),
    hosts_load(Hosts, HostsLoaded,
               (Index + 1) rem ProcessCount,
               ProcessIndex, ProcessCount, Service,
               IntervalDefault, IPv4Allowed, IPv6Allowed, Environment).

health_check(Hostname,
             #state{service = Service,
                    hosts = Hosts,
                    durations_down = DurationsDown,
                    error_level = ErrorLogLevel,
                    debug_level = DebugLogLevel} = State) ->
    Host0 = cloudi_x_trie:fetch(Hostname, Hosts),
    #host{ipv4 = IPv4Old,
          ipv6 = IPv6Old,
          health = Health,
          interval = Interval,
          dns_disabled = DNSDisabled} = Host0,
    erlang:send_after(Interval * 1000, Service, {host, Hostname}),
    TimeoutTotal = Interval * 1000 - ?TIMEOUT_DELTA,
    TimeoutDNS = if
        DNSDisabled =:= true ->
            0;
        DNSDisabled =:= false ->
            TimeoutTotal div 2
    end,
    TimeoutHealth = TimeoutTotal - TimeoutDNS,
    Host1 = health_check_dns(Host0, TimeoutDNS, ErrorLogLevel),
    Host2 = health_check_update_ips(Host1, IPv4Old, IPv6Old,
                                    ErrorLogLevel, DebugLogLevel),
    {HostN,
     DurationsDownNew} = if
        Health =:= tcp ->
            health_check_status_tcp(Host2, DurationsDown, TimeoutHealth,
                                    ErrorLogLevel, DebugLogLevel);
        Health =:= ping ->
            health_check_status_ping(Host2, DurationsDown, TimeoutHealth,
                                     ErrorLogLevel, DebugLogLevel)
    end,
    State#state{hosts = cloudi_x_trie:store(Hostname, HostN, Hosts),
                durations_down = DurationsDownNew}.

health_check_dns(#host{dns_disabled = true} = Host0, _, _) ->
    Host0;
health_check_dns(Host0, TimeoutDNS, ErrorLogLevel) ->
    TimeoutDNSIPv4 = TimeoutDNS div 2,
    TimeoutDNSIPv6 = TimeoutDNS - TimeoutDNSIPv4,
    case health_check_dns_ipv4(Host0, TimeoutDNSIPv4) of
        {ok, Host1} ->
            case health_check_dns_ipv6(Host1, TimeoutDNSIPv6) of
                {ok, #host{ipv4 = IPv4,
                           ipv6 = IPv6} = HostN} ->
                    if
                        IPv4 == [], IPv6 == [] ->
                            dns_failure(Host0, enetunreach, ErrorLogLevel);
                        true ->
                            dns_restored(HostN, ErrorLogLevel)
                    end;
                {error, Reason} ->
                    dns_failure(Host1, Reason, ErrorLogLevel)
            end;
        {error, Reason} ->
            dns_failure(Host0, Reason, ErrorLogLevel)
    end.

health_check_dns_ipv4(#host{ipv4_allowed = false} = Host, _) ->
    {ok, Host};
health_check_dns_ipv4(#host{name = Hostname,
                            ipv6_allowed = IPv6Allowed} = Host,
                      Timeout) ->
    case inet_getaddrs(Hostname, inet, Timeout) of
        {ok, IPv4} ->
            {ok, Host#host{ipv4 = lists:usort(IPv4)}};
        {error, nxdomain} when IPv6Allowed =:= true ->
            {ok, Host#host{ipv4 = []}};
        {error, _} = Error ->
            Error
    end.

health_check_dns_ipv6(#host{ipv6_allowed = false} = Host, _) ->
    {ok, Host};
health_check_dns_ipv6(#host{name = Hostname,
                            ipv4_allowed = IPv4Allowed} = Host,
                      Timeout) ->
    case inet_getaddrs(Hostname, inet6, Timeout) of
        {ok, IPv6} ->
            {ok, Host#host{ipv6 = lists:usort(IPv6)}};
        {error, nxdomain} when IPv4Allowed =:= true ->
            {ok, Host#host{ipv6 = []}};
        {error, _} = Error ->
            Error
    end.

health_check_update_ips(#host{dns_failed = true} = HostN, _, _, _, _) ->
    HostN;
health_check_update_ips(#host{ipv4 = IPv4,
                              ipv6 = IPv6,
                              dns_failed = false} = Host0,
                        IPv4Old, IPv6Old, ErrorLogLevel, DebugLogLevel) ->
    IPsOld0 = IPv4Old ++ IPv6Old,
    {IPsOldN,
     HostN} = health_check_update_ips_added(IPv4 ++ IPv6, IPsOld0,
                                            Host0, DebugLogLevel),
    health_check_update_ips_removed(IPsOldN, HostN,
                                    ErrorLogLevel, DebugLogLevel).

health_check_update_ips_added([], IPsOldN, HostN, _) ->
    {IPsOldN, HostN};
health_check_update_ips_added([IP | IPs], IPsOld0, Host0, DebugLogLevel) ->
    case cloudi_lists:delete_checked(IP, IPsOld0) of
        false ->
            HostN = dns_ip_added(Host0, IP, DebugLogLevel),
            health_check_update_ips_added(IPs, IPsOld0, HostN, DebugLogLevel);
        IPsOldN ->
            health_check_update_ips_added(IPs, IPsOldN, Host0, DebugLogLevel)
    end.

health_check_update_ips_removed([], HostN, _, _) ->
    HostN;
health_check_update_ips_removed([IPOld | IPsOld],
                                #host{health_failed_time_ip =
                                          HealthFailedTimeIP} = Host0,
                                ErrorLogLevel, DebugLogLevel) ->
    HostN = case maps:take(IPOld, HealthFailedTimeIP) of
        error ->
            dns_ip_removed_healthy(Host0, IPOld, DebugLogLevel);
        {TimeFailure, HealthFailedTimeIPNew} ->
            dns_ip_removed_failed(Host0#host{health_failed_time_ip =
                                                 HealthFailedTimeIPNew},
                                  IPOld, TimeFailure, ErrorLogLevel)
    end,
    health_check_update_ips_removed(IPsOld, HostN,
                                    ErrorLogLevel, DebugLogLevel).

health_check_status_tcp(#host{ipv4 = IPv4,
                              ipv6 = IPv6} = Host0,
                        DurationsDown, TimeoutHealth,
                        ErrorLogLevel, DebugLogLevel) ->
    Timeout = TimeoutHealth div health_check_status_ips_checks(IPv4, IPv6),
    Host1 = health_check_status_tcp_ips(IPv4, Host0,
                                        inet, Timeout,
                                        ErrorLogLevel, DebugLogLevel),
    HostN = health_check_status_tcp_ips(IPv6, Host1,
                                        inet6, Timeout,
                                        ErrorLogLevel, DebugLogLevel),
    health_check_status_ips_update(HostN, DurationsDown).

health_check_status_tcp_ips([], HostN, _, _, _, _) ->
    HostN;
health_check_status_tcp_ips([IP | IPs], Host0, Family, Timeout,
                            ErrorLogLevel, DebugLogLevel) ->
    HostN = tcp_test(Host0, IP, Family, Timeout,
                     ErrorLogLevel, DebugLogLevel),
    health_check_status_tcp_ips(IPs, HostN, Family, Timeout,
                                ErrorLogLevel, DebugLogLevel).

health_check_status_ping(#host{ipv4 = IPv4,
                               ipv6 = IPv6} = Host0,
                         DurationsDown, TimeoutHealth,
                         ErrorLogLevel, DebugLogLevel) ->
    Timeout = TimeoutHealth div health_check_status_ips_checks(IPv4, IPv6),
    HostN = health_check_status_ping_ips(IPv4 ++ IPv6, Host0, Timeout,
                                         ErrorLogLevel, DebugLogLevel),
    health_check_status_ips_update(HostN, DurationsDown).

health_check_status_ping_ips([], HostN, _, _, _) ->
    HostN;
health_check_status_ping_ips([IP | IPs],
                             #host{} = Host0,
                             Timeout, ErrorLogLevel, DebugLogLevel) ->
    HostN = case cloudi_os_process:shell("ping -c 1 -w ~w ~s",
                                         [erlang:max(1, Timeout div 1000),
                                          inet:ntoa(IP)]) of
        {0, _} ->
            ping_restored(Host0, IP, ErrorLogLevel, DebugLogLevel);
        {Status, Output} ->
            StatusStr = if
                Status > 128 ->
                    cloudi_os_process:signal_to_string(Status - 128);
                true ->
                    erlang:integer_to_list(Status)
            end,
            Reason = cloudi_string:
                     format("ping exited with ~s (stdout/stderr below)~n~ts",
                            [StatusStr, unicode:characters_to_binary(Output)]),
            ping_failure(Host0, IP, Reason, ErrorLogLevel)
    end,
    health_check_status_ping_ips(IPs, HostN,
                                 Timeout, ErrorLogLevel, DebugLogLevel).

health_check_status_ips_checks([], []) ->
    1;
health_check_status_ips_checks([], IPv6) ->
    length(IPv6);
health_check_status_ips_checks(IPv4, []) ->
    length(IPv4);
health_check_status_ips_checks(IPv4, IPv6) ->
    length(IPv4) + length(IPv6).

health_check_status_ips_update(#host{name = Hostname,
                                     health_failed = HealthFailedOld,
                                     health_failed_time = TimeFailure,
                                     health_failed_time_ip =
                                         HealthFailedTimeIP} = Host,
                               DurationsDown) ->
    if
        HealthFailedTimeIP == #{} ->
            if
                TimeFailure =:= undefined ->
                    false = HealthFailedOld,
                    {Host,
                     DurationsDown};
                is_integer(TimeFailure) ->
                    true = HealthFailedOld,
                    TimeRestored = cloudi_timestamp:native_monotonic(),
                    {Host#host{health_failed = false,
                               health_failed_time = undefined},
                     cloudi_availability:
                     durations_store([Hostname],
                                     {TimeFailure, TimeRestored},
                                     DurationsDown)}
            end;
        true ->
            {Host#host{health_failed = true},
             DurationsDown}
    end.

dns_failure(#host{dns_failed = true} = Host, _, _) ->
    Host;
dns_failure(#host{name = Hostname,
                  dns_failed = false,
                  dns_failure = DNSFailure} = Host,
            Reason, ErrorLogLevel) ->
    % called at most once per interval
    TimeFailure = cloudi_timestamp:native_monotonic(),
    ?LOG(ErrorLogLevel,
         "\"~s\" DNS failure: ~s",
         [Hostname, Reason]),
    if
        DNSFailure =:= undefined ->
            ok;
        is_function(DNSFailure) ->
            ok = DNSFailure(Hostname, Reason)
    end,
    Host#host{dns_failed = true,
              dns_failed_time = TimeFailure}.

dns_restored(#host{dns_failed = false} = Host, _) ->
    Host;
dns_restored(#host{name = Hostname,
                   dns_failed = true,
                   dns_failed_time = TimeFailure,
                   dns_restored = DNSRestored} = Host,
             ErrorLogLevel) ->
    % called at most once per interval
    TimeRestored = cloudi_timestamp:native_monotonic(),
    NanoSeconds = cloudi_timestamp:convert(TimeRestored - TimeFailure,
                                           native, nanosecond),
    ?LOG(ErrorLogLevel,
         "\"~s\" DNS restored~n after ~s",
         [Hostname,
          cloudi_timestamp:nanoseconds_to_string(NanoSeconds)]),
    if
        DNSRestored =:= undefined ->
            ok;
        is_function(DNSRestored) ->
            ok = DNSRestored(Hostname, TimeFailure, TimeRestored)
    end,
    Host#host{dns_failed = false,
              dns_failed_time = undefined}.

dns_ip_added(#host{name = Hostname,
                   dns_ip_added = DNSIPAdded} = Host,
             IP, DebugLogLevel) ->
    ?LOG(DebugLogLevel,
         "\"~s\" DNS IP ~s added",
         [Hostname, inet:ntoa(IP)]),
    if
        DNSIPAdded =:= undefined ->
            ok;
        is_function(DNSIPAdded) ->
            ok = DNSIPAdded(Hostname, IP)
    end,
    Host.

dns_ip_removed_healthy(#host{name = Hostname,
                             dns_ip_removed_healthy =
                                 DNSIPRemovedHealthy} = Host,
                       IP, DebugLogLevel) ->
    ?LOG(DebugLogLevel,
         "\"~s\" DNS IP ~s (healthy) removed",
         [Hostname, inet:ntoa(IP)]),
    if
        DNSIPRemovedHealthy =:= undefined ->
            ok;
        is_function(DNSIPRemovedHealthy) ->
            ok = DNSIPRemovedHealthy(Hostname, IP)
    end,
    Host.

dns_ip_removed_failed(#host{name = Hostname,
                            dns_ip_removed_failed =
                                DNSIPRemovedFailed} = Host,
                      IP, TimeFailure, ErrorLogLevel) ->
    TimeRemoved = cloudi_timestamp:native_monotonic(),
    NanoSeconds = cloudi_timestamp:convert(TimeRemoved - TimeFailure,
                                           native, nanosecond),
    ?LOG(ErrorLogLevel,
         "\"~s\" DNS IP ~s (failed) removed~n after ~s",
         [Hostname, inet:ntoa(IP),
          cloudi_timestamp:nanoseconds_to_string(NanoSeconds)]),
    if
        DNSIPRemovedFailed =:= undefined ->
            ok;
        is_function(DNSIPRemovedFailed) ->
            ok = DNSIPRemovedFailed(Hostname, IP, TimeFailure, TimeRemoved)
    end,
    Host.

inet_getaddrs(Hostname, Family, Timeout) ->
    % Erlang/OTP inet:getaddrs/3 is undocumented and fails to timeout
    % (in Erlang/OTP 25.0)
    % so a temporary process is used here for ensuring a timeout is possible
    Parent = self(),
    Pid = erlang:spawn_link(fun() ->
        Parent ! {self(), inet_getaddrs,
                  inet:getaddrs(Hostname, Family, Timeout)}
    end),
    receive
        {Pid, inet_getaddrs, Result} ->
            Result
    after
        Timeout ->
            true = erlang:unlink(Pid),
            true = erlang:exit(Pid, kill),
            {error, nxdomain}
    end.

tcp_test(#host{name = Hostname,
               port = Port,
               tcp_test = TCPTest} = HostN,
         IP, _, Timeout,
         ErrorLogLevel, DebugLogLevel)
    when is_function(TCPTest, 4) ->
    case TCPTest(Hostname, IP, Port, Timeout) of
        ok ->
            tcp_restored(HostN, IP, Port,
                         ErrorLogLevel, DebugLogLevel);
        {error, Reason} ->
            tcp_failure(HostN, IP, Port,
                        Reason, Timeout, ErrorLogLevel)
    end;
tcp_test(#host{port = Port,
               tcp_test = TCPTest} = Host0,
         IP, Family, Timeout,
         ErrorLogLevel, DebugLogLevel) ->
    case gen_tcp:connect(IP, Port, [Family], Timeout) of
        {ok, Socket} ->
            HostN = if
                TCPTest =:= undefined ->
                    tcp_restored(Host0, IP, Port,
                                 ErrorLogLevel, DebugLogLevel);
                is_function(TCPTest, 2) ->
                    case TCPTest(Socket, Timeout) of
                        ok ->
                            tcp_restored(Host0, IP, Port,
                                         ErrorLogLevel, DebugLogLevel);
                        {error, Reason} ->
                            tcp_failure(Host0, IP, Port,
                                        Reason, Timeout, ErrorLogLevel)
                    end
            end,
            ok = gen_tcp:close(Socket),
            HostN;
        {error, Reason} ->
            tcp_failure(Host0, IP, Port, Reason, Timeout, ErrorLogLevel)
    end.

tcp_failure(#host{name = Hostname,
                  health_failed_count = HealthFailedCount,
                  health_failed_time = HealthFailedTime,
                  health_failed_time_ip = HealthFailedTimeIP,
                  tcp_failure = TCPFailure} = Host,
            IP, Port, Reason, Timeout, ErrorLogLevel) ->
    case maps:is_key(IP, HealthFailedTimeIP) of
        true ->
            Host;
        false ->
            % executed for each ip address failure
            TimeFailure = cloudi_timestamp:native_monotonic(),
            ReasonInfo = if
                Reason =:= timeout ->
                    cloudi_string:format("~n (after ~w milliseconds)",
                                         [Timeout]);
                true ->
                    ""
            end,
            ?LOG(ErrorLogLevel,
                 "\"~s\" TCP failure: ~s port ~w failed:~n ~tp~s",
                 [Hostname, inet:ntoa(IP), Port, Reason, ReasonInfo]),
            if
                TCPFailure =:= undefined ->
                    ok;
                is_function(TCPFailure) ->
                    ok = TCPFailure(Hostname, IP, Port, Reason)
            end,
            {HealthFailedCountNew,
             HealthFailedTimeNew} = if
                HealthFailedTime =:= undefined ->
                    % first ip address to fail recently
                    {HealthFailedCount + 1,
                     TimeFailure};
                is_integer(HealthFailedTime) ->
                    {HealthFailedCount,
                     HealthFailedTime}
            end,
            HealthFailedTimeIPNew = maps:put(IP, TimeFailure,
                                             HealthFailedTimeIP),
            Host#host{health_failed_count = HealthFailedCountNew,
                      health_failed_time = HealthFailedTimeNew,
                      health_failed_time_ip = HealthFailedTimeIPNew}
    end.

tcp_restored(#host{name = Hostname,
                   health_failed_time_ip = HealthFailedTimeIP,
                   tcp_restored = TCPRestored} = Host,
             IP, Port, ErrorLogLevel, DebugLogLevel) ->
    case maps:take(IP, HealthFailedTimeIP) of
        error ->
            ?LOG(DebugLogLevel,
                 "\"~s\" TCP healthy: ~s port ~w",
                 [Hostname, inet:ntoa(IP), Port]),
            Host;
        {TimeFailure, HealthFailedTimeIPNew} ->
            % executed for each ip address restored
            TimeRestored = cloudi_timestamp:native_monotonic(),
            NanoSeconds = cloudi_timestamp:convert(TimeRestored - TimeFailure,
                                                   native, nanosecond),
            ?LOG(ErrorLogLevel,
                 "\"~s\" TCP restored: ~s port ~w~n after ~s",
                 [Hostname, inet:ntoa(IP), Port,
                  cloudi_timestamp:nanoseconds_to_string(NanoSeconds)]),
            if
                TCPRestored =:= undefined ->
                    ok;
                is_function(TCPRestored) ->
                    ok = TCPRestored(Hostname, IP, Port,
                                     TimeFailure, TimeRestored)
            end,
            Host#host{health_failed_time_ip = HealthFailedTimeIPNew}
    end.

ping_failure(#host{name = Hostname,
                   health_failed_count = HealthFailedCount,
                   health_failed_time = HealthFailedTime,
                   health_failed_time_ip = HealthFailedTimeIP,
                   ping_failure = PingFailure} = Host,
             IP, Reason, ErrorLogLevel) ->
    case maps:is_key(IP, HealthFailedTimeIP) of
        true ->
            Host;
        false ->
            % executed for each ip address failure
            TimeFailure = cloudi_timestamp:native_monotonic(),
            ?LOG(ErrorLogLevel,
                 "\"~s\" ping failure: ~s failed:~n~s",
                 [Hostname, inet:ntoa(IP), Reason]),
            if
                PingFailure =:= undefined ->
                    ok;
                is_function(PingFailure) ->
                    ok = PingFailure(Hostname, IP, Reason)
            end,
            {HealthFailedCountNew,
             HealthFailedTimeNew} = if
                HealthFailedTime =:= undefined ->
                    % first ip address to fail recently
                    {HealthFailedCount + 1,
                     TimeFailure};
                is_integer(HealthFailedTime) ->
                    {HealthFailedCount,
                     HealthFailedTime}
            end,
            HealthFailedTimeIPNew = maps:put(IP, TimeFailure,
                                             HealthFailedTimeIP),
            Host#host{health_failed_count = HealthFailedCountNew,
                      health_failed_time = HealthFailedTimeNew,
                      health_failed_time_ip = HealthFailedTimeIPNew}
    end.

ping_restored(#host{name = Hostname,
                    health_failed_time_ip = HealthFailedTimeIP,
                    ping_restored = PingRestored} = Host,
              IP, ErrorLogLevel, DebugLogLevel) ->
    case maps:take(IP, HealthFailedTimeIP) of
        error ->
            ?LOG(DebugLogLevel,
                 "\"~s\" ping healthy: ~s",
                 [Hostname, inet:ntoa(IP)]),
            Host;
        {TimeFailure, HealthFailedTimeIPNew} ->
            % executed for each ip address restored
            TimeRestored = cloudi_timestamp:native_monotonic(),
            NanoSeconds = cloudi_timestamp:convert(TimeRestored - TimeFailure,
                                                   native, nanosecond),
            ?LOG(ErrorLogLevel,
                 "\"~s\" ping restored: ~s~n after ~s",
                 [Hostname, inet:ntoa(IP),
                  cloudi_timestamp:nanoseconds_to_string(NanoSeconds)]),
            if
                PingRestored =:= undefined ->
                    ok;
                is_function(PingRestored) ->
                    ok = PingRestored(Hostname, IP, TimeFailure, TimeRestored)
            end,
            Host#host{health_failed_time_ip = HealthFailedTimeIPNew}
    end.

hosts_list(Hosts, DurationsDown) ->
    TimeNow = cloudi_timestamp:native_monotonic(),
    TimeDayStart = TimeNow - ?NATIVE_TIME_IN_DAY,
    TimeWeekStart = TimeNow - ?NATIVE_TIME_IN_WEEK,
    TimeMonthStart = TimeNow - ?NATIVE_TIME_IN_MONTH,
    TimeYearStart = TimeNow - ?NATIVE_TIME_IN_YEAR,
    TimeOffset = erlang:time_offset(),
    cloudi_x_trie:foldr(fun(Hostname,
                            #host{ipv4 = IPv4,
                                  ipv4_allowed = IPv4Allowed,
                                  ipv6 = IPv6,
                                  ipv6_allowed = IPv6Allowed,
                                  health = Health,
                                  port = Port,
                                  interval = Interval,
                                  start_time = TimeStart,
                                  dns_failed = DNSFailed,
                                  dns_failed_time = DNSFailedTime,
                                  health_failed = HealthFailed,
                                  health_failed_count = HealthFailedCount,
                                  health_failed_time = TimeFailure,
                                  health_failed_time_ip = HealthFailedTimeIP},
                            L) ->
        DurationsDownTmp = if
            TimeFailure =:= undefined ->
                DurationsDown;
            is_integer(TimeFailure) ->
                cloudi_availability:
                durations_store([Hostname],
                                {TimeFailure, TimeNow},
                                DurationsDown)
        end,
        HostInfo0 = [{health, Health},
                     {health_failed, HealthFailed},
                     {health_failed_time,
                      hosts_list_health_failed_time_ips(HealthFailedTimeIP,
                                                        TimeOffset)} |
                     hosts_list_status(TimeNow,
                                       TimeDayStart, TimeWeekStart,
                                       TimeMonthStart, TimeYearStart,
                                       TimeOffset,
                                       TimeStart, HealthFailedCount,
                                       cloudi_availability:
                                       durations_state(Hostname,
                                                       DurationsDownTmp))],
        HostInfo1 = if
            DNSFailedTime =:= undefined ->
                HostInfo0;
            is_integer(DNSFailedTime) ->
                DNSFailedMicroSeconds = cloudi_timestamp:
                                        convert(DNSFailedTime + TimeOffset,
                                                native, microsecond),
                [{dns_failed_time,
                  cloudi_timestamp:
                  microseconds_epoch_to_string(DNSFailedMicroSeconds)} |
                 HostInfo0]
        end,
        HostInfo2 = [{interval, Interval},
                     {dns_failed, DNSFailed} | HostInfo1],
        HostInfo3 = if
            Health =:= tcp ->
                [{port, Port} | HostInfo2];
            Health =:= ping ->
                HostInfo2
        end,
        HostInfo4 = if
            IPv6Allowed =:= true ->
                [{ipv6, [inet:ntoa(IP) || IP <- IPv6]} | HostInfo3];
            IPv6Allowed =:= false ->
                HostInfo3
        end,
        HostInfoN = if
            IPv4Allowed =:= true ->
                [{ipv4, [inet:ntoa(IP) || IP <- IPv4]} | HostInfo4];
            IPv4Allowed =:= false ->
                HostInfo4
        end,
        [{Hostname, HostInfoN} | L]
    end, [], Hosts).

hosts_list_status(TimeNow,
                  TimeDayStart, TimeWeekStart,
                  TimeMonthStart, TimeYearStart, TimeOffset,
                  TimeStart, TCPFailedCount,
                  DurationsStateDown) ->
    NanoSeconds = cloudi_timestamp:
                  convert(TimeNow - TimeStart, native, nanosecond),
    Tracked = cloudi_timestamp:
              nanoseconds_to_string(NanoSeconds),
    {ApproximateYearDown,
     NanoSecondsYearDown,
     ViewYearDown} = cloudi_availability:
                     durations_sum_with_view(DurationsStateDown,
                                             TimeYearStart,
                                             TimeNow,
                                             year,
                                             TimeOffset),
    {ApproximateMonthDown,
     NanoSecondsMonthDown,
     ViewMonthDown} = cloudi_availability:
                      durations_sum_with_view(DurationsStateDown,
                                              TimeMonthStart,
                                              TimeNow,
                                              month,
                                              TimeOffset),
    {ApproximateWeekDown,
     NanoSecondsWeekDown,
     ViewWeekDown} = cloudi_availability:
                     durations_sum_with_view(DurationsStateDown,
                                             TimeWeekStart,
                                             TimeNow,
                                             week,
                                             TimeOffset),
    {ApproximateDayDown,
     NanoSecondsDayDown,
     ViewDayDown} = cloudi_availability:
                    durations_sum_with_view(DurationsStateDown,
                                            TimeDayStart,
                                            TimeNow,
                                            day,
                                            TimeOffset),
    Status0 = [],
    Status1 = case cloudi_availability:
                   nanoseconds_to_availability_year(NanoSeconds,
                                                    ApproximateYearDown,
                                                    NanoSecondsYearDown) of
        ?AVAILABILITY_ZERO ->
            Status0;
        AvailabilityYear ->
            [{availability_year,
              AvailabilityYear} | Status0]
    end,
    Status2 = case cloudi_availability:
                   nanoseconds_to_availability_month(NanoSeconds,
                                                     ApproximateMonthDown,
                                                     NanoSecondsMonthDown) of
        ?AVAILABILITY_ZERO ->
            Status1;
        AvailabilityMonth ->
            [{availability_month,
              AvailabilityMonth} | Status1]
    end,
    Status3 = case cloudi_availability:
                   nanoseconds_to_availability_week(NanoSeconds,
                                                    ApproximateWeekDown,
                                                    NanoSecondsWeekDown) of
        ?AVAILABILITY_ZERO ->
            Status2;
        AvailabilityWeek ->
            [{availability_week,
              AvailabilityWeek} | Status2]
    end,
    Status4 = [{availability_day,
                cloudi_availability:
                nanoseconds_to_availability_day(NanoSeconds,
                                                ApproximateDayDown,
                                                NanoSecondsDayDown)} | Status3],
    DowntimeYear = TimeStart =< TimeMonthStart andalso
                   NanoSecondsYearDown > 0,
    DowntimeMonth = TimeStart =< TimeWeekStart andalso
                    (NanoSecondsMonthDown > 0 orelse
                     NanoSecondsYearDown > 0),
    DowntimeWeek = TimeStart =< TimeDayStart andalso
                   (NanoSecondsWeekDown > 0 orelse
                    NanoSecondsMonthDown > 0 orelse
                    NanoSecondsYearDown > 0),
    DowntimeDay = NanoSecondsDayDown > 0 orelse
                  NanoSecondsWeekDown > 0 orelse
                  NanoSecondsMonthDown > 0 orelse
                  NanoSecondsYearDown > 0,
    Status5 = if
        DowntimeYear =:= true ->
            [{outages_year,
              ViewYearDown} |
             Status4];
        DowntimeYear =:= false ->
            Status4
    end,
    Status6 = if
        DowntimeMonth =:= true ->
            [{outages_month,
              ViewMonthDown} |
             Status5];
        DowntimeMonth =:= false ->
            Status5
    end,
    Status7 = if
        DowntimeWeek =:= true ->
            [{outages_week,
              ViewWeekDown} |
             Status6];
        DowntimeWeek =:= false ->
            Status6
    end,
    Status8 = if
        DowntimeDay =:= true ->
            [{outages_day,
              ViewDayDown} |
             Status7];
        DowntimeDay =:= false ->
            Status7
    end,
    Status9 = if
        DowntimeYear =:= true ->
            [{downtime_year,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsYearDown,
                                       ApproximateYearDown)} |
             Status8];
        DowntimeYear =:= false ->
            Status8
    end,
    Status10 = if
        DowntimeMonth =:= true ->
            [{downtime_month,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsMonthDown,
                                       ApproximateMonthDown)} |
             Status9];
        DowntimeMonth =:= false ->
            Status9
    end,
    Status11 = if
        DowntimeWeek =:= true ->
            [{downtime_week,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsWeekDown,
                                       ApproximateWeekDown)} |
             Status10];
        DowntimeWeek =:= false ->
            Status10
    end,
    StatusN = if
        DowntimeDay =:= true ->
            [{downtime_day,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsDayDown,
                                       ApproximateDayDown)} |
             Status11];
        DowntimeDay =:= false ->
            Status11
    end,
    [{tracked, Tracked},
     {tracked_failures, erlang:integer_to_list(TCPFailedCount)} | StatusN].

hosts_list_health_failed_time_ips(HealthFailedTimeIP, TimeOffset) ->
    maps:fold(fun(IP, TimeFailure, L) ->
        MicroSeconds = cloudi_timestamp:
                       convert(TimeFailure + TimeOffset,
                               native, microsecond),
        [[{ip, inet:ntoa(IP)},
          {time,
           cloudi_timestamp:
           microseconds_epoch_to_string(MicroSeconds)}] | L]
    end, [], HealthFailedTimeIP).

hostname_to_ascii(Name) ->
    case lists:all(fun cloudi_x_idna_ucs:is_ascii/1, Name) of
        true ->
            Name;
        false ->
            cloudi_x_idna:utf8_to_ascii(
                erlang:binary_to_list(unicode:characters_to_binary(Name)))
    end.

convert_term_to_erlang(Result) ->
    convert_term_to_erlang_string(Result).

convert_term_to_erlang_string(Result) ->
    cloudi_string:format_to_binary("~p", [Result]).

convert_term_to_json(L, Method)
    when Method =:= hosts ->
    json_encode([{<<"success">>, true},
                 {erlang:atom_to_binary(Method, utf8),
                  [[{<<"hostname">>, unicode:characters_to_binary(Hostname)} |
                    convert_term_to_json_options(Host)]
                   || {Hostname, Host} <- L]}]).

convert_term_to_json_option([] = Value) ->
    Value;
convert_term_to_json_option([{Key, _} | _] = Value)
    when is_atom(Key) ->
    convert_term_to_json_options(Value);
convert_term_to_json_option([[{Key, _} | _] | _] = Value)
    when is_atom(Key) ->
    [convert_term_to_json_options(Options)
     || Options <- Value];
convert_term_to_json_option([H | _] = Value)
    when is_integer(H), H > 0 ->
    unicode:characters_to_binary(Value);
convert_term_to_json_option([[H | _] | _] = Value)
    when is_integer(H), H > 0 ->
    convert_term_to_json_strings(Value);
convert_term_to_json_option(Value)
    when is_number(Value) ->
    Value;
convert_term_to_json_option(Value)
    when is_atom(Value) ->
    if
        is_boolean(Value) ->
            Value;
        true ->
            erlang:atom_to_binary(Value, utf8)
    end.

convert_term_to_json_options([]) ->
    [];
convert_term_to_json_options([{Key, Value} | Options]) ->
    [{erlang:atom_to_binary(Key, utf8),
      convert_term_to_json_option(Value)} |
     convert_term_to_json_options(Options)].

convert_term_to_json_strings([]) ->
    [];
convert_term_to_json_strings([S | L]) ->
    [unicode:characters_to_binary(S) |
     convert_term_to_json_strings(L)].

json_encode(Term) ->
    cloudi_x_jsx:encode(Term, [{indent, 1}]).

