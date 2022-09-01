%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Health Check CloudI Service==
%%% Each interval, do a DNS lookup of a hostname (if a hostname was provided)
%%% and attempt to open a TCP port on each IP address to determine if it is
%%% healthy.
%%%
%%% A dns_failure function and/or a tcp_failure function can be provided for
%%% execution when the health check fails.  The DNS failure will only occur
%%% once for each hostname provided and old IP address information will still
%%% be used for the TCP port check.  A TCP failure can occur for each
%%% IP address.  If any of the IP addresses had a TCP failure the host's
%%% tcp_failed boolean will be set to true.
%%%
%%% A dns_restored function and/or a tcp_restored function can be provided for
%%% execution when the health check succeeds after previously failing.
%%% The native monotonic time is provided for when the failure occurred
%%% (TimeFailure) and when the restore occurred (TimeRestored)
%%% to provide the event duration.
%%%
%%% The availability and downtime of a host is based on the ability of all
%%% of its IP addresses to accept TCP connections on the configured port.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_health_check).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_availability.hrl").
-include_lib("cloudi_core/include/cloudi_constants.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_HOSTS,                         []).
-define(DEFAULT_IPV4,                        true).
        % May be set for each host too.
-define(DEFAULT_IPV6,                        true).
        % May be set for each host too.
-define(DEFAULT_DEBUG,                       true). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                error).

% hosts configuration arguments
-define(DEFAULT_PORT,                          80).
        % TCP port used for the host's TCP health check.
-define(DEFAULT_INTERVAL,                       5). % seconds
-define(DEFAULT_DNS_FAILURE,            undefined).
-define(DEFAULT_DNS_RESTORED,           undefined).
-define(DEFAULT_TCP_FAILURE,            undefined).
-define(DEFAULT_TCP_RESTORED,           undefined).
-define(DEFAULT_TCP_TEST,               undefined).
        % If the TCP connection needs a request/response to test the
        % server's liveness, this function can be provided to test
        % the socket.  Otherwise, only the ability of the server to
        % accept a TCP connection for the configured port is tested.

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
-type tcp_failure() ::
    fun((Name :: hostname(),
         IP :: inet:ip4_address() | inet:ip6_address(),
         Port :: tcp_port(),
         Reason :: atom()) ->
        ok).
-type tcp_restored() ::
    fun((Name :: hostname(),
         IP :: inet:ip4_address() | inet:ip6_address(),
         Port :: tcp_port(),
         TimeFailure :: cloudi_timestamp:native_monotonic(),
         TimeRestored :: cloudi_timestamp:native_monotonic()) ->
        ok).
-type tcp_test() ::
    fun((Socket :: gen_tcp:socket(),
         Timeout :: 1..?TIMEOUT_MAX_ERLANG) ->
        ok | {error, atom()}).
-export_type([hostname/0,
              tcp_port/0,
              interval/0,
              dns_failure/0,
              dns_restored/0,
              tcp_failure/0,
              tcp_restored/0,
              tcp_test/0]).

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
        port
            :: tcp_port(),
        interval
            :: interval(),
        start_time
            :: cloudi_timestamp:native_monotonic(),
        dns_failed = false
            :: boolean(),
        dns_failed_time = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        dns_failure
            :: undefined | dns_failure(),
        dns_restored
            :: undefined | dns_restored(),
        tcp_failed = false
            :: boolean(),
        tcp_failed_count = 0
            :: non_neg_integer(),
        tcp_failed_time = undefined
            :: undefined | cloudi_timestamp:native_monotonic(),
        tcp_failed_time_ip = #{}
            :: #{inet:ip_address() := cloudi_timestamp:native_monotonic()},
        tcp_failure
            :: undefined | tcp_failure(),
        tcp_restored
            :: undefined | tcp_restored(),
        tcp_test
            :: undefined | tcp_test()
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
        debug_level
            :: off | trace | debug | info | warn | error | fatal
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {hosts,                    ?DEFAULT_HOSTS},
        {ipv4,                     ?DEFAULT_IPV4},
        {ipv6,                     ?DEFAULT_IPV6},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Hosts, IPv4Allowed, IPv6Allowed,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
    true = is_list(Hosts) andalso (Hosts /= []),
    true = is_boolean(IPv4Allowed),
    true = is_boolean(IPv6Allowed),
    true = (IPv4Allowed /= false) orelse
           (IPv6Allowed /= false),
    Service = cloudi_service:self(Dispatcher),
    ProcessIndex = cloudi_service:process_index(Dispatcher),
    ProcessCount = cloudi_service:process_count(Dispatcher),
    HostsLoaded = hosts_load(Hosts, ProcessIndex, ProcessCount, Service,
                             IPv4Allowed, IPv6Allowed),
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
    {noreply, health_check(Hostname, State)}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

hosts_load(Hosts, ProcessIndex, ProcessCount, Service,
           IPv4Allowed, IPv6Allowed) ->
    hosts_load(Hosts, cloudi_x_trie:new(),
               0, ProcessIndex, ProcessCount, Service,
               IPv4Allowed, IPv6Allowed).

hosts_load([], HostsLoaded, _, _, _, _, _, _) ->
    HostsLoaded;
hosts_load([{Hostname, Args} | Hosts], HostsLoaded,
           ProcessIndex, ProcessIndex, ProcessCount, Service,
           IPv4Allowed, IPv6Allowed) ->
    true = is_list(Hostname) andalso is_integer(hd(Hostname)),
    false = cloudi_x_trie:is_key(Hostname, HostsLoaded),
    Defaults = [
        {port,                     ?DEFAULT_PORT},
        {interval,                 ?DEFAULT_INTERVAL},
        {ipv4,                     IPv4Allowed},
        {ipv6,                     IPv6Allowed},
        {dns_failure,              ?DEFAULT_DNS_FAILURE},
        {dns_restored,             ?DEFAULT_DNS_RESTORED},
        {tcp_failure,              ?DEFAULT_TCP_FAILURE},
        {tcp_restored,             ?DEFAULT_TCP_RESTORED},
        {tcp_test,                 ?DEFAULT_TCP_TEST}],
    [Port, Interval, IPv4AllowedHost, IPv6AllowedHost,
     DNSFailure0, DNSRestored0, TCPFailure0, TCPRestored0,
     TCPTest0] = cloudi_proplists:take_values(Defaults, Args),
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
    TCPFailureN = cloudi_args_type:
                  function_optional(TCPFailure0, 4),
    TCPRestoredN = cloudi_args_type:
                   function_optional(TCPRestored0, 5),
    TCPTestN = cloudi_args_type:
               function_optional(TCPTest0, 2),
    TimeStart = cloudi_timestamp:native_monotonic(),
    erlang:send_after(Interval * 1000, Service, {host, Hostname}),
    HostLoaded = #host{name = Hostname,
                       ipv4_allowed = IPv4AllowedHost,
                       ipv6_allowed = IPv6AllowedHost,
                       port = Port,
                       interval = Interval,
                       start_time = TimeStart,
                       dns_failure = DNSFailureN,
                       dns_restored = DNSRestoredN,
                       tcp_failure = TCPFailureN,
                       tcp_restored = TCPRestoredN,
                       tcp_test = TCPTestN},
    hosts_load(Hosts, cloudi_x_trie:store(Hostname, HostLoaded, HostsLoaded),
               (ProcessIndex + 1) rem ProcessCount,
               ProcessIndex, ProcessCount, Service,
               IPv4Allowed, IPv6Allowed);
hosts_load([{Hostname, _} | Hosts], HostsLoaded,
           Index, ProcessIndex, ProcessCount, Service,
           IPv4Allowed, IPv6Allowed) ->
    true = is_list(Hostname) andalso is_integer(hd(Hostname)),
    false = cloudi_x_trie:is_key(Hostname, HostsLoaded),
    hosts_load(Hosts, HostsLoaded,
               (Index + 1) rem ProcessCount,
               ProcessIndex, ProcessCount, Service,
               IPv4Allowed, IPv6Allowed).

health_check(Hostname,
             #state{service = Service,
                    hosts = Hosts,
                    durations_down = DurationsDown,
                    debug_level = DebugLogLevel} = State) ->
    Host0 = cloudi_x_trie:fetch(Hostname, Hosts),
    #host{interval = Interval} = Host0,
    Host1 = health_check_dns(Host0, DebugLogLevel),
    {HostN,
     DurationsDownNew} = health_check_tcp(Host1, DurationsDown, DebugLogLevel),
    erlang:send_after(Interval * 1000, Service, {host, Hostname}),
    State#state{hosts = cloudi_x_trie:store(Hostname, HostN, Hosts),
                durations_down = DurationsDownNew}.

health_check_dns(Host0, DebugLogLevel) ->
    case health_check_dns_ipv4(Host0) of
        {ok, Host1} ->
            case health_check_dns_ipv6(Host1) of
                {ok, #host{ipv4 = IPv4,
                           ipv6 = IPv6} = HostN} ->
                    if
                        IPv4 == [], IPv6 == [] ->
                            dns_failure(Host0, enetunreach, DebugLogLevel);
                        true ->
                            dns_restored(HostN, DebugLogLevel)
                    end;
                {error, Reason} ->
                    dns_failure(Host1, Reason, DebugLogLevel)
            end;
        {error, Reason} ->
            dns_failure(Host0, Reason, DebugLogLevel)
    end.

health_check_dns_ipv4(#host{ipv4_allowed = false} = Host) ->
    {ok, Host};
health_check_dns_ipv4(#host{name = Hostname,
                            ipv6_allowed = IPv6Allowed} = Host) ->
    case inet:getaddrs(Hostname, inet) of
        {ok, IPv4} ->
            {ok, Host#host{ipv4 = IPv4}};
        {error, nxdomain} when IPv6Allowed =:= true ->
            {ok, Host#host{ipv4 = []}};
        {error, _} = Error ->
            Error
    end.

health_check_dns_ipv6(#host{ipv6_allowed = false} = Host) ->
    {ok, Host};
health_check_dns_ipv6(#host{name = Hostname,
                            ipv4_allowed = IPv4Allowed} = Host) ->
    case inet:getaddrs(Hostname, inet6) of
        {ok, IPv6} ->
            {ok, Host#host{ipv6 = IPv6}};
        {error, nxdomain} when IPv4Allowed =:= true ->
            {ok, Host#host{ipv6 = []}};
        {error, _} = Error ->
            Error
    end.

health_check_tcp(#host{name = Hostname,
                       ipv4 = IPv4,
                       ipv6 = IPv6,
                       interval = Interval} = Host0,
                 DurationsDown, DebugLogLevel) ->
    Timeout = (Interval * 1000 - ?TIMEOUT_DELTA) div
              health_check_tcp_ips_checks(IPv4, IPv6),
    Host1 = health_check_tcp_ips(IPv4, Host0, inet, Timeout, DebugLogLevel),
    Host2 = health_check_tcp_ips(IPv6, Host1, inet6, Timeout, DebugLogLevel),
    HostN = health_check_tcp_ips_expire(Host2, DebugLogLevel),
    #host{tcp_failed = TCPFailedOld,
          tcp_failed_time = TimeFailure,
          tcp_failed_time_ip = TCPFailedTimeIP} = HostN,
    if
        TCPFailedTimeIP == #{} ->
            if
                TimeFailure =:= undefined ->
                    false = TCPFailedOld,
                    {HostN,
                     DurationsDown};
                is_integer(TimeFailure) ->
                    true = TCPFailedOld,
                    TimeRestored = cloudi_timestamp:native_monotonic(),
                    {HostN#host{tcp_failed = false,
                                tcp_failed_time = undefined},
                     cloudi_availability:
                     durations_store([Hostname],
                                     {TimeFailure, TimeRestored},
                                     DurationsDown)}
            end;
        true ->
            {HostN#host{tcp_failed = true},
             DurationsDown}
    end.

health_check_tcp_ips([], HostN, _, _, _) ->
    HostN;
health_check_tcp_ips([IP | IPs],
                     #host{port = Port,
                           tcp_test = TCPTest} = Host0,
                     Family, Timeout, DebugLogLevel) ->
    case gen_tcp:connect(IP, Port, [Family], Timeout) of
        {ok, Socket} ->
            HostN = if
                TCPTest =:= undefined ->
                    tcp_restored(Host0, IP, Port, DebugLogLevel);
                is_function(TCPTest) ->
                    case TCPTest(Socket, Timeout) of
                        ok ->
                            tcp_restored(Host0, IP, Port, DebugLogLevel);
                        {error, Reason} when is_atom(Reason) ->
                            tcp_failure(Host0, IP, Port, Reason, DebugLogLevel)
                    end
            end,
            ok = gen_tcp:close(Socket),
            health_check_tcp_ips(IPs, HostN, Family, Timeout, DebugLogLevel);
        {error, Reason} ->
            HostN = tcp_failure(Host0, IP, Port, Reason, DebugLogLevel),
            health_check_tcp_ips(IPs, HostN, Family, Timeout, DebugLogLevel)
    end.

health_check_tcp_ips_checks([], []) ->
    1;
health_check_tcp_ips_checks([], IPv6) ->
    length(IPv6);
health_check_tcp_ips_checks(IPv4, []) ->
    length(IPv4);
health_check_tcp_ips_checks(IPv4, IPv6) ->
    length(IPv4) + length(IPv6).

health_check_tcp_ips_expire(#host{dns_failed = true} = Host0, _) ->
    Host0;
health_check_tcp_ips_expire(#host{name = Hostname,
                                  ipv4 = IPv4,
                                  ipv6 = IPv6,
                                  dns_failed = false,
                                  tcp_failed_time_ip = TCPFailedTimeIP} = Host0,
                            DebugLogLevel) ->
    IPs = IPv4 ++ IPv6,
    true = IPs /= [],
    TCPFailedTimeIPNew = maps:filter(fun(IP, TimeFailure) ->
        case lists:member(IP, IPs) of
            true ->
                true;
            false ->
                TimeExpired = cloudi_timestamp:native_monotonic(),
                NanoSeconds = cloudi_timestamp:
                              convert(TimeExpired - TimeFailure,
                                      native, nanosecond),
                ?LOG(DebugLogLevel,
                     "\"~s\" TCP IP expired ~s after ~s",
                     [Hostname, inet:ntoa(IP),
                      cloudi_timestamp:nanoseconds_to_string(NanoSeconds)]),
                false
        end
    end, TCPFailedTimeIP),
    Host0#host{tcp_failed_time_ip = TCPFailedTimeIPNew}.

dns_failure(#host{dns_failed = true} = Host, _, _) ->
    Host;
dns_failure(#host{name = Hostname,
                  dns_failed = false,
                  dns_failure = DNSFailure} = Host,
            Reason, DebugLogLevel) ->
    % called at most once per interval
    TimeFailure = cloudi_timestamp:native_monotonic(),
    ?LOG(DebugLogLevel,
         "\"~s\" DNS failure: ~s",
         [Hostname, Reason]),
    if
        DNSFailure =:= undefined ->
            ok;
        is_function(DNSFailure) ->
            DNSFailure(Hostname, Reason)
    end,
    Host#host{dns_failed = true,
              dns_failed_time = TimeFailure}.

dns_restored(#host{dns_failed = false} = Host, _) ->
    Host;
dns_restored(#host{name = Hostname,
                   dns_failed = true,
                   dns_failed_time = TimeFailure,
                   dns_restored = DNSRestored} = Host,
             DebugLogLevel) ->
    % called at most once per interval
    TimeRestored = cloudi_timestamp:native_monotonic(),
    NanoSeconds = cloudi_timestamp:convert(TimeRestored - TimeFailure,
                                           native, nanosecond),
    ?LOG(DebugLogLevel,
         "\"~s\" DNS restored after ~s",
         [Hostname, cloudi_timestamp:nanoseconds_to_string(NanoSeconds)]),
    if
        DNSRestored =:= undefined ->
            ok;
        is_function(DNSRestored) ->
            DNSRestored(Hostname, TimeFailure, TimeRestored)
    end,
    Host#host{dns_failed = false,
              dns_failed_time = undefined}.

tcp_failure(#host{name = Hostname,
                  tcp_failed_count = TCPFailedCount,
                  tcp_failed_time = TCPFailedTime,
                  tcp_failed_time_ip = TCPFailedTimeIP,
                  tcp_failure = TCPFailure} = Host,
            IP, Port, Reason, DebugLogLevel) ->
    case maps:is_key(IP, TCPFailedTimeIP) of
        true ->
            Host;
        false ->
            % called for each ip address failure
            TimeFailure = cloudi_timestamp:native_monotonic(),
            ?LOG(DebugLogLevel,
                 "\"~s\" TCP failure: ~s port ~w failed: ~s",
                 [Hostname, inet:ntoa(IP), Port, Reason]),
            if
                TCPFailure =:= undefined ->
                    ok;
                is_function(TCPFailure) ->
                    TCPFailure(Hostname, IP, Port, Reason)
            end,
            {TCPFailedCountNew,
             TCPFailedTimeNew} = if
                TCPFailedTime =:= undefined ->
                    {TCPFailedCount + 1,
                     TimeFailure};
                is_integer(TCPFailedTime) ->
                    {TCPFailedCount,
                     TCPFailedTime}
            end,
            TCPFailedTimeIPNew = maps:put(IP, TimeFailure, TCPFailedTimeIP),
            Host#host{tcp_failed_count = TCPFailedCountNew,
                      tcp_failed_time = TCPFailedTimeNew,
                      tcp_failed_time_ip = TCPFailedTimeIPNew}
    end.

tcp_restored(#host{name = Hostname,
                   tcp_failed_time_ip = TCPFailedTimeIP,
                   tcp_restored = TCPRestored} = Host,
             IP, Port, DebugLogLevel) ->
    case maps:take(IP, TCPFailedTimeIP) of
        error ->
            Host;
        {TimeFailure, TCPFailedTimeIPNew} ->
            % called for each ip address restored
            TimeRestored = cloudi_timestamp:native_monotonic(),
            NanoSeconds = cloudi_timestamp:convert(TimeRestored - TimeFailure,
                                                   native, nanosecond),
            ?LOG(DebugLogLevel,
                 "\"~s\" TCP restored: ~s port ~w after ~s",
                 [Hostname, inet:ntoa(IP), Port,
                  cloudi_timestamp:nanoseconds_to_string(NanoSeconds)]),
            if
                TCPRestored =:= undefined ->
                    ok;
                is_function(TCPRestored) ->
                    TCPRestored(Hostname, IP, Port, TimeFailure, TimeRestored)
            end,
            Host#host{tcp_failed_time_ip = TCPFailedTimeIPNew}
    end.

hosts_list(Hosts, DurationsDown) ->
    TimeOffset = erlang:time_offset(),
    TimeNow = cloudi_timestamp:native_monotonic(),
    TimeDayStart = TimeNow - ?NATIVE_TIME_IN_DAY,
    TimeWeekStart = TimeNow - ?NATIVE_TIME_IN_WEEK,
    TimeMonthStart = TimeNow - ?NATIVE_TIME_IN_MONTH,
    TimeYearStart = TimeNow - ?NATIVE_TIME_IN_YEAR,
    cloudi_x_trie:foldr(fun(Hostname,
                            #host{ipv4 = IPv4,
                                  ipv4_allowed = IPv4Allowed,
                                  ipv6 = IPv6,
                                  ipv6_allowed = IPv6Allowed,
                                  port = Port,
                                  interval = Interval,
                                  start_time = TimeStart,
                                  dns_failed = DNSFailed,
                                  dns_failed_time = DNSFailedTime,
                                  tcp_failed = TCPFailed,
                                  tcp_failed_count = TCPFailedCount,
                                  tcp_failed_time = TimeFailure,
                                  tcp_failed_time_ip = TCPFailedTimeIP}, L) ->
        DurationsDownTmp = if
            TimeFailure =:= undefined ->
                DurationsDown;
            is_integer(TimeFailure) ->
                cloudi_availability:
                durations_store([Hostname],
                                {TimeFailure, TimeNow},
                                DurationsDown)
        end,
        HostInfo0 = [{tcp_failed, TCPFailed},
                     {tcp_failed_time,
                      hosts_list_tcp_failed_time_ips(TCPFailedTimeIP,
                                                     TimeOffset)} |
                     hosts_list_status(TimeNow,
                                       TimeDayStart, TimeWeekStart,
                                       TimeMonthStart, TimeYearStart,
                                       TimeStart, TCPFailedCount,
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
        HostInfo2 = [{port, Port},
                     {interval, Interval},
                     {dns_failed, DNSFailed} | HostInfo1],
        HostInfo3 = if
            IPv6Allowed =:= true ->
                [{ipv6, [inet:ntoa(IP) || IP <- IPv6]} | HostInfo2];
            IPv6Allowed =:= false ->
                HostInfo2
        end,
        HostInfoN = if
            IPv4Allowed =:= true ->
                [{ipv4, [inet:ntoa(IP) || IP <- IPv4]} | HostInfo3];
            IPv4Allowed =:= false ->
                HostInfo3
        end,
        [{Hostname, HostInfoN} | L]
    end, [], Hosts).

hosts_list_status(TimeNow,
                  TimeDayStart, TimeWeekStart,
                  TimeMonthStart, TimeYearStart,
                  TimeStart, TCPFailedCount,
                  DurationsStateDown) ->
    NanoSeconds = cloudi_timestamp:
                  convert(TimeNow - TimeStart, native, nanosecond),
    Tracked = cloudi_timestamp:
              nanoseconds_to_string(NanoSeconds),
    {ApproximateYearDown,
     NanoSecondsYearDown} = cloudi_availability:
                            durations_sum(DurationsStateDown,
                                          TimeYearStart),
    {ApproximateMonthDown,
     NanoSecondsMonthDown} = cloudi_availability:
                             durations_sum(DurationsStateDown,
                                           TimeMonthStart),
    {ApproximateWeekDown,
     NanoSecondsWeekDown} = cloudi_availability:
                            durations_sum(DurationsStateDown,
                                          TimeWeekStart),
    {ApproximateDayDown,
     NanoSecondsDayDown} = cloudi_availability:
                           durations_sum(DurationsStateDown,
                                         TimeDayStart),
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
    Status5 = if
        TimeStart =< TimeMonthStart,
        NanoSecondsYearDown > 0 ->
            [{downtime_year,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsYearDown,
                                       ApproximateYearDown)} |
             Status4];
        true ->
            Status4
    end,
    Status6 = if
        TimeStart =< TimeWeekStart,
        NanoSecondsMonthDown > 0 orelse
        NanoSecondsYearDown > 0 ->
            [{downtime_month,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsMonthDown,
                                       ApproximateMonthDown)} |
             Status5];
        true ->
            Status5
    end,
    Status7 = if
        TimeStart =< TimeDayStart,
        NanoSecondsWeekDown > 0 orelse
        NanoSecondsMonthDown > 0 orelse
        NanoSecondsYearDown > 0 ->
            [{downtime_week,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsWeekDown,
                                       ApproximateWeekDown)} |
             Status6];
        true ->
            Status6
    end,
    StatusN = if
        NanoSecondsDayDown > 0 orelse
        NanoSecondsWeekDown > 0 orelse
        NanoSecondsMonthDown > 0 orelse
        NanoSecondsYearDown > 0 ->
            [{downtime_day,
              cloudi_availability:
              nanoseconds_to_string_gt(NanoSecondsDayDown,
                                       ApproximateDayDown)} |
             Status7];
        true ->
            Status7
    end,
    [{tracked, Tracked},
     {tracked_failures, erlang:integer_to_list(TCPFailedCount)} | StatusN].

hosts_list_tcp_failed_time_ips(TCPFailureTimeIP, TimeOffset) ->
    maps:fold(fun(IP, TimeFailure, L) ->
        MicroSeconds = cloudi_timestamp:
                       convert(TimeFailure + TimeOffset,
                               native, microsecond),
        [[{ip, inet:ntoa(IP)},
          {time,
           cloudi_timestamp:
           microseconds_epoch_to_string(MicroSeconds)}] | L]
    end, [], TCPFailureTimeIP).

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
    erlang:list_to_binary(Value);
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
    [erlang:list_to_binary(S) |
     convert_term_to_json_strings(L)].

json_encode(Term) ->
    cloudi_x_jsx:encode(Term, [{indent, 1}]).

