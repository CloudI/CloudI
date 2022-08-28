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

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_HOSTS,                         []).
-define(DEFAULT_DEBUG,                       true). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                error).

% hosts configuration arguments
-define(DEFAULT_PORT,                          80).
        % TCP port used for the host's TCP health check.
-define(DEFAULT_INTERVAL,                       5). % seconds
-define(DEFAULT_IPV4,                        true).
-define(DEFAULT_IPV6,                        true).
-define(DEFAULT_DNS_FAILURE,            undefined).
-define(DEFAULT_DNS_RESTORED,           undefined).
-define(DEFAULT_TCP_FAILURE,            undefined).
-define(DEFAULT_TCP_RESTORED,           undefined).

% maximum timeout value for erlang:send_after/3 and gen_server:call
-define(TIMEOUT_MAX_ERLANG, 4294967295).
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
-export_type([hostname/0,
              tcp_port/0,
              interval/0,
              dns_failure/0,
              dns_restored/0,
              tcp_failure/0,
              tcp_restored/0]).

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
        tcp_failed_time = #{}
            :: #{inet:ip_address() := cloudi_timestamp:native_monotonic()},
        tcp_failure
            :: undefined | tcp_failure(),
        tcp_restored
            :: undefined | tcp_restored()
    }).

-record(state,
    {
        service
            :: cloudi_service:source(),
        prefix
            :: cloudi:service_name_pattern(),
        hosts
            :: cloudi_x_trie:cloudi_x_trie(), % hostname() -> #host{}
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
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [HostsL,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
    true = is_list(HostsL) andalso (HostsL /= []),
    Service = cloudi_service:self(Dispatcher),
    HostDefaults = [
        {port,                     ?DEFAULT_PORT},
        {interval,                 ?DEFAULT_INTERVAL},
        {ipv4,                     ?DEFAULT_IPV4},
        {ipv6,                     ?DEFAULT_IPV6},
        {dns_failure,              ?DEFAULT_DNS_FAILURE},
        {dns_restored,             ?DEFAULT_DNS_RESTORED},
        {tcp_failure,              ?DEFAULT_TCP_FAILURE},
        {tcp_restored,             ?DEFAULT_TCP_RESTORED}],
    Hosts = lists:foldl(fun({Hostname, L}, Lookup) ->
        true = is_list(Hostname) andalso is_integer(hd(Hostname)),
        false = cloudi_x_trie:is_key(Hostname, Lookup),
        [Port, Interval, IPv4Allowed, IPv6Allowed,
         DNSFailure0, DNSRestored0, TCPFailure0,
         TCPRestored0] = cloudi_proplists:take_values(HostDefaults, L),
        true = is_integer(Port) andalso (Port > 0),
        true = is_integer(Interval) andalso
               (Interval > 0) andalso
               (Interval =< ?TIMEOUT_MAX_ERLANG div 1000),
        true = is_boolean(IPv4Allowed),
        true = is_boolean(IPv6Allowed),
        true = (IPv4Allowed /= false) orelse
               (IPv6Allowed /= false),
        DNSFailureN = cloudi_args_type:
                      function_optional(DNSFailure0, 2),
        DNSRestoredN = cloudi_args_type:
                       function_optional(DNSRestored0, 3),
        TCPFailureN = cloudi_args_type:
                      function_optional(TCPFailure0, 4),
        TCPRestoredN = cloudi_args_type:
                       function_optional(TCPRestored0, 5),
        erlang:send_after(Interval * 1000, Service,
                          {host, Hostname}),
        cloudi_x_trie:store(Hostname,
                            #host{name = Hostname,
                                  ipv4_allowed = IPv4Allowed,
                                  ipv6_allowed = IPv6Allowed,
                                  port = Port,
                                  interval = Interval,
                                  dns_failure = DNSFailureN,
                                  dns_restored = DNSRestoredN,
                                  tcp_failure = TCPFailureN,
                                  tcp_restored = TCPRestoredN}, Lookup)
    end, cloudi_x_trie:new(), HostsL),
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
    {ok, #state{service = Service,
                prefix = Prefix,
                hosts = Hosts,
                debug_level = DebugLogLevel}}.

cloudi_service_handle_request(_RequestType, Name, _Pattern,
                              _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Source,
                              #state{prefix = Prefix,
                                     hosts = Hosts} = State,
                              _Dispatcher) ->
    Response = case cloudi_service_name:suffix(Prefix, Name) of
        "hosts.erl/get" ->
            convert_term_to_erlang(hosts_list(Hosts));
        "hosts.json/get" ->
            convert_term_to_json(hosts_list(Hosts), hosts)
    end,
    {reply, Response, State}.

cloudi_service_handle_info({host, Hostname}, State, _Dispatcher) ->
    {noreply, health_check(Hostname, State)}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

health_check(Hostname,
             #state{service = Service,
                    hosts = Hosts,
                    debug_level = DebugLogLevel} = State) ->
    Host0 = cloudi_x_trie:fetch(Hostname, Hosts),
    #host{interval = Interval} = Host0,
    Host1 = health_check_dns(Host0, DebugLogLevel),
    HostN = health_check_tcp(Host1, DebugLogLevel),
    erlang:send_after(Interval * 1000, Service,
                      {host, Hostname}),
    State#state{hosts = cloudi_x_trie:store(Hostname, HostN, Hosts)}.

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

health_check_tcp(#host{ipv4 = IPv4,
                       ipv6 = IPv6,
                       interval = Interval} = Host0,
                 DebugLogLevel) ->
    Timeout = timeout_tcp(IPv4, IPv6, Interval),
    Host1 = health_check_tcp_ips(IPv4, Host0, inet, Timeout, DebugLogLevel),
    HostN = health_check_tcp_ips(IPv6, Host1, inet6, Timeout, DebugLogLevel),
    #host{tcp_failed_time = TCPFailedTime} = HostN,
    if
        TCPFailedTime == #{} ->
            HostN#host{tcp_failed = false};
        true ->
            HostN#host{tcp_failed = true}
    end.

health_check_tcp_ips([], HostN, _, _, _) ->
    HostN;
health_check_tcp_ips([IP | IPs],
                     #host{port = Port} = Host0,
                     Family, Timeout, DebugLogLevel) ->
    case gen_tcp:connect(IP, Port, [Family], Timeout) of
        {ok, Socket} ->
            ok = gen_tcp:close(Socket),
            HostN = tcp_restored(Host0, IP, Port, DebugLogLevel),
            health_check_tcp_ips(IPs, HostN, Family, Timeout, DebugLogLevel);
        {error, Reason} ->
            HostN = tcp_failure(Host0, IP, Port, Reason, DebugLogLevel),
            health_check_tcp_ips(IPs, HostN, Family, Timeout, DebugLogLevel)
    end.

timeout_tcp([], [], Interval) ->
    Interval * 1000 - ?TIMEOUT_DELTA;
timeout_tcp([], IPv6, Interval) ->
    (Interval * 1000 - ?TIMEOUT_DELTA) div length(IPv6);
timeout_tcp(IPv4, [], Interval) ->
    (Interval * 1000 - ?TIMEOUT_DELTA) div length(IPv4);
timeout_tcp(IPv4, IPv6, Interval) ->
    (Interval * 1000 - ?TIMEOUT_DELTA) div (length(IPv4) + length(IPv6)).

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
                  tcp_failed_time = TCPFailedTime,
                  tcp_failure = TCPFailure} = Host,
            IP, Port, Reason, DebugLogLevel) ->
    case maps:is_key(IP, TCPFailedTime) of
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
            TCPFailedTimeNew = maps:put(IP, TimeFailure, TCPFailedTime),
            Host#host{tcp_failed_time = TCPFailedTimeNew}
    end.

tcp_restored(#host{name = Hostname,
                   tcp_failed_time = TCPFailedTime,
                   tcp_restored = TCPRestored} = Host,
             IP, Port, DebugLogLevel) ->
    case maps:take(IP, TCPFailedTime) of
        error ->
            Host;
        {TimeFailure, TCPFailedTimeNew} ->
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
            Host#host{tcp_failed_time = TCPFailedTimeNew}
    end.

hosts_list(Hosts) ->
    cloudi_x_trie:foldr(fun(Hostname,
                            #host{ipv4 = IPv4,
                                  ipv4_allowed = IPv4Allowed,
                                  ipv6 = IPv6,
                                  ipv6_allowed = IPv6Allowed,
                                  port = Port,
                                  interval = Interval,
                                  dns_failed = DNSFailed,
                                  dns_failed_time = DNSFailedTime,
                                  tcp_failed = TCPFailed,
                                  tcp_failed_time = TCPFailedTime}, L) ->
        TimeOffset = erlang:time_offset(),
        HostInfo0 = [{tcp_failed, TCPFailed},
                     {tcp_failed_time,
                      hosts_list_tcp_failed_time(TCPFailedTime, TimeOffset)}],
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

hosts_list_tcp_failed_time(TCPFailureTime, TimeOffset) ->
    maps:fold(fun(IP, TimeFailure, L) ->
        MicroSeconds = cloudi_timestamp:
                       convert(TimeFailure + TimeOffset,
                               native, microsecond),
        [[{ip, inet:ntoa(IP)},
          {time,
           cloudi_timestamp:
           microseconds_epoch_to_string(MicroSeconds)}] | L]
    end, [], TCPFailureTime).

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

