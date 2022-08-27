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
        % TCP port used for the host's health check.
-define(DEFAULT_INTERVAL,                       5). % seconds
-define(DEFAULT_IPV4,                        true).
-define(DEFAULT_IPV6,                        true).
-define(DEFAULT_DNS_FAILURE,            undefined).
-define(DEFAULT_TCP_FAILURE,            undefined).

% maximum timeout value for erlang:send_after/3 and gen_server:call
-define(TIMEOUT_MAX_ERLANG, 4294967295).
% ensure timeouts do not delay the health check interval
-define(TIMEOUT_DELTA, 100). % milliseconds

-type hostname() ::
    string(). % dns name or ip address
-type tcp_port() ::
    pos_integer().
-type interval() ::
    1..(?TIMEOUT_MAX_ERLANG div 1000).
-type dns_failure() ::
    fun((Name :: hostname(),
         Reason :: atom()) ->
        ok).
-type tcp_failure() ::
    fun((Name :: hostname(),
         IP :: inet:ip4_address() | inet:ip6_address(),
         Port :: tcp_port(),
         Reason :: atom()) ->
        ok).
-export_type([hostname/0,
              tcp_port/0,
              interval/0,
              dns_failure/0,
              tcp_failure/0]).

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
        dns_failure
            :: undefined | dns_failure(),
        tcp_failed = false
            :: boolean(),
        tcp_failure
            :: undefined | tcp_failure()
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
        {tcp_failure,              ?DEFAULT_TCP_FAILURE}],
    Hosts = lists:foldl(fun({Hostname, L}, Lookup) ->
        false = cloudi_x_trie:is_key(Hostname, Lookup),
        [Port, Interval, IPv4Allowed, IPv6Allowed, DNSFailure0,
         TCPFailure0] = cloudi_proplists:take_values(HostDefaults, L),
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
        TCPFailureN = cloudi_args_type:
                      function_optional(TCPFailure0, 4),
        erlang:send_after(Interval * 1000, Service,
                          {host, Hostname}),
        cloudi_x_trie:store(Hostname,
                            #host{name = Hostname,
                                  ipv4_allowed = IPv4Allowed,
                                  ipv6_allowed = IPv6Allowed,
                                  port = Port,
                                  interval = Interval,
                                  dns_failure = DNSFailureN,
                                  tcp_failure = TCPFailureN}, Lookup)
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
    case health_check_dns_ipv4(Host0, DebugLogLevel) of
        {ok, Host1} ->
            case health_check_dns_ipv6(Host1, DebugLogLevel) of
                {ok, HostN} ->
                    HostN;
                {error, HostN} ->
                    HostN
            end;
        {error, HostN} ->
            HostN
    end.

health_check_dns_ipv4(#host{ipv4_allowed = false} = Host, _) ->
    {ok, Host};
health_check_dns_ipv4(#host{name = Hostname,
                            ipv6_allowed = IPv6Allowed,
                            dns_failure = DNSFailure} = Host, DebugLogLevel) ->
    case inet:getaddrs(Hostname, inet) of
        {ok, IPv4} ->
            {ok, Host#host{ipv4 = IPv4,
                           dns_failed = false}};
        {error, nxdomain = Reason} ->
            if
                IPv6Allowed =:= true ->
                    {ok, Host#host{ipv4 = [],
                                   dns_failed = false}};
                IPv6Allowed =:= false ->
                    ok = dns_failure(DNSFailure,
                                     Hostname, Reason, DebugLogLevel),
                    {error, Host#host{dns_failed = true}}
            end;
        {error, Reason} ->
            ok = dns_failure(DNSFailure, Hostname, Reason, DebugLogLevel),
            {error, Host#host{dns_failed = true}}
    end.

health_check_dns_ipv6(#host{ipv6_allowed = false} = Host, _) ->
    {ok, Host};
health_check_dns_ipv6(#host{name = Hostname,
                            ipv4_allowed = IPv4Allowed,
                            dns_failure = DNSFailure} = Host, DebugLogLevel) ->
    case inet:getaddrs(Hostname, inet6) of
        {ok, IPv6} ->
            {ok, Host#host{ipv6 = IPv6,
                           dns_failed = false}};
        {error, nxdomain = Reason} ->
            if
                IPv4Allowed =:= true ->
                    {ok, Host#host{ipv6 = [],
                                   dns_failed = false}};
                IPv4Allowed =:= false ->
                    ok = dns_failure(DNSFailure,
                                     Hostname, Reason, DebugLogLevel),
                    {error, Host#host{dns_failed = true}}
            end;
        {error, Reason} ->
            ok = dns_failure(DNSFailure, Hostname, Reason, DebugLogLevel),
            {error, Host#host{dns_failed = true}}
    end.

health_check_tcp(#host{ipv4 = IPv4,
                       ipv6 = IPv6,
                       interval = Interval} = Host, DebugLogLevel) ->
    Timeout = timeout_tcp(IPv4, IPv6, Interval),
    Result = health_check_tcp_ips(IPv4, ok, inet, Timeout,
                                  Host, DebugLogLevel),
    case health_check_tcp_ips(IPv6, Result, inet6, Timeout,
                              Host, DebugLogLevel) of
        ok ->
            Host#host{tcp_failed = false};
        error ->
            Host#host{tcp_failed = true}
    end.

health_check_tcp_ips([], Result, _, _, _, _) ->
    Result;
health_check_tcp_ips([IP | IPs], Result, Family, Timeout,
                     #host{name = Hostname,
                           port = Port,
                           tcp_failure = TCPFailure} = Host, DebugLogLevel) ->
    case gen_tcp:connect(IP, Port, [Family], Timeout) of
        {ok, Socket} ->
            ok = gen_tcp:close(Socket),
            health_check_tcp_ips(IPs, Result, Family, Timeout,
                                 Host, DebugLogLevel);
        {error, Reason} ->
            ok = tcp_failure(TCPFailure,
                             Hostname, IP, Port, Reason, DebugLogLevel),
            health_check_tcp_ips(IPs, error, Family, Timeout,
                                 Host, DebugLogLevel)
    end.

timeout_tcp([], [], Interval) ->
    Interval * 1000 - ?TIMEOUT_DELTA;
timeout_tcp([], IPv6, Interval) ->
    (Interval * 1000 - ?TIMEOUT_DELTA) div length(IPv6);
timeout_tcp(IPv4, [], Interval) ->
    (Interval * 1000 - ?TIMEOUT_DELTA) div length(IPv4);
timeout_tcp(IPv4, IPv6, Interval) ->
    (Interval * 1000 - ?TIMEOUT_DELTA) div (length(IPv4) + length(IPv6)).

dns_failure(DNSFailure, Hostname, Reason, DebugLogLevel) ->
    % called at most once per interval
    ?LOG(DebugLogLevel,
         "\"~s\" DNS failure: ~s",
         [Hostname, Reason]),
    if
        DNSFailure =:= undefined ->
            ok;
        is_function(DNSFailure) ->
            DNSFailure(Hostname, Reason)
    end.

tcp_failure(TCPFailure, Hostname, IP, Port, Reason, DebugLogLevel) ->
    % called for each ip address failure
    ?LOG(DebugLogLevel,
         "\"~s\" TCP failure: ~s port ~w failed: ~s",
         [Hostname, inet:ntoa(IP), Port, Reason]),
    if
        TCPFailure =:= undefined ->
            ok;
        is_function(TCPFailure) ->
            TCPFailure(Hostname, IP, Port, Reason)
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
                                  tcp_failed = TCPFailed}, L) ->
        HostInfo0 = [{port, Port},
                     {interval, Interval},
                     {dns_failed, DNSFailed},
                     {tcp_failed, TCPFailed}],
        HostInfo1 = if
            IPv6Allowed =:= true ->
                [{ipv6, [inet:ntoa(IP) || IP <- IPv6]} | HostInfo0];
            IPv6Allowed =:= false ->
                HostInfo0
        end,
        HostInfoN = if
            IPv4Allowed =:= true ->
                [{ipv4, [inet:ntoa(IP) || IP <- IPv4]} | HostInfo1];
            IPv4Allowed =:= false ->
                HostInfo1
        end,
        [{Hostname, HostInfoN} | L]
    end, [], Hosts).

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

