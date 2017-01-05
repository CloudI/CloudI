%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang syslog Client Interface==
%%% A minimal syslog client interface that supports both RFC3164 and RFC5424.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2016 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------
-module(syslog_socket).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         stop_link/1,
         stop_link/2,
         send/3,
         send/4,
         send/5,
         facility/1,
         facility_valid/1,
         severity/1,
         severity_valid/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(TRANSPORT_DEFAULT, local).
-define(PROTOCOL_DEFAULT, rfc3164).
-define(UTF8_DEFAULT, true).
-define(FACILITY_DEFAULT, local0).
-define(TIMEOUT_DEFAULT, 5000). % milliseconds
-define(TIMEOUT_MAX_ERLANG, 4294967295). % milliseconds
-define(SP, $\s).

-type transport() :: local | udp | tcp | tls.
-type protocol() :: rfc3164 | rfc5424.
-type timeout_milliseconds() :: 1..?TIMEOUT_MAX_ERLANG.
-type app_name() :: nonempty_string().
-type facility() :: kernel | user | mail | daemon | auth0 | syslog |
                    print | news | uucp | clock0 | auth1 | ftp | ntp |
                    auth2 | auth3 | clock1 | local0 | local1 | local2 |
                    local3 | local4 | local5 | local6 | local7 |
                    non_neg_integer() |
                    % common aliases
                    auth | authpriv | cron | kern | lpr | security.
-type severity() :: emergency | alert | critical | error | warning |
                    notice | informational | debug |
                    0..7 |
                    % common aliases
                    emerg | panic | crit | err | warn | info.
-type message_id() :: string() | binary().
-type options() :: list({transport, transport()} |
                        {transport_options, list()} |
                        {protocol, protocol()} |
                        {utf8, boolean()} |
                        {facility, facility()} |
                        {app_name, app_name()} |
                        {path, nonempty_string()} |
                        {host, inet:ip_address() | inet:hostname()} |
                        {port, undefined | inet:port_number()} |
                        {timeout, timeout_milliseconds()}).
-export_type([transport/0,
              protocol/0,
              timeout_milliseconds/0,
              app_name/0,
              facility/0,
              severity/0,
              message_id/0,
              options/0]).

-record(state,
        {
            transport :: transport(),
            transport_options :: list(),
            protocol :: protocol(),
            utf8 :: boolean(),
            utf8_bom :: binary(),
            facility :: non_neg_integer(),
            app_name :: app_name(),
            path :: nonempty_string(),
            host :: inet:ip_address() | inet:hostname(),
            port :: inet:port_number(),
            timeout :: pos_integer(),
            socket = undefined :: undefined | inet:socket() | ssl:sslsocket(),
            hostname = undefined :: undefined | string(),
            os_pid = undefined :: undefined | string()
        }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start a syslog socket.===
%% Connection is immediate within the new Erlang process.
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Options :: options()) ->
    {ok, pid()} | {error, any()}.

start_link(Options) when is_list(Options) ->
    Defaults = [{transport, ?TRANSPORT_DEFAULT},
                {transport_options, undefined},
                {protocol, ?PROTOCOL_DEFAULT},
                {utf8, ?UTF8_DEFAULT},
                {facility, ?FACILITY_DEFAULT},
                {app_name, undefined},
                {path, "/dev/log"},
                {host, {127,0,0,1}},
                {port, undefined},
                {timeout, ?TIMEOUT_DEFAULT}],
    [Transport, TransportOptionsValue, Protocol,
     UTF8, FacilityValue, AppName,
     Path, Host, PortValue, Timeout] = take_values(Defaults, Options),
    true = is_list(AppName) andalso is_integer(hd(AppName)), % required
    if
        Protocol =:= rfc3164 ->
            ok;
        Protocol =:= rfc5424 ->
            true = (length(AppName) =< 48)
    end,
    true = is_boolean(UTF8),
    Facility = facility(FacilityValue),
    true = is_integer(Timeout) andalso
           (Timeout > 0) andalso (Timeout =< ?TIMEOUT_MAX_ERLANG),
    TransportOptions = if
        Transport =:= local ->
            if
                TransportOptionsValue =:= undefined ->
                    [local];
                is_list(TransportOptionsValue) ->
                    [local | TransportOptionsValue]
            end;
        Transport =:= udp ->
            if
                TransportOptionsValue =:= undefined ->
                    [];
                is_list(TransportOptionsValue) ->
                    TransportOptionsValue
            end;
        Transport =:= tcp ->
            if
                TransportOptionsValue =:= undefined ->
                    [{send_timeout, Timeout},
                     {send_timeout_close, true},
                     {keepalive, true},
                     {reuseaddr, true}];
                is_list(TransportOptionsValue) ->
                    TransportOptionsValue
            end;
        Transport =:= tls ->
            true = is_list(TransportOptionsValue) andalso
                   (length(TransportOptionsValue) > 0),
            TransportOptionsValue
    end,
    true = is_list(Path) andalso is_integer(hd(Path)),
    Port = if
        PortValue =:= undefined ->
            if
                Transport =:= local ->
                    0;
                Transport =:= udp ->
                    514;
                Transport =:= tcp ->
                    601;
                Transport =:= tls ->
                    6514
            end;
        is_integer(PortValue), PortValue > 0 ->
            PortValue
    end,
    gen_server:start_link(?MODULE,
                          [Transport, TransportOptions, Protocol,
                           UTF8, Facility, AppName,
                           Path, Host, Port, Timeout], []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop an existing syslog socket asynchronously.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_link(Pid :: pid()) ->
    ok.

stop_link(Pid) ->
    gen_server:cast(Pid, stop).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop an existing syslog socket synchronously.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_link(Pid :: pid(),
                Timeout :: timeout_milliseconds()) ->
    ok.

stop_link(Pid, Timeout) ->
    gen_server:call(Pid, stop, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send data to syslog without a timestamp or message_id.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Pid :: pid(),
           Severity :: severity(),
           Data :: iodata()) ->
    ok.

send(Pid, Severity, Data) ->
    send(Pid, Severity, undefined, "", Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send data to syslog without a message_id.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Pid :: pid(),
           Severity :: severity(),
           Timestamp :: undefined | erlang:timestamp(),
           Data :: iodata()) ->
    ok.

send(Pid, Severity, Timestamp, Data) ->
    send(Pid, Severity, Timestamp, "", Data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send data to syslog.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Pid :: pid(),
           Severity :: severity(),
           Timestamp :: undefined | erlang:timestamp(),
           MessageId :: message_id(),
           Data :: iodata()) ->
    ok.

send(Pid, Severity, Timestamp, MessageId, Data) ->
    gen_server:cast(Pid, {send, Severity, Timestamp, MessageId, Data}).

%%%------------------------------------------------------------------------
%%% facility and severity values are based on
%%% https://tools.ietf.org/html/rfc3164#section-4.1.1
%%% https://tools.ietf.org/html/rfc5424#section-6.2.1
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the syslog facility numerical value.===
%% @end
%%-------------------------------------------------------------------------

-spec facility(Facility :: facility()) ->
    non_neg_integer().

facility(kernel) ->         0; % kernel messages
facility(user) ->           1; % user-level messages
facility(mail) ->           2; % mail system
facility(daemon) ->         3; % system daemons
facility(auth0) ->          4; % security/authorization messages 0
facility(syslog) ->         5; % messages generated internally by syslogd
facility(print) ->          6; % line printer subsystem
facility(news) ->           7; % network news subsystem
facility(uucp) ->           8; % UUCP subsystem
facility(clock0) ->         9; % clock daemon 0
facility(auth1) ->         10; % security/authorization messages 1
facility(ftp) ->           11; % FTP daemon
facility(ntp) ->           12; % NTP subsystem
facility(auth2) ->         13; % log audit (security/authorization 2)
facility(auth3) ->         14; % log alert (security/authorization 3)
facility(clock1) ->        15; % clock daemon 1
facility(local0) ->        16; % local use 0
facility(local1) ->        17; % local use 1
facility(local2) ->        18; % local use 2
facility(local3) ->        19; % local use 3
facility(local4) ->        20; % local use 4
facility(local5) ->        21; % local use 5
facility(local6) ->        22; % local use 6
facility(local7) ->        23; % local use 7
facility(auth) ->          facility(auth0);
facility(authpriv) ->      facility(auth1);
facility(cron) ->          facility(clock0);
facility(kern) ->          facility(kernel);
facility(lpr) ->           facility(print);
facility(security) ->      facility(auth0);
facility(Facility) when is_integer(Facility), Facility >= 0 ->
    Facility.

%%-------------------------------------------------------------------------
%% @doc
%% ===Test the validity of a syslog facility value.===
%% @end
%%-------------------------------------------------------------------------

-spec facility_valid(Facility :: facility()) ->
    boolean().

facility_valid(Facility) ->
    try facility(Facility) of
        _ -> true
    catch
        _:_ -> false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the syslog severity numerical value.===
%% @end
%%-------------------------------------------------------------------------

-spec severity(Severity :: severity()) ->
    non_neg_integer().

severity(emergency) ->      0; % system is unusable
severity(alert) ->          1; % action must be taken immediately
severity(critical) ->       2; % critical conditions
severity(error) ->          3; % error conditions
severity(warning) ->        4; % warning conditions
severity(notice) ->         5; % normal but significant condition
severity(informational) ->  6; % informational messages
severity(debug) ->          7; % debug-level messages
severity(emerg) ->          severity(emergency);
severity(panic) ->          severity(emergency);
severity(crit) ->           severity(critical);
severity(err) ->            severity(error);
severity(warn) ->           severity(warning);
severity(info) ->           severity(informational);
severity(Severity) when is_integer(Severity), Severity >= 0, Severity =< 7 ->
    Severity.

%%-------------------------------------------------------------------------
%% @doc
%% ===Test the validity of a syslog severity value.===
%% @end
%%-------------------------------------------------------------------------

-spec severity_valid(Severity :: severity()) ->
    boolean().

severity_valid(Severity) ->
    try severity(Severity) of
        _ -> true
    catch
        _:_ -> false
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Transport, TransportOptions, Protocol,
      UTF8, Facility, AppName,
      Path, Host, Port, Timeout]) ->
    State0 = #state{transport = Transport,
                    transport_options = TransportOptions,
                    protocol = Protocol,
                    utf8 = UTF8,
                    utf8_bom = unicode:encoding_to_bom(utf8),
                    facility = Facility,
                    app_name = AppName,
                    path = Path,
                    host = Host,
                    port = Port,
                    timeout = Timeout},
    case transport_open(State0) of
        {ok, StateN} ->
            {ok, protocol_init(StateN)};
        {error, _} = Error ->
            {stop, Error, State0}
    end.

handle_call(stop, _, State) ->
    {stop, normal, ok, State};
handle_call(Request, _, State) ->
    {stop, {error, {call, Request}}, State}.

handle_cast({send, Severity, Timestamp0, MessageId, Data}, State) ->
    TimestampN = case Timestamp0 of
        undefined ->
            os:timestamp();
        {_, _, _} ->
            Timestamp0
    end,
    HEADER = protocol_header(Severity, TimestampN, MessageId, State),
    DATA = protocol_data(State),
    MSG = protocol_msg(Data, State),
    ok = transport_send([HEADER, DATA, MSG], State),
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    {stop, {error, {cast, Request}}, State}.

handle_info({Error, Socket}, #state{socket = Socket} = State)
    when Error =:= udp_closed;
         Error =:= tcp_closed;
         Error =:= ssl_closed ->
    {stop, {error, Error}, State#state{socket = undefined}};
handle_info({Error, Socket}, #state{socket = Socket} = State)
    when Error =:= udp_error;
         Error =:= tcp_error;
         Error =:= ssl_error ->
    {stop, {error, Error}, State};
handle_info(Request, State) ->
    {stop, {error, {info, Request}}, State}.

terminate(_, State) ->
    ok = transport_close(State),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

protocol_init(#state{protocol = rfc3164} = State) ->
    HostnameOnly = lists:takewhile(fun(C) -> C /= $. end, hostname()),
    State#state{hostname = HostnameOnly,
                os_pid = os:getpid()};
protocol_init(#state{protocol = rfc5424} = State) ->
    State#state{hostname = hostname(),
                os_pid = os:getpid()}.

protocol_header(Severity, Timestamp, MessageId,
                #state{transport = Transport,
                       protocol = rfc3164,
                       facility = Facility,
                       app_name = APP_NAME,
                       hostname = Hostname,
                       os_pid = PROCID}) ->
    PRIVAL = (Facility bsl 3) + severity(Severity),
    TIMESTAMP = timestamp_rfc3164(Timestamp),
    HOSTNAME_SP = if
        Transport =:= local ->
            [];
        true ->
            [Hostname, ?SP]
    end,
    SP_MSGID = case MessageId of
        [] ->
            [];
        <<>> ->
            [];
        _ ->
            [?SP, MessageId]
    end,
    [$<, int_to_dec_list(PRIVAL), $>,
     TIMESTAMP, ?SP,
     HOSTNAME_SP,
     % extra information as MSG prefix
     APP_NAME, $[, PROCID, $], $:, SP_MSGID];
protocol_header(Severity, Timestamp, MessageId,
                #state{protocol = rfc5424,
                       facility = Facility,
                       app_name = APP_NAME,
                       hostname = HOSTNAME,
                       os_pid = PROCID}) ->
    PRIVAL = (Facility bsl 3) + severity(Severity),
    VERSION = $1,
    TIMESTAMP = timestamp_rfc5424(Timestamp),
    MSGID = case MessageId of
        [] ->
            $-;
        <<>> ->
            $-;
        _ ->
            truncate(MessageId, 32)
    end,
    [$<, int_to_dec_list(PRIVAL), $>,
     VERSION, ?SP,
     TIMESTAMP, ?SP,
     truncate(HOSTNAME, 255), ?SP,
     truncate(APP_NAME, 48), ?SP,
     PROCID, ?SP,
     MSGID].

protocol_data(#state{protocol = rfc3164}) ->
    [];
protocol_data(#state{protocol = rfc5424}) ->
    STRUCTURED_DATA = $-,
    [?SP, STRUCTURED_DATA].

protocol_msg(Data,
             #state{utf8 = UTF8,
                    utf8_bom = UTF8BOM}) ->
    if
        Data == <<>>; Data == [] ->
            [];
        UTF8 =:= true ->
            [?SP, UTF8BOM, Data];
        UTF8 =:= false ->
            [?SP, Data] % ASCII
    end.

timestamp_rfc3164({_, _, _} = Timestamp) ->
    {{_, DateMM, DateDD},
     {TimeHH, TimeMM, TimeSS}} = calendar:now_to_local_time(Timestamp),
    [DateMM0, DateMM1, DateMM2] = if
        DateMM ==  1 -> "Jan";
        DateMM ==  2 -> "Feb";
        DateMM ==  3 -> "Mar";
        DateMM ==  4 -> "Apr";
        DateMM ==  5 -> "May";
        DateMM ==  6 -> "Jun";
        DateMM ==  7 -> "Jul";
        DateMM ==  8 -> "Aug";
        DateMM ==  9 -> "Sep";
        DateMM == 10 -> "Oct";
        DateMM == 11 -> "Nov";
        DateMM == 12 -> "Dec"
    end,
    [DateDD0, DateDD1] = int_to_dec_list(DateDD, 2, $\s),
    [TimeHH0, TimeHH1] = int_to_dec_list(TimeHH, 2, $0),
    [TimeMM0, TimeMM1] = int_to_dec_list(TimeMM, 2, $0),
    [TimeSS0, TimeSS1] = int_to_dec_list(TimeSS, 2, $0),
    [DateMM0, DateMM1, DateMM2, $\s,
     DateDD0, DateDD1, $\s,
     TimeHH0, TimeHH1, $:, TimeMM0, TimeMM1, $:, TimeSS0, TimeSS1].

timestamp_rfc5424({_, _, MicroSeconds} = Timestamp) ->
    {{DateYYYY, DateMM, DateDD},
     {TimeHH, TimeMM, TimeSS}} = calendar:now_to_universal_time(Timestamp),
    [DateYYYY0, DateYYYY1,
     DateYYYY2, DateYYYY3] = int_to_dec_list(DateYYYY, 4, $0),
    [DateMM0, DateMM1] = int_to_dec_list(DateMM, 2, $0),
    [DateDD0, DateDD1] = int_to_dec_list(DateDD, 2, $0),
    [TimeHH0, TimeHH1] = int_to_dec_list(TimeHH, 2, $0),
    [TimeMM0, TimeMM1] = int_to_dec_list(TimeMM, 2, $0),
    [TimeSS0, TimeSS1] = int_to_dec_list(TimeSS, 2, $0),
    [MicroSeconds0, MicroSeconds1,
     MicroSeconds2, MicroSeconds3,
     MicroSeconds4, MicroSeconds5] = int_to_dec_list(MicroSeconds, 6, $0),
    [DateYYYY0, DateYYYY1, DateYYYY2, DateYYYY3, $-,
     DateMM0, DateMM1, $-, DateDD0, DateDD1, $T,
     TimeHH0, TimeHH1, $:, TimeMM0, TimeMM1, $:, TimeSS0, TimeSS1, $.,
     MicroSeconds0, MicroSeconds1,
     MicroSeconds2, MicroSeconds3,
     MicroSeconds4, MicroSeconds5, $Z].

transport_open(#state{transport = Transport,
                      transport_options = TransportOptions} = State)
    when Transport =:= local; Transport =:= udp ->
    try gen_udp:open(0, TransportOptions) of
        {ok, Socket} ->
            {ok, State#state{socket = Socket}};
        {error, _} = Error ->
            Error
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end;
transport_open(#state{transport = tcp,
                      transport_options = TransportOptions,
                      host = Host,
                      port = Port,
                      timeout = Timeout} = State) ->
    try gen_tcp:connect(Host, Port, TransportOptions, Timeout) of
        {ok, Socket} ->
            {ok, State#state{socket = Socket}};
        {error, _} = Error ->
            Error
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end;
transport_open(#state{transport = tls,
                      transport_options = TransportOptions,
                      host = Host,
                      port = Port,
                      timeout = Timeout} = State) ->
    try ssl:connect(Host, Port, TransportOptions, Timeout) of
        {ok, Socket} ->
            {ok, State#state{socket = Socket}};
        {error, _} = Error ->
            Error
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end.

transport_send(Data, #state{transport = local,
                            path = Path,
                            port = Port,
                            socket = Socket}) ->
    ok = gen_udp:send(Socket, {local, Path}, Port, Data);
transport_send(Data, #state{transport = udp,
                            host = Host,
                            port = Port,
                            socket = Socket}) ->
    ok = gen_udp:send(Socket, Host, Port, Data);
transport_send(Data, #state{transport = tcp,
                            socket = Socket}) ->
    ok = gen_tcp:send(Socket, Data);
transport_send(Data, #state{transport = tls,
                            socket = Socket}) ->
    ok = ssl:send(Socket, Data).

transport_close(#state{socket = undefined}) ->
    ok;
transport_close(#state{transport = Transport,
                       socket = Socket})
    when Transport =:= local; Transport =:= udp ->
    _ = (catch gen_udp:close(Socket)),
    ok;
transport_close(#state{transport = tcp,
                       socket = Socket}) ->
    _ = (catch gen_tcp:close(Socket)),
    ok;
transport_close(#state{transport = tls,
                       socket = Socket}) ->
    _ = (catch ssl:close(Socket)),
    ok.

hostname() ->
    case lists:dropwhile(fun(C) -> C /= $@ end,
                         erlang:atom_to_list(node())) of
        [$@ | "nohost"] ->
            {ok, Hostname} = inet:gethostname(),
            Hostname;
        [$@ | FQDN] ->
            FQDN
    end.

truncate(Value, N) when is_list(Value) ->
    lists:sublist(Value, N);
truncate(Value, N) when is_binary(Value) ->
    case Value of
        <<ValuePrefix:N/binary-unit:8, _/binary>> ->
            ValuePrefix;
        _ ->
            Value
    end.

take_values(DefaultList, List)
    when is_list(DefaultList), is_list(List) ->
    take_values([], DefaultList, List).
take_values(Result, [], List) ->
    lists:reverse(Result) ++ List;
take_values(Result, [{Key, Default} | DefaultList], List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            take_values([Default | Result], DefaultList, List);
        {value, {Key, Value}, RemainingList} ->
            take_values([Value | Result], DefaultList, RemainingList)
    end.

int_to_dec_list(I) when is_integer(I), I >= 0 ->
    int_to_dec_list([], I).

int_to_dec_list(L, I)
    when I < 10 ->
    [int_to_dec(I) | L];
int_to_dec_list(L, I) ->
    int_to_dec_list([int_to_dec(I rem 10) | L], I div 10).

int_to_dec_list(I, N, Char) when is_integer(I), I >= 0 ->
    int_to_dec_list([], I, 1, N, Char).

int_to_dec_list(L, I, Count, N, Char)
    when I < 10 ->
    int_to_list_pad([int_to_dec(I) | L], N - Count, Char);
int_to_dec_list(L, I, Count, N, Char) ->
    int_to_dec_list([int_to_dec(I rem 10) | L], I div 10, Count + 1, N, Char).

int_to_list_pad(L, 0, _) ->
    L;
int_to_list_pad(L, Count, Char) ->
    int_to_list_pad([Char | L], Count - 1, Char).

int_to_dec(I) when 0 =< I, I =< 9 ->
    I + $0.

