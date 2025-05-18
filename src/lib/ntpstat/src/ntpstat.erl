%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==NTP client ntpstat functionality==
%%% Functionality similar to executing the following shell commands:
%%% $ ntpstat
%%% $ chronyc -n tracking
%%% $ ntpq -c 'timeout 5' -c raw -c 'rv 0' 127.0.0.1
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2025 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2025 Michael Truog
%%% @version 2.0.8 {@date} {@time}
%%%------------------------------------------------------------------------

-ifndef(ESCRIPT).
-module(ntpstat).
-endif.
-author('mjtruog at protonmail dot com').

%% external interface
-export([destroy/1,
         main/1,
         message/1,
         new/0,
         new/1,
         update/2]).

-define(PORT_DEFAULT, 123).

-type status_error() :: clock_unsynchronized | % ntpstatus exit status 1
                        clock_unknown.         % ntpstatus exit status 2
-type options() :: list({host, inet:ip_address() | inet:hostname()} |
                        {port, inet:port_number()}).
-type bytestring() :: list(byte()).

-record(status,
        {
            error = undefined :: status_error() | undefined,
            restart = false :: boolean(),
            message = "" :: bytestring(),
            host :: inet:ip_address() | inet:hostname(),
            port :: inet:port_number(),
            socket = undefined :: gen_udp:socket() | undefined,
            socket_error = undefined :: any(),
            sequence = 1 :: pos_integer(),
            leap_indicator = undefined :: non_neg_integer() | undefined,
            clock_source = undefined :: non_neg_integer() | undefined,
            count = undefined :: non_neg_integer() | undefined,
            code = undefined :: non_neg_integer() | undefined,
            payload = undefined :: #{bytestring() := bytestring()} | undefined
        }).

-type status() :: #status{}.

-export_type([status_error/0,
              options/0,
              bytestring/0,
              status/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Destroy ntpstat status.===
%% @end
%%-------------------------------------------------------------------------

-spec destroy(#status{}) ->
    ok.

destroy(#status{socket = undefined}) ->
    ok;
destroy(#status{socket = Socket}) ->
    gen_udp:close(Socket),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Escript Main Function.===
%% @end
%%-------------------------------------------------------------------------

-spec main(list(string())) ->
    no_return().

main(_) ->
    Timeout = 5000,
    Status = status_request(Timeout, new()),
    #status{error = Error,
            restart = Restart,
            message = Message,
            socket_error = SocketError} = Status,
    ok = destroy(Status),
    if
        Error =:= undefined ->
            io:format(Message, []),
            exit_code(0);
        Error =:= clock_unsynchronized ->
            io:format(standard_error,
                      "clock_unsynchronized (restart = ~p)~n",
                      [Restart]),
            exit_code(1);
        Error =:= clock_unknown ->
            io:format(standard_error,
                      "clock_unknown (reason = ~p)~n",
                      [SocketError]),
            exit_code(2)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current ntpstat output message if it exists.===
%% @end
%%-------------------------------------------------------------------------

-spec message(#status{}) ->
    bytestring() | undefined.

message(#status{error = undefined,
                message = Message}) ->
    Message;
message(#status{}) ->
    undefined.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create ntpstat status.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    #status{}.

new() ->
    new([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create ntpstat status with options.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Options :: options()) ->
    #status{}.

new(Options) ->
    Defaults = [{host, {127,0,0,1}},
                {port, ?PORT_DEFAULT}],
    [Host, Port] = take_values(Defaults, Options),
    true = is_integer(Port) andalso Port > 0,
    #status{host = Host,
            port = Port}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update ntpstat status.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Timeout :: timeout(),
             Status :: #status{}) ->
    {ok, #status{}} | {error, status_error(), #status{}}.

update(Timeout, Status) ->
    case status_request(Timeout, Status) of
        #status{error = undefined} = StatusNew ->
            {ok, StatusNew};
        #status{error = Error} = StatusNew ->
            {error, Error, StatusNew}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

status_request(Timeout,
               #status{socket = undefined} = Status) ->
    case gen_udp:open(0, [binary, {active, false}]) of
        {ok, Socket} ->
            status_request(Timeout, Status#status{socket = Socket});
        {error, Reason} ->
            status_result_error_socket(Reason, Status)
    end;
status_request(Timeout,
               #status{host = Host,
                       port = Port,
                       socket = Socket,
                       sequence = Sequence} = Status) ->
    % RFC 1305 NTP control message
    ProtocolVersion = 2,
    {Mode, OpCode} = {6, 2}, % control message
    Request = <<0:2,
                ProtocolVersion:3,
                Mode:3,
                0:3,
                OpCode:5,
                Sequence:16,
                0:(572 * 8)>>,
    true = 576 == byte_size(Request),
    SequenceNew = if
        Sequence < 65535 ->
            Sequence + 1;
        Sequence == 65535 ->
            1
    end,
    case gen_udp:send(Socket, Host, Port, Request) of
        ok ->
            case gen_udp:recv(Socket, 576, Timeout) of
                {ok,
                 {_Address, Port,
                  <<_:2,
                    ProtocolVersion:3,
                    Mode:3,
                    1:1,
                    0:1,
                    _More:1,
                    OpCode:5,
                    Sequence:16,
                    LeapIndicator:2,
                    ClockSource:6,
                    Count:4,
                    Code:4,
                    _AssociationID:16,
                    _Offset:16,
                    _Count:16,
                    PayloadBinary/binary>>}} ->
                    Payload = payload_lookup(PayloadBinary),
                    StatusNew = Status#status{sequence = SequenceNew,
                                              leap_indicator = LeapIndicator,
                                              clock_source = ClockSource,
                                              count = Count,
                                              code = Code,
                                              payload = Payload},
                    status_validate(StatusNew);
                {ok, _} ->
                    status_result_error_socket_close(malformed_reply,
                        Status#status{sequence = SequenceNew});
                {error, Reason} ->
                    status_result_error_socket_close(Reason,
                        Status#status{sequence = SequenceNew})
            end;
        {error, Reason} ->
            status_result_error_socket_close(Reason,
                Status#status{sequence = SequenceNew})
    end.

status_validate(#status{leap_indicator = 3,
                        code = Code} = Status) ->
    Restart = Code == 1,
    status_result_error(clock_unsynchronized, Restart, Status);
status_validate(#status{clock_source = ClockSource,
                        payload = PayloadLookup} = Status) ->
    ClockSourceStr = clock_source(ClockSource),
    RefIdStr = if
        ClockSource == 6 ->
            status_validate_value("refid", 1, 15, PayloadLookup);
        true ->
            ""
    end,
    StratumStr = status_validate_value("stratum", 1, 2, PayloadLookup),
    RootDispStr = status_validate_value("rootdisp", 1, 9, PayloadLookup),
    RootDelayStr = status_validate_value("rootdelay", 1, 9, PayloadLookup),
    PollStr = status_validate_value("tc", 1, 2, PayloadLookup),
    if
        RefIdStr =:= undefined;
        StratumStr =:= undefined;
        RootDispStr =:= undefined;
        RootDelayStr =:= undefined;
        PollStr =:= undefined ->
            status_result_error(clock_unsynchronized, Status);
        true ->
            RefIdStrFormat = if
                RefIdStr == "" ->
                    "";
                true ->
                    [$ , $(, RefIdStr, $)]
            end,
            RootMilliSeconds = round(erlang:list_to_float(RootDispStr) +
                                     erlang:list_to_float(RootDelayStr) / 2.0),
            PollFrequencySeconds = 1 bsl erlang:list_to_integer(PollStr),
            Message = lists:flatten(io_lib:format(
                "synchronized to ~s~s at stratum ~s~n"
                "   time correct to within ~p ms~n"
                "   polling server every ~p s~n",
                [ClockSourceStr, RefIdStrFormat, StratumStr,
                 RootMilliSeconds, PollFrequencySeconds])),
            status_result_ok(Message, Status)
    end.

status_validate_value(Key, LengthMin, LengthMax, PayloadLookup) ->
    case maps:find(Key, PayloadLookup) of
        {ok, Value} ->
            Length = length(Value),
            if
                Length >= LengthMin, Length =< LengthMax ->
                    Value;
                true ->
                    undefined
            end;
        error ->
            undefined
    end.

status_result_ok(Message = [_ | _], Status) ->
    Status#status{error = undefined,
                  restart = false,
                  message = Message,
                  socket_error = undefined}.

status_result_error(Error, Status) ->
    status_result_error(Error, false, Status).

status_result_error(Error, Restart, Status)
    when is_atom(Error), is_boolean(Restart) ->
    Status#status{error = Error,
                  restart = Restart,
                  message = "",
                  socket_error = undefined}.

status_result_error_socket(SocketError, Status) ->
    Status#status{error = clock_unknown,
                  restart = false,
                  message = "",
                  socket_error = SocketError}.

status_result_error_socket_close(SocketError,
                                 #status{socket = Socket} = Status) ->
    true = Socket /= undefined,
    gen_udp:close(Socket),
    status_result_error_socket(SocketError, Status#status{socket = undefined}).

payload_lookup(PayloadBinary) ->
    PayloadBinarySize = byte_size(PayloadBinary),
    true = 564 >= PayloadBinarySize,
    payload_lookup(erlang:binary_to_list(PayloadBinary), #{}).

payload_lookup([], Lookup) ->
    Lookup;
payload_lookup([0 | _], Lookup) ->
    Lookup;
payload_lookup([Ignore | Payload], Lookup)
    when Ignore == $ ; Ignore == $,; Ignore == $\r; Ignore == $\n ->
    payload_lookup(Payload, Lookup);
payload_lookup([C | Payload], Lookup) ->
    payload_lookup_key(Payload, [C], Lookup).

payload_lookup_key([$= | Payload], Key, Lookup) ->
    [C | PayloadNew] = Payload,
    if
        C == $" ->
            payload_lookup_value_str(PayloadNew, [], Key, Lookup);
        true ->
            payload_lookup_value(PayloadNew, [C], Key, Lookup)
    end;
payload_lookup_key([C | Payload], Key, Lookup) ->
    payload_lookup_key(Payload, [C | Key], Lookup).

payload_lookup_value([Terminator | Payload], Value, Key, Lookup)
    when Terminator == $,; Terminator == $\r; Terminator == $\n ->
    payload_lookup_update(Payload, Value, Key, Lookup);
payload_lookup_value([C | Payload], Value, Key, Lookup) ->
    payload_lookup_value(Payload, [C | Value], Key, Lookup).

payload_lookup_value_str([$" | Payload], Value, Key, Lookup) ->
    payload_lookup_update(Payload, Value, Key, Lookup);
payload_lookup_value_str([C | Payload], Value, Key, Lookup) ->
    payload_lookup_value_str(Payload, [C | Value], Key, Lookup).

payload_lookup_update(Payload, Value, Key, Lookup) ->
    KeyNew = lists:reverse(Key),
    ValueNew = lists:reverse(Value),
    payload_lookup(Payload, maps:put(KeyNew, ValueNew, Lookup)).

% RFC 1305, Appendix B, Section 2.2.1
clock_source(0) ->
    "unspecified";
clock_source(1) ->
    "atomic clock";
clock_source(2) ->
    "VLF radio";
clock_source(3) ->
    "HF radio";
clock_source(4) ->
    "UHF radio";
clock_source(5) ->
    "local net";
clock_source(6) ->
    "NTP server";
clock_source(7) ->
    "UDP/TIME";
clock_source(8) ->
    "wristwatch";
clock_source(9) ->
    "modem";
clock_source(_) ->
    "unknown source".

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

-spec exit_code(ExitCode :: integer()) -> no_return().

exit_code(ExitCode) when is_integer(ExitCode) ->
    erlang:halt(ExitCode, [{flush, true}]).
