%% -------------------------------------------------------------------
%%
%% Copyright (c) 2015 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
-module(exometer_test_udp_reporter).

-behaviour(exometer_report).
-export(
   [
    exometer_init/1,
    exometer_info/2,
    exometer_cast/2,
    exometer_call/3,
    exometer_report/5,
    exometer_subscribe/5,
    exometer_unsubscribe/4,
    exometer_newentry/2,
    exometer_setopts/4,
    exometer_terminate/2
   ]).

-behaviour(exometer_report_logger).
-export([
         logger_init_input/1,
         logger_init_output/1,
         logger_handle_data/2
        ]).

-import(exometer_util, [get_opt/2, get_opt/3]).

-include_lib("kernel/include/inet.hrl").
-record(st, {socket, address, port, type_map, prefix = []}).

-define(DEFAULT_HOST, "localhost").

exometer_init(Opts) ->
    Port = get_opt(port, Opts),
    {ok, Host} = inet:gethostbyname(get_opt(hostname, Opts, ?DEFAULT_HOST)),
    [Addr|_] = Host#hostent.h_addr_list,
    AddrType = Host#hostent.h_addrtype,
    TypeMap = get_opt(type_map, Opts, []),
    Prefix = get_opt(prefix, Opts, []),
    case gen_udp:open(0, [AddrType]) of
        {ok, Socket} ->
            {ok, #st{socket = Socket, address = Addr, port = Port,
                     type_map = TypeMap, prefix = Prefix}};
        {error, _} = Error ->
            Error
    end.

exometer_report(Metric, DataPoint, Extra, Value, #st{type_map = TypeMap,
                                                     prefix = Pfx} = St) ->
    RptType = exometer_util:report_type({Metric,DataPoint}, Extra, TypeMap),
    ok = send({report, [{prefix, Pfx},
                        {metric, Metric},
                        {datapoint, DataPoint},
                        {extra, Extra},
                        {report_type, RptType},
                        {value, Value}]}, St),
    {ok, St}.

exometer_subscribe(Metric, DataPoint, Extra, Interval, St) ->
    ok = send({subscribe, [{metric, Metric},
                           {datapoint, DataPoint},
                           {extra, Extra},
                           {interval, Interval}]}, St),
    {ok, St}.

exometer_unsubscribe(Metric, DataPoint, Extra, St) ->
    ok = send({unsubscribe, [{metric, Metric},
                             {datapoint, DataPoint},
                             {extra, Extra}]}, St),
    {ok, St}.

exometer_call(Req, From, St) ->
    ok = send({call, Req, From}, St),
    {reply, {test_reply, ok}, St}.

exometer_cast(Cast, St) ->
    ok = send({cast, Cast}, St),
    {ok, St}.

exometer_info(I, St) ->
    ok = send({info, I}, St),
    {ok, St}.

exometer_newentry(E, St) ->
    ok = send({newentry, E}, St),
    {ok, St}.

exometer_setopts(Metric, Opts, Status, St) ->
    ok = send({setopts, [{metric, Metric},
                         {options, Opts},
                         {status, Status}]}, St),
    {ok, St}.

exometer_terminate(_, _) ->
    ok.

send(Term, #st{socket = Socket, address = Address, port = Port}) ->
    gen_udp:send(Socket, Address, Port, term_to_binary(Term, [compressed])).


logger_init_input({Port, Opts}) ->
    Parent = self(),
    {Pid,_} = spawn_monitor(
                fun() ->
                        erlang:monitor(process, Parent),
                        {ok, Socket} = gen_udp:open(Port, [binary|Opts]),
                        check_active(Socket, Parent),
                        input_loop(Socket, Parent)
                end),
    {ok, Pid}.

logger_init_output(_) ->
    {ok, []}.

logger_handle_data(Data, St) ->
    {binary_to_term(iolist_to_binary([Data])), St}.

input_loop(Socket, Parent) ->
    case receive
             {udp, Socket, _Host, _Port, Data} ->
                 Parent ! {plugin, self(), Data};
             {udp_passive, Socket} ->
                 Parent ! {plugin_passive, self()}, check;
             {plugin_active, Active} ->
                 ct:log("{plugin_active, ~p}", [Active]),
                 inet:setopts(Socket, [{active, Active}]);
             {'DOWN', _, process, Parent, _} ->
                 exit(normal)
         after 5000 ->
                 io:fwrite(user, "input_loop timeout~n", [])
         end of
        check ->
            check_active(Socket, Parent);
        _ ->
            ok
    end,
    input_loop(Socket, Parent).

check_active(Socket, Parent) ->
    case inet:getopts(Socket, [active]) of
        {ok, [{active, A}]} ->
            case A of
                _ when A==0; A==false; A==once ->
                    Parent ! {plugin_passive, self()};
                _ ->
                    ok
            end;
        _ ->
            ok
    end.
