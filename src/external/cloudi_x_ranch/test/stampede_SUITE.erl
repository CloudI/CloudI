%% Copyright (c) 2019-2020, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(stampede_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-import(ct_helper, [doc/1]).
-import(ct_helper, [name/0]).

%% ct.

all() ->
	ct_helper:all(?MODULE).

init_per_suite(Config) ->
	{ok, _} = application:ensure_all_started(ranch),
	ok = application:start(stampede),
	%% Enable logging of progress reports.
	%% They will only be available in the HTML reports by default.
	ok = logger:set_primary_config(level, none),
	ok = logger:set_module_level(?MODULE, info),
	ok = logger:set_application_level(stampede, error),
	Config.

end_per_suite(_) ->
	ok = application:stop(stampede),
	ok = application:stop(ranch).

%% Tests.

stampede_tcp(_) ->
	doc("Start a TCP listener, establish a hundred connections, "
		"run stampede, confirm we can still connect."),
	%% Start a TCP listener.
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_tcp, #{},
		echo_protocol, []),
	%% Establish a hundred connections.
	ok = do_connect(100, ranch_tcp, ranch:get_port(Name), 1000),
	%% Set restart frequency of ranch_sup.
	do_set_sup_frequencies([ranch_sup], 999999, 1),
	%% Run stampede.
	{ok, _} = stampede:start_herd(ranch_stampede, {application, ranch},
		#{interval => {100, 100}, before_kill => fun do_log/1}),
	ok = stampede:activate(ranch_stampede),
	timer:sleep(10000),
	ok = stampede:stop_herd(ranch_stampede),
	timer:sleep(1000),
	%% Confirm we can still connect.
	ok = do_connect(1, ranch_tcp, ranch:get_port(Name), 1000),
	ok = ranch:stop_listener(Name).

stampede_ssl(_) ->
	doc("Start a SSL listener, establish a hundred connections, "
		"run stampede, confirm we can still connect."),
	%% Start a TCP listener.
	Name = name(),
	{ok, _} = ranch:start_listener(Name,
		ranch_ssl, ct_helper:get_certs_from_ets(),
		echo_protocol, []),
	%% Establish a hundred connections.
	ok = do_connect(100, ranch_ssl, ranch:get_port(Name), 1000),
	%% Set restart frequencies of ranch_sup and ssl_sup.
	do_set_sup_frequencies([ranch_sup, ssl_sup], 999999, 1),
	%% Run stampede.
	{ok, _} = stampede:start_herd(ranch_stampede, {application, ranch},
		#{interval => {100, 100}, before_kill => fun do_log/1}),
	{ok, _} = stampede:start_herd(ssl_stampede, {application, ssl},
		#{interval => {100, 100}, before_kill => fun do_log/1}),
	ok = stampede:activate(ranch_stampede),
	ok = stampede:activate(ssl_stampede),
	timer:sleep(10000),
	ok = stampede:stop_herd(ssl_stampede),
	ok = stampede:stop_herd(ranch_stampede),
	timer:sleep(1000),
	%% Confirm we can still connect.
	ok = do_connect(1, ranch_ssl, ranch:get_port(Name), 1000),
	ok = ranch:stop_listener(Name).

stampede_embedded(_) ->
	doc("Start an embedded TCP listener, establish a hundred connections, "
		"run stampede, confirm we can still connect."),
	%% Start embedded listener.
	Name = name(),
	{ok, SupPid} = embedded_sup:start_link(),
	{ok, _} = embedded_sup:start_listener(SupPid, Name,
		ranch_tcp, #{}, echo_protocol, []),
	%% Establish a hundred connections.
	ok = do_connect(100, ranch_tcp, ranch:get_port(Name), 1000),
	%% Set restart frequency of ranch_sup and embedded_sup.
	do_set_sup_frequencies([ranch_sup, SupPid], 999999, 1),
	%% Run stampede.
	{ok, _} = stampede:start_herd(ranch_stampede, {application, ranch},
		#{interval => {100, 100}, before_kill => fun do_log/1}),
	{ok, _} = stampede:start_herd(embedded_stampede, {supervisor, SupPid},
		#{interval => {100, 100}, before_kill => fun do_log/1}),
	ok = stampede:activate(ranch_stampede),
	ok = stampede:activate(embedded_stampede),
	timer:sleep(10000),
	ok = stampede:stop_herd(ranch_stampede),
	ok = stampede:stop_herd(embedded_stampede),
	timer:sleep(1000),
	%% Confirm we can still connect.
	ok = do_connect(1, ranch_tcp, ranch:get_port(Name), 1000),
	ok = embedded_sup:stop_listener(SupPid, Name),
	embedded_sup:stop(SupPid),
	ok.

do_set_sup_frequencies(Sups, Intensity, Period) ->
	StateFun = fun (S) -> setelement(7, setelement(6, S, Intensity), Period) end,
	_ = [sys:replace_state(Sup, StateFun) || Sup <- Sups],
	ok.

do_connect(0, _, _, _) ->
	ok;
do_connect(N, Transport, Port, Timeout) ->
	{ok, _} = Transport:connect("localhost", Port, [{active, false}], Timeout),
	do_connect(N - 1, Transport, Port, Timeout).

do_log(Pid) when is_pid(Pid) ->
	ct:log(info, "~p: ~p~n", [Pid, erlang:process_info(Pid)]),
	true;
do_log(Port) when is_port(Port) ->
	ct:log(info, "~p: ~p~n", [Port, erlang:port_info(Port)]),
	true.
