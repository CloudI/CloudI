%% Copyright (c) 2011-2020, Loïc Hoguin <essen@ninenines.eu>
%% Copyright (c) 2020, Jan Uhlig <j.uhlig@mailingwork.de>
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

-module(ranch_ssl).
-behaviour(ranch_transport).

-export([name/0]).
-export([secure/0]).
-export([messages/0]).
-export([listen/1]).
-export([disallowed_listen_options/0]).
-export([accept/2]).
-export([handshake/2]).
-export([handshake/3]).
-export([handshake_continue/2]).
-export([handshake_continue/3]).
-export([handshake_cancel/1]).
-export([connect/3]).
-export([connect/4]).
-export([recv/3]).
-export([recv_proxy_header/2]).
-export([send/2]).
-export([sendfile/2]).
-export([sendfile/4]).
-export([sendfile/5]).
-export([setopts/2]).
-export([getopts/2]).
-export([getstat/1]).
-export([getstat/2]).
-export([controlling_process/2]).
-export([peername/1]).
-export([sockname/1]).
-export([shutdown/2]).
-export([close/1]).
-export([cleanup/1]).

-type ssl_opt() :: {alpn_preferred_protocols, [binary()]}
	| {anti_replay, '10k' | '100k' | {integer(), integer(), integer()}}
	| {beast_mitigation, one_n_minus_one | zero_n | disabled}
	| {cacertfile, file:filename()}
	| {cacerts, [public_key:der_encoded()]}
	| {cert, public_key:der_encoded()}
	| {certfile, file:filename()}
	| {ciphers, ssl:ciphers()}
	| {client_renegotiation, boolean()}
	| {crl_cache, [any()]}
	| {crl_check, boolean() | peer | best_effort}
	| {depth, integer()}
	| {dh, binary()}
	| {dhfile, file:filename()}
	| {eccs, [ssl:named_curve()]}
	| {fail_if_no_peer_cert, boolean()}
	| {handshake, hello | full}
	| {hibernate_after, timeout()}
	| {honor_cipher_order, boolean()}
	| {honor_ecc_order, boolean()}
	| {key, ssl:key()}
	| {key_update_at, pos_integer()}
	| {keyfile, file:filename()}
	| {log_alert, boolean()}
	| {log_level, logger:level()}
	| {max_handshake_size, integer()}
	| {middlebox_comp_mode, boolean()}
	| {next_protocols_advertised, [binary()]}
	| {padding_check, boolean()}
	| {partial_chain, fun()}
	| {password, string()}
	| {protocol, tls | dtls}
	| {psk_identity, string()}
	| {reuse_session, fun()}
	| {reuse_sessions, boolean()}
	| {secure_renegotiate, boolean()}
	| {session_tickets, disabled | stateful | stateless}
	| {signature_algs, [{ssl:hash(), ssl:sign_algo()}]}
	| {signature_algs_cert, [ssl:sign_scheme()]}
	| {sni_fun, fun()}
	| {sni_hosts, [{string(), ssl_opt()}]}
	| {supported_groups, [ssl:group()]}
	| {user_lookup_fun, {fun(), any()}}
	| {verify, verify_none | verify_peer}
	| {verify_fun, {fun(), any()}}
	| {versions, [ssl:protocol_version()]}.
-export_type([ssl_opt/0]).

-type opt() :: ranch_tcp:opt() | ssl_opt().
-export_type([opt/0]).

-type opts() :: [opt()].
-export_type([opts/0]).

-spec name() -> ssl.
name() -> ssl.

-spec secure() -> boolean().
secure() ->
	true.

-spec messages() -> {ssl, ssl_closed, ssl_error, ssl_passive}.
messages() -> {ssl, ssl_closed, ssl_error, ssl_passive}.

-spec listen(ranch:transport_opts(opts())) -> {ok, ssl:sslsocket()} | {error, atom()}.
listen(TransOpts) ->
	ok = cleanup(TransOpts),
	SocketOpts = maps:get(socket_opts, TransOpts, []),
	case lists:keymember(cert, 1, SocketOpts)
			orelse lists:keymember(certfile, 1, SocketOpts)
			orelse lists:keymember(sni_fun, 1, SocketOpts)
			orelse lists:keymember(sni_hosts, 1, SocketOpts) of
		true ->
			Logger = maps:get(logger, TransOpts, logger),
			do_listen(SocketOpts, Logger);
		false ->
			{error, no_cert}
	end.

do_listen(SocketOpts0, Logger) ->
	SocketOpts1 = ranch:set_option_default(SocketOpts0, backlog, 1024),
	SocketOpts2 = ranch:set_option_default(SocketOpts1, nodelay, true),
	SocketOpts3 = ranch:set_option_default(SocketOpts2, send_timeout, 30000),
	SocketOpts = ranch:set_option_default(SocketOpts3, send_timeout_close, true),
	%% We set the port to 0 because it is given in the Opts directly.
	%% The port in the options takes precedence over the one in the
	%% first argument.
	ssl:listen(0, ranch:filter_options(SocketOpts, disallowed_listen_options(),
		[binary, {active, false}, {packet, raw}, {reuseaddr, true}], Logger)).

%% 'binary' and 'list' are disallowed but they are handled
%% specifically as they do not have 2-tuple equivalents.
-spec disallowed_listen_options() -> [atom()].
disallowed_listen_options() ->
	[alpn_advertised_protocols, client_preferred_next_protocols,
		fallback, server_name_indication, srp_identity
		|ranch_tcp:disallowed_listen_options()].

-spec accept(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, closed | timeout | atom()}.
accept(LSocket, Timeout) ->
	ssl:transport_accept(LSocket, Timeout).

-spec handshake(inet:socket() | ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {ok, ssl:sslsocket(), ssl:protocol_extensions()} | {error, any()}.
handshake(CSocket, Timeout) ->
	handshake(CSocket, [], Timeout).

-spec handshake(inet:socket() | ssl:sslsocket(), opts(), timeout())
	-> {ok, ssl:sslsocket()} | {ok, ssl:sslsocket(), ssl:protocol_extensions()} | {error, any()}.
handshake(CSocket, Opts, Timeout) ->
	case ssl:handshake(CSocket, Opts, Timeout) of
		OK = {ok, _} ->
			OK;
		OK = {ok, _, _} ->
			OK;
		Error = {error, _} ->
			Error
	end.

-spec handshake_continue(ssl:sslsocket(), timeout())
	-> {ok, ssl:sslsocket()} | {error, any()}.
handshake_continue(CSocket, Timeout) ->
	handshake_continue(CSocket, [], Timeout).

-spec handshake_continue(ssl:sslsocket(), [ssl:tls_server_option()], timeout())
	-> {ok, ssl:sslsocket()} | {error, any()}.
handshake_continue(CSocket, Opts, Timeout) ->
	case ssl:handshake_continue(CSocket, Opts, Timeout) of
		OK = {ok, _} ->
			OK;
		Error = {error, _} ->
			Error
	end.

-spec handshake_cancel(ssl:sslsocket()) -> ok.
handshake_cancel(CSocket) ->
	ok = ssl:handshake_cancel(CSocket).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}]).

%% @todo Probably filter Opts?
-spec connect(inet:ip_address() | inet:hostname(),
	inet:port_number(), any(), timeout())
	-> {ok, inet:socket()} | {error, atom()}.
connect(Host, Port, Opts, Timeout) when is_integer(Port) ->
	ssl:connect(Host, Port,
		Opts ++ [binary, {active, false}, {packet, raw}],
		Timeout).

-spec recv(ssl:sslsocket(), non_neg_integer(), timeout())
	-> {ok, any()} | {error, closed | atom()}.
recv(Socket, Length, Timeout) ->
	ssl:recv(Socket, Length, Timeout).

-spec recv_proxy_header(ssl:sslsocket(), timeout())
	-> {ok, ranch_proxy_header:proxy_info()}
	| {error, closed | atom()}
	| {error, protocol_error, atom()}.
recv_proxy_header(SSLSocket, Timeout) ->
	%% There's currently no documented way to perform a TCP recv
	%% on an sslsocket(), even before the TLS handshake. However
	%% nothing prevents us from retrieving the TCP socket and using
	%% it. Since it's an undocumented interface this may however
	%% make forward-compatibility more difficult.
	{sslsocket, {gen_tcp, TCPSocket, _, _}, _} = SSLSocket,
	ranch_tcp:recv_proxy_header(TCPSocket, Timeout).

-spec send(ssl:sslsocket(), iodata()) -> ok | {error, atom()}.
send(Socket, Packet) ->
	ssl:send(Socket, Packet).

-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, Filename) ->
	sendfile(Socket, Filename, 0, 0, []).

-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd(),
		non_neg_integer(), non_neg_integer())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes) ->
	sendfile(Socket, File, Offset, Bytes, []).

%% Unlike with TCP, no syscall can be used here, so sending files
%% through SSL will be much slower in comparison. Note that unlike
%% file:sendfile/5 this function accepts either a file or a file name.
-spec sendfile(ssl:sslsocket(), file:name_all() | file:fd(),
		non_neg_integer(), non_neg_integer(), ranch_transport:sendfile_opts())
	-> {ok, non_neg_integer()} | {error, atom()}.
sendfile(Socket, File, Offset, Bytes, Opts) ->
	ranch_transport:sendfile(?MODULE, Socket, File, Offset, Bytes, Opts).

%% @todo Probably filter Opts?
-spec setopts(ssl:sslsocket(), list()) -> ok | {error, atom()}.
setopts(Socket, Opts) ->
	ssl:setopts(Socket, Opts).

-spec getopts(ssl:sslsocket(), [atom()]) -> {ok, list()} | {error, atom()}.
getopts(Socket, Opts) ->
	ssl:getopts(Socket, Opts).

-spec getstat(ssl:sslsocket()) -> {ok, list()} | {error, atom()}.
getstat(Socket) ->
	ssl:getstat(Socket).

-spec getstat(ssl:sslsocket(), [atom()]) -> {ok, list()} | {error, atom()}.
getstat(Socket, OptionNames) ->
	ssl:getstat(Socket, OptionNames).

-spec controlling_process(ssl:sslsocket(), pid())
	-> ok | {error, closed | not_owner | atom()}.
controlling_process(Socket, Pid) ->
	ssl:controlling_process(Socket, Pid).

-spec peername(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
peername(Socket) ->
	ssl:peername(Socket).

-spec sockname(ssl:sslsocket())
	-> {ok, {inet:ip_address(), inet:port_number()} | {local, binary()}} | {error, atom()}.
sockname(Socket) ->
	ssl:sockname(Socket).

-spec shutdown(ssl:sslsocket(), read | write | read_write)
	-> ok | {error, atom()}.
shutdown(Socket, How) ->
	ssl:shutdown(Socket, How).

-spec close(ssl:sslsocket()) -> ok.
close(Socket) ->
	ssl:close(Socket).

-spec cleanup(ranch:transport_opts(opts())) -> ok.
cleanup(#{socket_opts:=SocketOpts}) ->
	case lists:keyfind(ip, 1, lists:reverse(SocketOpts)) of
		{ip, {local, SockFile}} ->
			_ = file:delete(SockFile),
			ok;
		_ ->
			ok
	end;
cleanup(_) ->
	ok.
