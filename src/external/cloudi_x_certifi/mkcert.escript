#!/usr/bin/env escript
%% -*- erlang -*-

-module(mkcert).
-compile([compressed]).

-define(CERTIFI_URL, "https://mkcert.org/generate/").
-define(CA_BUNDLE, "cacerts.pem").
-define(WEAK_BUNDLE, "weak.pem").
-define(OLD_ROOT, "old_root.pem").


init_script() ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(inets),
    ok.

fetch_bundle() ->
    Headers = [{"User-Agent", "erlang-certifi/1.0"},
               {"Connection", "close"},
               {"Cache-Control", "no-cache"},
               {"Pragma", "no-cache"}],
    Req = {?CERTIFI_URL, Headers},
    case httpc:request(get, Req, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, Body};
        {ok, {{_, Status, _}, _, _}} ->
            {http_error, Status}
    end.

script_dir() ->
	filename:dirname(escript:script_name()).

priv_dir() ->
	filename:join(script_dir(), "priv").

src_dir() ->
	filename:join(script_dir(), "src").


build_module(Name, Certs) ->
    Pems = public_key:pem_decode(Certs),
    Cacerts = [Der || {'Certificate', Der, _} <- Pems],

    Src = ["-module(", Name, ").\n",
           "-export([ders/0]).\n\n",
           "ders() ->", io_lib:format("~n    ~p.~n", [Cacerts])],

    Filename = filename:join(src_dir(), Name ++ ".erl"),
    ok = file:write_file(Filename, iolist_to_binary(Src)),
    ok.


main(_) ->
    init_script(),
    io:format("==> Fetch cacerts...~n"),
    {ok, Cacerts} = fetch_bundle(),

    %% store cacerts
    io:format("==> Store cacerts in priv directory...~n"),
    ok = file:write_file(filename:join(priv_dir(), ?CA_BUNDLE), Cacerts),

    %% build weak certificates
    {ok, OldRoot} = file:read_file(filename:join(priv_dir(), ?OLD_ROOT)),
    Weakcerts = << Cacerts/binary, OldRoot/binary >>,
    ok = file:write_file(filename:join(priv_dir(), ?WEAK_BUNDLE), Weakcerts),

    io:format("==> Build sources modules...~n"),

    ok = build_module("certifi_cacerts", Cacerts),
    ok = build_module("certifi_weakcerts", Weakcerts),
    ok.
