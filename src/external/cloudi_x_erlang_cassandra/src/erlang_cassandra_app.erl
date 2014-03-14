%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc Application file supporting erlang_cassandra
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erlang_cassandra_app).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


-spec start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: [term()]) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    erlang_cassandra_sup:start_link().

stop(_State) ->
    ok.
