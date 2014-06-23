%% Copyright (c) 2013-2014 Krzysztof Rutka
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.

%% @doc API module.
%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>
-module(erlcql).

%% API
-export([start_link/0,
         start_link/1,
         start_link/2]).
-export([q/2, q/3,
         e/3, e/4]).
-export([default/1]).

-include("erlcql.hrl").

-opaque query_ref() :: {Ref :: reference(),
                        Pid :: pid(),
                        Stream :: integer()}.

-export_type([response/0,
              consistency/0,
              compression/0,
              event_type/0,
              event_fun/0,
              query_ref/0]).
-export_type([values/0,
              type/0,
              native_type/0,
              uuid/0,
              collection_type/0,
              erlcql_list/0,
              erlcql_set/0,
              erlcql_map/0]).

%%-----------------------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, Reason :: term()}.
start_link() ->
    erlcql_client:start_link([]).

-spec start_link(string()) -> {ok, pid()} | ignore | {error, Reason :: term()}.
start_link(Host) ->
    start_link(Host, []).

-spec start_link(string(), proplists:proplist()) ->
          {ok, pid()} | ignore | {error, Reason :: term()}.
start_link(Host, Opts) ->
    Opts2 = [{host, Host} | Opts],
    erlcql_client:start_link(Opts2).

-spec q(pid(), iodata()) -> result() | {error, Reason :: term()}.
q(Pid, Query) ->
    q(Pid, Query, default(consistency)).

-spec q(pid(), iodata(), consistency()) ->
          result() | {error, Reason :: term()}.
q(Pid, Query, Consistency) ->
    erlcql_client:'query'(Pid, Query, Consistency).

-spec e(pid(), binary(), values()) ->
          result() | {error, Reason :: term()}.
e(Pid, QueryId, Values) ->
    e(Pid, QueryId, Values, default(consistency)).

-spec e(pid(), binary(), values(), consistency()) ->
          result() | {error, Reason :: term()}.
e(Pid, QueryId, Values, Consistency) ->
    erlcql_client:execute(Pid, QueryId, Values, Consistency).

-spec default(atom()) -> term().
default(host) -> "localhost";
default(port) -> 9042;
default(username) -> <<"cassandra">>;
default(password) -> <<"cassandra">>;
default(cql_version) -> undefined;
default(compression) -> false;
default(tracing) -> false;
default(event_handler) -> self();
default(consistency) -> quorum;
default(auto_reconnect) -> false;
default(register) -> [];
default(use) -> undefined;
default(prepare) -> [];
default(reconnect_start) -> 1000;
default(reconnect_max) -> 30000;
default(keepalive) -> false. 
