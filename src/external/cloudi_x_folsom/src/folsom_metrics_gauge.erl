%%%
%%% Copyright 2011, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%


%%%-------------------------------------------------------------------
%%% File:      folsom_metrics_gauge.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_metrics_gauge).

-behaviour(gen_server).

-export([new/1,
         update/2,
         clear/1,
         get_value/1]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("folsom.hrl").

new(Name) ->
    gen_server:call(?SERVER, {new, Name}).

update(Name, Value) ->
    Gauge = {Name, Value},
    ets:insert(?GAUGE_TABLE, Gauge).

clear(Name) ->
    new(Name).

get_value(Name) ->
    [{_, Values}] = ets:lookup(?GAUGE_TABLE, Name),
    Values.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({new, Name}, _From, State) ->
    Reply = case ets:member(?GAUGE_TABLE, Name) of
                false ->
                    Gauge = {Name, 0},
                    ets:insert(?GAUGE_TABLE, Gauge),
                    ets:insert(?FOLSOM_TABLE, {Name, #metric{type = gauge}});
                true ->
                    true
            end,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.
