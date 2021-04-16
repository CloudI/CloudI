%%
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
%%% File:      folsom_sup.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(folsom_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, create_tables/0]).

%% Supervisor callbacks
-export([init/1]).

-include("folsom.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    create_tables(),

    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    TimerServer = {folsom_meter_timer_server,
              {folsom_meter_timer_server, start_link, []},
              Restart, Shutdown, Type, [folsom_meter_timer_server]},

    HistETSServer = {folsom_metrics_histogram_ets,
              {folsom_metrics_histogram_ets, start_link, []},
              Restart, Shutdown, Type, [folsom_metrics_histogram_ets]},

    SlideSup = {folsom_sample_slide_sup, {folsom_sample_slide_sup, start_link, []},
                permanent, 5000, supervisor, [folsom_sample_slide_sup]},

    SpiralServer = {folsom_metrics_spiral,
                    {folsom_metrics_spiral, start_link, []},
                    Restart, Shutdown, Type, [folsom_metrics_spiral]},

    HistoryServer = {folsom_metrics_history,
                    {folsom_metrics_history, start_link, []},
                    Restart, Shutdown, Type, [folsom_metrics_history]},

    HistogramServer = {folsom_metrics_histogram,
                       {folsom_metrics_histogram, start_link, []},
                       Restart, Shutdown, Type, [folsom_metrics_histogram]},

    DurationServer = {folsom_metrics_duration,
                      {folsom_metrics_duration, start_link, []},
                      Restart, Shutdown, Type, [folsom_metrics_duration]},

    CounterServer = {folsom_metrics_counter,
                     {folsom_metrics_counter, start_link, []},
                     Restart, Shutdown, Type, [folsom_metrics_counter]},

    GaugeServer = {folsom_metrics_gauge,
                     {folsom_metrics_gauge, start_link, []},
                     Restart, Shutdown, Type, [folsom_metrics_gauge]},

    MeterServer = {folsom_metrics_meter,
                   {folsom_metrics_meter, start_link, []},
                   Restart, Shutdown, Type, [folsom_metrics_meter]},

    MeterReaderServer = {folsom_metrics_meter_reader,
                         {folsom_metrics_meter_reader, start_link, []},
                         Restart, Shutdown, Type, [folsom_metrics_meter_reader]},

    {ok, {SupFlags, [SlideSup, TimerServer, HistETSServer, SpiralServer, HistoryServer,
                     HistogramServer, DurationServer, CounterServer, GaugeServer,
                     MeterServer, MeterReaderServer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_tables() ->
    Tables = [
              {?FOLSOM_TABLE, [set, named_table, public, {read_concurrency, true}]},
              {?COUNTER_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?GAUGE_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?HISTOGRAM_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?METER_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?METER_READER_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?HISTORY_TABLE, [set, named_table, public, {write_concurrency, true}]},
              {?DURATION_TABLE, [ordered_set, named_table, public, {write_concurrency, true}]},
              {?SPIRAL_TABLE, [set, named_table, public, {write_concurrency, true}]}
             ],
    [maybe_create_table(ets:info(Name), Name, Opts) || {Name, Opts} <- Tables],
    ok.

maybe_create_table(undefined, Name, Opts) ->
    ets:new(Name, Opts);
maybe_create_table(_, _, _) ->
    ok.
