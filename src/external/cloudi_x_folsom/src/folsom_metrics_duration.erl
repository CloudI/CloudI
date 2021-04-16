%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2012 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% File:      folsom_metrics_duration.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc       Tracks the time something takes. If
%%%            you can, use folsom_metrics:histogram_timed_update/2,3,4.
%%%            This is for the case when you can't wrap your timed action
%%%            in a fun. Calling timer_start / timer_end in the correct
%%%            order is the calling code's responsibility.
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_duration).

-behaviour(gen_server).

-export([new/1,
         new/4,
         update/2,
         get_value/1,
         get_values/1
         ]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-include("folsom.hrl").

new(Name) ->
    gen_server:call(?SERVER, {new, Name}).

new(Name, SampleType, SampleSize, Alpha) ->
    gen_server:call(?SERVER, {new, Name, SampleType, SampleSize, Alpha}).

update(Name, timer_start) ->
    StartTime = os:timestamp(),
    ets:update_element(?DURATION_TABLE, Name, {3, StartTime});
update(Name, timer_end) ->
    EndTime = os:timestamp(),
    case ets:lookup_element(?DURATION_TABLE, Name, 3) of
        undefined ->
            ok;
        StartTime  ->
            %% potential race, but then you're using it wrong
            ets:update_element(?DURATION_TABLE, Name, {3, undefined}),
            Duration = timer:now_diff(EndTime, StartTime),
            ets:update_counter(?DURATION_TABLE, Name, {2, 1}),
            ets:update_element(?DURATION_TABLE, Name, {4, Duration}),
            folsom_metrics_histogram:update(Name, Duration)
    end.

% gets the duration tuple from ets
get_value(Name) ->
    [Dur] = ets:lookup(?DURATION_TABLE, Name),
    Dur.

% pulls the sample out of the record gotten from ets
% and the duration
get_values(Name) ->
    Values = folsom_metrics_histogram:get_values(Name),
    {Name, Cnt, _Start, Last} = get_value(Name),
    WantedMetrics = application:get_env(folsom, enabled_metrics, ?DEFAULT_METRICS),
    Stats = bear:get_statistics_subset(Values, WantedMetrics),
    [{count, Cnt}, {last, Last} | Stats].

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({new, Name}, _From, State) ->
    Reply = case ets:member(?DURATION_TABLE, Name) of
                false ->
                    true = folsom_metrics_histogram:new(Name),
                    %% {Name, count, start, last}
                    Dur = {Name, 0, undefined, 0},
                    ets:insert(?DURATION_TABLE, Dur),
                    ets:insert(?FOLSOM_TABLE, {Name, #metric{type = duration}});
                true ->
                    true
            end,
    {reply, Reply, State};
handle_call({new, Name, SampleType, SampleSize, Alpha}, _From, State) ->
    Reply = case ets:member(?DURATION_TABLE, Name) of
                false ->
                    true = folsom_metrics_histogram:new(Name, SampleType, SampleSize, Alpha),
                    %% {Name, count, start, last}
                    Dur = {Name, 0, undefined, 0},
                    ets:insert(?DURATION_TABLE, Dur),
                    ets:insert(?FOLSOM_TABLE, {Name, #metric{type = duration}});
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
