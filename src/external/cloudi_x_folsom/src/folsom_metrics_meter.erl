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
%%% File:      folsom_metrics_meter.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_meter).

-behaviour(gen_server).

-export([new/1,
         tick/1,
         mark/1,
         mark/2,
         get_values/1,
         get_acceleration/1
        ]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-record(meter, {
          instant,
          one,
          five,
          fifteen,
          day,
          count = 0,
          start_time
         }).

-include("folsom.hrl").

new(Name) ->
    gen_server:call(?SERVER, {new, Name}).

tick(Name) ->
    #meter{instant = Instant,
           one = OneMin,
           five = FiveMin,
           fifteen = FifteenMin,
           day = OneDay} = Meter = get_value(Name),

    Instant1 = folsom_ewma:tick(Instant),
    OneMin1 = folsom_ewma:tick(OneMin),
    FiveMin1 = folsom_ewma:tick(FiveMin),
    FifteenMin1 = folsom_ewma:tick(FifteenMin),
    OneDay1 = folsom_ewma:tick(OneDay),

    ets:insert(?METER_TABLE,
               {Name, Meter#meter{
                                  instant = Instant1,
                                  one = OneMin1,
                                  five = FiveMin1,
                                  fifteen = FifteenMin1,
                                  day = OneDay1}}).

mark(Name) ->
    mark(Name, 1).

mark(Name, Value) ->
    #meter{
           instant = Instant,
           count = Count,
           one = OneMin,
           five = FiveMin,
           fifteen = FifteenMin,
           day = OneDay} = Meter = get_value(Name),

    Instant1 = folsom_ewma:update(Instant, Value),
    OneMin1 = folsom_ewma:update(OneMin, Value),
    FiveMin1 = folsom_ewma:update(FiveMin, Value),
    FifteenMin1 = folsom_ewma:update(FifteenMin, Value),
    OneDay1 = folsom_ewma:update(OneDay, Value),

    ets:insert(?METER_TABLE, {Name, Meter#meter{count = Count + Value,
                                                instant = Instant1,
                                                one = OneMin1,
                                                five = FiveMin1,
                                                fifteen = FifteenMin1,
                                                day = OneDay1}}).

get_values(Name) ->
    #meter{instant = Instant,
           one = OneMin,
           five = FiveMin,
           fifteen = FifteenMin,
           day = OneDay,
           count = Count} = Meter = get_value(Name),

    L = [
         {count, Count},
         {instant, get_rate(Instant)},
         {one, get_rate(OneMin)},
         {five, get_rate(FiveMin)},
         {fifteen, get_rate(FifteenMin)},
         {day, get_rate(OneDay)},
         {mean, get_mean_rate(Meter)},
         {acceleration, get_acceleration(Name)}
        ],

    [ {K,V} || {K,V} <- L, V /= undefined ].

get_acceleration(Name) ->
    #meter{instant = Instant,
           one = OneMin,
           five = FiveMin,
           fifteen = FifteenMin} = get_value(Name),

    [{instant_to_one, calc_acceleration(get_rate(Instant), get_rate(OneMin), 60)},
     {one_to_five, calc_acceleration(get_rate(OneMin), get_rate(FiveMin), 300)},
     {five_to_fifteen, calc_acceleration(get_rate(FiveMin), get_rate(FifteenMin), 600)},
     {one_to_fifteen, calc_acceleration(get_rate(OneMin), get_rate(FifteenMin), 900)}
    ].

% internal functions

get_rate(EWMA) ->
    folsom_ewma:rate(EWMA).

get_mean_rate(#meter{count = Count, start_time = Start}) ->
    calc_mean_rate(Start, Count).

get_value(Name) ->
    [{_, Value}] = ets:lookup(?METER_TABLE, Name),
    Value.

calc_mean_rate(_, 0) ->
    0.0;
calc_mean_rate(Start, Count) ->
    case folsom_utils:now_epoch_micro() - Start of
        0 -> 0.0;
        Elapsed -> Count / Elapsed
    end.

calc_acceleration(Rate1, Rate2, Interval) ->
     % most current velocity minus previous velocity
    get_rate(Rate1, Rate2, Interval).

get_rate(Value1, Value2, Interval) ->
    Delta = Value1 - Value2,
    Delta / Interval.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({new, Name}, _From, State) ->
    Reply = case ets:member(?METER_TABLE, Name) of
                false ->
                    ok = folsom_meter_timer_server:register(Name, folsom_metrics_meter),
                    Instant = folsom_ewma:instant_ewma(),
                    OneMin = folsom_ewma:one_minute_ewma(),
                    FiveMin = folsom_ewma:five_minute_ewma(),
                    FifteenMin = folsom_ewma:fifteen_minute_ewma(),
                    OneDay = folsom_ewma:one_day_ewma(),

                    ets:insert(?METER_TABLE,
                               {Name, #meter{
                                         instant = Instant,
                                         one = OneMin,
                                         five = FiveMin,
                                         fifteen = FifteenMin,
                                         day = OneDay,
                                         start_time = folsom_utils:now_epoch_micro()}}),
                    ets:insert(?FOLSOM_TABLE, {Name, #metric{type = meter}});
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
