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
%%% File:      folsom_metrics_counter.erl
%%% @author    joe williams <j@boundary.com>
%%% @doc
%%% @end
%%%-----------------------------------------------------------------

-module(folsom_metrics_counter).

-behaviour(gen_server).

-export([new/1,
         inc/1,
         inc/2,
         dec/1,
         dec/2,
         get_value/1,
         clear/1,
         delete/1]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

-define(WIDTH, 16). %% Keep this a power of two

-include("folsom.hrl").

new(Name) ->
    gen_server:call(?SERVER, {new, Name}).

inc(Name) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), 1).

inc(Name, Value) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), Value).

dec(Name) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), -1).

dec(Name, Value) ->
    ets:update_counter(?COUNTER_TABLE, key(Name), -Value).

get_value(Name) ->
    Count = lists:sum(ets:select(?COUNTER_TABLE, [{{{Name,'_'},'$1'},[],['$1']}])),
    Count.

clear(Name) ->
    Counters = [{{Name,N}, 0} || N <- lists:seq(0,?WIDTH-1)],
    true = ets:insert(?COUNTER_TABLE, Counters).

delete(Name) ->
    Counters = [{Name,N} || N <- lists:seq(0,?WIDTH-1)],
    [ets:delete(?COUNTER_TABLE, Counter) || Counter <- Counters],
    ok.

key(Name) ->
    X = erlang:system_info(scheduler_id),
    Rnd = X band (?WIDTH-1),
    {Name, Rnd}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({new, Name}, _From, State) ->
    Reply = case ets:member(?COUNTER_TABLE, Name) of
                false ->
                    Counters = [{{Name,N}, 0} || N <- lists:seq(0,?WIDTH-1)],
                    true = ets:insert(?COUNTER_TABLE, Counters),
                    ets:insert(?FOLSOM_TABLE, {Name, #metric{type = counter}});
                true ->
                    true
            end,
    {reply, Reply, State};
handle_call({clear, Name}, _From, State) ->
    Reply = case ets:member(?COUNTER_TABLE, Name) of
                false ->
                    Counters = [{{Name,N}, 0} || N <- lists:seq(0,?WIDTH-1)],
                    true = ets:insert(?COUNTER_TABLE, Counters),
                    ets:insert(?FOLSOM_TABLE, {Name, #metric{type = counter}});
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
