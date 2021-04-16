%%%
%%% Copyright 2012, Basho Technologies, Inc.  All Rights Reserved.
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
%%% File:      folsom_metrics_spiral.erl
%%% @author    Russell Brown <russelldb@basho.com>
%%% @doc A total count, and sliding window count of events over the last
%%%      minute.
%%% @end
%%%------------------------------------------------------------------

-module(folsom_metrics_spiral).

-behaviour(gen_server).

-export([new/1,
         new/2,
         update/2,
         trim/2,
         get_value/1,
         get_values/1
        ]).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {}).

%% size of the window in seconds
-define(WINDOW, 60).
-define(WIDTH, 16). %% Keep this a power of two

-include("folsom.hrl").

new(Name) ->
    new(Name, fast).

new(Name, Update) when Update == fast orelse Update == no_exceptions ->
    gen_server:call(?SERVER, {new, Name, Update}).

update(Name, Value) ->
    #spiral{tid=Tid, update=Update} = get_value(Name),
    Moment = folsom_utils:now_epoch(),
    X = erlang:system_info(scheduler_id),
    Rnd = X band (?WIDTH-1),
    folsom_utils:Update(Tid, {Moment, Rnd}, Value),
    ets:update_counter(Tid, {count, Rnd}, Value).

get_value(Name) ->
    [{Name, Spiral}] =  ets:lookup(?SPIRAL_TABLE, Name),
    Spiral.

trim(Tid, _Window) ->
    Oldest = oldest(),
    ets:select_delete(Tid, [{{{'$1','_'},'_'}, [{is_integer, '$1'}, {'<', '$1', Oldest}], ['true']}]).

get_values(Name) ->
    Oldest = oldest(),
    #spiral{tid=Tid} = get_value(Name),
    Count = lists:sum(ets:select(Tid, [{{{count,'_'},'$1'},[],['$1']}])),
    One = lists:sum(ets:select(Tid, [{{{'$1','_'},'$2'},[{is_integer, '$1'}, {'>=', '$1', Oldest}],['$2']}])),

    [{count, Count}, {one, One}].

oldest() ->
    folsom_utils:now_epoch() - ?WINDOW.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({new, Name, Update}, _From, State) ->
    Reply = case folsom_ets:handler_exists(Name) of
                false ->
                    UpdateFun = case Update of
                                    fast ->
                                        update_counter;
                                    no_exceptions ->
                                        update_counter_no_exceptions
                                end,
                    Spiral = #spiral{update=UpdateFun},
                    Pid = folsom_sample_slide_sup:start_slide_server(?MODULE,
                                                                     Spiral#spiral.tid,
                                                                     ?WINDOW),
                    ets:insert_new(Spiral#spiral.tid,
                                   [{{count, N}, 0} || N <- lists:seq(0,?WIDTH-1)]),
                    true = ets:insert(?FOLSOM_TABLE, {Name, #metric{type = spiral}}),
                    ets:insert(?SPIRAL_TABLE, {Name, Spiral#spiral{server=Pid}});
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
