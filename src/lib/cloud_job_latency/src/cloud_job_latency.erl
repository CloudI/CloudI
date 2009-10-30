%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Work Module For Testing System Latency==
%%% The test quantifies Cloudi system latency that occurs during trip1
%%% when work task data is sent to the worker, the processing time
%%% that takes place based on the task size and task time target, and during
%%% trip2 when the results are collected in the same order the tasks
%%% were created so that the data can be sent to any of the
%%% configured databases.
%%%
%%% This test depends on gettimeofday function calls within the Erlang VM
%%% and the C++ work library.  So remote machines require time synchronization
%%% to produce accurate latency results.  It is best if ntpd is setup to
%%% synchronize the time on the Cloudi machines
%%% (or if something more complex is in place).
%%%
%%% The trip2 latency is largely determined by the ?RESULT_COLLECTION_DELAY
%%% macro setting in lib/cloud/src/cloud_work_manager.erl
%%% (which is set to 1 second).
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2009 Michael Truog
%%% @version 0.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_job_latency).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloud_work_interface).
-behaviour(gen_server).

%% cloud_work_interface callbacks
-export([start_link/2,
         handle_stop/1,
         handle_get_initial_task_size/0,
         handle_get_task_time_target/0,
         handle_get_task/3,
         handle_drain_binary_output/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").

-record(state,
    {
    % lookups host -> {mean, stddev}
    trip1 = rbdict:new(),
    processing = rbdict:new(),
    trip2 = rbdict:new()}).

%%%------------------------------------------------------------------------
%%% Callback functions from cloud_work_interface
%%%------------------------------------------------------------------------

start_link(WorkTitle, [])
    when is_atom(WorkTitle) ->
    ?LOG_ERROR("make sure all machines have their clocks synced!!!", []),
    case gen_server:start_link({local, WorkTitle}, ?MODULE, [], []) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

handle_stop(WorkTitle)
    when is_atom(WorkTitle) ->
    gen_server:call(WorkTitle, stop).

handle_get_initial_task_size() ->
    0.99. % percentage

%handle_get_task_time_target() -> (1.0 / 3600.0).   %  1 second in hours
handle_get_task_time_target() -> (1.0 / 60.0).     %  1 minute in hours
%handle_get_task_time_target() -> (5.0 / 60.0).     %  5 minutes in hours
%handle_get_task_time_target() -> (15.0 / 60.0).    % 15 minutes in hours
%handle_get_task_time_target() -> (30.0 / 60.0).    % 30 minutes in hours
%handle_get_task_time_target() -> 1.0.              %  1 hour

handle_get_task(_, _, TaskSize)
    when is_float(TaskSize) ->
    case handle_get_task_time_target() of
        % 1 second maximum
        Time when Time =< (1.0000001 / 3600.0) ->
            TargetSleep = erlang:round((Time / (1.0 / 3600.0)) * 1000000.0),
            Sleep = math_extensions:ceil(TargetSleep * TaskSize),
            {CreateMegaSecs, CreateSecs, CreateMicroSecs} = erlang:now(),
            {<<Sleep:32/unsigned-integer-native, "usec",
               CreateMegaSecs:32/unsigned-integer-native,
               CreateSecs:32/unsigned-integer-native,
               CreateMicroSecs:32/unsigned-integer-native>>, []};
        % multiple seconds
        Time ->
            TargetSleep = erlang:round(Time * 3600.0),
            Sleep = math_extensions:ceil(TargetSleep * TaskSize),
            {CreateMegaSecs, CreateSecs, CreateMicroSecs} = erlang:now(),
            {<<Sleep:32/unsigned-integer-native, "sec ",
               CreateMegaSecs:32/unsigned-integer-native,
               CreateSecs:32/unsigned-integer-native,
               CreateMicroSecs:32/unsigned-integer-native>>, []}
    end.

handle_drain_binary_output(WorkTitle, DataList)
    when is_atom(WorkTitle), is_list(DataList) ->
    try gen_server:call(WorkTitle, {data, DataList}) of
        ok ->
            []
    catch
        _:_ ->
            DataList
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.
handle_call({data, DataList}, _, State) ->
    NewState = drain_latency_data(DataList, State),
    Trip1Lookup = NewState#state.trip1,
    Trip2Lookup = NewState#state.trip2,
    rbdict:fold(fun(MachineName, StdDev, _) ->
        {Count, _, _} = StdDev,
        {Trip1Mean, Trip1StdDev} =
            math_extensions:stddev(rbdict:fetch(MachineName, Trip1Lookup)),
        {ProcessingMean, ProcessingStdDev} =
            math_extensions:stddev(StdDev),
        {Trip2Mean, Trip2StdDev} =
            math_extensions:stddev(rbdict:fetch(MachineName, Trip2Lookup)),
        Total = Trip1Mean + ProcessingMean + Trip2Mean,
        ?LOG_INFO("~n~s (~p samples): ~n"
                  "         trip1 = ~13.3f ms +/- ~10.3f  (~7.3f %)~n"
                  "    processing = ~13.3f ms +/- ~10.3f  (~7.3f %)~n"
                  "         trip2 = ~13.3f ms +/- ~10.3f  (~7.3f %)",
            [MachineName, Count - 1,
             Trip1Mean, Trip1StdDev, Trip1Mean / Total * 100.0,
             ProcessingMean, ProcessingStdDev, ProcessingMean / Total * 100.0,
             Trip2Mean, Trip2StdDev, Trip2Mean / Total * 100.0]),
        ok
    end, ok, NewState#state.processing),
    {reply, ok, NewState};
handle_call(stop, _, State) ->
    {stop, "stop requested", ok, State};
handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.
handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.
handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.
terminate(_, _) ->
    ok.
code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

drain_latency_data(DataList, State) ->
    drain_latency_data(rbdict:new(), rbdict:new(), rbdict:new(),
        DataList, State).

drain_latency_data(Trip1Dict, ProcessingDict, Trip2Dict, [],
                   #state{trip1 = Trip1Lookup,
                          processing = ProcessingLookup,
                          trip2 = Trip2Lookup} = State) ->
    NewTrip1Lookup = rbdict:fold(fun(MachineName, Trip1List, L) ->
        case rbdict:find(MachineName, L) of
            error ->
                rbdict:store(MachineName,
                    math_extensions:stddev(Trip1List), L);
            {ok, StdDev} ->
                rbdict:store(MachineName,
                    math_extensions:stddev(Trip1List, StdDev), L)
        end
    end, Trip1Lookup, Trip1Dict),
    NewProcessingLookup = rbdict:fold(fun(MachineName, ProcessingList, L) ->
        case rbdict:find(MachineName, L) of
            error ->
                rbdict:store(MachineName,
                    math_extensions:stddev(ProcessingList), L);
            {ok, StdDev} ->
                rbdict:store(MachineName,
                    math_extensions:stddev(ProcessingList, StdDev), L)
        end
    end, ProcessingLookup, ProcessingDict),
    NewTrip2Lookup = rbdict:fold(fun(MachineName, Trip2List, L) ->
        case rbdict:find(MachineName, L) of
            error ->
                rbdict:store(MachineName,
                    math_extensions:stddev(Trip2List), L);
            {ok, StdDev} ->
                rbdict:store(MachineName,
                    math_extensions:stddev(Trip2List, StdDev), L)
        end
    end, Trip2Lookup, Trip2Dict),
    State#state{trip1 = NewTrip1Lookup,
                processing = NewProcessingLookup,
                trip2 = NewTrip2Lookup};

drain_latency_data(Trip1Dict, ProcessingDict, Trip2Dict,
                   [<<CreateMegaSecs:32/unsigned-integer-native,
                      CreateSecs:32/unsigned-integer-native,
                      CreateMicroSecs:32/unsigned-integer-native,
                      GetMegaSecs:32/unsigned-integer-native,
                      GetSecs:32/unsigned-integer-native,
                      GetMicroSecs:32/unsigned-integer-native,
                      ReturnMegaSecs:32/unsigned-integer-native,
                      ReturnSecs:32/unsigned-integer-native,
                      ReturnMicroSecs:32/unsigned-integer-native,
                      MachineName/binary>>
                      | Remaining], State) ->
    CreateTime = {CreateMegaSecs, CreateSecs, CreateMicroSecs},
    GetTime = {GetMegaSecs, GetSecs, GetMicroSecs},
    ReturnTime = {ReturnMegaSecs, ReturnSecs, ReturnMicroSecs},
    Trip1 = time_extensions:elapsed(GetTime, CreateTime),
    Processing = time_extensions:elapsed(ReturnTime, GetTime),
    Trip2 = time_extensions:elapsed(erlang:now(), ReturnTime),
    NewTrip1Dict = rbdict:update(MachineName, fun(Trip1List) ->
        [Trip1 | Trip1List]
    end, [Trip1], Trip1Dict),
    NewProcessingDict = rbdict:update(MachineName, fun(ProcessingList) ->
        [Processing | ProcessingList]
    end, [Processing], ProcessingDict),
    NewTrip2Dict = rbdict:update(MachineName, fun(Trip2List) ->
        [Trip2 | Trip2List]
    end, [Trip2], Trip2Dict),
    drain_latency_data(NewTrip1Dict, NewProcessingDict, NewTrip2Dict,
                       Remaining, State).

