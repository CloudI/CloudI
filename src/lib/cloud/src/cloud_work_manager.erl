%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Work Manager==
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
%%% @version 0.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_work_manager).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0]).
-export([get_task_speed_lookup/0, update/1, decode/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_work_status.hrl").
-include("WorkerProtocol.hrl").
-include("cloud_logger.hrl").
-include("rbdict.hrl").

%% delay between receiving results and passing them to the data module
%% (preserving the order defined by the work module,
%%  for each of the data repositories specified with data titles)
-define(RESULT_COLLECTION_DELAY, 1000). % 1 second

-record(state,
    {
    work_status = undefined,
    data_type_lookup = undefined,
    task_speed_lookup = undefined,
    result_collection_timer = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the work manager server process.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link() -> {'ok', pid()} | {'error', any()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current task lookup from the work manager.===
%% @end
%%-------------------------------------------------------------------------

-spec get_task_speed_lookup() -> rbdict().

get_task_speed_lookup() ->
    gen_server:call(?MODULE, get_task_speed_lookup).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update the work manager with recently scheduled processes.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Processes :: list()) -> 'ok'.

update(Processes) ->
    gen_server:call(?MODULE, {update, Processes}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decode asn1 data coming from the worker cnode interface and respond by sending asn1 data.===
%% Server state is modified to reflect any changes within the work manager.
%% @end
%%-------------------------------------------------------------------------

-spec decode(#state{},
             Source :: pid(),
             Data :: binary(),
             Self :: pid()) -> #state{}.

decode(#state{work_status = WorkStatus,
              data_type_lookup = DataTypeLookup,
              task_speed_lookup = WorkLookup,
              result_collection_timer = Timer} = State,
       Source, Data, Self)
    when is_pid(Source), is_binary(Data), is_pid(Self) ->
    case 'WorkerProtocol':decode('WorkerMessage', Data) of
        {ok, {'pullJobTaskRequest', PullRequest}} ->
            case respond(PullRequest, Source,
                         WorkLookup, WorkStatus) of
                error ->
                    State;
                {Response, WorkLookup, UpdatedWorkStatus} ->
                    Source ! {asn1, Self, Response},
                    State#state{work_status = UpdatedWorkStatus}
            end;
        {ok, {'pushJobTaskResultRequest', PushRequest}} ->
            case respond(PushRequest, Source, 
                         WorkLookup, DataTypeLookup, WorkStatus) of
                error ->
                    State;
                ignore ->
                    State;
                {Response, NewWorkLookup, NewDataTypeLookup, NewWorkStatus} ->
                    Source ! {asn1, Self, Response},
                    NewTimer = if
                        Timer == undefined ->
                            erlang:send_after(?RESULT_COLLECTION_DELAY,
                                Self, result_collection);
                        true ->
                            Timer
                    end,
                    State#state{work_status = NewWorkStatus,
                                data_type_lookup = NewDataTypeLookup,
                                task_speed_lookup = NewWorkLookup,
                                result_collection_timer = NewTimer}
            end;
        {error, Reason} ->
            ?LOG_ERROR("asn1 decode error: ~p", [Reason]),
            State
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    {ok, #state{work_status = cloud_work_status:new(),
                data_type_lookup = rbsets:new(),
                task_speed_lookup = cloud_task_speed_lookup:new()}}.

handle_call(get_task_speed_lookup, _,
            #state{task_speed_lookup = WorkLookup} = State) ->
    {reply, WorkLookup, State};

handle_call({update, RunningProcesses}, _,
            #state{work_status = WorkStatus,
                   task_speed_lookup = WorkLookup} = State) ->
    UpdatedWorkStatus = cloud_work_status:update(
        RunningProcesses, WorkStatus),
    UpdatedWorkLookup = cloud_task_speed_lookup:update(
        RunningProcesses, WorkLookup),
    {reply, ok, State#state{work_status = UpdatedWorkStatus,
                            task_speed_lookup = UpdatedWorkLookup}};

handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({asn1, Source, Data}, State) when is_pid(Source), is_binary(Data) ->
    {noreply, immediate_gc:sync_fun(cloud_work_manager, decode,
                                    [State, Source, Data, self()])};

handle_info(result_collection,
            #state{work_status = WorkStatus,
                   data_type_lookup = DataTypeLookup} = State) ->
    %StartTime = erlang:now(),
    {DrainedState, NewWorkStatus} = 
        immediate_gc:sync_fun(cloud_work_status, drain_output,
                              [WorkStatus, DataTypeLookup]),
    %?LOG_DEBUG("drain_output/2 took ~p milliseconds (~p task results remain)",
    %           [time_extensions:elapsed(erlang:now(), StartTime),
    %            cloud_work_status:output_length(NewWorkStatus)]),
    Timer = if
        DrainedState == ok ->
            undefined;
        true ->
            erlang:send_after(?RESULT_COLLECTION_DELAY,
                self(), result_collection)
    end,
    {noreply, State#state{work_status = NewWorkStatus,
                          result_collection_timer = Timer}};

handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, #state{result_collection_timer = undefined}) ->
    ok;

terminate(_, #state{result_collection_timer = Timer}) ->
    erlang:cancel_timer(Timer),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-define(PULL_TYPE, 'WorkerMessage_pullJobTaskResponse').

%% create a task for the type of work that was requested
respond(#'WorkerMessage_pullJobTaskRequest'{
        workerName = WorkerName,
        workTitle = WorkTitle,
        id = Id
    } = Request, Pid, WorkLookup, WorkStatus)
    when is_pid(Pid) ->
    case cloud_work_status:has_active_work(WorkTitle, WorkStatus) of
        false ->
            % no work is present yet, just respond with a null task
            {ok, Answer} = 'WorkerProtocol':encode(
                'WorkerMessage',
                {'pullJobTaskResponse',
                 #?PULL_TYPE{
                     workTitle = WorkTitle,
                     id = Id,
                     totalIds = 0,
                     sequence = 0,
                     taskSize = float_to_list(0.0),
                     taskData = <<>>,
                     queries = [],
                     failureCount = 0
                 }
                }),
            {list_to_binary(Answer), WorkLookup, WorkStatus};
        true ->
            Node = node(Pid),
            case cloud_work_status:get_sequence_number(
                    WorkTitle, Id, Node, WorkStatus) of
                error ->
                    error;
                {Sequence, TotalIds, undefined, UpdatedWorkStatus} ->
                    TaskSize = cloud_task_speed_lookup:get_tasksize(
                        Node, WorkTitle,
                        cloud_work_interface:get_initial_task_size(WorkTitle),
                        WorkLookup),
                    case cloud_work_interface:get_task(
                        WorkTitle, Sequence, TaskSize) of
                        {<<>>, []} ->
                            % the work module is done or has failed
                            case cloud_work_status:is_inactive_work(
                                WorkTitle, WorkStatus) of
                                error ->
                                    error;
                                {ok, NewWorkStatus} ->
                                    {ok, Answer} = 'WorkerProtocol':encode(
                                        'WorkerMessage',
                                        {'pullJobTaskResponse',
                                         #?PULL_TYPE{
                                             workTitle = WorkTitle,
                                             id = Id,
                                             totalIds = 0,
                                             sequence = 0,
                                             taskSize = float_to_list(0.0),
                                             taskData = <<>>,
                                             queries = [],
                                             failureCount = 0
                                         }
                                        }),
                                    {list_to_binary(Answer),
                                     WorkLookup, NewWorkStatus}
                            end;
                        {TaskData, Queries} ->
                            ProtocolQueries = lists:map(fun({T, Q}) ->
                                #'Query'{type = T, 'query' = Q}
                            end, Queries),
                            case cloud_work_status:set_cached_task(
                                WorkTitle, Id, #work_status_working_task{
                                    task_size = TaskSize,
                                    task_data = TaskData,
                                    queries = ProtocolQueries,
                                    failure_count = 0},
                                UpdatedWorkStatus) of
                                error ->
                                    error;
                                {Sequence, FinalWorkStatus} ->
                                    {ok, Answer} = 'WorkerProtocol':encode(
                                        'WorkerMessage',
                                        {'pullJobTaskResponse',
                                         #?PULL_TYPE{
                                            workTitle = WorkTitle,
                                            id = Id,
                                            totalIds = TotalIds,
                                            sequence = Sequence,
                                            taskSize = 
                                                erlang:float_to_list(TaskSize),
                                            taskData = TaskData,
                                            queries = ProtocolQueries,
                                            failureCount = 0
                                         }
                                        }),
                                    {list_to_binary(Answer),
                                     WorkLookup, FinalWorkStatus}
          
                            end
                    end;
                % a process asking for a cached task must have failed,
                % but the same task can be received to
                % attempt the same work again
                {Sequence, TotalIds, #work_status_working_task{
                    task_size = TaskSize,
                    task_data = TaskData,
                    queries = ProtocolQueries,
                    failure_count = FailureCount}, UpdatedWorkStatus} ->
                    {ok, Answer} = 'WorkerProtocol':encode(
                        'WorkerMessage',
                        {'pullJobTaskResponse',
                         #?PULL_TYPE{
                             workTitle = WorkTitle,
                             id = Id,
                             totalIds = TotalIds,
                             sequence = Sequence,
                             taskSize = erlang:float_to_list(TaskSize),
                             taskData = TaskData,
                             queries = ProtocolQueries,
                             failureCount = FailureCount
                         }
                        }),
                    {list_to_binary(Answer), WorkLookup, UpdatedWorkStatus}
            end
    end.

-define(PUSH_TYPE, 'WorkerMessage_pushJobTaskResultResponse').

%% collect the results of a task for a type of work
respond(#'WorkerMessage_pushJobTaskResultRequest'{
        workerName = WorkerName,
        workTitle = WorkTitle,
        id = Id,
        sequence = Sequence,
        taskSize = StrTaskSize,
        taskData = TaskData,
        elapsedTime = StrElapsedTime,
        returnValue = ReturnValue,
        queries = Queries,
        failureCount = FailureCount
    } = Request, Pid, WorkLookup, DataTypeLookup, WorkStatus)
    when is_pid(Pid) ->
    Task = cloud_work_status:get_cached_task(WorkTitle, Id, WorkStatus),
    if
        Task == error ->
            % old data from the worker has been sent and
            % replying to it will make the future queries
            % pull valid tasks
            {ok, Answer} = 'WorkerProtocol':encode(
                'WorkerMessage',
                {'pushJobTaskResultResponse',
                 #?PUSH_TYPE{
                     workTitle = WorkTitle,
                     id = Id,
                     taskData = TaskData
                 }
                }),
            {list_to_binary(Answer),
             WorkLookup, DataTypeLookup, WorkStatus};
        true ->
            if
                ReturnValue == false ->
                    % work task failed, so respond to acknowledge the failure
                    % (the next task request by the
                    %  same id will get the same task)
                    case cloud_work_status:set_cached_task(
                        WorkTitle, Id, Task#work_status_working_task{
                            failure_count = (FailureCount + 1)
                        }, WorkStatus) of
                        error ->
                            error;
                        {Sequence, UpdatedWorkStatus} ->
                            {ok, Answer} = 'WorkerProtocol':encode(
                                'WorkerMessage',
                                {'pushJobTaskResultResponse',
                                 #?PUSH_TYPE{
                                     workTitle = WorkTitle,
                                     id = Id,
                                     taskData = TaskData
                                 }
                                }),
                            ?LOG_WARNING("task ~p failed~n"
                                "task data: ~p", [Sequence, 
                                erlang:list_to_binary(TaskData)]),
                            {list_to_binary(Answer),
                             WorkLookup, DataTypeLookup,
                             UpdatedWorkStatus}
                    end;
                true ->
                    TaskSize = erlang:list_to_float(StrTaskSize),
                    ElapsedTime = erlang:list_to_float(StrElapsedTime),
                    Node = node(Pid),
                    {GenericQueries, UpdatedDataTypeLookup} = 
                        check_result_queries(Queries, DataTypeLookup),
                    case cloud_work_status:store_output(
                        WorkTitle, Id, Sequence, GenericQueries, WorkStatus) of
                        error ->
                            error;
                        duplicate ->
                            ignore;
                        {ok, NewWorkStatus} ->
                            NewWorkLookup = cloud_task_speed_lookup:add(
                                Node, WorkTitle, TaskSize,
                                cloud_work_interface:get_task_time_target(
                                    WorkTitle),
                                ElapsedTime, WorkLookup),
                            {ok, Answer} = 'WorkerProtocol':encode(
                                'WorkerMessage',
                                {'pushJobTaskResultResponse',
                                 #?PUSH_TYPE{
                                     workTitle = WorkTitle,
                                     id = Id,
                                     taskData = TaskData
                                 }
                                }),
                            {list_to_binary(Answer),
                             NewWorkLookup, UpdatedDataTypeLookup,
                             NewWorkStatus}
                    end
            end
    end.

%% check the query data to confirm it is valid and store it in a generic way
%% so that external data modules can consume the query data after accumulation.
check_result_queries(QueriesIn, DataTypeLookup)
    when is_list(QueriesIn) ->
    check_result_queries([], QueriesIn, DataTypeLookup).

check_result_queries(QueriesOut, [], DataTypeLookup)
    when is_list(QueriesOut) ->
    {QueriesOut, DataTypeLookup};

check_result_queries(QueriesOut, [
        #'Query'{type = DataTitleString, 'query' = Query} | QueriesIn
    ], DataTypeLookup)
    when is_list(QueriesOut), is_list(DataTitleString), is_binary(Query) ->
    try erlang:list_to_existing_atom(DataTitleString) of
        DataTitle ->
            case rbsets:is_element(DataTitle, DataTypeLookup) of
                true ->
                    check_result_queries(QueriesOut ++ [
                            {DataTitle, Query}
                        ], QueriesIn, DataTypeLookup);
                false ->
                    case cloud_data_repository_sup:exists(DataTitle) of
                        true ->
                            check_result_queries(QueriesOut ++ [
                                    {DataTitle, Query}
                                ], QueriesIn, 
                                rbsets:add_element(DataTitle, DataTypeLookup));
                        false ->
                            ?LOG_WARNING("data title process \"~s\" "
                                "does not exist, discarding query",
                                [DataTitleString]),
                            check_result_queries(QueriesOut,
                                QueriesIn, DataTypeLookup)
                    end
            end
    catch
        error:badarg -> 
            ?LOG_WARNING("data title atom \"~s\" does not exist, "
                "discarding query", [DataTitleString]),
            check_result_queries(QueriesOut, QueriesIn, DataTypeLookup)
    end.

