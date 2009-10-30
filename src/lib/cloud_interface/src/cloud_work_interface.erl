%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Work Interface Behavior==
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

-module(cloud_work_interface).
-author('mjtruog [at] gmail (dot) com').

%% behavior callbacks
-export([behaviour_info/1]).

%% behavior external interface
-export([stop/1,
         get_initial_task_size/1, get_task_time_target/1, get_task/3,
         drain_binary_output/2]).

-include("cloud_logger.hrl").
-include("cloud_types.hrl").

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===cloud_work_interface callbacks.===
%% ====start_link/2====
%%   `start_link(WorkTitle :: atom(), Arguments :: list(any())) -> ok | {error, any()}.'
%%
%%   The start_link/2 function will perform any initialization necessary
%%   before the instance (represented by the work title) is accessed
%%   for creating work tasks.  The Arguments to the work module were
%%   provided by the work configuration and should be paired with the
%%   unique suffix on the work module to create the work title
%%   (i.e., "work title" == "work_module.unique_suffix").
%%   
%% ====handle_stop/2====
%%   `handle_stop(WorkTitle :: atom()) -> any().'
%%
%%   The handle_stop/2 function will be called when it is no longer necessary
%%   to create work tasks because the work title has either finished
%%   processing, has failed, or is being interrupted.
%%
%% ====handle_get_initial_task_size/0====
%%   `handle_get_initial_task_size() -> float().'
%%
%%   The handle_get_initial_task_size/0 function is called when determining
%%   the initial task size to use when allocating a task for a worker
%%   thread on a particular node.  After receiving results and the time
%%   they took, the scheduler adjusts the task size so that the node
%%   meets the task time target.  The initial task size is always a floating
%%   point value in the range (0..1).
%%
%% ====handle_get_task_time_target/0====
%%   `handle_get_task_time_target() -> float().'
%%
%%   The handle_get_task_time_target/0 function provides the task time target.
%%   The task time target is the amount of time a task should take to
%%   execute.  Since the task size to achieve the task time target is
%%   unknown and node specific, the scheduler changes the task size so
%%   that the task execution time gets closer to the task time target.
%%   The task execution time is assumed to be somewhat unstable and
%%   can converge indefinitely if the work has irregular performance.
%%   The initial task size gives a general hint to the scheduler so that
%%   it might achieve the task time target sooner.  However, it is
%%   easiest for most situations to use the smallest task size possible
%%   and rely on the scheduler to determine the work task size.
%%
%% ====handle_get_task/3====
%%   `handle_get_task(WorkTitle :: atom(), SequenceNumber :: integer(), TaskSize :: float()) -> {binary(), list(string())}.'
%%
%%   The handle_get_task/3 function returns a new work task that is
%%   sent to a worker thread by the work_manager process.  The TaskSize
%%   determines how large of a task to allocate for the worker thread.
%%   The SequenceNumber is used to identify the task (and is unique
%%   for the life of the Cloudi instance, since this is the unwrapped
%%   integer, i.e., an integer that could exceed 32bits) and could be
%%   used as part of the task data.  The work task is defined by the
%%   binary TaskData that is returned and if the TaskData is empty
%%   all work is assumed to have been completed.  The work function
%%   input queries are also returned in a list but are not required for a
%%   work task.
%%
%% ====handle_drain_binary_output/2====
%%   `handle_drain_binary_output(WorkTitle :: atom(), DataList :: list(string())) -> list(string()).'
%%
%%   The handle_drain_binary_output/2 function processes any output queries
%%   that the C++ work function generates with the "binary" data title.
%%   This function could be used as a type of feedback that alters the
%%   tasks that are generated by the work module.  However, usage of this
%%   function is atypical.  The handle_drain_binary_output/2 function
%%   operates in the same way as any other internal data module function
%%   that is passed to cloud_data_interface:do_queries_group/5.
%%   The handle_drain_binary_output/2 function must consume all valid
%%   queries that match the "binary" data type and return the remaining
%%   queries for further processing.
%%
%% @end
%%-------------------------------------------------------------------------

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [
        {start_link, 2},
        {handle_stop, 1},
        {handle_get_initial_task_size, 0},
        {handle_get_task_time_target, 0},
        {handle_get_task, 3},
        {handle_drain_binary_output, 2}
    ];
behaviour_info(_) ->
    undefined.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the work module.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(WorkTitle :: cstring()) -> 'ok' | 'error'.

stop(WorkTitle) when is_list(WorkTitle) ->
    call_work(WorkTitle, 'handle_stop', [], true,
              error).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Get the initial task size for allocating the first task.===
%% The task size is always a floating point value less than 1.0.
%% @end
%%-------------------------------------------------------------------------

-spec get_initial_task_size(WorkTitle :: cstring()) -> float().

get_initial_task_size(WorkTitle) when is_list(WorkTitle) ->
    call_work(WorkTitle, 'handle_get_initial_task_size', [], false,
              0.0).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the time target for the execution of a task, in hours as a floating point number.===
%% Make sure all nodes can handle their assigned work
%% within the time target to avoid excessive memory consumption.
%% @end
%%-------------------------------------------------------------------------

-spec get_task_time_target(WorkTitle :: cstring()) -> float().
    
get_task_time_target(WorkTitle) when is_list(WorkTitle) ->
    call_work(WorkTitle, 'handle_get_task_time_target', [], false,
              0.0).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Get task data to send to the worker so the worker will understand the task parameters.===
%% @end
%%-------------------------------------------------------------------------

-spec get_task(WorkTitle :: cstring(),
               Sequence :: non_neg_integer(),
               TaskSize :: float()) ->
    {binary(), list({cstring(), cstring()})}.

get_task(WorkTitle, Sequence, TaskSize)
    when is_list(WorkTitle), is_integer(Sequence), is_float(TaskSize) ->
    call_work(WorkTitle, 'handle_get_task', [Sequence, TaskSize], true,
              {<<>>, []}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the work module with any data output that uses the special 'binary' data title===
%% All data that can not be handled by the work module (for any reason) should
%% be returned by the function and it will be retained in the work status.
%% @end
%%-------------------------------------------------------------------------

-spec drain_binary_output(WorkTitle :: cstring(),
                          DataList :: list(cstring())) ->
    list(cstring()).

drain_binary_output(WorkTitle, DataList)
    when is_list(WorkTitle), is_list(DataList) ->
    call_work(WorkTitle, 'handle_drain_binary_output', [DataList], true,
              DataList).
    
%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

call_work(WorkTitle, Function, Arguments, AddWorkInstance, Error)
    when is_list(WorkTitle), is_atom(Function), is_list(Arguments) ->
    try erlang:list_to_existing_atom(WorkTitle) of
        WorkInstance ->
            CallArguments = if
                AddWorkInstance ->
                    [WorkInstance] ++ Arguments;
                true ->
                    Arguments
            end,
            case string_extensions:before_character($., WorkTitle) of
                [] ->
                    erlang:apply(WorkInstance, Function, CallArguments);
                L when is_list(L) ->
                    try erlang:list_to_existing_atom(L) of
                        WorkModule ->
                            erlang:apply(WorkModule, Function, CallArguments)
                    catch
                        error:badarg ->
                            ?LOG_ERROR("invalid work module ~p", [L]),
                            Error
                    end
            end
    catch
        error:badarg ->
            ?LOG_ERROR("invalid work title ~p", [WorkTitle]),
            Error
    end.

