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
%%% @version 0.0.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_work_interface).
-author('mjtruog [at] gmail (dot) com').

%% behavior callbacks
-export([behaviour_info/1]).

%% behavior external interface
-export([stop/1, get_initial_task_size/1, get_task_time_target/1, get_task/3]).

-include("cloud_logger.hrl").

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [
        {start_link, 2},
        {handle_stop, 1},
        {handle_get_initial_task_size, 0},
        {handle_get_task_time_target, 0},
        {handle_get_task, 3}
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

-spec stop(WorkTitle :: string()) -> 'ok' | 'error'.

stop(WorkTitle) when is_list(WorkTitle) ->
    call_work(WorkTitle, 'handle_stop', [], true,
              error).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Get the initial task size for allocating the first task.===
%% The task size is always a floating point value less than 1.0.
%% @end
%%-------------------------------------------------------------------------

-spec get_initial_task_size(WorkTitle :: string()) -> float().

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

-spec get_task_time_target(WorkTitle :: string()) -> float().
    
get_task_time_target(WorkTitle) when is_list(WorkTitle) ->
    call_work(WorkTitle, 'handle_get_task_time_target', [], false,
              0.0).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Get task data to send to the worker so the worker will understand the task parameters.===
%% @end
%%-------------------------------------------------------------------------

-spec get_task(WorkTitle :: string(),
               Sequence :: non_neg_integer(),
               TaskSize :: float()) ->
    {binary(), list({string(), string()})}.

get_task(WorkTitle, Sequence, TaskSize)
    when is_list(WorkTitle), is_integer(Sequence),
         is_float(TaskSize), TaskSize < 1.0 ->
    call_work(WorkTitle, 'handle_get_task', [Sequence, TaskSize], true,
              {<<>>, []}).
    
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

