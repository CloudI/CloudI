%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Task Size Calculation==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_task_size).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         get/3,
         put/5]).

-include("cloudi_logger.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a new task size lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec new() -> dict().

new() ->
    dict:new().

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the task size information.===
%% @end
%%-------------------------------------------------------------------------

-spec get(TaskSize :: float(),
          Pid :: pid(),
          State :: dict()) -> float().

get(TaskSize, Pid, State)
    when is_pid(Pid) ->
    case dict:find(node(Pid), State) of
        {ok, OldTaskSize} ->
            OldTaskSize;
        error ->
            TaskSize
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store task size information.===
%% ElapsedTime and TargetTime are in hours.
%% @end
%%-------------------------------------------------------------------------

-spec put(TaskSize :: float(),
          TargetTime :: float(),
          ElapsedTime :: float(),
          Pid :: pid(),
          State :: dict()) -> dict().

put(TaskSize, TargetTime, ElapsedTime, Pid, State)
    when is_float(TaskSize), is_float(TargetTime), is_float(ElapsedTime),
         is_pid(Pid) ->
    dict:store(node(Pid),
               smooth_task_size(TaskSize * (TargetTime / ElapsedTime),
                                TaskSize, TargetTime, ElapsedTime), State).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

smooth_task_size(NewTaskSize, OldTaskSize, TargetTime, ElapsedTime)
    when is_float(NewTaskSize), is_float(OldTaskSize),
         is_float(TargetTime), is_float(ElapsedTime) ->
    Difference = erlang:abs((TargetTime - ElapsedTime) / ElapsedTime),
    % determine the truncated moving average period based on the
    % percentage difference between the elapsed time and the target time
    % (empirically found solution that is relatively stable
    %  and provides slow convergance, until a better solution is found)
    %SmoothingFactor = if
    %    Difference =< 1.0 ->
    %        math_extensions:product(lists:seq(
    %            math_extensions:floor(math:log(Difference) /
    %            math:log(3.0)) *
    %            -2 + 1,
    %        1, -2)) * 8 * erlang:float(Threads);
    %    true ->
    %        math_extensions:product(lists:seq(
    %            math_extensions:ceil(math:log(Difference) /
    %            math:log(50.0)) *
    %            2 + 1,
    %        1, -2)) * 8 * erlang:float(Threads)
    %end,
    % smoothing method as separate sequences
    SmoothingFactor = if
        Difference =< 0.0625 ->
            7560.0;
        Difference =< 0.125 ->
            840.0;
        Difference =< 0.25 ->
            120.0;
        Difference =< 0.5 ->
            24.0;
        Difference =< 1.0 ->
            8.0;
        Difference =< 50.0 ->                   % = 1.0 * 50
            24.0;                               % = 8.0 * 3
        Difference =< 2500.0 ->                 % = 50.0 * 50
            120.0;                              % = 24.0 * 5
        Difference =< 125000.0 ->               % = 2500.0 * 50
            840.0;                              % = 120.0 * 7
        Difference =< 6250000.0 ->              % = 125000.0 * 50
            7560.0;                             % = 840.0 * 9
        true ->
            83160.0                             % = 7560.0 * 11
    end,
    % perform truncated moving average
    % (but do not allow the value to grow too large,
    %  in case the task size is being ignored)
    erlang:min(OldTaskSize + (NewTaskSize - OldTaskSize) / SmoothingFactor,
               1000.0).

