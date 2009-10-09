%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Replace a Link with a Monitor to a Process Hierarchy or a Single Process==
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

-module(monitor_link).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([call/5, call/6]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Call a function to create a remote link that is monitored by the calling process.===
%% Any link the called function generates is removed.  This is meant for
%% supervisor:start_link on a remote node, or the other stdlib behaviours.
%% @end
%%-------------------------------------------------------------------------

-spec call(OldPid :: pid() | 'undefined',
           Node :: atom(),
           M :: atom(),
           F :: atom(),
           A :: list()) ->
    {'ok', pid()} |
    {'error', any()}.

call(OldPid, Node, M, F, A) ->
    call(OldPid, Node, M, F, A, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Call a function to create a remote process that is monitored by the calling process, with a timeout.===
%% Any link the called function generates is removed.  This is meant for
%% supervisor:start_link on a remote node, or the other stdlib behaviours.
%% @end
%%-------------------------------------------------------------------------

-spec call(OldPid :: pid() | 'undefined',
           Node :: atom(),
           M :: atom(),
           F :: atom(),
           A :: list(),
           Timeout :: pos_integer()) ->
    {'ok', pid()} |
    {'error', any()}.

call(OldPid, Node, M, F, A, Timeout) ->
    Parent = self(),
    try erlang:spawn(Node, fun() ->
            case erlang:apply(M, F, A) of
                {ok, Pid} = Result ->
                    unlink(Pid),
                    Parent ! {self(), Result};
                {error, _} = Result ->
                    Parent ! {self(), Result}
            end
        end) of
        Child when is_pid(Child) ->
            receive
                {Child, {ok, Pid} = Result} ->
                    erlang:monitor(process, Pid),
                    Result;
                {Child, {error, {already_started, OldPid}}} ->
                    % assume an old monitor exists
                    {ok, OldPid};
                {Child, {error, {already_started, Pid}}} ->
                    erlang:monitor(process, Pid),
                    {ok, Pid};
                {Child, {error, _} = Result} ->
                    Result
            after
                Timeout ->
                    {error, timeout}
            end
    catch
        error:Reason ->
            {error, Reason}
    end.

