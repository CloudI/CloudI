%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Application Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2013 Michael Truog
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-include("cpg_constants.hrl").

-ifdef(CPG_ETS_CACHE).
-define(CPG_ETS_CACHE_START(R),
        ChildSpec1 = {cpg_ets1,
                      {cpg_ets, start_link, []},
                      permanent, brutal_kill, worker, [cpg_ets]},
        ChildSpec2 = {cpg_ets2,
                      {cpg_ets, start_link, []},
                      permanent, brutal_kill, worker, [cpg_ets]},
        ChildSpec3 = {cpg_ets3,
                      {cpg_ets, start_link, []},
                      permanent, brutal_kill, worker, [cpg_ets]},
        ok = cpg_ets:table_create(),
        case R of
            {ok, SupervisorPid} = Result ->
                {ok, Child1} = supervisor:start_child(SupervisorPid,
                                                      ChildSpec1),
                {ok, Child2} = supervisor:start_child(SupervisorPid,
                                                      ChildSpec2),
                {ok, _} = supervisor:start_child(SupervisorPid,
                                                 ChildSpec3),
                ok = cpg_ets:table_owners(Child1, Child2),
                Result;
            Result ->
                Result
        end).
-else.
-define(CPG_ETS_CACHE_START(R),
        R).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the CPG application supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(ScopeList :: list(atom())) ->
    {'ok', pid()} |
    {'error', any()}.

start_link([A | _] = ScopeList) when is_atom(A) ->
    ?CPG_ETS_CACHE_START(supervisor:start_link(?MODULE, [ScopeList])).

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

%% @private
%% @doc
%% @end

init([ScopeList]) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    {ok,
     {{one_for_one, MaxRestarts, MaxTime},
      child_specifications(ScopeList)}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

child_specifications([_ | _] = ScopeList) ->
    child_specifications([], ScopeList).

child_specifications(ChildSpecs, []) ->
    ChildSpecs;

child_specifications(ChildSpecs, [Scope | L]) when is_atom(Scope) ->
    Shutdown = 2000, % milliseconds
    ChildSpec = {Scope,
                 {cpg, start_link, [Scope]},
                 permanent, Shutdown, worker, [cpg]},
    child_specifications([ChildSpec | ChildSpecs], L).

