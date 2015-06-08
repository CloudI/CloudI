%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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
%%% @version 1.2.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/1,
         start_scope/1]).

%% supervisor callbacks
-export([init/1]).

-include("cpg_constants.hrl").

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

start_link([A | _] = ScopeList)
    when is_atom(A) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ScopeList]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start a CPG scope.===
%% @end
%%-------------------------------------------------------------------------

-spec start_scope(Scope :: atom()) ->
    'ok' |
    {'error', any()}.

start_scope(Scope)
    when is_atom(Scope) ->
    case erlang:whereis(Scope) of
        undefined ->
            case supervisor:start_child(?MODULE, child_specification(Scope)) of
                {ok, _} ->
                    ok;
                {ok, _, _} ->
                    ok;
                {error, Reason} ->
                    {error, {start_error, Reason}}
            end;
        _ ->
            {error, {already_started, Scope}}
    end.

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

child_specifications(ChildSpecs, [Scope | L]) ->
    child_specifications([child_specification(Scope) | ChildSpecs], L).

child_specification(Scope)
    when is_atom(Scope) ->
    Shutdown = 2000, % milliseconds
    {Scope,
     {cpg, start_link, [Scope]},
     permanent, Shutdown, worker, [cpg]}.

