%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Application Supervisor==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2020 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2012-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_sup).
-author('mjtruog at protonmail dot com').

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
    {ok, pid()} |
    {error, any()}.

start_link(ScopeList)
    when is_list(ScopeList) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ScopeList]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start a CPG scope.===
%% @end
%%-------------------------------------------------------------------------

-spec start_scope(Scope :: atom()) ->
    ok |
    {error, {already_started, atom()} | {start_error, any()}}.

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

child_specifications(ScopeList) ->
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

