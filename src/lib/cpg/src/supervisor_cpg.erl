%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Supervisor.==
%%% Can provide process migration after a failure, or simply load balancing
%%% for a supervisor.  Use as an interface, not a behaviour (i.e., it is just
%%% a supervisor you can call functions on).  cpg_supervisor only provides the
%%% one_for_one supervisor strategy to avoid complexity.
%%%
%%% Meant to provide better functionality than:
%%% https://github.com/jbrisbin/rabbit_common/blob/master/src/mirrored_supervisor.erl
%%% (mnesia is avoided and state is minimized)
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(supervisor_cpg).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% supervisor callbacks
-export([init/1]).

%% supervisor_cpg interface functions
-export([start_link/3,
         start_link/4,
         start_nomad_child/4,
         start_remote_child/2]).

%% supervisor interface functions
-export([start_child/2,
         restart_child/2,
         delete_child/2,
         terminate_child/2,
         which_children/1,
         count_children/1,
         check_childspecs/1]).

%%%------------------------------------------------------------------------
%%% supervisor_cpg interface functions
%%%------------------------------------------------------------------------
start_link(Name, MaxR, MaxT) ->
    start_link(Name, MaxR, MaxT, []).

start_link(_, MaxR, MaxT, _)
    when is_integer(MaxR) =:= false; is_integer(MaxT) =:= false ->
    erlang:exit(badarg);
start_link(Name, MaxR, MaxT, ChildSpecs)
    when MaxR >= 0, MaxT > 0, is_list(ChildSpecs) ->
    supervisor:start_link(?MODULE,
                          [supervisor_cpg_spawn:cpg_name(Name),
                           MaxR, MaxT, ChildSpecs]).

start_nomad_child(Name, MaxR, MaxT, ChildSpec) ->
    supervisor_cpg_spawn:start_nomad_child(Name, MaxR, MaxT, ChildSpec).

start_remote_child(Name, ChildSpec) ->
    supervisor_cpg_spawn:start_remote_child(Name, ChildSpec).

%%%------------------------------------------------------------------------
%%% supervisor interface functions
%%%------------------------------------------------------------------------

start_child(Name, ChildSpec) ->
    supervisor_cpg_spawn:start_child(Name, ChildSpec).

terminate_child(Name, Id) ->
    supervisor_cpg_spawn:terminate_child(Name, Id).

delete_child(Name, Id) ->
    supervisor_cpg_spawn:delete_child(Name, Id).

restart_child(Name, Id) ->
    supervisor_cpg_spawn:restart_child(Name, Id).

which_children(Name) ->
    supervisor_cpg_spawn:which_children(Name).

count_children(Name) ->
    supervisor_cpg_spawn:count_children(Name).

check_childspecs(ChildSpecs) ->
    supervisor:check_childspecs(ChildSpecs).

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([SpawnName, MaxR, MaxT, ChildSpecs]) ->
    {ok,
     {{one_for_all, 0, 1},
      [{supervisor_cpg_spawn,
        {supervisor_cpg_spawn, start_link,
         [SpawnName, MaxR, MaxT, ChildSpecs]},
        permanent, brutal_kill,
        worker, [supervisor_cpg_spawn]}]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

