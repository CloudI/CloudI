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
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
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

