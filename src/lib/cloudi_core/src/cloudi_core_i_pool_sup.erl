%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Pool Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_pool_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/3, start_link_done/1,
         start_children/2, which_children/1]).

%% supervisor callbacks
-export([init/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

start_link(Name, Count, {_, StartFunc, Restart, Shutdown, Type, Modules})
    when is_atom(Name), is_integer(Count), Count > 0 ->
    ChildSpecs = lists:foldl(fun(Id, L) ->
        [{Id, StartFunc, Restart, Shutdown, Type, Modules} | L]
    end, [], lists:seq(1, Count)),
    Result = supervisor:start_link(?MODULE, [Name, ChildSpecs, self()]),
    case Result of
        {ok, _} ->
            % make the startup synchronous to avoid causing problems for
            % supervisor dependencies listed sequentially
            % (i.e., within the same supervisor)
            receive startup_done -> ok end,
            Result;
        _ ->
            Result
    end.

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

start_link_done(Parent)
    when is_pid(Parent) ->
    Parent ! startup_done,
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

start_children(Supervisor, ChildSpecs)
    when is_pid(Supervisor), is_list(ChildSpecs) ->
    cloudi_lists:itera(fun(Child, L, F) ->
        case supervisor:start_child(Supervisor, Child) of
            {ok, Pid} ->
                F([Pid | L]);
            {ok, Pid, _} ->
                F([Pid | L]);
            {error, _} = Error ->
                Error
        end
    end, [], ChildSpecs).

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

which_children(Supervisor)
    when is_pid(Supervisor) ->
    lists:foldl(fun
        ({cloudi_core_i_pool, _, _, _}, L) ->
            L;
        ({_, undefined, _, _}, L) ->
            L;
        ({_, Pid, _, _}, L) ->
            [Pid | L]
    end, [], supervisor:which_children(Supervisor)).

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([Name, ChildSpecs, Parent]) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    Shutdown = 2000, % milliseconds (2 seconds)
    {ok, {{one_for_one, MaxRestarts, MaxTime}, 
          [{cloudi_core_i_pool,
            {cloudi_core_i_pool, start_link,
             [Name, ChildSpecs, self(), Parent]},
            permanent, Shutdown, worker, [cloudi_core_i_pool]}]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

