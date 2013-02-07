%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service Supervisor==
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

-module(cloudi_internal_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/0,
         create_internal/11]).

%% supervisor callbacks
-export([init/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

create_internal(ProcessIndex, Module, Args, Timeout, Prefix,
                TimeoutSync, TimeoutAsync,
                DestRefresh, DestDeny, DestAllow, ConfigOptions)
    when is_integer(ProcessIndex), is_atom(Module), is_list(Args),
         is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutSync), is_integer(TimeoutAsync) ->
    true = (DestRefresh == immediate_closest) orelse
           (DestRefresh == lazy_closest) orelse
           (DestRefresh == immediate_furthest) orelse
           (DestRefresh == lazy_furthest) orelse
           (DestRefresh == immediate_random) orelse
           (DestRefresh == lazy_random) orelse
           (DestRefresh == immediate_local) orelse
           (DestRefresh == lazy_local) orelse
           (DestRefresh == immediate_remote) orelse
           (DestRefresh == lazy_remote) orelse
           (DestRefresh == none),
    case supervisor:start_child(?MODULE, [ProcessIndex, Module, Args,
                                          Timeout, Prefix,
                                          TimeoutSync, TimeoutAsync,
                                          DestRefresh, DestDeny, DestAllow,
                                          ConfigOptions]) of
        {ok, Pid} ->
            {ok, Pid};
        {ok, Pid, _} ->
            {ok, Pid};
        {error, _} = Error ->
            Error
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([]) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    Shutdown = 2000, % milliseconds (2 seconds)
    {ok, {{simple_one_for_one, MaxRestarts, MaxTime}, 
          [{undefined,
            {cloudi_internal, start_link, []},
            temporary, Shutdown, worker, []}]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

