%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_internal_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/0,
         create_internal/15,
         create_internal_done/3]).

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

create_internal(ProcessIndex, ProcessCount, GroupLeader,
                Module, Args, Timeout, Prefix,
                TimeoutSync, TimeoutAsync, TimeoutTerm,
                DestRefresh, DestDeny, DestAllow,
                ConfigOptions, ID)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
         is_atom(Module), is_list(Args), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutSync), is_integer(TimeoutAsync),
         is_integer(TimeoutTerm) ->
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
           (DestRefresh == immediate_newest) orelse
           (DestRefresh == lazy_newest) orelse
           (DestRefresh == immediate_oldest) orelse
           (DestRefresh == lazy_oldest) orelse
           (DestRefresh == none),
    case supervisor:start_child(?MODULE, [ProcessIndex,
                                          ProcessCount, GroupLeader,
                                          Module, Args, Timeout, Prefix,
                                          TimeoutSync, TimeoutAsync,
                                          TimeoutTerm,
                                          DestRefresh, DestDeny, DestAllow,
                                          ConfigOptions, ID, self()]) of
        {ok, Dispatcher} ->
            result(Dispatcher);
        {ok, Dispatcher, _} ->
            result(Dispatcher);
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

create_internal_done(Parent, Dispatcher, ReceiverPid)
    when is_pid(Parent), is_pid(Dispatcher), is_pid(ReceiverPid) ->
    Parent ! {self, Dispatcher, ReceiverPid},
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([]) ->
    MaxRestarts = 0,
    MaxTime = 1,
    Shutdown = infinity, % cloudi_core_i_services_monitor handles shutdown
    {ok, {{simple_one_for_one, MaxRestarts, MaxTime}, 
          [{undefined,
            {cloudi_core_i_services_internal, start_link, []},
            temporary, Shutdown, worker, [cloudi_core_i_services_internal]}]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

result(Dispatcher) ->
    % must not call cloudi_service:self/1 due to the possibility
    % that external source code (called within cloudi_service_init/4)
    % will consume any incoming messages it doesn't understand
    % (e.g., in the source code doing a start_link)
    MonitorRef = erlang:monitor(process, Dispatcher),
    receive
        {self, Dispatcher, Service} ->
            % sent before cloudi_service_init/4 via create_internal_done/3
            erlang:demonitor(MonitorRef),
            {ok, Service};
        {'DOWN', MonitorRef, process, Dispatcher, Info} ->
            {error, Info}
    end.

