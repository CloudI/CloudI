%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI External Service Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_external_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/0,
         create_external/15]).

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

create_external(Protocol, SocketPath,
                ThreadIndex, ProcessIndex, ProcessCount,
                CommandLine, BufferSize, Timeout, Prefix,
                TimeoutSync, TimeoutAsync, DestRefresh,
                DestDeny, DestAllow, ConfigOptions)
    when is_atom(Protocol), is_list(SocketPath), is_integer(ThreadIndex),
         is_integer(ProcessIndex), is_integer(ProcessCount),
         is_list(CommandLine),
         is_integer(BufferSize), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutSync), is_integer(TimeoutAsync) ->
    true = (Protocol == tcp) orelse
           (Protocol == udp) orelse
           (Protocol == local),
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
    case supervisor:start_child(?MODULE,
                                [Protocol, SocketPath,
                                 ThreadIndex, ProcessIndex, ProcessCount,
                                 CommandLine, BufferSize, Timeout, Prefix,
                                 TimeoutSync, TimeoutAsync, DestRefresh,
                                 DestDeny, DestAllow, ConfigOptions]) of
        {ok, Dispatcher} ->
            result(Dispatcher);
        {ok, Dispatcher, _} ->
            result(Dispatcher);
        {error, _} = Error ->
            Error
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([]) ->
    MaxRestarts = 0,
    MaxTime = 1,
    Shutdown = infinity, % cloudi_core_i_services_monitor handles shutdown
    {ok, {{simple_one_for_one, MaxRestarts, MaxTime}, 
          [{undefined,
            {cloudi_core_i_services_external, start_link, []},
            temporary, Shutdown, worker, [cloudi_core_i_services_external]}]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

result(Service) ->
    try cloudi_core_i_services_external:port(Service) of
        Port ->
            {ok, Service, Port}
    catch
        exit:{timeout, _} ->
            {error, external_spawn_timeout};
        _:Reason ->
            {error, {Reason, erlang:get_stacktrace()}}
    end.
