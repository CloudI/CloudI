%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI External Service Supervisor==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_external_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/0,
         create_external/17]).

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
                TimeoutSync, TimeoutAsync, TimeoutTerm,
                DestRefresh, DestDeny, DestAllow,
                ConfigOptions, ID)
    when is_atom(Protocol), is_list(SocketPath), is_integer(ThreadIndex),
         is_integer(ProcessIndex), is_integer(ProcessCount),
         is_list(CommandLine),
         is_integer(BufferSize), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutSync), is_integer(TimeoutAsync),
         is_integer(TimeoutTerm) ->
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
                                 TimeoutSync, TimeoutAsync, TimeoutTerm,
                                 DestRefresh, DestDeny, DestAllow,
                                 ConfigOptions, ID]) of
        {ok, Dispatcher} ->
            result(Dispatcher, Timeout);
        {ok, Dispatcher, _} ->
            result(Dispatcher, Timeout);
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

result(Service, Timeout) ->
    try cloudi_core_i_services_external:port(Service, Timeout) of
        Port ->
            {ok, Service, Port}
    catch
        exit:{timeout, _} ->
            {error, external_spawn_timeout};
        exit:{noproc, _} ->
            {error, external_spawn_timeout};
        _:Reason ->
            {error, Reason}
    end.
