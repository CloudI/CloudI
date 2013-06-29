%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service API Module==
%%% A module that exposes dynamic configuration of CloudI.
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
%%% @version 1.2.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_api).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([acl_add/2,
         acl_remove/2,
         services_add/2,
         services_remove/2,
         services_restart/2,
         services/1,
         nodes_add/2,
         nodes_remove/2,
         nodes_alive/1,
         nodes_dead/1,
         nodes/1,
         loglevel_set/2,
         log_redirect/2,
         code_path_add/2,
         code_path_remove/2,
         code_path/1]).

-include("cloudi_constants.hrl").
-include("cloudi_service_api.hrl").

-type dest_refresh() :: lazy_closest | immediate_closest |
                        lazy_furthest | immediate_furthest |
                        lazy_random | immediate_random |
                        lazy_local | immediate_local |
                        lazy_remote | immediate_remote |
                        lazy_newest | immediate_newest |
                        lazy_oldest | immediate_oldest |
                        none.
-export_type([dest_refresh/0]).

-type timeout_milliseconds() :: ?TIMEOUT_DELTA..4294967295.
-export_type([timeout_milliseconds/0]).

-type acl() :: list(atom() | string()).
-export_type([acl/0]).

-type dest_list() :: undefined | acl().
-export_type([dest_list/0]).

-type seconds() :: 1..3600.
-export_type([seconds/0]).

-type service_options_internal() ::
    list({priority_default, -128..127} |
         {queue_limit, undefined | pos_integer()} |
         {dest_refresh_start, 100..3600000} |
         {dest_refresh_delay, 100..3600000} |
         {request_timeout_adjustment, boolean()} |
         {response_timeout_adjustment, boolean()} |
         {request_pid_uses, infinity | pos_integer()} |
         {request_pid_options,
          list({fullsweep_after, non_neg_integer()} |
               {min_heap_size, non_neg_integer()} |
               {min_bin_vheap_size, non_neg_integer()})} |
         {info_pid_uses, infinity | pos_integer()} |
         {info_pid_options,
          list({fullsweep_after, non_neg_integer()} |
               {min_heap_size, non_neg_integer()} |
               {min_bin_vheap_size, non_neg_integer()})} |
         {duo_mode, boolean()}).
-type service_options_external() ::
    list({priority_default, -128..127} |
         {queue_limit, undefined | pos_integer()} |
         {dest_refresh_start, 100..3600000} |
         {dest_refresh_delay, 100..3600000} |
         {request_timeout_adjustment, boolean()} |
         {response_timeout_adjustment, boolean()}).
-export_type([service_options_internal/0,
              service_options_external/0]).

-type loglevel() :: fatal | error | warn | info | debug | trace | off.
-export_type([loglevel/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Add ACL entries.===
%% Add more ACL entries to be later used when starting services. An ACL
%% entry is an Erlang atom() -> list(atom() | string()) relationship which
%% provides a logical grouping of service name patterns
%% (e.g., {api, ["/cloudi/api/"]}). When providing a service name pattern
%% for an ACL entry, a non-pattern will be assumed to be a prefix
%% (i.e., "/cloudi/api/" == "/cloudi/api/*").
%% @end
%%-------------------------------------------------------------------------

-spec acl_add(L :: list({atom(), acl()}), 
              Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

acl_add([_ | _] = L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:acl_add(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove ACL entries.===
%% Remove ACL entries that are no longer needed. Running services will
%% retain their configuration, so this impacts services that are started
%% in the future.
%% @end
%%-------------------------------------------------------------------------

-spec acl_remove(L :: list(atom()),
                 Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

acl_remove([_ | _] = L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:acl_remove(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add service instances.===
%% Provide service configuration using the same syntax found in the
%% configuration file (i.e., /usr/local/etc/cloudi/cloudi.conf).
%% @end
%%-------------------------------------------------------------------------

-spec services_add(L :: list(#internal{} | #external{}),
                   Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

services_add([_ | _] = L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services_add(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove service instances.===
%% Provide the Service UUIDs for the services that should be stopped.
%% The Service UUID is shown in the output of services/1. When the
%% service is stopped, its running instance is removed from CloudI, but
%% does not impact any other running instances (even if they are the same
%% service module or binary).
%% @end
%%-------------------------------------------------------------------------

-spec services_remove(L :: list(<<_:128>>),
                      Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

services_remove([_ | _] = L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services_remove(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart service instances.===
%% Provide the Service UUIDs for the services that should be restarted.
%% The Service UUID is shown in the output of services/1. When the service
%% is restarted, the old instance is stopped and a new instance is started.
%% During the restart delay, it is possible to lose queued service
%% requests and received asynchronous responses. Keeping the state
%% separate between the service instances is important to prevent failures
%% within the new instance.
%% @end
%%-------------------------------------------------------------------------

-spec services_restart(L :: list(<<_:128>>), 
                       Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

services_restart([_ | _] = L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services_restart(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List all service instances with each service's UUID.===
%% @end
%%-------------------------------------------------------------------------

-spec services(Timeout :: pos_integer()) ->
    {ok, binary()} |
    {error, any()}.

services(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add CloudI nodes.===
%% Explicitly add a CloudI node name, so that services between all other
%% CloudI nodes and the added nodes can send each other service requests.
%% @end
%%-------------------------------------------------------------------------

-spec nodes_add(L :: list(node()),
                Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

nodes_add(L, Timeout)
    when length(L) >= 1,
         is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:nodes_add(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Explicitly remove CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_remove(L :: list(node()),
                   Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

nodes_remove(L, Timeout)
    when length(L) >= 1,
         is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:nodes_remove(L, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List all the CloudI nodes known to be connected.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_alive(Timeout :: pos_integer()) ->
    {ok, list(node())} |
    {error, any()}.

nodes_alive(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:alive(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List all the CloudI nodes that are disconnected but expected to reconnect.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes_dead(Timeout :: pos_integer()) ->
    {ok, list(node())} |
    {error, any()}.

nodes_dead(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:dead(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List both the connected and disconnected CloudI nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec nodes(Timeout :: pos_integer()) ->
    {ok, list(node())} |
    {error, any()}.

nodes(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:nodes(Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Modify the current loglevel.===
%% CloudI uses asynchronous logging with flow control (backpressure
%% handling) to prevent misbehaving services from causing instability.
%% @end
%%-------------------------------------------------------------------------

-spec loglevel_set(Level :: loglevel(),
                   Timeout :: pos_integer()) ->
    ok.

loglevel_set(Level, Timeout)
    when is_atom(Level),
         is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_logger:change_loglevel(Level).

%%-------------------------------------------------------------------------
%% @doc
%% ===Redirect the log output.===
%% Redirect all local log output to a remote CloudI node.
%% Use 'undefined' as the node name to log locally.
%% @end
%%-------------------------------------------------------------------------

-spec log_redirect(Node :: undefined | node(),
                   Timeout :: pos_integer()) ->
    ok.

log_redirect(Node, Timeout)
    when is_atom(Node),
         is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:logger_redirect(Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a directory to the CloudI Erlang VM code server's search paths.===
%% The path is always appended to the list of search paths (you should not
%% need to rely on search path order because of unique naming).
%% @end
%%-------------------------------------------------------------------------

-spec code_path_add(Dir :: file:filename(),
                    Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

code_path_add(Dir, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    case code:add_pathz(Dir) of
        true ->
            ok;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a directory from the CloudI Erlang VM code server's search paths.===
%% This doesn't impact any running services, only services that will be
%% started in the future. 
%% @end
%%-------------------------------------------------------------------------

-spec code_path_remove(Dir :: file:filename(),
                       Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

code_path_remove(Dir, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    case code:del_path(Dir) of
        true ->
            ok;
        false ->
            {error, does_not_exist};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===List all the CloudI Erlang VM code server search paths.===
%% The order is the same order the directories are searched.
%% @end
%%-------------------------------------------------------------------------

-spec code_path(Timeout :: pos_integer()) ->
    {ok, list(file:filename())}.

code_path(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    {ok, code:get_path()}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

