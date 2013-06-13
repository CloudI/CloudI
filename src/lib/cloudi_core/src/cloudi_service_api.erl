%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
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
%%% @version 1.2.4 {@date} {@time}
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
-export_type([timeout/0]).

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

-spec acl_add(L :: list({atom(), list(acl())}), 
              Timeout :: pos_integer()) ->
    ok.

acl_add(L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:acl_add(L, Timeout).

-spec acl_remove(L :: list(atom()),
                 Timeout :: pos_integer()) ->
    ok.

acl_remove(L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:acl_remove(L, Timeout).

-spec services_add(L :: list(#internal{} | #external{}),
                   Timeout :: pos_integer()) ->
    ok.

services_add(L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services_add(L, Timeout).

-spec services_remove(L :: list(<<_:128>>),
                      Timeout :: pos_integer()) ->
    ok.

services_remove(L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services_remove(L, Timeout).

-spec services_restart(L :: list(<<_:128>>), 
                       Timeout :: pos_integer()) ->
    ok.

services_restart(L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services_restart(L, Timeout).

-spec services(Timeout :: pos_integer()) ->
    ok.

services(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:services(Timeout).

-spec nodes_add(L :: list(atom()),
                Timeout :: pos_integer()) ->
    ok.

nodes_add(L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:nodes_add(L, Timeout).

-spec nodes_remove(L :: list(atom()),
                   Timeout :: pos_integer()) ->
    ok.

nodes_remove(L, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_configurator:nodes_remove(L, Timeout).

-spec nodes_alive(Timeout :: pos_integer()) ->
    list(atom()).

nodes_alive(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:alive(Timeout).

-spec nodes_dead(Timeout :: pos_integer()) ->
    list(atom()).

nodes_dead(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:dead(Timeout).

-spec nodes(Timeout :: pos_integer()) ->
    list(atom()).

nodes(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:nodes(Timeout).

-spec loglevel_set(Level :: loglevel(),
                   Timeout :: pos_integer()) ->
    ok.

loglevel_set(Level, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_logger:change_loglevel(Level).

-spec log_redirect(Node :: atom(),
                   Timeout :: pos_integer()) ->
    ok.

log_redirect(Node, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    cloudi_nodes:logger_redirect(Node).

-spec code_path_add(Dir :: file:filename(),
                    Timeout :: pos_integer()) ->
    ok.

code_path_add(Dir, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    code:add_pathz(Dir).

-spec code_path_remove(Dir :: file:filename(),
                       Timeout :: pos_integer()) ->
    ok.

code_path_remove(Dir, Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    code:del_path(Dir).

-spec code_path(Timeout :: pos_integer()) ->
    list(file:filename()).

code_path(Timeout)
    when is_integer(Timeout), Timeout > ?TIMEOUT_DELTA ->
    code:get_path().

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

