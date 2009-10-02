%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi API==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009 Michael Truog
%%% @version 0.0.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_api).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([add_job/1, remove_job/1,
         add_machine/1, remove_machine/1,
         add_data/1, remove_data/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Add work to the cloud dynamically.===
%% The data repositories the work expects to be present should be
%% present before adding the work.
%% @end
%%-------------------------------------------------------------------------

-spec add_job(ConfigurationString :: string()) -> 'ok' | {'error', any()}.

add_job(ConfigurationString) when is_list(ConfigurationString) ->
    cloud_leader:add_job(ConfigurationString).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove work from the cloud dynamically.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_job(WorkTitle :: string()) -> 'ok' | 'error'.

remove_job(WorkTitle) when is_list(WorkTitle) ->
    cloud_leader:remove_job(WorkTitle).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a machine to the cloud dynamically.===
%% @end
%%-------------------------------------------------------------------------

-spec add_machine(ConfigurationString :: string()) -> 'ok' | {'error', any()}.

add_machine(ConfigurationString) when is_list(ConfigurationString) ->
    cloud_leader:add_machine(ConfigurationString).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a machine from the cloud dynamically.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_machine(HostName :: string()) -> 'ok' | {'error', any()}.

remove_machine(HostName) when is_list(HostName) ->
    cloud_leader:remove_machine(HostName).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a data repository to the cloud dynamically.===
%% @end
%%-------------------------------------------------------------------------

-spec add_data(ConfigurationString :: string()) -> 'ok' | {'error', any()}.

add_data(ConfigurationString) when is_list(ConfigurationString) ->
    cloud_data_repository_sup:add_data(ConfigurationString).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a data repository from the cloud dynamically.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_data(DataTitle :: string()) -> 'ok' | {'error', any()}.

remove_data(DataTitle) when is_list(DataTitle) ->
    cloud_data_repository_sup:remove_data(DataTitle).

