%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2014, Michael Truog <mjtruog at gmail dot com>
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
%%%------------------------------------------------------------------------

% defaults from cloudi_core_i_configuration_defaults.hrl are set below
% (so the same defaults used in the proplist format are provided).
% records are provided without defaults in cloudi_core_i_configuration.erl
% to satisfy dialyzer.

% internal service parameters
-record(internal,
    {
        prefix = "/"
            :: cloudi:service_name_pattern(),
        module
            :: atom() | file:filename(),
        args = []
            :: list(),
        dest_refresh = immediate_closest
            :: cloudi_service_api:dest_refresh(),
        timeout_init = 5000
            :: cloudi_service_api:timeout_milliseconds(),
        timeout_async = 5000
            :: cloudi_service_api:timeout_milliseconds(),
        timeout_sync = 5000
            :: cloudi_service_api:timeout_milliseconds(),
        dest_list_deny = undefined
            :: cloudi_service_api:dest_list(),
        dest_list_allow = undefined
            :: cloudi_service_api:dest_list(),
        count_process = 1
            :: pos_integer() | float(),
        max_r = 5
            :: non_neg_integer(),
        max_t = 300
            :: cloudi_service_api:seconds(),
        options = []
            :: cloudi_service_api:service_options_internal()
    }).
    
% external service parameters
-record(external,
    {
        prefix = "/"
            :: cloudi:service_name_pattern(),
        file_path
            :: file:filename(),
        args = ""
            :: string(),
        env = []
            :: list({string(), string()}),
        dest_refresh = immediate_closest
            :: cloudi_service_api:dest_refresh(),
        protocol = default
            :: 'default' | 'local' | 'tcp' | 'udp',
        buffer_size = default
            :: 'default' | pos_integer(),
        timeout_init = 5000
            :: cloudi_service_api:timeout_milliseconds(),
        timeout_async = 5000
            :: cloudi_service_api:timeout_milliseconds(),
        timeout_sync = 5000
            :: cloudi_service_api:timeout_milliseconds(),
        dest_list_deny = undefined
            :: cloudi_service_api:dest_list(),
        dest_list_allow = undefined
            :: cloudi_service_api:dest_list(),
        count_process = 1
            :: pos_integer() | float(),
        count_thread = 1
            :: pos_integer() | float(),
        max_r = 5
            :: non_neg_integer(),
        max_t = 300
            :: cloudi_service_api:seconds(),
        options = []
            :: cloudi_service_api:service_options_external()
    }).

