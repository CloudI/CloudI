%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2022 Michael Truog <mjtruog at protonmail dot com>
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
            :: atom() | cloudi_service_api:file_path(),
        args = []
            :: cloudi_service_api:args_internal(),
        dest_refresh = immediate_closest
            :: cloudi_service_api:dest_refresh(),
        timeout_init = 5000
            :: cloudi_service_api:timeout_initialize_period(),
        timeout_async = 5000
            :: cloudi_service_api:timeout_send_async_period(),
        timeout_sync = 5000
            :: cloudi_service_api:timeout_send_sync_period(),
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
            :: cloudi_service_api:file_path(),
        args = ""
            :: cloudi_service_api:args_external(),
        env = []
            :: cloudi_service_api:env_external(),
        dest_refresh = immediate_closest
            :: cloudi_service_api:dest_refresh(),
        protocol = default
            :: 'default' | 'local' | 'tcp' | 'udp',
        buffer_size = default
            :: 'default' | pos_integer(),
        timeout_init = 5000
            :: cloudi_service_api:timeout_initialize_period(),
        timeout_async = 5000
            :: cloudi_service_api:timeout_send_async_period(),
        timeout_sync = 5000
            :: cloudi_service_api:timeout_send_sync_period(),
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

