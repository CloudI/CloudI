%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Runtime Environment==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2014-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_environment).
-author('mjtruog at protonmail dot com').

%% external interface
-export([lookup/0,
         status/0,
         transform/1,
         transform/2]).

-type lookup() :: cloudi_x_trie:cloudi_x_trie().
-export_type([lookup/0]).

-include("cloudi_environment.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get an environment variable lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec lookup() ->
    lookup().

lookup() ->
    cloudi_core_i_spawn:environment_lookup().

%%-------------------------------------------------------------------------
%% @doc
%% ===Get execution environment status.===
%% @end
%%-------------------------------------------------------------------------

-spec status() ->
    nonempty_list({atom(), string()}).

status() ->
    ApplicationVersions0 = application:loaded_applications(),
    {value, {kernel, _, RuntimeErlangKernelVersion},
     ApplicationVersions1} = lists:keytake(kernel, 1, ApplicationVersions0),
    {value, {stdlib, _, RuntimeErlangStdlibVersion},
     ApplicationVersions2} = lists:keytake(stdlib, 1, ApplicationVersions1),
    {value, {sasl, _, RuntimeErlangSaslVersion},
     ApplicationVersions3} = lists:keytake(sasl, 1, ApplicationVersions2),
    {value, {compiler, _, RuntimeErlangCompilerVersion},
     ApplicationVersions4} = lists:keytake(compiler, 1, ApplicationVersions3),
    {value, {cloudi_core, _, RuntimeCloudIVersion},
     _} = lists:keytake(cloudi_core, 1, ApplicationVersions4),
    RuntimeMachineProcessors = erlang:system_info(schedulers),
    status_static() ++
    [{runtime_erlang_erts_version, erlang:system_info(version)},
     {runtime_erlang_kernel_version, RuntimeErlangKernelVersion},
     {runtime_erlang_stdlib_version, RuntimeErlangStdlibVersion},
     {runtime_erlang_sasl_version, RuntimeErlangSaslVersion},
     {runtime_erlang_compiler_version, RuntimeErlangCompilerVersion},
     {runtime_cloudi_version, RuntimeCloudIVersion},
     {runtime_machine_processors, RuntimeMachineProcessors}].

%%-------------------------------------------------------------------------
%% @doc
%% ===Transform a string, substituting environment variable values from the lookup.===
%% Use ${VARIABLE} or $VARIABLE syntax, where VARIABLE is a name with
%% [a-zA-Z0-9_] ASCII characters.
%% @end
%%-------------------------------------------------------------------------

-spec transform(String :: string()) ->
    string().

transform(String) ->
    Lookup = lookup(),
    cloudi_core_i_spawn:environment_transform(String, Lookup).

%%-------------------------------------------------------------------------
%% @doc
%% ===Transform a string, substituting environment variable values from the lookup.===
%% Use ${VARIABLE} or $VARIABLE syntax, where VARIABLE is a name with
%% [a-zA-Z0-9_] ASCII characters.
%% @end
%%-------------------------------------------------------------------------

-spec transform(String :: string(),
                Lookup :: lookup()) ->
    string().

transform(String, Lookup) ->
    cloudi_core_i_spawn:environment_transform(String, Lookup).

