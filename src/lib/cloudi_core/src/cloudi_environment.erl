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
%%% Copyright (c) 2014-2019 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2019 Michael Truog
%%% @version 1.8.0 {@date} {@time}
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
-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("kernel/include/file.hrl").

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
    RuntimeErtsVersion = erlang:system_info(version),
    FileErts = filename:join([code:root_dir(), "lib",
                              "erts-" ++ RuntimeErtsVersion,
                              "ebin", "erts.app"]),
    FileKernel = [_ | _] = code:where_is_file("kernel.app"),
    FileStdlib = [_ | _] = code:where_is_file("stdlib.app"),
    FileSasl = [_ | _] = code:where_is_file("sasl.app"),
    FileCompiler = [_ | _] = code:where_is_file("compiler.app"),
    FileCloudI = [_ | _] = code:where_is_file("cloudi_core.app"),
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
    [{build_erlang_erts_c_compiler_version, erts_c_compiler_version()},
     {install_erlang_erts_time, status_file_time(FileErts)},
     {install_erlang_kernel_time, status_file_time(FileKernel)},
     {install_erlang_stdlib_time, status_file_time(FileStdlib)},
     {install_erlang_sasl_time, status_file_time(FileSasl)},
     {install_erlang_compiler_time, status_file_time(FileCompiler)},
     {install_cloudi_time, status_file_time(FileCloudI)},
     {runtime_erlang_erts_version, RuntimeErtsVersion},
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

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

status_file_time(FilePath) ->
    case file:read_file_info(FilePath, [raw, {time, posix}]) of
        {ok, #file_info{mtime = MTime}} ->
            cloudi_timestamp:seconds_epoch_to_string(MTime);
        {error, Reason} ->
            ?LOG_ERROR("filesystem error (~ts): ~tp",
                       [FilePath, Reason]),
            ""
    end.

erts_c_compiler_version() ->
    case erlang:system_info(c_compiler_used) of
        {undefined, _} ->
            "";
        {Name, undefined}
        when is_atom(Name) ->
            erlang:atom_to_list(Name);
        {Name, V}
        when is_atom(Name), is_tuple(V) ->
            [_ | Version] = lists:flatten([[$., io_lib:format("~p", [I])]
                                           || I <- erlang:tuple_to_list(V)]),
            erlang:atom_to_list(Name) ++ [$  | Version];
        {Name, Version} ->
            cloudi_string:format("~tp ~tp", [Name, Version]);
        Unexpected ->
            ?LOG_ERROR("erlang:system_info(c_compiler_used) invalid: ~tp",
                       [Unexpected]),
            ""
    end.

