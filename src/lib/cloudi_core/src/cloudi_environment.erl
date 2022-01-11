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
%%% Copyright (c) 2014-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
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
-include("cloudi_logger.hrl").
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
    cloudi_x_trie:new(lists:map(fun(Entry) ->
        cloudi_string:splitl($=, Entry, input)
    end, os:getenv())).

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
    FileCompiler = [_ | _] = code:where_is_file("compiler.app"),
    FileCloudI = [_ | _] = code:where_is_file("cloudi_core.app"),
    ApplicationVersions0 = application:loaded_applications(),
    {value, {kernel, _, RuntimeErlangKernelVersion},
     ApplicationVersions1} = lists:keytake(kernel, 1, ApplicationVersions0),
    {value, {stdlib, _, RuntimeErlangStdlibVersion},
     ApplicationVersions2} = lists:keytake(stdlib, 1, ApplicationVersions1),
    {value, {compiler, _, RuntimeErlangCompilerVersion},
     ApplicationVersions3} = lists:keytake(compiler, 1, ApplicationVersions2),
    {value, {cloudi_core, _, RuntimeCloudIVersion},
     _} = lists:keytake(cloudi_core, 1, ApplicationVersions3),
    RuntimeErlangCompilation = erlang_compilation(),
    RuntimeMachineProcessors = erlang:system_info(schedulers),
    ?CODE_STATUS_STATIC ++
    [{build_erlang_erts_c_compiler_version, erts_c_compiler_version()},
     {install_erlang_erts_time, status_file_time(FileErts)},
     {install_erlang_kernel_time, status_file_time(FileKernel)},
     {install_erlang_stdlib_time, status_file_time(FileStdlib)},
     {install_erlang_compiler_time, status_file_time(FileCompiler)},
     {install_cloudi_time, status_file_time(FileCloudI)},
     {runtime_erlang_erts_version, RuntimeErtsVersion},
     {runtime_erlang_kernel_version, RuntimeErlangKernelVersion},
     {runtime_erlang_stdlib_version, RuntimeErlangStdlibVersion},
     {runtime_erlang_compiler_version, RuntimeErlangCompilerVersion},
     {runtime_erlang_compilation, RuntimeErlangCompilation},
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
    transform(String, lookup()).

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
    transform(String, [], Lookup).

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

-ifdef(ERLANG_OTP_VERSION_24_FEATURES).
erlang_compilation() ->
    case erlang:system_info(emu_flavor) of
        jit ->
            "jit";
        _ ->
            "aot"
    end.
-else.
erlang_compilation() ->
    "aot".
-endif.

% transform a string using a lookup containing environment variables
% (the loop doesn't have error conditions by design)
transform([], Output, _) ->
    lists:reverse(Output);

transform([$\\, $$ | String], Output, Lookup) ->
    transform(String, [$$ | Output], Lookup);

transform([$$ | String], Output, Lookup) ->
    case String of
        [${ | Rest] ->
            transform_delimiter(Rest, Output, [], Lookup);
        _ ->
            transform_bare(String, Output, [], Lookup)
    end;

transform([C | String], Output, Lookup) ->
    transform(String, [C | Output], Lookup).

transform_delimiter([], Output, _, _) ->
    lists:reverse(Output);

transform_delimiter([$} | String], Output, Key, Lookup) ->
    transform_value(Key, String, Output, Lookup);

transform_delimiter([$= | String], Output, _, Lookup) ->
    transform(String, Output, Lookup);

transform_delimiter([C | String], Output, Key, Lookup) ->
    transform_delimiter(String, Output, [C | Key], Lookup).

transform_bare([] = String, Output, Key, Lookup) ->
    transform_value(Key, String, Output, Lookup);

transform_bare([C | String], Output, _, Lookup)
    when C == $}; C == $= ->
    transform(String, Output, Lookup);

transform_bare([C | String], Output, Key, Lookup)
    when C /= $$, C /= $/, C /= $ , C /= $', C /= $", C /= $` ->
    transform_bare(String, Output, [C | Key], Lookup);

transform_bare(String, Output, Key, Lookup) ->
    transform_value(Key, String, Output, Lookup).

transform_value([], String, Output, Lookup) ->
    transform(String, Output, Lookup);

transform_value(Key, String, Output, Lookup) ->
    case cloudi_x_trie:find(lists:reverse(Key), Lookup) of
        {ok, Value} ->
            transform(String, lists:reverse(Value, Output), Lookup);
        error ->
            transform(String, Output, Lookup)
    end.

