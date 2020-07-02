%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==cgroups Manipulation Functions==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2016-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2016-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cgroups).
-author('mjtruog at protonmail dot com').

%% external interface
-export([create/4,
         delete/2,
         delete_recursive/2,
         destroy/1,
         new/0,
         new/1,
         shell/2,
         update/4,
         update_or_create/4]).

-record(cgroups,
    {
        version :: pos_integer(),
        path :: string(),
        mounted :: boolean()
    }).
-define(APPLICATION, cgroups).

-type options() :: list({version_default, pos_integer()} |
                        {version_default_required, boolean()} |
                        {path_v1, string()} |
                        {path_v2, string()} |
                        {path_mounts, string() | undefined}).
-export_type([options/0]).

% for features specific to Erlang/OTP version 20.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-define(ERLANG_OTP_VERSION_20_FEATURES, true).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a specific cgroup.===
%% Files cpuset.cpus and cpuset.mems are set if they are not initialized
%% due to cgroup.clone_children (using the root values).
%% @end
%%-------------------------------------------------------------------------

-spec create(CGroupPath :: nonempty_string(),
             OSPids :: list(pos_integer()),
             CGroupParameters :: list({string(), string()}),
             State :: #cgroups{}) ->
    ok |
    {error, any()}.

create([_ | _] = CGroupPath, OSPids, CGroupParameters,
       #cgroups{path = Path} = State) ->
    CGroupPathValid = cgroup_path_valid(CGroupPath),
    if
        CGroupPathValid =:= false ->
            {error, {invalid_cgroup_path, CGroupPath}};
        true ->
            CGroupPathFull = Path ++ CGroupPath,
            case filelib:is_dir(CGroupPathFull) of
                true ->
                    {error, {exists, CGroupPathFull}};
                false ->
                    create_cgroup(CGroupPath, OSPids, CGroupParameters, State)
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a specific cgroup.===
%% The cgroup must not contain any OS processes for this
%% function to succeed.
%% @end
%%-------------------------------------------------------------------------

-spec delete(CGroupPath :: nonempty_string(),
             State :: #cgroups{}) ->
    ok |
    {error, any()}.

delete([_ | _] = CGroupPath,
       #cgroups{path = Path}) ->
    CGroupPathValid = cgroup_path_valid(CGroupPath),
    if
        CGroupPathValid =:= false ->
            {error, {invalid_cgroup_path, CGroupPath}};
        true ->
            CGroupPathFull = Path ++ CGroupPath,
            case shell("rmdir \"~s\"", [CGroupPathFull]) of
                {0, _} ->
                    ok;
                {Status, Output} ->
                    {error, {rmdir, Status, Output}}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a specific cgroup and as many non-leaf cgroups as possible.===
%% The cgroup must not contain any OS processes for this
%% function to succeed.
%% @end
%%-------------------------------------------------------------------------

-spec delete_recursive(CGroupPath :: nonempty_string(),
                       State :: #cgroups{}) ->
    ok |
    {error, any()}.

delete_recursive([_ | _] = CGroupPath,
                 #cgroups{path = Path}) ->
    CGroupPathValid = cgroup_path_valid(CGroupPath),
    if
        CGroupPathValid =:= false ->
            {error, {invalid_cgroup_path, CGroupPath}};
        true ->
            CGroupPathFull = Path ++ CGroupPath,
            case shell("rmdir \"~s\"", [CGroupPathFull]) of
                {0, _} ->
                    _ = delete_recursive_subpath(subdirectory(CGroupPathFull),
                                                 Path),
                    ok;
                {Status, Output} ->
                    {error, {rmdir, Status, Output}}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Destroy cgroups state data.===
%% @end
%%-------------------------------------------------------------------------

-spec destroy(#cgroups{}) ->
    ok.

destroy(#cgroups{mounted = false}) ->
    ok;
destroy(#cgroups{path = Path,
                 mounted = true}) ->
    _ = shell("umount \"~s\"", [Path]),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create new cgroups state data.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    {ok, #cgroups{}} |
    {error, any()}.

new() ->
    new([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create new cgroups state data with local options.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Options0 :: options()) ->
    {ok, #cgroups{}} |
    {error, any()}.

new(Options0) ->
    {VersionDefault,
     Options1} = option(version_default, Options0),
    {VersionDefaultRequired,
     Options2} = option(version_default_required, Options1),
    {PathV1,
     Options3} = option(path_v1, Options2),
    {PathV2,
     Options4} = option(path_v2, Options3),
    {PathMounts,
     OptionsN} = option(path_mounts, Options4),
    [] = OptionsN,
    true = is_integer(VersionDefault) andalso (VersionDefault > 0),
    true = is_boolean(VersionDefaultRequired),
    true = is_list(PathV1) andalso
           ($/ == hd(lists:reverse(PathV1))) andalso (length(PathV1) > 1),
    true = is_list(PathV2) andalso
           ($/ == hd(lists:reverse(PathV2))) andalso (length(PathV2) > 1),
    true = (PathMounts =:= undefined) orelse
           (is_list(PathMounts) andalso is_integer(hd(PathMounts))),
    new_state(VersionDefault, VersionDefaultRequired,
              PathV1, PathV2, PathMounts).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a command with the default shell.===
%% @end
%%-------------------------------------------------------------------------

-spec shell(Command :: string(),
            Arguments :: list()) ->
    {non_neg_integer(), list(binary())}.

shell(Command, Arguments) ->
    Shell = erlang:open_port({spawn_executable, "/bin/sh"},
                             [{args, ["-"]}, {cd, "/"},
                              stream, binary, stderr_to_stdout, exit_status]),
    Exec = io_lib:format(Command, Arguments),
    true = erlang:port_command(Shell, [Exec, "\nexit $?\n"]),
    shell_output(Shell, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a cgroup path.===
%% May be used on the cgroup root path.
%% @end
%%-------------------------------------------------------------------------

-spec update(CGroupPath :: string(),
             OSPids :: list(pos_integer()),
             CGroupParameters :: list({string(), string()}),
             State :: #cgroups{}) ->
    ok |
    {error, any()}.

update(CGroupPath, OSPids, CGroupParameters,
       #cgroups{version = Version,
                path = Path}) ->
    CGroupPathValid = cgroup_path_valid(CGroupPath),
    OSPidsValid = lists:all(fun(OSPid) ->
        is_integer(OSPid) andalso (OSPid > 0)
    end, OSPids),
    CGroupParametersValid = lists:all(fun(CGroupParameter) ->
        case CGroupParameter of
            {[_ | _] = SubsystemParameter, Value} when is_list(Value) ->
                quoteless(SubsystemParameter) andalso quoteless(Value);
            _ ->
                false
        end
    end, CGroupParameters),
    if
        CGroupPathValid =:= false ->
            {error, {invalid_cgroup_path, CGroupPath}};
        OSPidsValid =:= false ->
            {error, {invalid_os_pids, OSPids}};
        CGroupParametersValid =:= false ->
            {error, {invalid_cgroup_parameters, CGroupParameters}};
        true ->
            CGroupPathFull = Path ++ CGroupPath,
            case update_parameters(CGroupParameters, Version,
                                   CGroupPathFull, Path) of
                ok ->
                    update_pids(OSPids, Version, CGroupPathFull);
                {error, _} = Error ->
                    Error
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update or create a specific cgroup.===
%% @end
%%-------------------------------------------------------------------------

-spec update_or_create(CGroupPath :: nonempty_string(),
                       OSPids :: list(pos_integer()),
                       CGroupParameters :: list({string(), string()}),
                       State :: #cgroups{}) ->
    ok |
    {error, any()}.

update_or_create([_ | _] = CGroupPath, OSPids, CGroupParameters,
                 #cgroups{path = Path} = State) ->
    CGroupPathValid = cgroup_path_valid(CGroupPath),
    if
        CGroupPathValid =:= false ->
            {error, {invalid_cgroup_path, CGroupPath}};
        true ->
            CGroupPathFull = Path ++ CGroupPath,
            case filelib:is_dir(CGroupPathFull) of
                true ->
                    update(CGroupPath, OSPids, CGroupParameters, State);
                false ->
                    create_cgroup(CGroupPath, OSPids, CGroupParameters, State)
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

new_state(1 = Version, VersionDefaultRequired,
          PathV1, PathV2, PathMounts) ->
    case new_paths(PathV1, PathV2, PathMounts) of
        {ok, {MountedV1, NewPathV1}, {MountedV2, NewPathV2}} ->
            case new_state_init(Version, MountedV1, NewPathV1) of
                {ok, _} = Success ->
                    Success;
                {error, _} = Error when VersionDefaultRequired =:= false ->
                    case new_state_init(2, MountedV2, NewPathV2) of
                        {ok, _} = Success ->
                            Success;
                        {error, _} ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
new_state(2 = Version, VersionDefaultRequired,
          PathV1, PathV2, PathMounts) ->
    case new_paths(PathV1, PathV2, PathMounts) of
        {ok, {MountedV1, NewPathV1}, {MountedV2, NewPathV2}} ->
            case new_state_init(Version, MountedV2, NewPathV2) of
                {ok, _} = Success ->
                    Success;
                {error, _} = Error when VersionDefaultRequired =:= false ->
                    case new_state_init(1, MountedV1, NewPathV1) of
                        {ok, _} = Success ->
                            Success;
                        {error, _} ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
new_state(Version, _, _, _, _) ->
    {error, {version_default, Version}}.

new_state_init(Version, true, Path) ->
    {ok, #cgroups{version = Version,
                  path = Path,
                  mounted = false}};
new_state_init(1 = Version, false, Path) ->
    case shell("mount -t cgroup none \"~s\"", [Path]) of
        {0, _} ->
            {ok, #cgroups{version = Version,
                          path = Path,
                          mounted = true}};
        {Status, Output} ->
            {error, {path_v1, Status, Output}}
    end;
new_state_init(2 = Version, false, Path) ->
    case shell("mount -t cgroup2 none \"~s\"", [Path]) of
        {0, _} ->
            {ok, #cgroups{version = Version,
                          path = Path,
                          mounted = true}};
        {Status, Output} ->
            {error, {path_v2, Status, Output}}
    end.

new_paths(PathV1, PathV2, undefined) ->
    {ok, {false, PathV1}, {false, PathV2}};
new_paths(PathV1, PathV2, PathMounts) ->
    case shell("cat \"~s\"", [PathMounts]) of
        {0, Mounts} ->
            MountsL = split(shell_output_string(Mounts), "\n"),
            {MountsPathV1,
             MountsPathV2} = new_mounts(MountsL),
            ResultV1 = if
                MountsPathV1 =:= undefined ->
                    {false, PathV1};
                is_list(MountsPathV1) ->
                    {true, MountsPathV1}
            end,
            ResultV2 = if
                MountsPathV2 =:= undefined ->
                    {false, PathV2};
                is_list(MountsPathV2) ->
                    {true, MountsPathV2}
            end,
            {ok, ResultV1, ResultV2};
        {Status, Output} ->
            {error, {path_mounts, Status, Output}}
    end.

new_mounts(MountsL) ->
    new_mounts(MountsL, undefined, undefined).

new_mounts([], PathV1, PathV2) ->
    {PathV1, PathV2};
new_mounts([Mount | MountsL], PathV1, PathV2) ->
    case split(Mount, " ") of
        [_, NewPathV1, "cgroup" | _] ->
            new_mounts(MountsL, NewPathV1 ++ "/", PathV2);
        [_, NewPathV2, "cgroup2" | _] ->
            new_mounts(MountsL, PathV1, NewPathV2 ++ "/");
        _ ->
            new_mounts(MountsL, PathV1, PathV2)
    end.

create_cgroup(CGroupPath, OSPids, CGroupParameters,
              #cgroups{path = Path} = State) ->
    CGroupPathFull = Path ++ CGroupPath,
    case shell("mkdir -p \"~s\"", [CGroupPathFull]) of
        {0, _} ->
            case create_update(CGroupPathFull, Path) of
                ok ->
                    update(CGroupPath, OSPids, CGroupParameters, State);
                {error, _} = Error ->
                    Error
            end;
        {Status, Output} ->
            {error, {mkdir, Status, Output}}
    end.

create_update(CGroupPathFull, Path) ->
    case create_update_get(CGroupPathFull, "cpuset.cpus", Path) of
        {ok, CPUS} ->
            case create_update_get(CGroupPathFull, "cpuset.mems", Path) of
                {ok, MEMS} ->
                    if
                        CPUS =:= undefined,
                        MEMS =:= undefined ->
                            ok;
                        true ->
                            create_update_set(CGroupPathFull, CPUS, MEMS, Path)
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

create_update_set(CGroupPathFull, CPUS, MEMS, Path) ->
    case create_update_set_subpath(CGroupPathFull, CPUS, MEMS, Path) of
        ok ->
            CGroupSubPathFull = CGroupPathFull ++ "/",
            case create_update_set_value(CGroupSubPathFull,
                                         "cpuset.cpus", CPUS) of
                ok ->
                    case create_update_set_value(CGroupSubPathFull,
                                                 "cpuset.mems", MEMS) of
                        ok ->
                            ok;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

create_update_set_subpath(CGroupPathFull, CPUS, MEMS, Path) ->
    case subdirectory(CGroupPathFull) of
        Path ->
            ok;
        CGroupSubPathFull ->
            case create_update_set_subpath(CGroupSubPathFull,
                                           CPUS, MEMS, Path) of
                ok ->
                    case create_update_set_value(CGroupSubPathFull,
                                                 "cpuset.cpus", CPUS) of
                        ok ->
                            case create_update_set_value(CGroupSubPathFull,
                                                         "cpuset.mems", MEMS) of
                                ok ->
                                    ok;
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

create_update_set_value(_, _, undefined) ->
    ok;
create_update_set_value(CGroupSubPathFull, SubsystemParameter, Value) ->
    case shell("cat \"~s~s\"", [CGroupSubPathFull, SubsystemParameter]) of
        {0, OldValue} ->
            NeedsUpdate = strip(shell_output_string(OldValue)) == "",
            if
                NeedsUpdate =:= true ->
                    case shell("echo \"~s\" > \"~s~s\"",
                               [Value, CGroupSubPathFull,
                                SubsystemParameter]) of
                        {0, _} ->
                            ok;
                        {Status, Output} ->
                            ErrorReason = subsystem_parameter_error_reason(
                                SubsystemParameter, "_set"),
                            {error, {ErrorReason, Status, Output}}
                    end;
                NeedsUpdate =:= false ->
                    ok
            end;
        {Status, Output} ->
            ErrorReason = subsystem_parameter_error_reason(SubsystemParameter,
                                                           "_check"),
            {error, {ErrorReason, Status, Output}}
    end.

create_update_get(CGroupPathFull, SubsystemParameter, Path) ->
    case shell("cat \"~s/~s\"", [CGroupPathFull, SubsystemParameter]) of
        {0, Contents} ->
            NeedsUpdate = strip(shell_output_string(Contents)) == "",
            if
                NeedsUpdate =:= true ->
                    subsystem_parameter_subpath(subdirectory(CGroupPathFull),
                                                SubsystemParameter, Path);
                NeedsUpdate =:= false ->
                    {ok, undefined}
            end;
        {Status, Output} ->
            ErrorReason = subsystem_parameter_error_reason(SubsystemParameter,
                                                           "_get"),
            {error, {ErrorReason, Status, Output}}
    end.

delete_recursive_subpath(Path, Path) ->
    ok;
delete_recursive_subpath(CGroupSubPathFull, Path) ->
    case shell("rmdir \"~s\"", [CGroupSubPathFull]) of
        {0, _} ->
            delete_recursive_subpath(subdirectory(CGroupSubPathFull), Path);
        {_, _} ->
            ok
    end.

update_parameters(CGroupParameters, 1, CGroupPathFull, _) ->
    update_parameters(CGroupParameters, CGroupPathFull);
update_parameters(CGroupParameters, 2, CGroupPathFull, Path) ->
    Controllers = lists:usort([subsystem(SubsystemParameter)
                               || {SubsystemParameter, _} <- CGroupParameters]),
    ControlAdded = ["+" ++ Controller || Controller <- Controllers],
    ControlRemoved = ["-" ++ Controller || Controller <- Controllers],
    CGroupSubPathFull = subdirectory(CGroupPathFull),
    case subtree_control_add(CGroupSubPathFull,
                             lists:join($ , ControlAdded),
                             Path) of
        ok ->
            Result = update_parameters(CGroupParameters, CGroupPathFull),
            case subtree_control_remove(CGroupSubPathFull,
                                        lists:join($ , ControlRemoved),
                                        Path) of
                ok ->
                    Result;
                {error, _} = Error ->
                    if
                        Result =:= ok ->
                            Error;
                        true ->
                            Result
                    end
            end;
        {error, _} = Error ->
            Error
    end.

update_parameters([], _) ->
    ok;
update_parameters([{SubsystemParameter, Value} | CGroupParameters],
                  CGroupPathFull) ->
    case shell("echo \"~s\" > \"~s/~s\"",
               [Value, CGroupPathFull, SubsystemParameter]) of
        {0, _} ->
            update_parameters(CGroupParameters, CGroupPathFull);
        {Status, Output} ->
            {error, {subsystem_parameter, Status, Output}}
    end.

update_pids(OSPids, _, CGroupPathFull) ->
    update_pids(OSPids, CGroupPathFull).

update_pids([], _) ->
    ok;
update_pids([OSPid | OSPids], CGroupPathFull) ->
    case shell("echo \"~w\" > \"~s/cgroup.procs\"",
               [OSPid, CGroupPathFull]) of
        {0, _} ->
            update_pids(OSPids, CGroupPathFull);
        {Status, Output} ->
            {error, {procs, Status, Output}}
    end.

subsystem([_ | _] = SubsystemParameter) ->
    {Subsystem, _} = lists:splitwith(fun(C) -> C /= $. end, SubsystemParameter),
    Subsystem.

subsystem_parameter_subpath(Path, SubsystemParameter, Path) ->
    subsystem_parameter_get(Path, SubsystemParameter);
subsystem_parameter_subpath(CGroupSubPathFull, SubsystemParameter, Path) ->
    case subsystem_parameter_get(CGroupSubPathFull, SubsystemParameter) of
        {ok, ""} ->
            subsystem_parameter_subpath(subdirectory(CGroupSubPathFull),
                                        SubsystemParameter, Path);
        {ok, _} = Success ->
            Success;
        {error, _} = Error ->
            Error
    end.

subsystem_parameter_get(CGroupSubPathFull, SubsystemParameter) ->
    case shell("cat \"~s~s\"", [CGroupSubPathFull, SubsystemParameter]) of
        {0, Contents} ->
            {ok, strip(shell_output_string(Contents))};
        {Status, Output} ->
            ErrorReason = subsystem_parameter_error_reason(SubsystemParameter,
                                                           "_get"),
            {error, {ErrorReason, Status, Output}}
    end.

subsystem_parameter_error_reason(SubsystemParameter, Suffix) ->
    erlang:list_to_atom(lists:map(fun(C) ->
        if
            C == $. ->
                $_;
            true ->
                C
        end
    end, SubsystemParameter) ++ Suffix).

subtree_control_add(Path, Value, Path) ->
    subtree_control_set(Path, Value);
subtree_control_add(CGroupSubPathFull, Value, Path) ->
    case subtree_control_add(subdirectory(CGroupSubPathFull),
                             Value, Path) of
        ok ->
            subtree_control_set(CGroupSubPathFull, Value);
        {error, _} = Error ->
            Error
    end.

subtree_control_remove(Path, Value, Path) ->
    subtree_control_set(Path, Value);
subtree_control_remove(CGroupSubPathFull, Value, Path) ->
    case subtree_control_set(CGroupSubPathFull, Value) of
        ok ->
            subtree_control_remove(subdirectory(CGroupSubPathFull),
                                   Value, Path);
        {error, _} = Error ->
            Error
    end.

subtree_control_set(CGroupSubPathFull, Value) ->
    case shell("echo \"~s\" > \"~scgroup.subtree_control\"",
               [Value, CGroupSubPathFull]) of
        {0, _} ->
            ok;
        {Status, Output} ->
            {error, {subtree_control, Status, Output}}
    end.

subdirectory(Path) ->
    case lists:reverse(Path) of
        [$/ | PathRev] ->
            filename:dirname(lists:reverse(PathRev)) ++ "/";
        _ ->
            filename:dirname(Path) ++ "/"
    end.

strip(Value) ->
    lists:filter(fun(C) ->
        not (C == $  orelse C == $\t orelse C == $\n orelse C == $\r)
    end, Value).

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
split(String, SearchPattern) ->
    string:split(String, SearchPattern, all).
-else.
split(String, SearchPattern)
    when is_list(String) ->
    [erlang:binary_to_list(S)
     || S <- split(erlang:list_to_binary(String), SearchPattern)];
split(String, SearchPattern)
    when is_binary(String) ->
    Pattern = if
        is_binary(SearchPattern) ->
            [SearchPattern];
        is_integer(hd(SearchPattern)) ->
            [erlang:list_to_binary(SearchPattern)];
        is_list(SearchPattern) ->
            [erlang:iolist_to_binary(S) || S <- SearchPattern]
    end,
    binary:split(String, Pattern, [global]).
-endif.

cgroup_path_valid([]) ->
    true;
cgroup_path_valid([_ | _] = CGroupPath) ->
    ($/ /= hd(CGroupPath)) andalso
    ($/ /= hd(lists:reverse(CGroupPath))) andalso
    quoteless(CGroupPath).

quoteless(String) ->
    lists:all(fun(C) ->
         C /= $"
    end, String).

option(Key, Options) ->
    case lists:keytake(Key, 1, Options) of
        false ->
            {ok, Value} = application:get_env(?APPLICATION, Key),
            {Value, Options};
        {value, {Key, Value}, NewOptions} ->
            {Value, NewOptions}
    end.

shell_output(Shell, Output) ->
    receive
        {Shell, {data, Data}} ->
            shell_output(Shell, [Data | Output]);
        {Shell, {exit_status, Status}} ->
            {Status, lists:reverse(Output)}
    end.

shell_output_string(IOList) ->
    erlang:binary_to_list(erlang:iolist_to_binary(IOList)).

