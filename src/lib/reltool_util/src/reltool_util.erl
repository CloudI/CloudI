%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==reltool Utility Functions==
%%% All the functions here are probably considered unorthodox, but
%%% are useful for runtime usage of applications and releases.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(reltool_util).
-author('mjtruog [at] gmail (dot) com').

-export([application_start/1,
         application_start/2,
         application_start/3,
         application_stop/1,
         application_running/1,
         application_running/2,
         application_loaded/1,
         script_start/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually.===
%% @end
%%-------------------------------------------------------------------------

-spec application_start(Application :: atom()) ->
    ok |
    {error, any()}.

application_start(Application)
    when is_atom(Application) ->
    application_start(Application, [], 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually with a specific configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec application_start(Application :: atom(),
                        Env :: list({atom(), any()})) ->
    ok |
    {error, any()}.

application_start(Application, Env)
    when is_atom(Application), is_list(Env) ->
    application_start(Application, Env, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually with a specific configuration and timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec application_start(Application :: atom(),
                        Env :: list({atom(), any()}),
                        Timeout :: pos_integer() | infinity) ->
    ok |
    {error, any()}.

application_start(Application, Env, Timeout)
    when is_atom(Application), is_list(Env) ->
    case ensure_application_loaded(Application) of
        ok ->
            case application_start_set_env(Env, Application, Timeout) of
                ok ->
                    case applications_dependencies(Application) of
                        {ok, As} ->
                            case application_start_dependencies(As) of
                                ok ->
                                    ensure_application_started(Application);
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop an application and its dependencies.===
%% @end
%%-------------------------------------------------------------------------

-spec application_stop(Application :: atom()) ->
    ok |
    {error, any()}.

application_stop(Application)
    when is_atom(Application) ->
    case applications_dependencies(Application) of
        {ok, StopAs0} ->
            case ensure_application_stopped(Application) of
                ok ->
                    StopAs1 = delete_all(kernel, StopAs0),
                    StopAs2 = delete_all(stdlib, StopAs1),
                    Apps = application:loaded_applications(),
                    {value, _, OtherApps0} = lists:keytake(Application,
                                                           1, Apps),
                    case application_stop_external(StopAs2, OtherApps0) of
                        {ok, OtherAppsN} ->
                            RequiredAs = application_stop_ignore(OtherAppsN),
                            StopAsN = lists:reverse(lists:foldl(fun(A, As) ->
                                delete_all(A, As)
                            end, StopAs2, RequiredAs)),
                            application_stop_dependencies(StopAsN);
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if an application is currently running.===
%% @end
%%-------------------------------------------------------------------------

-spec application_running(Application :: atom()) ->
    {ok, {atom(), string()}} |
    {error, any()}.

application_running(Application)
    when is_atom(Application) ->
    application_running(Application, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if an application is currently running with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec application_running(Application :: atom(),
                          Timeout :: pos_integer() | infinity) ->
    {ok, {atom(), string()}} |
    {error, any()}.

application_running(Application, Timeout)
    when is_atom(Application) ->
    try application:which_applications(Timeout) of
        Apps ->
            case lists:keyfind(Application, 1, Apps) of
                {Application, _, VSN} ->
                    {ok, {Application, VSN}};
                false ->
                    {error, {not_found, Application}}
            end
    catch exit:{timeout, _} ->
        {error, application_controller_timeout}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if an application is currently loaded.===
%% @end
%%-------------------------------------------------------------------------

-spec application_loaded(Application :: atom()) ->
    {ok, {atom(), string()}} |
    {error, any()}.

application_loaded(Application)
    when is_atom(Application) ->
    Apps = application:loaded_applications(),
    case lists:keyfind(Application, 1, Apps) of
        {Application, _, VSN} ->
            {ok, {Application, VSN}};
        false ->
            {error, {not_found, Application}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Start everything specified within a script file.===
%% A script file is the input used when creating a boot file, which is the
%% file used when first starting the Erlang VM.  This function checks
%% all applications to determine if they are already running with the
%% expected versions.  All modules are checked to make sure they have
%% been loaded, if they are expected to have been loaded. Normally,
%% the script is only used in the binary boot file format and only a single
%% boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The script file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

-spec script_start(FilePath :: string()) ->
    {ok, atom()} |
    {error, any()}.

script_start(FilePath)
    when is_list(FilePath) ->
    true = lists:suffix(".script", FilePath),
    % system name and version are ignored
    {ok, [{script, {_Name, _Vsn}, Instructions}]} = file:consult(FilePath),
    Dir = filename:dirname(FilePath),
    % expects the typical directory structure produced by reltool
    DirNames = filename:split(Dir),
    case erlang:length(DirNames) of
        DirNamesLength when DirNamesLength > 2 ->
            Root = lists:sublist(DirNames, DirNamesLength - 2),
            case filelib:is_dir(filename:join(Root ++ ["lib"])) of
                true ->
                    script_instructions(Instructions, Root);
                false ->
                    {error, invalid_release_structure}
            end;
        _ ->
            {error, invalid_release_directory}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

ensure_application_loaded(A) ->
    case application:load(A) of
         ok ->
             ok;
        {error, {already_loaded, A}} ->
             ok;
        {error, _} = Error ->
            Error
     end.

ensure_application_started(A) ->
    case application:start(A, temporary) of
        ok ->
            ok;
        {error, {already_started, A}} ->
            ok;
        {error, _} = Error ->
            Error
    end.

ensure_application_stopped(A) ->
    case application:stop(A) of
        ok ->
            ok;
        {error, {not_started, A}} ->
            ok;
        {error, _} = Error ->
            Error
    end.

application_start_set_env([], _, _) ->
    ok;
application_start_set_env([{K, V} | L], Application, Timeout) ->
    try application:set_env(Application, K, V, Timeout) of
        ok ->
            application_start_set_env(L, Application, Timeout)
    catch
        exit:{timeout, _} ->
            {error, application_controller_timeout}
    end;
application_start_set_env(_, _, _) ->
    {error, invalid_application_env}.

application_start_dependencies([]) ->
    ok;
application_start_dependencies([A | As]) ->
    case ensure_application_started(A) of
        ok ->
            application_start_dependencies(As);
        {error, _} = Error ->
            Error
    end.

application_stop_external([], Apps) ->
    {ok, Apps};
application_stop_external([A | As], Apps) ->
    case lists:keytake(A, 1, Apps) of
        false ->
            {error, {not_started, A}};
        {value, _, NextApps} ->
            application_stop_external(As, NextApps)
    end.

application_stop_ignore(L) ->
    application_stop_ignore(sets:new(), L).

application_stop_ignore(Required, []) ->
    sets:to_list(Required);
application_stop_ignore(Required, [{A, _, _} | L]) ->
    case applications_dependencies(A) of
        {ok, As} ->
            application_stop_ignore(
                sets:union(Required, sets:from_list(As)), L);
        {error, _} = Error ->
            Error
    end.

application_stop_dependencies([]) ->
    ok;
application_stop_dependencies([A | As]) ->
    case ensure_application_stopped(A) of
        ok ->
            application_stop_dependencies(As);
        {error, _} = Error ->
            Error
    end.

applications_dependencies(A) ->
    case application:get_key(A, applications) of
        undefined ->
            {error, {undefined_dependencies, A}};
        {ok, As} ->
            applications_dependencies(As, As)
    end.

applications_dependencies([], As) ->
    {ok, As};
applications_dependencies([A | Rest], As) ->
    case ensure_application_loaded(A) of
        ok ->
            case application:get_key(A, applications) of
                undefined ->
                    {error, {undefined_dependencies, A}};
                {ok, []} ->
                    applications_dependencies(Rest, As);
                {ok, NextAs} ->
                    case applications_dependencies(NextAs, NextAs ++ As) of
                        {ok, NewAs} ->
                            applications_dependencies(Rest, NewAs);
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

script_instructions(L, Root) ->
    Apps = application:loaded_applications(),
    script_instructions(L, preload, undefined, Root, Apps).

script_instructions([], started, Application, _, _) ->
    {ok, Application};
script_instructions([{progress, Progress} | L],
                    _, Application, Root, Apps) ->
    script_instructions(L, Progress, Application, Root, Apps);
script_instructions([{preLoaded, _} | L],
                    preload, Application, Root, Apps) ->
    script_instructions(L, preload, Application, Root, Apps);
script_instructions([{kernel_load_completed} | L],
                    preloaded, Application, Root, Apps) ->
    script_instructions(L, kernel_load_completed, Application, Root, Apps);
script_instructions([{path, Paths} | L],
                    preloaded, Application, Root, Apps) ->
    case ensure_code_paths(Paths, Apps) of
        ok ->
            script_instructions(L, preloaded, Application, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_instructions([{primLoad, Modules} | L],
                    preloaded, Application, Root, Apps) ->
    Loaded = lists:all(fun(M) ->
        is_module_loaded(M) =:= true
    end, Modules),
    if
        Loaded ->
            script_instructions(L, preloaded, Application, Root, Apps);
        true ->
            {error, modules_not_preloaded}
    end;
script_instructions([{kernel_load_completed} | L],
                    kernel_load_completed, Application, Root, Apps) ->
    script_instructions(L, kernel_load_completed, Application, Root, Apps);
script_instructions([{primLoad, Modules} | L],
                    kernel_load_completed, Application, Root, Apps) ->
    case ensure_all_loaded(Modules) of
        ok ->
            script_instructions(L, kernel_load_completed,
                                Application, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_instructions([{path, Paths} | L],
                    kernel_load_completed, Application, Root, Apps) ->
    case load_all_paths(Paths, Root) of
        ok ->
            script_instructions(L, kernel_load_completed,
                                Application, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_instructions([{path, _} | L],
                    modules_loaded, Application, Root, Apps) ->
    script_instructions(L, modules_loaded, Application, Root, Apps);
script_instructions([{kernelProcess, _, _} | L],
                    modules_loaded, Application, Root, Apps) ->
    script_instructions(L, modules_loaded, Application, Root, Apps);
script_instructions([{apply, {application, load, [AppDescr]}} | L],
                    init_kernel_started, Application, Root, Apps) ->
    {application, A, [_ | _] = AppSpecKeys} = AppDescr,
    case lists:keyfind(A, 1, Apps) of
        {A, _, VSN} ->
            {vsn, RequestedVSN} = lists:keyfind(vsn, 1, AppSpecKeys),
            if
                VSN == RequestedVSN ->
                    script_instructions(L, init_kernel_started,
                                        Application, Root, Apps);
                true ->
                    {error, {version_mismatch, A, RequestedVSN, VSN}}
            end;
        false ->
            case application:load(AppDescr) of
                ok ->
                    script_instructions(L, init_kernel_started,
                                        Application, Root, Apps);
                {error, _} = Error ->
                    Error
            end
    end;
script_instructions([{apply, {application, start_boot, [A, permanent]}} | L],
                    applications_loaded, Application, Root, Apps)
    when A =:= kernel; A =:= stdlib ->
    % if this code is being used, kernel and stdlib should have already
    % been started with the boot file that was used to start the Erlang VM
    script_instructions(L, applications_loaded, Application, Root, Apps);
script_instructions([{apply, {application, start_boot, [A, permanent]}} | L],
                    applications_loaded, _, Root, Apps) ->
    case ensure_application_started(A) of
        ok ->
            script_instructions(L, applications_loaded, A, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_instructions([{apply, {c, erlangrc, []}} | L],
                    applications_loaded, Application, Root, Apps) ->
    script_instructions(L, applications_loaded, Application, Root, Apps).

ensure_all_loaded([]) ->
    ok;
ensure_all_loaded([Module | Modules]) ->
    case is_module_loaded(Module) of
        true ->
            Loaded = lists:all(fun(M) ->
                is_module_loaded(M) =:= true
            end, Modules),
            if
                Loaded ->
                    ok;
                true ->
                    {error, modules_partially_loaded}
            end;
        false ->
            NotLoaded = lists:all(fun(M) ->
                is_module_loaded(M) =:= false
            end, Modules),
            if
                NotLoaded ->
                    load_all_modules([Module | Modules]);
                true ->
                    {error, modules_partially_loaded}
            end
    end.

ensure_code_paths([], _) ->
    ok;
ensure_code_paths([P | Paths], Apps) ->
    ["$ROOT", "lib", NameVSN, "ebin"] = filename:split(P),
    {Name, VSN} = split_name_vsn(NameVSN),
    Application = erlang:list_to_existing_atom(Name),
    case lists:keyfind(Application, 1, Apps) of
        {Application, _, VSN} ->
            ensure_code_paths(Paths, Apps);
        {Application, _, InvalidVSN} ->
            {error, {version_mismatch, Application, VSN, InvalidVSN}};
        false ->
            {error, {not_loaded, Application, VSN}}
    end.

is_module_loaded(Module) when is_atom(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            true;
        false ->
            false
    end.

load_all_modules([]) ->
    ok;
load_all_modules([Module | Modules]) ->
    case code:load_file(Module) of
        {module, Module} ->
            load_all_modules(Modules);
        {error, Reason} ->
            {error, {Reason, Module}}
    end.

load_all_paths([], _) ->
    ok;
load_all_paths([P | Paths], Root) ->
    ["$ROOT", "lib", NameVSN, "ebin"] = filename:split(P),
    CodePath = filename:join(Root ++ ["lib", NameVSN, "ebin"]),
    case code:add_pathz(CodePath) of
        true ->
            load_all_paths(Paths, Root);
        {error, Reason} ->
            {error, {Reason, CodePath}}
    end.

split_name_vsn(NameVSN) ->
    split_name_vsn([], [], NameVSN).
split_name_vsn([_ | _] = Name, VSN, []) ->
    [$- | FinalVSN] = lists:reverse(VSN),
    {Name, FinalVSN};
split_name_vsn(Name, NameSegment, [$- | L]) ->
    split_name_vsn(lists:reverse(NameSegment) ++ Name, [$-], L);
split_name_vsn(Name, VSN, [C | L]) ->
    split_name_vsn(Name, [C | VSN], L).

delete_all(Elem, List) when is_list(List) ->
    delete_all(Elem, [], List).
delete_all(Elem, L, [Elem | T]) ->
    delete_all(Elem, L, T);
delete_all(Elem, L, [H | T]) ->
    delete_all(Elem, [H | L], T);
delete_all(_, L, []) ->
    lists:reverse(L).
