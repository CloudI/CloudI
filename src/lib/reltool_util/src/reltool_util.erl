%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
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
%%% @version 0.5.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(reltool_util).
-author('mjtruog [at] gmail (dot) com').

-export([application_start/1,
         application_start/2,
         application_start/3,
         application_stop/1,
         application_remove/1,
         application_remove/2,
         application_purged/1,
         application_purged/2,
         application_running/1,
         application_running/2,
         application_loaded/1,
         ensure_application_loaded/1,
         ensure_application_started/1,
         ensure_application_stopped/1,
         module_loaded/1,
         module_purged/1,
         module_purged/2,
         script_start/1,
         script_remove/1,
         script_remove/2]).

-compile({no_auto_import, [{module_loaded, 1}]}).

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
%% Only stop dependencies that are not required for other applications.
%% @end
%%-------------------------------------------------------------------------

-spec application_stop(Application :: atom()) ->
    ok |
    {error, any()}.

application_stop(Application) ->
    case application_stop_dependencies(Application) of
        {ok, _} ->
            ok;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop and purge the modules of an application and all of its dependencies.===
%% Only application dependencies that are not required for other
%% applications are removed.
%% @end
%%-------------------------------------------------------------------------

-spec application_remove(Application :: atom()) ->
    ok |
    {error, any()}.

application_remove(Application) ->
    application_remove(Application, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop and purge the modules of an application and all of its dependencies with a timeout.===
%% Only application dependencies that are not required for other
%% applications are removed.
%% @end
%%-------------------------------------------------------------------------

-spec application_remove(Application :: atom(),
                         Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

application_remove(Application, Timeout)
    when is_atom(Application), is_integer(Timeout), Timeout > 0 ->
    case application_stop_dependencies(Application) of
        {ok, Applications} ->
            TimeoutSlice = erlang:round(
                0.5 + Timeout / erlang:length(Applications)),
            applications_purged(Applications, TimeoutSlice);
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Purge a loaded application's modules and unload the application.===
%% The application is stopped if it is running, but its dependencies are
%% ignored.
%% @end
%%-------------------------------------------------------------------------

-spec application_purged(Application :: atom()) ->
    ok |
    {error, any()}.

application_purged(Application) ->
    application_purged(Application, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Purge a loaded application's modules and unload the application with a specific timeout.===
%% The application is stopped if it is running, but its dependencies are
%% ignored.
%% @end
%%-------------------------------------------------------------------------

-spec application_purged(Application :: atom(),
                         Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

application_purged(Application, Timeout)
    when is_atom(Application) ->
    case application_loaded(Application) of
        {ok, _} ->
            case ensure_application_stopped(Application) of
                ok ->
                    case application:get_key(Application, modules) of
                        {ok, Modules} ->
                            case modules_purged(Modules, Timeout) of
                                ok ->
                                    application:unload(Application);
                                {error, _} = Error ->
                                    Error
                            end;
                        undefined ->
                            {error, {modules_missing, Application}}
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
%% ===Make sure an application is loaded.===
%% @end
%%-------------------------------------------------------------------------

-spec ensure_application_loaded(Application :: atom()) ->
    ok |
    {error, any()}.

ensure_application_loaded(Application) ->
    case application:load(Application) of
        ok ->
            case application:get_key(Application, modules) of
                {ok, Modules} ->
                    lists:foreach(fun(M) ->
                        ok = module_loaded(M)
                    end, Modules);
                undefined ->
                    ok
            end;
        {error, {already_loaded, Application}} ->
            ok;
        {error, _} = Error ->
            Error
     end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Make sure an application is started.===
%% @end
%%-------------------------------------------------------------------------

-spec ensure_application_started(Application :: atom()) ->
    ok |
    {error, any()}.

ensure_application_started(Application) ->
    case application:start(Application, temporary) of
        ok ->
            ok;
        {error, {already_started, Application}} ->
            ok;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Make sure an application is stopped.===
%% @end
%%-------------------------------------------------------------------------

-spec ensure_application_stopped(Application :: atom()) ->
    ok |
    {error, any()}.

ensure_application_stopped(Application) ->
    case application:stop(Application) of
        ok ->
            ok;
        {error, {not_started, Application}} ->
            ok;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Make sure a module is loaded.===
%% If the module is not loaded, attempt to load it.
%% @end
%%-------------------------------------------------------------------------

-spec module_loaded(Module :: atom()) ->
    ok |
    {error, any()}.

module_loaded(Module)
    when is_atom(Module) ->
    case is_module_loaded(Module) of
        false ->
            case code:load_file(Module) of
                {module, Module} ->
                    ok;
                {error, _} = Error ->
                    Error
            end;
        true ->
            ok
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Make sure a module is purged.===
%% If the module is not loaded, ignore it.
%% @end
%%-------------------------------------------------------------------------

-spec module_purged(Module :: atom()) ->
    ok |
    {error, any()}.

module_purged(Module) ->
    module_purged(Module, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Make sure a module is purged with a timeout.===
%% If the module is not loaded, ignore it.
%% @end
%%-------------------------------------------------------------------------

-spec module_purged(Module :: atom(),
                    Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

module_purged(Module, Timeout)
    when is_atom(Module) ->
    modules_purged([Module], Timeout).

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
    {ok, list(atom())} |
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
                    % on success, return the last application to be started
                    % (should be the main application since all the application
                    %  dependencies are started first)
                    script_start_instructions(Instructions, Root);
                false ->
                    {error, invalid_release_structure}
            end;
        _ ->
            {error, invalid_release_directory}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop everything specified within a script file.===
%% A script file is the input used when creating a boot file, which is the
%% file used when first starting the Erlang VM.  This function checks
%% all applications to determine applications which can be safely removed
%% (assuming the application dependencies are correct).  The applications
%% will then be stopped and their modules will be purged.  Normally,
%% the script is only used in the binary boot file format and only a single
%% boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The script file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

-spec script_remove(FilePath :: string()) ->
    ok |
    {error, any()}.

script_remove(FilePath) ->
    script_remove(FilePath, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop everything specified within a script file with a timeout.===
%% A script file is the input used when creating a boot file, which is the
%% file used when first starting the Erlang VM.  This function checks
%% all applications to determine applications which can be safely removed
%% (assuming the application dependencies are correct).  The applications
%% will then be stopped and their modules will be purged.  Normally,
%% the script is only used in the binary boot file format and only a single
%% boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The script file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

-spec script_remove(FilePath :: string(),
                    Timeout :: pos_integer()) ->
    ok |
    {error, any()}.

script_remove(FilePath, Timeout)
    when is_list(FilePath), is_integer(Timeout), Timeout > 0 ->
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
                    case script_remove_instructions(Instructions) of
                        {ok, Applications} ->
                            TimeoutSlice = erlang:round(
                                0.5 + Timeout / erlang:length(Applications)),
                            applications_remove(Applications, TimeoutSlice);
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, invalid_release_structure}
            end;
        _ ->
            {error, invalid_release_directory}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

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

application_stop_all([]) ->
    ok;
application_stop_all([A | As]) ->
    case ensure_application_stopped(A) of
        ok ->
            application_stop_all(As);
        {error, _} = Error ->
            Error
    end.

application_stop_dependencies(Application)
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
                    % determine applications which are not dependencies
                    case application_stop_external(StopAs2, OtherApps0) of
                        {ok, OtherAppsN} ->
                            % check to see the required applications
                            % separate from the application dependencies
                            RequiredAs = application_stop_ignore(OtherAppsN),
                            % ignore all applications that are requirements
                            % of other applications
                            StopAsN = lists:reverse(lists:foldl(fun(A, As) ->
                                delete_all(A, As)
                            end, StopAs2, RequiredAs)),
                            % stop all the application dependencies
                            % that are no longer required
                            case application_stop_all(StopAsN) of
                                ok ->
                                    {ok, [Application | StopAsN]};
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

applications_remove([], _) ->
    ok;
applications_remove([Application | Applications], Timeout) ->
    case application_remove(Application, Timeout) of
        ok ->
            applications_remove(Applications, Timeout);
        {error, _} = Error ->
            Error
    end.

applications_purged([], _) ->
    ok;
applications_purged([Application | Applications], Timeout) ->
    case application_purged(Application, Timeout) of
        ok ->
            applications_purged(Applications, Timeout);
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

applications_top_level(Applications) ->
    true = erlang:length(lists:usort(Applications)) ==
           erlang:length(Applications), % no duplicates
    case applications_top_level(Applications, sets:new()) of
        {ok, Dependencies} ->
            TopLevelApplications = lists:foldl(fun(A, L) ->
                lists:delete(A, L)
            end, Applications, sets:to_list(Dependencies)),
            {ok, TopLevelApplications};
        {error, _} = Error ->
            Error
    end.

applications_top_level([], Dependencies) ->
    {ok, Dependencies};
applications_top_level([Application | Applications], Dependencies) ->
    case applications_dependencies(Application) of
        {ok, As} ->
            applications_top_level(Applications,
                                   sets:union(sets:from_list(As),
                                              Dependencies));
        {error, _} = Error ->
            Error
    end.

script_start_instructions(L, Root) ->
    Apps = application:loaded_applications(),
    script_start_instructions(L, preload, [], Root, Apps).

script_start_instructions([], started, Applications, _, _) ->
    applications_top_level(Applications);
script_start_instructions([{progress, Progress} | L],
                          _, Applications, Root, Apps) ->
    script_start_instructions(L, Progress, Applications, Root, Apps);
script_start_instructions([{preLoaded, _} | L],
                          preload, Applications, Root, Apps) ->
    script_start_instructions(L, preload, Applications, Root, Apps);
script_start_instructions([{kernel_load_completed} | L],
                          preloaded, Applications, Root, Apps) ->
    script_start_instructions(L, kernel_load_completed,
                              Applications, Root, Apps);
script_start_instructions([{path, Paths} | L],
                          preloaded, Applications, Root, Apps) ->
    case ensure_code_paths(Paths, Apps) of
        ok ->
            script_start_instructions(L, preloaded, Applications, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_start_instructions([{primLoad, Modules} | L],
                          preloaded, Applications, Root, Apps) ->
    Loaded = lists:all(fun(M) ->
        is_module_loaded(M) =:= true
    end, Modules),
    if
        Loaded ->
            script_start_instructions(L, preloaded, Applications, Root, Apps);
        true ->
            {error, modules_not_preloaded}
    end;
script_start_instructions([{kernel_load_completed} | L],
                          kernel_load_completed, Applications, Root, Apps) ->
    script_start_instructions(L, kernel_load_completed,
                              Applications, Root, Apps);
script_start_instructions([{primLoad, Modules} | L],
                          kernel_load_completed, Applications, Root, Apps) ->
    case ensure_all_loaded(Modules) of
        ok ->
            script_start_instructions(L, kernel_load_completed,
                                      Applications, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_start_instructions([{path, Paths} | L],
                          kernel_load_completed, Applications, Root, Apps) ->
    case load_all_paths(Paths, Root) of
        ok ->
            script_start_instructions(L, kernel_load_completed,
                                      Applications, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_start_instructions([{path, _} | L],
                          modules_loaded, Applications, Root, Apps) ->
    script_start_instructions(L, modules_loaded, Applications, Root, Apps);
script_start_instructions([{kernelProcess, _, _} | L],
                          modules_loaded, Applications, Root, Apps) ->
    script_start_instructions(L, modules_loaded, Applications, Root, Apps);
script_start_instructions([{apply, {application, load, [AppDescr]}} | L],
                          init_kernel_started, Applications, Root, Apps) ->
    {application, A, [_ | _] = AppSpecKeys} = AppDescr,
    case lists:keyfind(A, 1, Apps) of
        {A, _, VSN} ->
            {vsn, RequestedVSN} = lists:keyfind(vsn, 1, AppSpecKeys),
            if
                VSN == RequestedVSN ->
                    script_start_instructions(L, init_kernel_started,
                                              [A | Applications], Root, Apps);
                true ->
                    {error, {version_mismatch, A, RequestedVSN, VSN}}
            end;
        false ->
            case application:load(AppDescr) of
                ok ->
                    script_start_instructions(L, init_kernel_started,
                                              [A | Applications], Root, Apps);
                {error, _} = Error ->
                    Error
            end
    end;
script_start_instructions([{apply,
                            {application, start_boot, [A | _]}} | L],
                          applications_loaded, Applications, Root, Apps)
    when A =:= kernel; A =:= stdlib ->
    % if this code is being used, kernel and stdlib should have already
    % been started with the boot file that was used to start the Erlang VM
    script_start_instructions(L, applications_loaded, Applications, Root, Apps);
script_start_instructions([{apply,
                            {application, start_boot, [A | _]}} | L],
                          applications_loaded, Applications, Root, Apps) ->
    case ensure_application_started(A) of
        ok ->
            script_start_instructions(L, applications_loaded,
                                      Applications, Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_start_instructions([{apply, {c, erlangrc, _}} | L],
                          applications_loaded, Applications, Root, Apps) ->
    script_start_instructions(L, applications_loaded,
                              Applications, Root, Apps).

script_remove_instructions(L) ->
    script_remove_instructions(L, preload, []).

script_remove_instructions([], started, Applications) ->
    applications_top_level(Applications);
script_remove_instructions([{progress, Progress} | L], _, Applications) ->
    script_remove_instructions(L, Progress, Applications);
script_remove_instructions([{preLoaded, _} | L], preload, Applications) ->
    script_remove_instructions(L, preload, Applications);
script_remove_instructions([{kernel_load_completed} | L],
                           preloaded, Applications) ->
    script_remove_instructions(L, kernel_load_completed, Applications);
script_remove_instructions([{path, _} | L],
                           preloaded, Applications) ->
    script_remove_instructions(L, preloaded, Applications);
script_remove_instructions([{primLoad, _} | L],
                           preloaded, Applications) ->
    script_remove_instructions(L, preloaded, Applications);
script_remove_instructions([{kernel_load_completed} | L],
                           kernel_load_completed, Applications) ->
    script_remove_instructions(L, kernel_load_completed, Applications);
script_remove_instructions([{primLoad, _} | L],
                           kernel_load_completed, Applications) ->
    script_remove_instructions(L, kernel_load_completed, Applications);
script_remove_instructions([{path, _} | L],
                           kernel_load_completed, Applications) ->
    script_remove_instructions(L, kernel_load_completed, Applications);
script_remove_instructions([{path, _} | L],
                           modules_loaded, Applications) ->
    script_remove_instructions(L, modules_loaded, Applications);
script_remove_instructions([{kernelProcess, _, _} | L],
                           modules_loaded, Applications) ->
    script_remove_instructions(L, modules_loaded, Applications);
script_remove_instructions([{apply, {application, load, [AppDescr]}} | L],
                           init_kernel_started, Applications) ->
    {application, A, [_ | _]} = AppDescr,
    script_remove_instructions(L, init_kernel_started, [A | Applications]);
script_remove_instructions([{apply, {application, start_boot, _}} | L],
                           applications_loaded, Applications) ->
    script_remove_instructions(L, applications_loaded, Applications);
script_remove_instructions([{apply, {c, erlangrc, _}} | L],
                           applications_loaded, Applications) ->
    script_remove_instructions(L, applications_loaded, Applications).

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

-define(MODULES_PURGED_DELTA, 100).
modules_purged(Modules, Timeout)
    when is_list(Modules), is_integer(Timeout), Timeout > 0 ->
    modules_purged(Modules, [], Timeout).

modules_purged([], [], _) ->
    ok;
modules_purged([], BusyModules, Timeout) ->
    case erlang:max(Timeout - ?MODULES_PURGED_DELTA, 0) of
        0 ->
            case lists:dropwhile(fun code:purge/1, BusyModules) of
                [] ->
                    ok;
                _ ->
                    {error, timeout}
            end;
        NextTimeout ->
            receive after ?MODULES_PURGED_DELTA -> ok end,
            modules_purged(lists:reverse(BusyModules), [], NextTimeout)
    end;
modules_purged([Module | Modules], BusyModules, Timeout) ->
    case is_module_loaded(Module) of
        true ->
            true = code:delete(Module),
            case code:soft_purge(Module) of
                true ->
                    modules_purged(Modules, BusyModules, Timeout);
                false ->
                    modules_purged(Modules, [Module | BusyModules], Timeout)
            end;
        false ->
            modules_purged(Modules, BusyModules, Timeout)
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
