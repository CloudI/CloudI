%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==reltool Utility Functions==
%%% All the functions here are probably considered unorthodox, but
%%% are useful for runtime usage of applications and releases.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(reltool_util).
-author('mjtruog at protonmail dot com').

-export([application_env/1,
         application_start/1,
         application_start/2,
         application_start/3,
         applications_start/1,
         applications_start/2,
         application_stop/1,
         application_stop/2,
         application_remove/1,
         application_remove/2,
         application_remove/3,
         application_purged/1,
         application_purged/2,
         application_running/1,
         application_running/2,
         application_loaded/1,
         application_modules/1,
         application_modules/2,
         ensure_application_loaded/1,
         ensure_application_started/1,
         ensure_application_stopped/1,
         config_load/1,
         module_load/1,
         module_loaded/1,
         module_unload/1,
         module_reload/1,
         is_module_loaded/1,
         is_module_loaded/2,
         module_purged/1,
         module_purged/2,
         module_exports/1,
         module_behaviours/1,
         is_deprecated/3,
         module_version/1,
         script_start/1,
         script_remove/1,
         script_remove/2,
         script_remove/3,
         boot_start/1,
         boot_remove/1,
         boot_remove/2,
         boot_remove/3]).

-define(IS_MODULE_LOADED_DELTA, 100).
-define(MODULES_PURGED_DELTA, 100).

-compile({no_auto_import, [{module_loaded, 1}]}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the application's env settings.===
%% Only get the env settings from the application's .app file.
%% @end
%%-------------------------------------------------------------------------

-spec application_env(Application :: atom()) ->
    {ok, list({atom(), any()})} |
    {error, any()}.

application_env(Application)
    when is_atom(Application) ->
    case code:where_is_file(erlang:atom_to_list(Application) ++ ".app") of
        non_existing ->
            {error, not_found};
        Path ->
            case file:consult(Path) of
                {ok, [{application, Application, L}]} ->
                    case lists:keyfind(env, 1, L) of
                        {env, Env} ->
                            {ok, Env};
                        false ->
                            {ok, []}
                    end;
                {ok, _} ->
                    {error, invalid_file};
                {error, _} = Error ->
                    Error
            end
    end.

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
%% ===Start all the dependent applications manually.===
%% @end
%%-------------------------------------------------------------------------

-spec applications_start(Applications :: list(atom() | {atom(), list()})) ->
    ok |
    {error, any()}.

applications_start([_ | _] = Applications) ->
    applications_start(Applications, 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start all the dependent applications manually.===
%% @end
%%-------------------------------------------------------------------------

-spec applications_start(Applications :: list(atom() | {atom(), list()}),
                         Timeout :: pos_integer() | infinity) ->
    ok |
    {error, any()}.

applications_start([_ | _] = Applications, Timeout) ->
    applications_start_element(Applications, Timeout).

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
    application_stop(Application, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop an application and its dependencies with a list of applications to ignore.===
%% Only stop dependencies that are not required for other applications.
%% @end
%%-------------------------------------------------------------------------

-spec application_stop(Application :: atom(),
                       Ignore :: list(atom())) ->
    ok |
    {error, any()}.

application_stop(Application, Ignore)
    when is_atom(Application), is_list(Ignore) ->
    case application_stop_dependencies(Application, Ignore) of
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
    application_remove(Application, 5000, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop and purge the modules of an application and all of its dependencies with a timeout.===
%% Only application dependencies that are not required for other
%% applications are removed.
%% @end
%%-------------------------------------------------------------------------

-spec application_remove(Application :: atom(),
                         Timeout :: pos_integer() | infinity) ->
    ok |
    {error, any()}.

application_remove(Application, Timeout) ->
    application_remove(Application, Timeout, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop and purge the modules of an application and all of its dependencies with a timeout and a list of applications to ignore.===
%% Only application dependencies that are not required for other
%% applications are removed.
%% @end
%%-------------------------------------------------------------------------

-spec application_remove(Application :: atom(),
                         Timeout :: pos_integer() | infinity,
                         Ignore :: list(atom())) ->
    ok |
    {error, any()}.

application_remove(Application, infinity, Ignore)
    when is_atom(Application), is_list(Ignore) ->
    case application_stop_dependencies(Application, Ignore) of
        {ok, Applications} ->
            applications_purged(Applications, infinity);
        {error, _} = Error ->
            Error
    end;
application_remove(Application, Timeout, Ignore)
    when is_atom(Application), is_integer(Timeout), Timeout > 0,
         is_list(Ignore) ->
    case application_stop_dependencies(Application, Ignore) of
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
                         Timeout :: pos_integer() | infinity) ->
    ok |
    {error, any()}.

application_purged(Application, Timeout)
    when is_atom(Application) ->
    case application_loaded(Application) of
        {ok, _} ->
            case ensure_application_stopped(Application) of
                ok ->
                    case application_modules(Application) of
                        {ok, Modules} ->
                            case modules_purged(Modules, Timeout) of
                                ok ->
                                    application:unload(Application);
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
%% ===Retrieve a list of application modules.===
%% @end
%%-------------------------------------------------------------------------

-spec application_modules(Application :: atom()) ->
    {ok, list(atom())} |
    {error, any()}.

application_modules(Application) ->
    application_modules(Application, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Retrieve a list of application modules with filter options.===
%% Options can contain {behavior, ModuleName} to list all the modules
%% that use a specific behaviour (the information will not be present if
%% the beam file was stripped).
%% @end
%%-------------------------------------------------------------------------

-spec application_modules(Application :: atom(),
                          Options :: list({atom(), any()})) ->
    {ok, list(atom())} |
    {error, any()}.

application_modules(Application, Options)
    when is_atom(Application), is_list(Options) ->
    case application:get_key(Application, modules) of
        {ok, Modules} ->
            {ok, modules_filter(Options, Modules, false)};
        undefined ->
            {error, {modules_missing, Application}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Make sure an application is loaded.===
%% @end
%%-------------------------------------------------------------------------

-spec ensure_application_loaded(Application :: atom()) ->
    ok |
    {error, any()}.

ensure_application_loaded(kernel) ->
    ok;
ensure_application_loaded(stdlib) ->
    ok;
ensure_application_loaded(Application) ->
    Loaded = case application:load(Application) of
        ok ->
            ok;
        {error, {already_loaded, Application}} ->
            ok;
        {error, _} = Error ->
            Error
     end,
     if
        Loaded =:= ok ->
            case application:get_key(Application, modules) of
                {ok, Modules} ->
                    % make sure all modules are loaded, even if the
                    % application information is already loaded, since
                    % loading the application data does not automatically
                    % load its modules
                    lists:foreach(fun(M) ->
                        % valid results, others cause a crash
                        case module_loaded(M) of
                            ok ->
                                ok;
                            {error, nofile} ->
                                error_logger:warning_msg("broken "
                                                         "application ~p "
                                                         "missing ~p file~n",
                                                         [Application, M]);
                            {error, Reason} ->
                                error_logger:error_msg("application ~p load "
                                                       "error ~p on ~p file~n",
                                                       [Application,
                                                        Reason, M])
                        end
                    end, Modules);
                undefined ->
                    ok
            end;
        true ->
            Loaded
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
%% ===Load a config file from a release.===
%% All applications with configuration values are loaded if they are not
%% already loaded.
%% @end
%%-------------------------------------------------------------------------

-spec config_load(FilePath :: string()) ->
    {ok, list(atom())} |
    {error, any()}.

config_load(FilePath)
    when is_list(FilePath) ->
    true = lists:suffix(".config", FilePath),
    {ok, [ApplicationEnvs]} = file:consult(FilePath),
    config_load_application(ApplicationEnvs, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Load a module.===
%% @end
%%-------------------------------------------------------------------------

-spec module_load(Module :: atom()) ->
    ok |
    {error, any()}.

module_load(Module)
    when is_atom(Module) ->
    case code:load_file(Module) of
        {module, Module} ->
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
    case is_module_loaded_check(Module) of
        false ->
            module_load(Module);
        true ->
            ok
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Unload a module.===
%% @end
%%-------------------------------------------------------------------------

-spec module_unload(Module :: atom()) ->
    ok |
    {error, any()}.

module_unload(Module)
    when is_atom(Module) ->
    case code:delete(Module) of
        true ->
            ok;
        false ->
            case is_module_loaded_check(Module) of
                true ->
                    code:purge(Module),
                    case code:delete(Module) of
                        true ->
                            ok;
                        false ->
                            {error, not_unloaded}
                    end;
                false ->
                    {error, not_loaded}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Reload a module.===
%% @end
%%-------------------------------------------------------------------------

-spec module_reload(Module :: atom()) ->
    ok |
    {error, any()}.

module_reload(Module)
    when is_atom(Module) ->
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            code:soft_purge(Module),
            ok;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Wait to check if a module is loaded.===
%% @end
%%-------------------------------------------------------------------------

-spec is_module_loaded(Module :: atom()) ->
    ok |
    {error, any()}.

is_module_loaded(Module)
    when is_atom(Module) ->
    case is_module_loaded(Module, 5000) of
        {ok, _} ->
            ok;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Wait to check if a module is loaded.===
%% Return a new timeout value with the elapsed time subtracted.
%% @end
%%-------------------------------------------------------------------------

-spec is_module_loaded(Module :: atom(),
                       Timeout :: non_neg_integer()) ->
    {ok, non_neg_integer()} |
    {error, any()}.

is_module_loaded(Module, Timeout)
    when is_atom(Module), is_integer(Timeout), Timeout >= 0 ->
    case is_module_loaded_check(Module) of
        true ->
            {ok, Timeout};
        false ->
            case erlang:max(Timeout - ?IS_MODULE_LOADED_DELTA, 0) of
                0 ->
                    {error, timeout};
                NextTimeout ->
                    receive after ?IS_MODULE_LOADED_DELTA -> ok end,
                    is_module_loaded(Module, NextTimeout)
            end
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
                    Timeout :: non_neg_integer() | infinity) ->
    ok |
    {error, any()}.

module_purged(Module, Timeout)
    when is_atom(Module) ->
    modules_purged([Module], Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===List the exported functions of a module.===
%% @end
%%-------------------------------------------------------------------------

-spec module_exports(Module :: atom()) ->
    list({atom(), pos_integer()}).

module_exports(Module)
    when is_atom(Module) ->
    {exports, L0} = lists:keyfind(exports, 1, Module:module_info()),
    {value, _, L1} = lists:keytake(module_info, 1, L0),
    {value, _, L2} = lists:keytake(module_info, 1, L1),
    L2.

%%-------------------------------------------------------------------------
%% @doc
%% ===List the behaviours used by a module.===
%% The information will not be present if the beam file was stripped.
%% @end
%%-------------------------------------------------------------------------

-spec module_behaviours(Module :: module()) ->
    list(module()).

module_behaviours(Module)
    when is_atom(Module) ->
    Behaviours = lists:flatmap(fun(Attribute) ->
        case Attribute of
            {behaviour, L} ->
                L;
            {behavior, L} ->
                L;
            _ ->
                []
        end
    end, Module:module_info(attributes)),
    Behaviours.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if a module function is marked as deprecated.===
%% The value false will always be returned if the beam file was stripped.
%% @end
%%-------------------------------------------------------------------------

-spec is_deprecated(Module :: module(),
                    Function :: atom(),
                    Arity :: non_neg_integer()) ->
    boolean().

is_deprecated(Module, Function, Arity) ->
    Attributes = Module:module_info(attributes),
    lists:any(fun(Attribute) ->
        case Attribute of
            {deprecated, [_ | _] = Deprecated} ->
                lists:any(fun(Deprecate) ->
                    case Deprecate of
                        module ->
                            true;
                        {Function, FunctionArity}
                            when FunctionArity == Arity;
                                 FunctionArity == '_' ->
                            true;
                        {Function, FunctionArity, _}
                            when FunctionArity == Arity;
                                 FunctionArity == '_' ->
                            true;
                        _ ->
                            false
                    end
                end, Deprecated);
            _ ->
                false
        end
    end, Attributes).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the current version of the module.===
%% A list is returned with an entry for each use of the -vsn attribute
%% in the order within the module file for the currently loaded version
%% (the result is consistent with beam_lib:version/1).
%% The information will not be present if the beam file was stripped.
%% @end
%%-------------------------------------------------------------------------

-spec module_version(Module :: atom()) ->
    list(any()).

module_version(Module)
    when is_atom(Module) ->
    Version = lists:flatmap(fun(Attribute) ->
        case Attribute of
            {vsn, VSN} ->
                VSN;
            _ ->
                []
        end
    end, Module:module_info(attributes)),
    Version.

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
    case file:consult(FilePath) of
        {ok, [{script, {_Name, _Vsn}, _Instructions} = Script]} ->
            Dir = filename:dirname(FilePath),
            script_start_data(Script, Dir);
        {error, _} = Error ->
            Error
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
    script_remove(FilePath, 5000, []).

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
                    Timeout :: pos_integer() | infinity) ->
    ok |
    {error, any()}.

script_remove(FilePath, Timeout) ->
    script_remove(FilePath, Timeout, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop everything specified within a script file with a timeout and a list of applications to ignore.===
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
                    Timeout :: pos_integer() | infinity,
                    Ignore :: list(atom())) ->
    ok |
    {error, any()}.

script_remove(FilePath, Timeout, Ignore)
    when is_list(FilePath),
         ((is_integer(Timeout) andalso (Timeout > 0)) orelse
          (Timeout =:= infinity)), is_list(Ignore) ->
    true = lists:suffix(".script", FilePath),
    case file:consult(FilePath) of
        {ok, [{script, {_Name, _Vsn}, _Instructions} = Script]} ->
            Dir = filename:dirname(FilePath),
            script_remove_data(Script, Dir, Timeout, Ignore);
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Start everything specified within a boot file.===
%% A boot file is used when first starting the Erlang VM.  This function checks
%% all applications to determine if they are already running with the
%% expected versions.  All modules are checked to make sure they have
%% been loaded, if they are expected to have been loaded. Normally,
%% only a single boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The boot file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

-spec boot_start(FilePath :: string()) ->
    {ok, list(atom())} |
    {error, any()}.

boot_start(FilePath)
    when is_list(FilePath) ->
    true = lists:suffix(".boot", FilePath),
    case file:read_file(FilePath) of
        {ok, ScriptData} ->
            Dir = filename:dirname(FilePath),
            script_start_data(erlang:binary_to_term(ScriptData), Dir);
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop everything specified within a boot file.===
%% A boot file is used when first starting the Erlang VM.  This function checks
%% all applications to determine applications which can be safely removed
%% (assuming the application dependencies are correct).  The applications
%% will then be stopped and their modules will be purged.  Normally,
%% only a single boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The boot file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

-spec boot_remove(FilePath :: string()) ->
    ok |
    {error, any()}.

boot_remove(FilePath) ->
    boot_remove(FilePath, 5000, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop everything specified within a boot file with a timeout.===
%% A boot file is used when first starting the Erlang VM.  This function checks
%% all applications to determine applications which can be safely removed
%% (assuming the application dependencies are correct).  The applications
%% will then be stopped and their modules will be purged.  Normally,
%% only a single boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The boot file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

-spec boot_remove(FilePath :: string(),
                  Timeout :: pos_integer() | infinity) ->
    ok |
    {error, any()}.

boot_remove(FilePath, Timeout) ->
    boot_remove(FilePath, Timeout, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop everything specified within a boot file with a timeout and a list of applications to ignore.===
%% A boot file is used when first starting the Erlang VM.  This function checks
%% all applications to determine applications which can be safely removed
%% (assuming the application dependencies are correct).  The applications
%% will then be stopped and their modules will be purged.  Normally,
%% only a single boot file is used during the lifetime of the Erlang VM
%% (so it is unclear if using this function is bad or just unorthodox).
%% The boot file is expected to be within a release directory created
%% by reltool.
%% @end
%%-------------------------------------------------------------------------

-spec boot_remove(FilePath :: string(),
                  Timeout :: pos_integer() | infinity,
                  Ignore :: list(atom())) ->
    ok |
    {error, any()}.

boot_remove(FilePath, Timeout, Ignore)
    when is_list(FilePath),
         ((is_integer(Timeout) andalso (Timeout > 0)) orelse
          (Timeout =:= infinity)), is_list(Ignore) ->
    true = lists:suffix(".boot", FilePath),
    case file:read_file(FilePath) of
        {ok, ScriptData} ->
            Dir = filename:dirname(FilePath),
            script_remove_data(erlang:binary_to_term(ScriptData),
                               Dir, Timeout, Ignore);
        {error, _} = Error ->
            Error
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

applications_start_element([], _) ->
    ok;
applications_start_element([Application | Applications], Timeout)
    when is_atom(Application) ->
    case application_start(Application, [], Timeout) of
        ok ->
            applications_start_element(Applications, Timeout);
        {error, _} = Error ->
            Error
    end;
applications_start_element([{Application, Env} | Applications], Timeout)
    when is_atom(Application), is_list(Env) ->
    case application_start(Application, Env, Timeout) of
        ok ->
            applications_start_element(Applications, Timeout);
        {error, _} = Error ->
            Error
    end.

application_stop_dependencies_ignore([], L) ->
    L;
application_stop_dependencies_ignore([Application | Ignore], L) ->
    application_stop_dependencies_ignore(Ignore, delete_all(Application, L)).

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

application_stop_dependencies(Application, Ignore)
    when is_atom(Application) ->
    case applications_dependencies(Application) of
        {ok, StopAs0} ->
            case ensure_application_stopped(Application) of
                ok ->
                    StopAs1 = application_stop_dependencies_ignore([kernel,
                                                                    stdlib |
                                                                    Ignore],
                                                                   StopAs0),
                    Apps = application:loaded_applications(),
                    {value, _, OtherApps0} = lists:keytake(Application,
                                                           1, Apps),
                    % determine applications which are not dependencies
                    case application_stop_external(StopAs1, OtherApps0) of
                        {ok, OtherAppsN} ->
                            % check to see the required applications
                            % separate from the application dependencies
                            RequiredAs = application_stop_ignore(OtherAppsN),
                            % ignore all applications that are requirements
                            % of other applications
                            StopAsN = lists:reverse(lists:foldl(fun(A, As) ->
                                delete_all(A, As)
                            end, StopAs1, RequiredAs)),
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

applications_remove([], _, _) ->
    ok;
applications_remove([Application | Applications], Timeout, Ignore) ->
    case application_remove(Application, Timeout, Ignore) of
        ok ->
            applications_remove(Applications, Timeout, Ignore);
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
    Included = case application:get_key(A, included_applications) of
        undefined ->
            ok;
        {ok, LoadAs} ->
            applications_dependencies_load(LoadAs)
    end,
    if
        Included =:= ok ->
            case application:get_key(A, applications) of
                undefined ->
                    {error, {undefined_dependencies, A}};
                {ok, As} when A =:= common_test ->
                    % XXX avoid error in Erlang/OTP until ct gets fixed
                    % (necessary for Erlang =< R16B02)
                    AsCT = case lists:member(sasl, As) of
                        true ->
                            As;
                        false ->
                            [sasl | As]
                    end,
                    applications_dependencies(AsCT, AsCT);
                {ok, As} when A =:= elixir ->
                    % XXX avoid error in Elixir until its .app file gets fixed
                    % (necessary for Elixir =< 0.14.3)
                    AsElixir0 = case lists:member(crypto, As) of
                        true ->
                            As;
                        false ->
                            [crypto | As]
                    end,
                    AsElixir1 = case lists:member(compiler, As) of
                        true ->
                            AsElixir0;
                        false ->
                            [compiler | AsElixir0]
                    end,
                    AsElixirN = case lists:member(syntax_tools, As) of
                        true ->
                            AsElixir1;
                        false ->
                            [syntax_tools | AsElixir1]
                    end,
                    applications_dependencies(AsElixirN, AsElixirN);
                {ok, As} ->
                    applications_dependencies(As, As)
            end;
        true ->
            Included
    end.

applications_dependencies_load([]) ->
    ok;
applications_dependencies_load([A | Rest]) ->
    case ensure_application_loaded(A) of
        ok ->
            As1 = case application:get_key(A, included_applications) of
                undefined ->
                    [];
                {ok, As0} ->
                    As0
            end,
            LoadAs = case application:get_key(A, applications) of
                undefined ->
                    As1;
                {ok, As2} ->
                    As1 ++ As2
            end,
            case applications_dependencies_load(LoadAs) of
                ok ->
                    applications_dependencies_load(Rest);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

applications_dependencies([], As) ->
    {ok, As};
applications_dependencies([A | Rest], As) ->
    case ensure_application_loaded(A) of
        ok ->
            Included = case application:get_key(A, included_applications) of
                undefined ->
                    ok;
                {ok, LoadAs} ->
                    applications_dependencies_load(LoadAs)
            end,
            if
                Included =:= ok ->
                    case application:get_key(A, applications) of
                        undefined ->
                            {error, {undefined_dependencies, A}};
                        {ok, []} ->
                            applications_dependencies(Rest, As);
                        {ok, NextAs} ->
                            OtherAs = As -- NextAs,
                            case applications_dependencies(NextAs,
                                                           NextAs ++ OtherAs) of
                                {ok, NewAs} ->
                                    applications_dependencies(Rest, NewAs);
                                {error, _} = Error ->
                                    Error
                            end
                    end;
                true ->
                    Included
            end;
        {error, _} = Error ->
            Error
    end.

applications_top_level(Applications) ->
    true = erlang:length(lists:usort(Applications)) ==
           erlang:length(Applications), % no duplicates
    case applications_top_level(Applications, sets:new()) of
        {ok, Dependencies} ->
            TopLevelApplications = Applications -- sets:to_list(Dependencies),
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

script_start_data({script, {_Name, _Vsn}, Instructions}, Dir) ->
    % system name and version are ignored
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
        is_module_loaded_check(M) =:= true
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
            % order loaded applications based on the start order
            script_start_instructions(L, applications_loaded,
                                      [A | lists:delete(A, Applications)],
                                      Root, Apps);
        {error, _} = Error ->
            Error
    end;
script_start_instructions([{apply, {c, erlangrc, _}} | L],
                          applications_loaded, Applications, Root, Apps) ->
    script_start_instructions(L, applications_loaded,
                              Applications, Root, Apps).

script_remove_data({script, {_Name, _Vsn}, Instructions},
                   Dir, Timeout, Ignore) ->
    % system name and version are ignored
    % expects the typical directory structure produced by reltool
    DirNames = filename:split(Dir),
    case erlang:length(DirNames) of
        DirNamesLength when DirNamesLength > 2 ->
            Root = lists:sublist(DirNames, DirNamesLength - 2),
            case filelib:is_dir(filename:join(Root ++ ["lib"])) of
                true ->
                    case script_remove_instructions(Instructions) of
                        {ok, Applications} ->
                            NewTimeout = if
                                Timeout =:= infinity ->
                                    Timeout;
                                is_integer(Timeout) ->
                                    erlang:round(0.5 + Timeout /
                                        erlang:length(Applications))
                            end,
                            applications_remove(Applications,
                                                NewTimeout,
                                                Ignore);
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, invalid_release_structure}
            end;
        _ ->
            {error, invalid_release_directory}
    end.

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
script_remove_instructions([{apply,
                             {application, start_boot, [A | _]}} | L],
                           applications_loaded, Applications) ->
    % order loaded applications based on the start order
    script_remove_instructions(L, applications_loaded,
                               [A | lists:delete(A, Applications)]);
script_remove_instructions([{apply, {c, erlangrc, _}} | L],
                           applications_loaded, Applications) ->
    script_remove_instructions(L, applications_loaded, Applications).

ensure_all_loaded([]) ->
    ok;
ensure_all_loaded([Module | Modules]) ->
    case is_module_loaded_check(Module) of
        true ->
            Loaded = lists:all(fun(M) ->
                is_module_loaded_check(M) =:= true
            end, Modules),
            if
                Loaded ->
                    ok;
                true ->
                    {error, modules_partially_loaded}
            end;
        false ->
            NotLoaded = lists:all(fun(M) ->
                is_module_loaded_check(M) =:= false
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

config_load_application([], Applications) ->
    {ok, lists:reverse(Applications)};
config_load_application([{Application, Env} | ApplicationEnvs], Applications) ->
    case ensure_application_loaded(Application) of
        ok ->
            lists:foreach(fun({Key, Value}) ->
                ok = application:set_env(Application, Key, Value)
            end, Env),
            config_load_application(ApplicationEnvs,
                                    [Application | Applications]);
        {error, _} = Error ->
            Error
    end.

is_module_loaded_check(Module) when is_atom(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            true;
        false ->
            false
    end.

modules_unload([]) ->
    ok;
modules_unload([Module | Modules]) ->
    _ = module_unload(Module),
    modules_unload(Modules).

modules_purged(Modules, infinity) ->
    modules_purged(Modules, 5000);
modules_purged(Modules, Timeout)
    when is_list(Modules), is_integer(Timeout), Timeout >= 0 ->
    ok = modules_unload(Modules),
    modules_purged(Modules, [], Timeout).

modules_purged([], [], _) ->
    ok;
modules_purged([], BusyModules, Timeout) ->
    case erlang:max(Timeout - ?MODULES_PURGED_DELTA, 0) of
        0 ->
            % attempt to force the purge, killing any processes that remain
            % executing the code
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
    % purge the Module if no processes remain executing the code
    case code:soft_purge(Module) of
        true ->
            modules_purged(Modules, BusyModules, Timeout);
        false ->
            modules_purged(Modules, [Module | BusyModules], Timeout)
    end.

modules_filter([], Modules, _) ->
    Modules;
modules_filter([{Behaviour, Name} | Options], Modules, Loaded)
    when Behaviour =:= behaviour; Behaviour =:= behavior ->
    NewModules = lists:filter(fun(Module) ->
        Names = if
            Loaded =:= true ->
                module_behaviours(Module);
            Loaded =:= false ->
                case code:is_loaded(Module) of
                    {file, _} ->
                        module_behaviours(Module);
                    false ->
                        []
                end
        end,
        lists:member(Name, Names)
    end, Modules),
    modules_filter(Options, NewModules, true);
modules_filter([_ | _], _, _) ->
    erlang:exit(badarg).

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
