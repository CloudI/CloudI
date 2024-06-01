%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%% Copyright 2014 Ulf Wiger
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%=============================================================================

%% @doc Setup utility for erlang applications
%%
%% This API contains:
%% * Support functions for system install ({@link find_hooks/0},
%%   {@link run_hooks/0}, {@link lib_dirs/0}).
%% * Functions for managing and inspecting the system environment
%%   ({@link home/0}, {@link log_dir/0}, {@link data_dir/0},
%%    {@link verify_directories/0}, {@link verify_dir/0}).
%% * Support functions for application environments ({@link get_env/2},
%%   {@link get_all_env/1}, {@link find_env_vars/1}, {@link expand_value/2}).
%% * Functions for controlling dynamic load/upgrade of applications
%%   ({@link find_app/1}, {@link pick_vsn/3}, {@link reload_app/1},
%%    {@link patch_app/1}).
%%
%% == Variable expansion ==
%%
%% Setup supports variable substitution in application environments. It provides
%% some global variables, `"$HOME", "$DATA_DIR", "$LOG_DIR"', corresponding to
%% the API functions {@link home/0}, {@link data_dir/0} and {@link log_dir},
%% as well as some application-specific variables, `"$APP", "$PRIV_DIR",
%% "$LIB_DIR".
%%
%% The normal way to use these variables is by embedding them in file names,
%% e.g. `{my_logs, "$LOG_DIR/$APP"}', but a variable can also be referenced as:
%% * ``{'$value',Var}'' - The variable's value is used as-is (which means that
%%   ``{'$value', "$APP"}'' expands to an atom corresponding to the current
%%   app name.)
%% * ``{'$string', Var}'' - The value is represented as a string (list). If the
%%   value isn't a "string type", `io_lib:format("~w",[Value])' is used.
%% * ``{'$binary', Var}'' - Like ``'$string''', but using binary representation.
%%
%% Custom variables can be defined by using either:
%% * *global scope* - The `setup' environment variable `vars', containing a
%%   list of `{VarName, Definition}' tuples
%% * *application-local scope* - Defining an application-local environment
%%   variable ``'$setup_vars''', on the same format as above.
%%
%% The `VarName' shall be a string, e.g. `"MYVAR"' (no `$' prefix).
%% `Definition' can be one of:
%% * `{value, Val}' - the value of the variable is exactly `Val'
%% * `{expand, Val}' - `Val' is expanded in its turn
%% * `{apply, M, F, A}' - Use the return value of `apply(M, F, A)'.
%%
%% When using a variable expansion, either insert the variable reference in
%% a string (or binary), or use one of the following formats:
%% * ``'{'$value', Var}''' - Use value as-is
%% * ``'{'$string', Var}''' - Use the string representation of the value
%% * ``'{'$binary', Var}''' - Use the binary representation of the value.
%%
%% Example:
%% <pre lang="erlang">
%% 2> application:set_env(setup, vars, [{"PLUS", {apply,erlang,'+',[1,2]}},
%% 2>                                   {"FOO", {value, {foo,1}}}]).
%% ok
%% 3> application:set_env(stdlib, '$setup_vars',
%% 3>                     [{"MINUS", {apply,erlang,'-',[4,3]}},
%% 3>                      {"BAR", {value, "bar"}}]).
%% ok
%% 4> application:set_env(setup, v1, "/$BAR/$PLUS/$MINUS/$FOO").
%% ok
%% 5> setup:get_env(setup,v1).
%% {ok,"/$BAR/3/$MINUS/{foo,1}"}
%% 6> application:set_env(stdlib, v1, "/$BAR/$PLUS/$MINUS/$FOO").
%% ok
%% 7> setup:get_env(stdlib,v1).
%% {ok,"/bar/3/1/{foo,1}"}
%% </pre>
%%
%% In the above example, the first expansion (command no. 5), leaves `$BAR'
%% and `$MINUS' unexpanded, since they are defined in the `stdlib' application,
%% and thus not known to `setup'. In command no. 6, however, they <em>are</em>
%% in context, and are expanded. The variables `$PLUS' and `$FOO' have global
%% context and are expanded in both cases.
%%
%% It is also possible to refer to environment variables in the same
%% application. These are referenced as `"$env(VarName)"'. The corresponding
%% values are expanded in turn - take care not to create expansion loops!
%% The same rules for expansion as above apply.
%%
%% Example:
%% <pre lang="erlang">
%% 2> application:set_env(setup,foo,"foo").
%% ok
%% 3> application:set_env(setup,foo_dir,"$HOME/$env(foo)").
%% ok
%% 4> setup:get_env(setup,foo_dir).
%% {ok,"/Users/uwiger/git/setup/foo"}
%% </pre>
%%
%% == Customizing setup ==
%% The following environment variables can be used to customize `setup':
%% * `{home, Dir}' - The topmost directory of the running system. This should
%%    be a writeable area.
%% * `{data_dir, Dir}' - A directory where applications are allowed to create
%%    their own subdirectories and save data. Default is `Home/data.Node'.
%% * `{log_dir, Dir}' - A directory for logging. Default is `Home/log.Node'.
%% * `{stop_when_done, true|false}' - When invoking `setup' for an install,
%%    `setup' normally remains running, allowing for other operations to be
%% * `{stop_delay, Millisecs}' - If `stop_when_done' is true, and the node
%%    is going to shut down, setup will first wait for a specified number of
%%    milliseconds (default: 5000). This can be useful in order to allow
%%    asynchronous operations to complete before shutting down.
%%    performed from the shell or otherwise. If `{stop_when_done, true}', the
%%    node is shut down once `setup' is finished.
%% * `{abort_on_error, true|false}' - When running install or upgrade hooks,
%%    `setup' will normally keep going even if some hooks fail. A more strict
%%    semantics can be had by setting `{abort_on_error, true}', in which case
%%    `setup' will raise an exception if an error occurs.
%% * `{mode, atom()}' - Specifies the context for running 'setup'. Default is
%%   `normal'. The `setup' mode has special significance, since it's the default
%%    mode for setup hooks, if no other mode is specified and the node has been
%%    started with the setup-generated `install.boot' script. In theory, one may
%%    specify any atom value, but it's probably wise to stick to the values
%%    'normal', 'setup' and 'upgrade' as global contexts, and instead trigger
%%    other mode hooks by explicitly calling {@link run_hooks/1}.
%% * `{verify_directories, boolean()}' - At startup, setup will normally ensure that
%%    the directories used by setup actually exist. This behavior can be disabled through
%%    the environment variable `{verify_directories, false}'. This can be desirable
%%    if setup is used mainly e.g. for environment variable expansion, but not for
%%    disk storage.
%% * `{run_timeout, Millisecs}' - Set a time limit for how long it may take for
%%    setup to process the setup hooks. Default is `infinity'. If the timeout
%%    is exceeded, the application start sequence will be aborted, which will
%%    cause a (rather inelegant) boot sequence failure.
%% @end
-module(setup).

-export([home/0,
         log_dir/0,
         data_dir/0,
         verify_directories/0,
         verify_dir/1,
         mode/0,
         find_hooks/0, find_hooks/1, find_hooks/2,
         run_hooks/0, run_hooks/1, run_hooks/2,
         run_selected_hooks/2,
         applications/0,
         find_env_vars/1,
         get_env/2, get_env/3,
         get_all_env/1,
         expand_value/2,  % expand_value/3 recommended instead
         expand_value/3,
         patch_app/1, patch_app/3,
         find_app/1, find_app/2,
         pick_vsn/3,
         reload_app/1, reload_app/2, reload_app/3,
         keep_release/1,
         lib_dirs/0, lib_dirs/1,
         lib_dirs_under_path/1]).
-export([read_config_script/3,   % (Name, F, Opts)
         read_config_script/4]). % (Name, F, Vars, Opts)

-export([ok/1]).


-export([run_setup/0]).

-export([main/1]).  % new escript entry point

-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(THROW(E), {'___SETUP_THROW___', E}).

-define(if_verbose(Expr),
        case get(verbose) of
            true -> Expr;
            _    -> ok
        end).

%% @spec home() -> Directory
%% @doc Returns the configured `home' directory, or a best guess (`$CWD')
%% @end
%%
home() ->
    home_([]).

home_(Vis) ->
    case get_env_v(setup, home, Vis) of
        undefined ->
            CWD = cwd(),
            D = filename:absname(CWD),
            application:set_env(setup, home, D),
            D;
        {ok, D} when is_binary(D) ->
            binary_to_list(D);
        {ok, D} when is_list(D) ->
            D;
        {error,_} = Error ->
            Error;
        Other ->
            {error, Other}
    end.

%% @spec log_dir() -> Directory
%% @doc Returns the configured log dir, or a best guess (`home()/log.Node')
%% @end
%%
log_dir() ->
    log_dir_([]).

log_dir_(Vis) ->
    setup_dir(log_dir, "log." ++ atom_to_list(node()), Vis).

%% @spec data_dir() -> Directory
%% @doc Returns the configured data dir, or a best guess (`home()/data.Node').
%%
%% @end
%%
data_dir() ->
    data_dir_([]).

data_dir_(Vis) ->
    setup_dir(data_dir, "data." ++ atom_to_list(node()), Vis).

setup_dir(Key, Default, Vis) ->
    case get_env_v(setup, Key, Vis) of
        undefined ->
            D = filename:absname(filename:join(home(), Default)),
            application:set_env(setup, Key, D),
            D;
        {ok, D} when is_binary(D) ->
            binary_to_list(D);
        {ok, D} when is_list(D) ->
            D;
        Other ->
            {error, Other}
    end.

maybe_verify_directories() ->
    case get_env(setup, verify_directories, true) of
        true ->
            verify_directories();
        false ->
            ok
    end.

%% @spec verify_directories() -> ok
%% @doc Ensures that essential directories exist and are writable.
%% Currently, the directories corresponding to {@link home/0},
%% {@link log_dir/0} and {@link data_dir/0} are verified.
%% @end
%%
verify_directories() ->
    _ = verify_dir(home()),
    _ = verify_dir(log_dir()),
    _ = verify_dir(data_dir()),
    ok.

%% @spec verify_dir(Dir) -> Dir
%% @doc Ensures that the directory Dir exists and is writable.
%% @end
%%
verify_dir(Directory) ->
    case filelib:ensure_dir(filename:join(Directory, "dummy")) of
        ok ->
            Directory;
        {error, Reason} ->
            error({verify_dir, {Reason, Directory}}, [Directory])
    end.

ok({ok, Result}) ->
    Result;
ok(Other) ->
    setup_lib:abort("Expected {ok, Value}~n", [Other]).


%% @spec find_env_vars(Env) -> [{AppName, Value}]
%% @doc Searches all loaded apps for instances of the `Env' environment variable.
%%
%% The environment variables are expanded according to the rules outlined in
%% {@section Variable expansion}
%% @end
find_env_vars(Env) ->
    GEnv = global_env(),
    lists:flatmap(
      fun({A,_,_}) ->
              case app_get_env(A, Env) of
                  {ok, Val} when Val =/= undefined ->
                      NewEnv = private_env(A, GEnv),
                      [{A, expand_env(NewEnv, Val, A, [Env])}];
                  _ ->
                      []
              end
      end, application:loaded_applications()).

get_env(A, Key) ->
    case app_get_env(A, Key) of
        {ok, Val} ->
            {ok, expand_value(A, Val)};
        Other ->
            Other
    end.

get_env(A, Key, Default) ->
    case get_env(A, Key) of
        {ok, Val} ->
            Val;
        _ ->
            Default
    end.

get_env_v(A, Key, V) ->
    try get_env_v_(A, Key, V)
    catch
        throw:?THROW(Error) ->
            Error
    end.

get_env_v_(A, Key, V) ->
    case lists:member(Key, V) of
        false ->
            case app_get_env(A, Key) of
                {ok, Val} ->
                    {ok, expand_value_v(A, Key, Val, V)};
                Other ->
                    Other
            end;
        true ->
            throw(?THROW({error, {loop_detected, Key}}))
    end.

-spec get_all_env(atom()) -> [{atom(), any()}].
%% @doc Like `application:get_all_env/1', but with variable expansion.
%%
%% The variable expansion is performed according to the rules outlined in
%% {@section Variable expansion}.
%% @end
get_all_env(A) ->
    Vars = private_env(A, global_env()),
    [{K, expand_env(Vars, V, A, [K])} ||
        {K, V} <- application:get_all_env(A)].

-spec expand_value(atom(), any()) -> any().
%% @doc Expand `Value' using global variables and the variables of `App'
%%
%% The variable expansion is performed according to the rules outlined in
%% {@section Variable expansion}. If a loop is detected (a variable ends
%% up referencing itself), an exception is raised.
%% Use of {@link expand_value/3} (also providing the initial key name) is
%% recommended; this function is primarily here for backward compatibility
%% purposes.
%% @end
expand_value(App, Value) ->
    expand_env(private_env(App, global_env()), Value, App, []).

-spec expand_value(atom(), atom(), any()) -> any().
%% @doc Expand `Value' using global variables and the variables of `App'
%%
%% The variable expansion is performed according to the rules outlined in
%% {@section Variable expansion}. The `Key' name as second argument is used
%% for loop detection, in which case an exception will be raised..
%% @end
expand_value(App, Key, Value) ->
    try expand_value_v(App, Key, Value, [])
    catch
        throw:?THROW(Error) ->
            error(Error)
    end.

expand_value_v(App, K, Value, V) ->
    expand_env(private_env(App), Value, App, [K|V]).

global_env() ->
    Acc = [{K, fun(V1) -> env_value(K, V1) end} ||
              K <- ["DATA_DIR", "LOG_DIR", "HOME"]],
    custom_global_env(Acc).

custom_global_env(Acc) ->
    lists:foldl(fun(E, Acc1) ->
                        custom_env1(E, Acc1, setup)
                end, Acc,
                [{K,V} || {K,V} <- app_get_env(setup, vars, []),
                          is_list(K)]).

private_env(A) ->
    private_env(A, global_env()).

private_env(A, GEnv) ->
    Acc = [{K, fun(Vis1) -> env_value(K, A, Vis1) end} ||
              K <- ["APP", "PRIV_DIR", "LIB_DIR"]],
    custom_private_env(A, Acc ++ GEnv).

custom_private_env(A, Acc) ->
    lists:foldl(fun(E, Acc1) ->
                        custom_env1(E, Acc1, A)
                end, Acc,
                [{K, V} ||
                    {K,V} <- app_get_env(A, '$setup_vars', []),
                    is_list(K)]).

%% Wrapped for tracing purposes
app_get_env(A, K) ->
    application:get_env(A, K).

%% Wrapped for tracing purposes
app_get_env(A, K, Default) ->
    %% Apparently, some still use setup on R15B ...
    case application:get_env(A, K) of
        {ok, Val} -> Val;
        _ ->
            Default
    end.

%% Wrapped for tracing purposes
app_get_key(A, K) ->
    application:get_key(A, K).

custom_env1({K, V}, Acc, A) ->
    [{K, fun(Vis1) -> custom_env_value(K, V, Acc, A, Vis1) end} | Acc].

expand_env(_, {T,"$env(" ++ S} = X, A, Vis)
  when T=='$value'; T=='$string'; T=='$binary' ->
    try Res = case get_env_name_l(S) of
                  false -> undefined;
                  {Name,[]} ->
                      get_env_v_(A, Name, Vis)
              end,
         case {Res, T} of
             {undefined, '$value'} -> undefined;
             {undefined, '$string'} -> "";
             {undefined, '$binary'} -> <<>>;
             {{ok,V}   , '$value'} -> V;
             {{ok,V}   , '$string'} -> binary_to_list(stringify(V));
             {{ok,V}   , '$binary'} -> stringify(V)
         end
    catch
        error:_ -> X
    end;
expand_env(Vs, {T,"$" ++ S}, _, Vis)
  when T=='$value'; T=='$string'; T=='$binary' ->
    case {lists:keyfind(S, 1, Vs), T} of
        {false, '$value'}  -> undefined;
        {false, '$string'} -> "";
        {false, '$binary'} -> <<>>;
        {{_,V}, '$value'}  -> V(Vis);
        {{_,V}, '$string'} -> binary_to_list(stringify(V(Vis)));
        {{_,V}, '$binary'} -> stringify(V(Vis))
    end;
expand_env(Vs, T, A, V) when is_tuple(T) ->
    list_to_tuple([expand_env(Vs, X, A, V) || X <- tuple_to_list(T)]);
expand_env(Vs, L, A, V) when is_list(L) ->
    case setup_lib:is_string(L) of
        true ->
            do_expand_env(L, Vs, A, list, V);
        false ->
            %% [expand_env(Vs, X, A) || X <- L]
            expand_env_l(Vs, L, A, V)
    end;
expand_env(Vs, M, A, V) when is_map(M) ->
    maps:from_list(expand_env_l(Vs, maps:to_list(M), A, V));
expand_env(Vs, B, A, V) when is_binary(B) ->
    do_expand_env(B, Vs, A, binary, V);
expand_env(_, X, _, _) ->
    X.

-spec expand_env_l(list(), maybe_improper_list(), any(), any()) ->
                          maybe_improper_list().
expand_env_l(_Vs, [], _A, _V) ->
    [];
expand_env_l(Vs, [H|T], A, V) when is_list(T) ->
    [expand_env(Vs, H, A, V) | expand_env_l(Vs, T, A, V)];
expand_env_l(Vs, [H|T], A, V) ->
    [expand_env(Vs, H, A, V) | expand_env(Vs, T, A, V)].


%% do_expand_env(X, Vs, Type) ->
%%     lists:foldl(fun({K, Val}, Xx) ->
%%                         re:replace(Xx, [$\\, $$ | K],
%%                                    stringify(Val()), [{return,Type}])
%%                 end, X, Vs).

do_expand_env(X, Vs, A, binary, V) ->
    do_expand_env_b(iolist_to_binary(X), Vs, A, V);
do_expand_env(X, Vs, A, list, V) ->
    binary_to_list(do_expand_env_b(iolist_to_binary(X), Vs, A, V)).

do_expand_env_b(<<"$env(", T/binary>>, Vs, A, Vis) ->
    case get_env_name_b(T) of
        {K, T1} ->
            case get_env_v_(A, K, Vis) of
                {ok, V} ->
                    Res = expand_env(Vs, V, A, Vis),
                    <<(stringify(Res))/binary,
                      (do_expand_env_b(T1, Vs, A, Vis))/binary>>;
                undefined ->
                    <<"$env(", (do_expand_env_b(T, Vs, A, Vis))/binary>>
            end;
        false ->
            do_expand_env_b(T, Vs, A, Vis)
    end;
do_expand_env_b(<<"$", T/binary>>, Vs, A, Vis) ->
    case match_var_b(Vs, T, Vis) of
        {Res, T1} ->
            <<Res/binary, (do_expand_env_b(T1, Vs, A, Vis))/binary>>;
        false ->
            <<"$", (do_expand_env_b(T, Vs, A, Vis))/binary>>
    end;
do_expand_env_b(<<H, T/binary>>, Vs, A, Vis) ->
    <<H, (do_expand_env_b(T, Vs, A, Vis))/binary>>;
do_expand_env_b(<<>>, _, _, _) ->
    <<>>.

get_env_name_b(B) ->
    get_env_name_b(B, <<>>).

get_env_name_b(<<")", T/binary>>, Acc) ->
    try {binary_to_existing_atom(Acc, latin1), T}
    catch
        error:_ -> false
    end;
get_env_name_b(<<H, T/binary>>, Acc) ->
    get_env_name_b(T, <<Acc/binary, H>>);
get_env_name_b(<<>>, _) ->
    false.

get_env_name_l(L) ->
    get_env_name_l(L, []).

get_env_name_l(")" ++ T, Acc) ->
    try {list_to_existing_atom(lists:reverse(Acc)), T}
    catch
        error:_ -> false
    end;
get_env_name_l([H|T], Acc) ->
    get_env_name_l(T, [H|Acc]);
get_env_name_l([], _) ->
    false.

match_var_b([{K,V}|T], B, Vis) ->
    case re:split(B, "^" ++ K, [{return, binary}]) of
        [_] ->
            match_var_b(T, B, Vis);
        [<<>>, Rest] ->
            {stringify(V(Vis)), Rest}
    end;
match_var_b([], _, _) ->
    false.

env_value("LOG_DIR" , Vis) -> log_dir_(Vis);
env_value("DATA_DIR", Vis) -> data_dir_(Vis);
env_value("HOME"    , Vis) -> home_(Vis).

env_value("APP"     , A, _Vis) -> A;
env_value("PRIV_DIR", A, _Vis) -> priv_dir(A);
env_value("LIB_DIR" , A, _Vis) -> lib_dir(A).

custom_env_value(_K, {value, V}, _Vs, _A, _Vis) ->
    V;
custom_env_value(K, {expand, V}, Vs, A, Vis) ->
    expand_env(Vs, V, A, [K|Vis]);
custom_env_value(K, {apply, M, F, As}, _Vs, _A, _Vis) ->
    %% Not ideal, but don't want to introduce exceptions in get_env()
    try apply(M, F, As)
    catch
        error:_ ->
            {error, {custom_setup_env, K}}
    end.

%% This function is more general than to_string/1 below
stringify(V) ->
    try iolist_to_binary(V)
    catch
        error:badarg ->
            iolist_to_binary(io_lib:format("~w", [V]))
    end.

priv_dir(A) ->
    case code:priv_dir(A) of
        {error, bad_name} ->
            case is_cur_dir(A) of
                true ->
                    filename:join(cwd(), "priv");
                false ->
                    error({cannot_get_priv_dir, A})
            end;
        D -> D
    end.

lib_dir(A) ->
    case code:lib_dir(A) of
        {error, bad_name} ->
            case is_cur_dir(A) of
                true ->
                    cwd();
                false ->
                    error({cannot_get_lib_dir, A})
            end;
        D -> D
    end.

cwd() ->
    {ok, CWD} = file:get_cwd(),
    CWD.

is_cur_dir(A) ->
    As = atom_to_list(A),
    filename:basename(cwd()) == As.


%% @spec patch_app(AppName::atom()) -> true | {error, Reason}
%%
%% @doc Adds an application's "development" path to a target system
%%
%% This function locates the given application (`AppName') along the `$ERL_LIBS'
%% path, and prepends it to the code path of the existing system. This is useful
%% not least when one wants to add e.g. a debugging or trace application to a
%% target system.
%%
%% The function will not add the same path again, if the new path is already
%% the 'first' path entry for the application `A'.
%% @end
patch_app(A) when is_atom(A) ->
    patch_app(A, latest).

patch_app(A, Vsn) ->
    patch_app(A, Vsn, lib_dirs()).

patch_app(A, Vsn, LibDirs) ->
    case find_app(A, LibDirs) of
        [_|_] = Found ->
            {_ActualVsn, Dir} = pick_vsn(A, Found, Vsn),
            error_logger:info_msg("[~p vsn ~p] code:add_patha(~s)", [A, _ActualVsn, Dir]),
            code:add_patha(Dir);
        [] ->
            error(no_matching_vsn)
    end.

%% @spec pick_vsn(App::atom(), Dirs::[{Vsn::string(),Dir::string()}], Which) ->
%%          {Vsn, Dir}
%%  where
%%     Which = 'latest' | 'next' | Regexp
%%
%% @doc Picks the specified version out of a list returned by {@link find_app/1}
%%
%% * If `Which' is a string, it will be used as a `re' regexp pattern, and the
%%   first matching version will be returned.
%%
%% * If `Which = latest', the last entry in the list will be returned (assumes
%%   that the list is sorted in ascending version order).
%%
%% * If `Which = next', the next version following the current version of the
%%   application `A' is returned, assuming `A' is loaded; if `A' is not loaded,
%%   the first entry in the list is returned.
%%
%% If no matching version is found, the function raises an exception.
%% @end
pick_vsn(_, Dirs, latest) ->
    lists:last(Dirs);
pick_vsn(A, Dirs, next) ->
    case app_get_key(A, vsn) of
        {ok, Cur} ->
            case lists:dropwhile(fun({V, _}) -> V =/= Cur end, Dirs) of
                [_, {_, _} = Next |_] -> Next;
                _ -> error(no_matching_vsn)
            end;
        _ ->
            hd(Dirs)
    end;
pick_vsn(_, Dirs, Vsn) ->
    case [X || {V, _} = X <- Dirs,
               re:run(V, Vsn) =/= nomatch] of
        [Found|_] ->
            Found;
        [] ->
            error(no_matching_vsn)
    end.


%% @spec find_app(A::atom()) -> [{Vsn, Dir}]
%% @equiv find_app(A, lib_dirs())
find_app(A) ->
    find_app(A, lib_dirs()).

%% @spec find_app(A::atom(), LibDirs::[string()]) -> [{Vsn, Dir}]
%%
%% @doc Locates application `A' along LibDirs (see {@link lib_dirs/0} and
%% {@link lib_dirs/1}) or under the OTP root, returning all found candidates.
%% The version is extracted from the `.app' file; thus, no version suffix
%% in the path name is required.
%% @end
find_app(A, LibDirs) ->
    Astr = to_string(A),
    CurDir = case code:lib_dir(A) of
                 {error,_} -> [];
                 D ->
                     [filename:join(D, "ebin")]
             end,
    CurRoots = current_roots(),
    InLib = [P || P <- LibDirs,
                  is_app_dir(Astr, P)],
    InRoots = lists:append([in_root(Astr, R) || R <- CurRoots]),
    setup_lib:sort_vsns(
      lists:usort(CurDir ++ InRoots ++ InLib), atom_to_list(A)).

to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(A) when is_list(A) ->
    A.

is_app_dir(AStr, D) ->
    Pat = AStr ++ "(-[0-9]+(\\..+)?)?/ebin\$",
    re:run(D, Pat) =/= nomatch.
    %% case lists:reverse(filename:split(D)) of
    %%     ["ebin", App|_] ->
    %%         case re:split(App, <<"-">>, [{return,list}]) of
    %%             [A|_] -> true;
    %%             _ -> false
    %%         end;
    %%     _ ->
    %%         false
    %% end.

current_roots() ->
    CurPath = code:get_path(),
    roots_of(CurPath).

roots_of(Path) ->
    lists:foldr(
      fun(D, Acc) ->
              case lists:reverse(filename:split(D)) of
                  ["ebin",_| [_|_] = T] ->
                      ordsets:add_element(filename:join(lists:reverse(T)), Acc);
                  _ ->
                      Acc
              end
      end, ordsets:new(), Path).

in_root(A, R) ->
    Paths = filelib:wildcard(filename:join([R, "*", "ebin"])),
    [P || P <- Paths, is_app_dir(A, P)].
    %% Pat = atom_to_list(A) ++ "(-[0-9]+(\\..+)?)?/ebin\$",
    %% [P || P <- Paths,
    %%       re:run(P, Pat) =/= nomatch].

%% @spec reload_app(AppName::atom()) -> {ok, NotPurged} | {error, Reason}
%%
%% @equiv reload_app(AppName, latest)
reload_app(A) ->
    reload_app(A, latest).

%% @spec reload_app(AppName::atom(), ToVsn) -> {ok,UnPurged} | {error,Reason}
%%
%% @equiv reload_app(AppName, latest, lib_dirs())
reload_app(A, ToVsn) ->
    reload_app(A, ToVsn, lib_dirs()).

%% @spec reload_app(AppName::atom(), ToVsn, LibDirs) ->
%%           {ok, Unpurged} | {error, Reason}
%%  where
%%    ToVsn = 'latest' | 'next' | Vsn,
%%    LibDirs = [string()]
%%    Vsn   = string()
%%
%% @doc Loads or upgrades an application to the specified version
%%
%% This function is a convenient function for 'upgrading' an application.
%% It locates the given version (using {@link find_app/1} and {@link pick_vsn/3})
%% and loads it in the most appropriate way:
%%
%% * If the application isn't already loaded, it loads the application and
%%   all its modules.
%%
%% * If the application is loaded, it generates an appup script and performs
%%   a soft upgrade. If the new version of the application has an `.appup' script
%%   on-disk, that script is used instead.
%%
%% The application is searched for along the existing path (that is, under
%% the roots of the existing code path, allowing for e.g. $ROOT/lib/app-1.0
%% and $ROOT/lib/app-1.2 to be found and tested against the version condition),
%% and also along `LibDirs' (see {@link lib_dirs/0} an {@link lib_dirs/1}).
%%
%% The generated appup script is of the form:
%%
%% * add modules not present in the previous version of the application
%%
%% * do a soft upgrade on pre-existing modules, using suspend-code_change-resume
%%
%% * delete modules that existed in the old version, but not in the new.
%%
%% The purge method used is `brutal_purge' - see {@link //sasl/appup}.
%%
%% For details on how the new version is chosen, see {@link find_app/1} and
%% {@link pick_vsn/3}.
%% @end
reload_app(A, ToVsn0, LibDirs) ->
    case app_get_key(A, vsn) of
        undefined ->
            ok = application:load(A),
            {ok, Modules} = app_get_key(A, modules),
            _ = [c:l(M) || M <- Modules],
            {ok, []};
        {ok, FromVsn} ->
            {ToVsn, NewPath} = pick_vsn(A, find_app(A, LibDirs), ToVsn0),
            if ToVsn == FromVsn ->
                    {error, same_version};
               true ->
                    error_logger:info_msg("[~p vsn ~p] soft upgrade from ~p",
                                          [A, ToVsn, FromVsn]),
                    reload_app(
                      A, FromVsn, filename:join(code:lib_dir(A), "ebin"),
                      NewPath, ToVsn)
            end
    end.

reload_app(A, OldVsn, OldPath, NewPath, NewVsn) ->
    {_NewVsn, Script, NewApp} = make_appup_script(A, OldVsn, NewPath),
    reload_app(A, OldVsn, OldPath, NewPath, NewVsn, Script, NewApp).

reload_app(A, _OldVsn, _OldPath, NewPath, NewVsn, Script, _NewApp) ->
    LibDir = filename:dirname(NewPath),
    _ = remove_path(NewPath, A),
    case release_handler:eval_appup_script(A, NewVsn, LibDir, Script) of
        {ok, Unpurged} ->
            _ = [code:purge(M) || {M, brutal_purge} <- Unpurged],
            {ok, [U || {_, Mode} = U <- Unpurged, Mode =/= brutal_purge]};
        Other ->
            Other
    end.

remove_path(P, A) ->
    CurPath = code:get_path(),
    case lists:member(P, CurPath) of
        true ->
            %% don't remove if it's the only path
            case [Px || Px <- path_entries(A, CurPath),
                        Px =/= P] of
                [] ->
                    true;
                [_|_] ->
                    code:set_path([Px || Px <- CurPath,
                                         Px =/= P])
            end;
        false ->
            true
    end.

path_entries(A, Path) ->
    Pat = atom_to_list(A) ++ "[^/]*/ebin\$",
    [P || P <- Path,
          re:run(P, Pat) =/= nomatch].

make_appup_script(A, OldVsn, NewPath) ->
    {application, _, NewAppTerms} = NewApp =
        read_app(filename:join(NewPath, atom_to_list(A) ++ ".app")),
    %% OldAppTerms = application:get_all_key(A),
    %% _OldApp = {application, A, OldAppTerms},
    case find_script(A, NewPath, OldVsn, up) of
        {NewVsn, Script} ->
            {NewVsn, Script, NewApp};
        false ->
            {ok, OldMods} = app_get_key(A, modules),
            {modules, NewMods} = lists:keyfind(modules, 1, NewAppTerms),
            {vsn, NewVsn} = lists:keyfind(vsn, 1, NewAppTerms),
            {DelMods,AddMods,ChgMods} = {OldMods -- NewMods,
                                         NewMods -- OldMods,
                                         intersection(NewMods, OldMods)},
            {NewVsn,
             [{load_object_code,{A, NewVsn, NewMods}}]
             ++ [point_of_no_return]
             ++ [{load, {M, brutal_purge, brutal_purge}} || M <- AddMods]
             ++ [{suspend, ChgMods} || ChgMods =/= []]
             ++ [{load, {M, brutal_purge,brutal_purge}} || M <- ChgMods]
             ++ [{code_change, up, [{M, setup} || M <- ChgMods]} ||
                    ChgMods =/= []]
             ++ [{resume, ChgMods} || ChgMods =/= []]
             ++ [{remove, {M, brutal_purge,brutal_purge}} || M <- DelMods]
             ++ [{purge, DelMods} || DelMods =/= []],
             NewApp}
    end.

read_app(F) ->
    case setup_file:consult(F) of
        {ok, [App]} ->
            App;
        {error,_} = Error ->
            error(Error, [F])
    end.

%% slightly modified (and corrected!) version of release_handler:find_script/4.
find_script(App, Dir, OldVsn, UpOrDown) ->
    Appup = filename:join([Dir, "ebin", atom_to_list(App)++".appup"]),
    case setup_file:consult(Appup) of
        {ok, [{NewVsn, UpFromScripts, _DownToScripts}]} ->
            Scripts = case UpOrDown of
                          up -> UpFromScripts
                          %% down -> DownToScripts
                      end,
            case lists:dropwhile(fun({Re,_}) ->
                                         re:run(OldVsn, Re) == nomatch
                                 end, Scripts) of
                [{_OldVsn, Script}|_] ->
                    {NewVsn, Script};
                [] ->
                    false
            end;
        {error, enoent} ->
            false;
        {error, _} ->
            false
    end.


%% find_procs(Mods) ->
%%     Ps = release_handler_1:get_supervised_procs(),
%%     lists:flatmap(
%%       fun({P,_,_,Ms}) ->
%%               case intersection(Ms, Mods) of
%%                   [] -> [];
%%                   I  -> [{P, I}]
%%               end
%%       end, Ps).

intersection(A, B) ->
    A -- (A -- B).



%% @hidden
%%
%% Called from the start function. Will verify directories, then call
%% all setup hooks in all applications, and execute them in order.
%% Afterwards, setup will either finish and leave the system running, or
%% stop, terminating all nodes automatically.
%%
run_setup() ->
    error_logger:info_msg("Setup running ...", []),
    AbortOnError = check_abort_on_error(),
    try run_setup_()
    catch
        error:Error:Stacktrace ->
            error_logger:error_report([{run_setup_failed, Error},
                                       {abort_on_error, AbortOnError},
                                       {stacktrace, Stacktrace}]),
            if AbortOnError ->
                    erlang:error(Error);
               true ->
                    ok
            end
    end.

run_setup_() ->
    Res = maybe_verify_directories(),
    error_logger:info_msg("Directories verified. Res = ~p", [Res]),
    Mode = mode(),
    Hooks = find_hooks(Mode),
    run_selected_hooks(Mode, Hooks, _Recheck = true),
    error_logger:info_msg(
      "Setup finished processing hooks (Mode=~p)...", [Mode]),
    ok.

%% @hidden
main(Args) ->
    setup_gen:main(Args).

%% @spec find_hooks() -> [{PhaseNo, [{M,F,A}]}]
%% @doc Finds all custom setup hooks in all applications.
%% The setup hooks must be of the form
%% ``{'$setup_hooks', [{PhaseNo, {M, F, A}} | {Mode, [{PhaseNo, {M,F,A}}]}]}'',
%% where PhaseNo should be (but doesn't have to be) an integer.
%% If `Mode' is not specified, the hook will pertain to the `setup' mode.
%%
%% The hooks will be called in order:
%% - The phase numbers will be sorted.
%% - All hooks for a specific PhaseNo will be called in sequence,
%%   in the same order as the applications appear in the boot script
%%   (and, if included applications exist, in preorder traversal order).
%%
%% A suggested convention is:
%% - Create the database at phase 100
%% - Create tables (or configure schema) at 200
%% - Populate the database at 300
%%
%% Using the `setup' environment variable `modes', it is possible to
%% define a mode that includes all hooks from different modes.
%% The format is `[{M1, [M2,...]}]'. The expansion is done recursively,
%% so a mode entry in the right-hand side of a pair can expand into other
%% modes. In order to be included in the final list of modes, an expanding
%% mode needs to include itself in the right-hand side. For example:
%%
%% - Applying `a' to `[{a, [b]}]' returns `[b]'
%% - Applying `a' to `[{a, [a,b]}]' returns `[a,b]'
%% - Applying `a' to `[{a, [a,b]},{b,[c,d]}]' returns `[a,c,d]'
%%
%% A typical application of this would be `[{test, [normal, test]}]', where
%% starting in the `test' mode would cause all `normal' and all `test' hooks
%% to be executed.
%% @end
%%
find_hooks() ->
    find_hooks(mode()).

%% @spec find_hooks(Mode) -> [{PhaseNo, [{M, F, A}]}]
%% @doc Find all setup hooks for `Mode' in all applications
%% @end
find_hooks(Mode) when is_atom(Mode) ->
    Applications = applications(),
    find_hooks(Mode, Applications).

%% @spec find_hooks(Mode, Applications) -> [{PhaseNo, [{M, F, A}]}]
%% @doc Find all setup hooks for `Mode' in `Applications'.
%% @end
find_hooks(Mode, Applications) ->
    find_hooks_(maybe_expand_mode(Mode), Applications).

maybe_expand_mode(Mode) ->
    maybe_expand_mode(Mode, app_get_env(setup, modes, [])).

maybe_expand_mode(Mode, Modes) ->
    maybe_expand_mode(Mode, Modes, ordsets:new()).

maybe_expand_mode(Mode, Modes, Acc) ->
    case lists:keyfind(Mode, 1, Modes) of
        {_, Ms} ->
            Modes1 = lists:keydelete(Mode, 1, Modes),
            lists:foldl(
                      fun(M, Acc1) ->
                              maybe_expand_mode(M, Modes1, Acc1)
                      end, Acc, Ms);
        false ->
            ordsets:add_element(Mode, Acc)
    end.

find_hooks_(Modes, Applications) ->
    lists:foldl(
      fun(A, Acc) ->
              case app_get_env(A, '$setup_hooks') of
                  {ok, Hooks} ->
                      lists:foldl(
                        fun(H, Acc1) ->
                                f_find_hooks_(H, A, Modes, Acc1)
                        end, Acc, Hooks);
                  _ ->
                      Acc
              end
      end, orddict:new(), Applications).

f_find_hooks_(Hook, A, Modes, Acc) ->
    IsSetup = lists:member(setup, Modes),
    case Hook of
        {Mode1, [{_, {_,_,_}}|_] = L} ->
            case lists:member(Mode1, Modes) of
                true -> find_hooks_1(Mode1, A, L, Acc);
                false -> Acc
            end;
        {Mode1, [{_, [{_, _, _}|_]}|_] = L} ->
            case lists:member(Mode1, Modes) of
                true -> find_hooks_1(Mode1, A, L, Acc);
                false -> Acc
            end;
        {N, {_, _, _} = MFA} when IsSetup ->
            orddict:append(N, MFA, Acc);
        {N, [{_, _, _}|_] = L} when IsSetup ->
            lists:foldl(
              fun(MFA, Acc1) ->
                      orddict:append(N, MFA, Acc1)
              end, Acc, L);
        _ ->
            Acc
    end.


find_hooks_1(Mode, A, L, Acc1) ->
    lists:foldl(
      fun({N, {_,_,_} = MFA}, Acc2) ->
              orddict:append(N, MFA, Acc2);
         ({N, [{_,_,_}|_] = MFAs}, Acc2) ->
              lists:foldl(
                fun({_,_,_} = MFA1, Acc3) ->
                        orddict:append(
                          N, MFA1, Acc3);
                   (Other1, Acc3) ->
                        error_logger:info_msg(
                          "Invalid hook: ~p~n"
                          "  App  : ~p~n"
                          "  Mode : ~p~n"
                          "  Phase: ~p~n",
                          [Other1, A, Mode, N]),
                        Acc3
                end, Acc2, MFAs)
      end, Acc1, L).

-spec mode() -> normal | atom().
%% @doc Returns the current "setup mode".
%%
%% The mode can be defined using the `setup' environment variable `mode'.
%% The default value is `normal'. The mode is used to select which setup
%% hooks to execute when starting the `setup' application.
%% @end
mode() ->
    case app_get_env(setup, mode) of
        {ok, M} ->
            M;
        _ ->
            case init:get_argument(boot) of
                {ok, [[Boot]]} ->
                    case filename:basename(Boot) of
                        "install" -> setup;
                        _ -> normal
                    end;
                _ ->
                    normal
            end
    end.

%% @spec run_hooks() -> ok
%% @doc Execute all setup hooks for current mode in order.
%%
%% See {@link find_hooks/0} for details on the order of execution.
%% @end
run_hooks() ->
    run_hooks(applications()).

%% @spec run_hooks(Applications) -> ok
%% @doc Execute setup hooks for current mode in `Applications' in order.
%%
%% See {@link find_hooks/0} for details on the order of execution.
%% @end
run_hooks(Apps) ->
    run_hooks(mode(), Apps).

%% @spec run_hooks(Mode, Applications) -> ok
%% @doc Execute setup hooks for `Mode' in `Applications' in order
%%
%% Note that no assumptions can be made about which process each setup hook
%% runs in, nor whether it runs in the same process as the previous hook.
%% See {@link find_hooks/0} for details on the order of execution.
%% @end
run_hooks(Mode, Apps) ->
    Hooks = find_hooks(Mode, Apps),
    run_selected_hooks(Mode, Hooks).

%% @spec run_selected_hooks(Mode, Hooks) -> ok
%% @doc Execute specified setup hooks in order
%%
%% Exceptions are caught and printed. This might/should be improved, but the
%% general idea is to complete as much as possible of the setup, and perhaps
%% repair afterwards. However, the fact that something went wrong should be
%% remembered and reflected at the end.
%% @end
%%
run_selected_hooks(Mode, Hooks) ->
    run_selected_hooks(Mode, Hooks, false).

%% @spec run_selected_hooks(Mode, Hooks, Recheck) -> ok
%% @doc Execute specified hooks in order, re-checking after each completed phase
%% for new hooks (new applications may have been loaded as a result of running their
%% hooks.) Only phases higher than the ones already run will be considered after each
%% re-check.
run_selected_hooks(Mode, Hooks, Recheck) when is_atom(Mode), is_list(Hooks) ->
    AbortOnError = check_abort_on_error(),
    case Recheck of
        true ->
            run_and_recheck(Hooks, Mode, AbortOnError, []);
        false ->
            lists:foreach(fun(Ph) -> run_phase(Ph, Mode, AbortOnError) end, Hooks)
    end.

run_phase({Phase, MFAs}, Mode, AbortOnError) ->
    error_logger:info_msg("Setup phase [~p] ~p~n", [Mode, Phase]),
    lists:foreach(fun({M, F, A}) ->
                          try_apply(M, F, A, AbortOnError)
                  end, MFAs).

run_and_recheck([{PhaseNo, _MFAs} = Phase|_], Mode, AbortOnError, Visited) ->
    ok = run_phase(Phase, Mode, AbortOnError),
    Visited1 = [PhaseNo | Visited],
    NMax = lists:max(Visited1),
    Hooks = [LaterPhase || {N, _} = LaterPhase <- find_hooks(Mode),
                           N > NMax],
    run_and_recheck(Hooks, Mode, AbortOnError, Visited1);
run_and_recheck([], _, _, _) ->
    ok.

check_abort_on_error() ->
    case app_get_env(setup, abort_on_error) of
        {ok, F} when is_boolean(F) -> F;
        {ok, Other} ->
            error_logger:error_msg("Invalid abort_on_error flag (~p)~n"
                                   "Aborting...~n", [Other]),
            error({invalid_abort_on_error, Other});
        _ -> false
    end.

try_apply(M, F, A, Abort) ->
    {_Pid, Ref} = spawn_monitor(
                   fun() ->
                           exit(try {ok, apply(M, F, A)}
                                catch
                                    Type:Exception:Stacktrace ->
                                        {error, {Type, Exception, Stacktrace}}
                                end)
                   end),
    receive
        {'DOWN', Ref, _, _, Return} ->
            case Return of
                {ok, Result} ->
                    report_result(Result, M, F, A);
                {error, {Type, Exception, Stacktrace}} ->
                    report_error(Type, Exception, Stacktrace, M, F, A),
                    if Abort ->
                            error_logger:error_msg(
                              "Abort on error is set. Terminating sequence~n",[]),
                            error({Exception, Stacktrace});
                       true ->
                            ok
                    end
            end
    end.

report_result(Result, M, F, A) ->
    MFAString = format_mfa(M, F, A),
    error_logger:info_msg(MFAString ++ "-> ~p~n", [Result]).

report_error(Type, Error, Stacktrace, M, F, A) ->
    ErrTypeStr = case Type of
                     error -> "ERROR: ";
                     throw -> "THROW: ";
                     exit  -> "EXIT:  "
                 end,
    MFAString = format_mfa(M, F, A),
    error_logger:error_msg(MFAString ++ "-> " ++ ErrTypeStr ++ "~p~n~p~n",
                           [Error, Stacktrace]).


format_mfa(M, F, A) ->
    lists:flatten([atom_to_list(M),":",atom_to_list(F),
                   "(", format_args(A), ")"]).

format_args([])         -> "";
format_args([A])        -> format_arg(A);
format_args([A, B | T]) -> [format_arg(A), "," | format_args([B | T])].

format_arg(A) ->
    io_lib:fwrite("~p", [A]).

%% @spec applications() -> [atom()]
%% @doc Find all applications - either from the boot script or all loaded apps.
%% The applications list is sorted in top application order, where included
%% applications follow directly after the top application they are included in.
%% @end
%%
applications() ->
    Apps = [A || {A, _, _} <- application:loaded_applications()],
    group_applications(Apps).

%% Sort apps in preorder traversal order.
%% That is, for each "top application", all included apps follow before the
%% next top application. Normally, there will be no included apps, in which
%% case the list will maintain its original order.
%%
group_applications(Apps) ->
    group_applications(Apps, []).

group_applications([H | T], Acc) ->
    case app_get_key(H, included_applications) of
        {ok, []} ->
            group_applications(T, [{H,[]}|Acc]);
        {ok, Incls} ->
            AllIncls = all_included(Incls),
            group_applications(T -- AllIncls,
                               [{H, AllIncls}
                                | lists:foldl(
                                    fun(A,Acc1) ->
                                            lists:keydelete(A,1,Acc1)
                                    end, Acc, AllIncls)])
    end;
group_applications([], Acc) ->
    unfold(lists:reverse(Acc)).

unfold([{A,Incl}|T]) ->
    [A|Incl] ++ unfold(T);
unfold([]) ->
    [].

all_included([H | T]) ->
    case app_get_key(H, included_applications) of
        {ok, []} ->
            [H | all_included(T)];
        {ok, Incls} ->
            [H | all_included(Incls)] ++ all_included(T)
    end;
all_included([]) ->
    [].

%% @spec keep_release(RelVsn) -> ok
%% @doc Generates a release based on what's running in the current node.
%% @end
keep_release(RelVsn) ->
    %% 0. Check
    RelDir = setup_lib:releases_dir(),
    case filelib:is_dir(TargetDir = filename:join(RelDir, RelVsn)) of
        true -> error({target_dir_exists, TargetDir});
        false -> verify_dir(TargetDir)
    end,
    %% 1. Collect info
    Loaded = application:loaded_applications(),
    LoadedNames = [element(1,A) || A <- Loaded],
    Running = application:which_applications(),
    RunningNames = [element(1,A) || A <- Running],
    OnlyLoaded = LoadedNames -- RunningNames,
    Included = lists:flatmap(
                 fun(A) ->
                         case app_get_key(A, included_applications) of
                             {ok, []} ->
                                 [];
                             {ok, As} ->
                                 [{A, As}]
                         end
                 end, LoadedNames),
    {Name,_} = init:script_id(),
    Conf = [
            {name, Name},
            {apps, app_list(OnlyLoaded, Loaded, Included)}
            | [{root, R} || R <- current_roots() -- [otp_root()]]
           ]
        ++ [{env, env_diff(LoadedNames)}],
    setup_lib:write_script(
      ConfF = filename:join(TargetDir, "setup.conf"), [Conf]),
    setup_gen:run([{name, Name}, {outdir, TargetDir}, {conf, ConfF}]).
     %% {loaded, Loaded},
     %% {running, Running},
     %% {only_loaded, OnlyLoaded},
     %% {included, Included},
     %% {env, env_diff(LoadedNames)},
     %% {roots, current_roots() -- [otp_root()]},
     %% {rel_dir, setup_lib:releases_dir()}].

app_list(OnlyLoaded, AllLoaded, Included) ->
    lists:map(
      fun({A, _, V}) ->
              case {lists:member(A, OnlyLoaded),
                    lists:keyfind(A, 1, Included)} of
                  {true,false} -> {A, V, load};
                  {true,{_,I}} -> {A, V, load, I};
                  {false,false} -> {A, V};
                  {false,{_,I}} -> {A, V, I}
              end
      end, AllLoaded).

env_diff([A|As]) ->
    AppF = filename:join([code:lib_dir(A), "ebin", atom_to_list(A) ++ ".app"]),
    LiveEnv = lists:keydelete(included_applications, 1,
                              application:get_all_env(A)),
    DiskEnv = fetch_env(AppF),
    case LiveEnv -- DiskEnv of
        [_|_] = Diff ->
            [{A, Diff}|env_diff(As)];
        [] ->
            env_diff(As)
    end;
env_diff([]) ->
    [].

fetch_env(AppF) ->
    case setup_file:consult(AppF) of
        {ok, [{application,_,Terms}]} ->
            proplists:get_value(env, Terms, []);
        {error, Reason} ->
            error({reading_app_file, [AppF, Reason]})
    end.

otp_root() ->
    {ok, [[Root]]} = init:get_argument(root),
    filename:join(Root, "lib").

%% Modified from code_server:get_user_lib_dirs():

%% @spec lib_dirs() -> [string()]
%% @equiv union(lib_dirs("ERL_SETUP_LIBS"), lib_dirs("ERL_LIBS"))
lib_dirs() ->
    A = lib_dirs("ERL_SETUP_LIBS"),
    B = lib_dirs("ERL_LIBS"),
    A ++ (B -- A).

%% @spec lib_dirs(Env::string()) -> [string()]
%% @doc Returns an expanded list of application directories under a lib path
%%
%% This function expands the (ebin/) directories under e.g. `$ERL_SETUP_LIBS' or
%% `$ERL_LIBS'. `$ERL_SETUP_LIB' has the same syntax and semantics as
%% `$ERL_LIBS', but is (hopefully) only recognized by the `setup' application.
%% This can be useful e.g. when keeping a special 'extensions' or 'plugin'
%% root that is handled via `setup', but not treated as part of the normal
%% 'automatic code loading path'.
%% @end
lib_dirs(Env) ->
    case os:getenv(Env) of
        L when is_list(L) ->
            lib_dirs_under_path(L);
        false ->
            []
    end.

lib_dirs_under_path(L) ->
    LibDirs = split_paths(L, path_separator(), [], []),
    get_user_lib_dirs_1(LibDirs).

path_separator() ->
    case os:type() of
        {win32, _} -> $;;
        _          -> $:
    end.

get_user_lib_dirs_1([Dir|DirList]) ->
    case erl_prim_loader:list_dir(Dir) of
        {ok, Dirs} ->
            {Paths,_Libs} = make_path(Dir, Dirs),
            %% Only add paths trailing with ./ebin.
            [P || P <- Paths, filename:basename(P) =:= "ebin"] ++
                get_user_lib_dirs_1(DirList);
        error ->
            get_user_lib_dirs_1(DirList)
    end;
get_user_lib_dirs_1([]) -> [].

split_paths([S|T], S, Path, Paths) ->
    split_paths(T, S, [], [lists:reverse(Path) | Paths]);
split_paths([C|T], S, Path, Paths) ->
    split_paths(T, S, [C|Path], Paths);
split_paths([], _S, Path, Paths) ->
    lists:reverse(Paths, [lists:reverse(Path)]).


make_path(BundleDir, Bundles0) ->
    Bundles = choose_bundles(Bundles0),
    make_path(BundleDir, Bundles, [], []).

choose_bundles(Bundles) ->
    ArchiveExt = archive_extension(),
    Bs = lists:sort([create_bundle(B, ArchiveExt) || B <- Bundles]),
    [FullName || {_Name,_NumVsn,FullName} <-
                     choose(lists:reverse(Bs), [], ArchiveExt)].

create_bundle(FullName, ArchiveExt) ->
    BaseName = filename:basename(FullName, ArchiveExt),
    case split(BaseName, "-") of
        [_, _|_] = Toks ->
            VsnStr = lists:last(Toks),
            case vsn_to_num(VsnStr) of
                {ok, VsnNum} ->
                    Name = join(lists:sublist(Toks, length(Toks)-1),"-"),
                    {Name,VsnNum,FullName};
                false ->
                    {FullName,[0],FullName}
            end;
        _ ->
            {FullName,[0],FullName}
    end.

%% Convert "X.Y.Z. ..." to [K, L, M| ...]
vsn_to_num(Vsn) ->
    case is_vsn(Vsn) of
        true ->
            {ok, [list_to_integer(S) || S <- split(Vsn, ".")]};
        _  ->
            false
    end.

is_vsn(Str) when is_list(Str) ->
    Vsns = split(Str, "."),
    lists:all(fun is_numstr/1, Vsns).

is_numstr(Cs) ->
    lists:all(fun (C) when $0 =< C, C =< $9 -> true;
                  (_)                       -> false
              end, Cs).

split(Cs, S) ->
    split1(Cs, S, []).

split1([C|S], Seps, Toks) ->
    case lists:member(C, Seps) of
        true -> split1(S, Seps, Toks);
        false -> split2(S, Seps, Toks, [C])
    end;
split1([], _Seps, Toks) ->
    lists:reverse(Toks).

split2([C|S], Seps, Toks, Cs) ->
    case lists:member(C, Seps) of
        true -> split1(S, Seps, [lists:reverse(Cs)|Toks]);
        false -> split2(S, Seps, Toks, [C|Cs])
    end;
split2([], _Seps, Toks, Cs) ->
    lists:reverse([lists:reverse(Cs)|Toks]).

join([H1, H2| T], S) ->
    H1 ++ S ++ join([H2| T], S);
join([H], _) ->
    H;
join([], _) ->
    [].

choose([{Name,NumVsn,NewFullName}=New|Bs], Acc, ArchiveExt) ->
    case lists:keyfind(Name, 1, Acc) of
        {_, NV, OldFullName} when NV =:= NumVsn ->
            case filename:extension(OldFullName) =:= ArchiveExt of
                false ->
                    choose(Bs,Acc, ArchiveExt);
                true ->
                    Acc2 = lists:keystore(Name, 1, Acc, New),
                    choose(Bs,Acc2, ArchiveExt)
            end;
        {_, _, _} ->
            choose(Bs,Acc, ArchiveExt);
        false ->
            choose(Bs,[{Name,NumVsn,NewFullName}|Acc], ArchiveExt)
    end;
choose([],Acc, _ArchiveExt) ->
    Acc.

make_path(_,[],Res,Bs) ->
    {Res,Bs};
make_path(BundleDir,[Bundle|Tail],Res,Bs) ->
    Dir = filename:append(BundleDir,Bundle),
    Ebin = filename:append(Dir,"ebin"),
    %% First try with /ebin
    case is_dir(Ebin) of
        true ->
            make_path(BundleDir,Tail,[Ebin|Res],[Bundle|Bs]);
        false ->
            Ebins = ebins_in_archive(Dir),
            make_path(BundleDir, Tail, Ebins ++ Res, [Bundle|Bs])
    end.

ebins_in_archive(Dir) ->
    case setup_file:list_dir(Dir) of
        {ok, Fs} ->
            lists:foldr(
              fun(F, Acc) ->
                      Ebin = filename:join([Dir, F, "ebin"]),
                      case is_dir(Ebin) of
                          true ->
                              [Ebin | Acc];
                          false ->
                              Acc
                      end
              end, [], Fs);
        _ ->
            []
    end.

is_dir(D) ->
    case erl_prim_loader:read_file_info(D) of
        {ok, #file_info{type = directory}} ->
            true;
        _ ->
            false
    end.

archive_extension() ->
    init:archive_extension().

read_config_script(F, Name, Opts) ->
    read_config_script(F, Name, [], Opts).

read_config_script(F, Name, Vars, Opts) ->
    Dir = filename:dirname(F),
    Absname = filename:absname(F),
    case file_script(F, script_vars([{'Name', Name},
                                     {'SCRIPT', Absname},
                                     {'CWD', filename:absname(Dir)},
                                     {'OPTIONS', Opts} | Vars])) of
        {ok, Conf} when is_list(Conf) ->
            expand_config_script(Conf, Name, [], Opts);
        Error ->
            setup_lib:abort("Error reading conf (~s): ~p~n", [F, Error])
    end.

expand_config_script([{include, F}|T], Name, Acc, Opts) ->
    Incl = read_config_script(F, Name, [], Opts),
    expand_config_script(T, Name, [Incl|Acc], Opts);
expand_config_script([{include, F, Vars}|T], Name, Acc, Opts) ->
    Incl = read_config_script(F, Name, Vars, Opts),
    expand_config_script(T, Name, [Incl|Acc], Opts);
expand_config_script([{include_lib, LibF}|T], Name, Acc, Opts) ->
    ?if_verbose(io:fwrite("include_lib: ~s~n", [LibF])),
    expand_include_lib(LibF, [], T, Name, Acc, Opts);
expand_config_script([{include_lib, LibF, Vars}|T], Name, Acc, Opts) ->
    ?if_verbose(io:fwrite("include_lib: ~s (~p)~n", [LibF, Vars])),
    expand_include_lib(LibF, Vars, T, Name, Acc, Opts);
expand_config_script([H|T], Name, Acc, Opts) ->
    expand_config_script(T, Name, [H|Acc], Opts);
expand_config_script([], _, Acc, _) ->
    lists:flatten(lists:reverse(Acc)).

expand_include_lib(LibF, Vars, T, Name, Acc, Opts) ->
    Fullname = find_lib_script(LibF),
    Incl = read_config_script(Fullname, Name, Vars, Opts),
    expand_config_script(T, Name, [Incl|Acc], Opts).

find_lib_script(LibF) ->
    case filename:split(LibF) of
        [App|Tail] ->
            ?if_verbose(io:fwrite("lib: ~s~n", [App])),
            try code_lib_dir(App) of
                {error, bad_name} ->
                    setup_lib:abort(
                      "Error including conf (~s): no such lib (~s)~n",
                      [LibF, App]);
                LibDir when is_list(LibDir) ->
                    filename:join([LibDir | Tail])
            catch
                error:_ ->
                    setup_lib:abort(
                      "Error including conf (~s): no such lib (~s)~n",
                      [LibF, App])
            end;
        [] ->
            setup_lib:abort("Invalid include conf: no file specified~n", [])
    end.


code_lib_dir("setup") ->
    IsEscript = setup_lib:is_escript(),
    case IsEscript of
        true ->
            filename:dirname(
              filename:absname(
                escript:script_name()));
        false ->
            code:lib_dir(setup)
    end;
code_lib_dir(App) when is_list(App); is_binary(App) ->
    try code:lib_dir(binary_to_existing_atom(
                       iolist_to_binary(App), latin1))
    catch error:_ -> undefined
    end.

%% -- a modified version of file:script/2
%% -- The main difference: call erl_eval:exprs() with a local_function handler

file_script(File, Bs) ->
    case setup_file:open(File, [read]) of
        {ok, Fd} ->
            R = eval_stream(Fd, return, Bs),
            _ = setup_file:close(Fd),
            R;
        Error ->
            Error
    end.

eval_stream(Fd, Handling, Bs) ->
    _ = epp:set_encoding(Fd),
    eval_stream(Fd, Handling, 1, undefined, [], Bs).

eval_stream(Fd, H, Line, Last, E, Bs) ->
    eval_stream2(io:parse_erl_exprs(Fd, '', Line), Fd, H, Last, E, Bs).

eval_stream2({ok,Form,EndLine}, Fd, H, Last, E, Bs0) ->
    try erl_eval:exprs(Form, Bs0, local_func_handler()) of
        {value,V,Bs} ->
            eval_stream(Fd, H, EndLine, {V}, E, Bs)
    catch
        Class:Reason:Stacktrace ->
            Error = {EndLine,?MODULE,{Class,Reason, Stacktrace}},
            eval_stream(Fd, H, EndLine, Last, [Error|E], Bs0)
    end;
eval_stream2({error,What,EndLine}, Fd, H, Last, E, Bs) ->
    eval_stream(Fd, H, EndLine, Last, [What | E], Bs);
eval_stream2({eof,EndLine}, _Fd, H, Last, E, _Bs) ->
    case {H, Last, E} of
        {return, {Val}, []} ->
            {ok, Val};
        {return, undefined, E} ->
            {error, hd(lists:reverse(E, [{EndLine,?MODULE,undefined_script}]))};
        %% {ignore, _, []} ->
        %%     ok;
        {_, _, [_|_] = E} ->
            {error, hd(lists:reverse(E))}
    end.

%% %% -- end file:script/2 copy-paste

local_func_handler() ->
    {eval, fun local_func/3}.

local_func(b, [], Bs) ->
    {value, erl_eval:bindings(Bs), Bs};
local_func(eval, Params, Bs) ->
    [F|T] = [erl_parse:normalise(P) || P <- Params],
    Vars = case T of
               [] -> [];
               [Vs] -> Vs
           end,
    Absname = filename:absname(F),
    {value, file_script(F, script_vars(Vars ++ [{'SCRIPT', Absname}|Bs])), Bs};
local_func(eval_lib, Params, Bs) ->
    [LibF|T] = [erl_parse:normalise(P) || P <- Params],
    try find_lib_script(LibF) of
        Fullname ->
            Vars = case T of
                       [] -> [];
                       [Vs] -> Vs
                   end,
            Res = file_script(Fullname, script_vars(
                                          Vars ++ [{'SCRIPT', Fullname}|Bs])),
            {value, Res, Bs}
    catch
        error:_ ->
            {value, {error, enoent}, Bs}
    end;
local_func(F, A, _) ->
    erlang:error({script_undef, F, A, []}).

script_vars(Vs) ->
    lists:foldl(fun({K,V}, Acc) ->
                        erl_eval:add_binding(K, V, Acc)
                end, erl_eval:new_bindings(), Vs).

%% Unit tests
-ifdef(TEST).

setup_test_() ->
    {foreach,
     fun() ->
             application:load(setup)
     end,
     fun(_) ->
             application:stop(setup),
             application:unload(setup)
     end,
     [
      ?_test(t_expand_modes()),
      ?_test(t_find_hooks()),
      ?_test(t_find_hooks_1()),
      ?_test(t_expand_vars()),
      ?_test(t_nested_includes())
     ]}.

t_expand_modes() ->
    [a] = maybe_expand_mode(a, []),
    [a] = maybe_expand_mode(a, [{a, [a]}]),
    [a,b,c] = maybe_expand_mode(a, [{a, [a,b]},
                                    {b, [b,c]}]),
    [b] = maybe_expand_mode(a, [{a, [b]}]),
    [a,b,c] = maybe_expand_mode(a, [{a, [a,b]},
                                    {b, [b,c]},
                                    {c, [c,a]}]),
    [c,d] = maybe_expand_mode(a, [{a, [b]},
                                  {b, [c,d]}]),
    ok.

t_find_hooks() ->
    application:set_env(setup, '$setup_hooks',
                        [{100, [{a, hook, [100,1]},
                                {a, hook, [100,2]}]},
                         {200, [{a, hook, [200,1]}]},
                         {upgrade, [{100, [{a, upgrade_hook, [100,1]}]}]},
                         {setup, [{100, [{a, hook, [100,3]}]}]},
                         {normal, [{300, {a, normal_hook, [300,1]}}]}
                        ]),
    NormalHooks = find_hooks(normal),
    [{300, [{a, normal_hook, [300,1]}]}] = NormalHooks,
    UpgradeHooks = find_hooks(upgrade),
    [{100, [{a, upgrade_hook, [100,1]}]}] = UpgradeHooks,
    SetupHooks = find_hooks(setup),
    [{100, [{a,hook,[100,1]},
            {a,hook,[100,2]},
            {a,hook,[100,3]}]},
     {200, [{a,hook,[200,1]}]}] = SetupHooks,
    ok.

t_find_hooks_1() ->
    application:set_env(setup, modes, [{test, [setup, normal, test]}]),
    application:set_env(setup, '$setup_hooks',
                        [{100, [{a, hook, [100,1]},
                                {a, hook, [100,2]}]},
                         {200, [{a, hook, [200,1]}]},
                         {upgrade, [{100, [{a, upgrade_hook, [100,1]}]}]},
                         {setup, [{100, [{a, hook, [100,3]}]}]},
                         {normal, [{300, {a, normal_hook, [300,1]}}]},
                         {test, [{400, {a, test_hook, [400,1]}}]}
                        ]),
    NormalHooks = find_hooks(normal),
    [{300, [{a, normal_hook, [300,1]}]}] = NormalHooks,
    UpgradeHooks = find_hooks(upgrade),
    [{100, [{a, upgrade_hook, [100,1]}]}] = UpgradeHooks,
    SetupHooks = find_hooks(setup),
    [{100, [{a,hook,[100,1]},
            {a,hook,[100,2]},
            {a,hook,[100,3]}]},
     {200, [{a,hook,[200,1]}]}] = SetupHooks,
    TestHooks = find_hooks(test),
    [{100, [{a,hook,[100,1]},
            {a,hook,[100,2]},
            {a,hook,[100,3]}]},
     {200, [{a,hook,[200,1]}]},
     {300, [{a,normal_hook, [300,1]}]},
     {400, [{a,test_hook, [400,1]}]}] = TestHooks,
     ok.


t_expand_vars() ->
    %% global env
    application:set_env(setup, vars, [{"PLUS", {apply,erlang,'+',[1,2]}},
                                      {"FOO", {value, {foo,1}}}]),
    %% private env, stdlib
    application:set_env(stdlib, '$setup_vars',
                        [{"MINUS", {apply,erlang,'-',[4,3]}},
                         {"BAR", {value, "bar"}}]),
    application:set_env(setup, envy, 17),
    application:set_env(setup, v1, "/$BAR/$PLUS/$MINUS/$FOO/$env(envy)"),
    application:set_env(setup, v2, {'$value', "$FOO"}),
    application:set_env(setup, v3, {'$string', "$env(envy)"}),
    application:set_env(stdlib, v1, {'$string', "$FOO"}),
    application:set_env(stdlib, v2, {'$binary', "$FOO"}),
    application:set_env(stdlib, v3, {"$PLUS", "$MINUS", "$BAR"}),
    application:set_env(stdlib, v4, [a|b]),
    %% $BAR and $MINUS are not in setup's context
    {ok, "/$BAR/3/$MINUS/{foo,1}/17"} = setup:get_env(setup, v1),
    {ok, {foo,1}} = setup:get_env(setup, v2),
    {ok, "17"} = setup:get_env(setup, v3),
    {ok, "{foo,1}"} = setup:get_env(stdlib, v1),
    {ok, <<"{foo,1}">>} = setup:get_env(stdlib,v2),
    {ok, {"3", "1", "bar"}} = setup:get_env(stdlib,v3),
    {ok, [a|b]} = setup:get_env(stdlib, v4),
    ok.

t_nested_includes() ->
    to_file_("a.config", [{apps,[kernel,stdlib,setup]},
                          {env,[{setup,[{a,1}]}]}]),
    to_file_("b.config", [{include,"a.config"},
                          {set_env, [{setup, [{a,2}]}]}]),
    to_file_("c.config", [{include, "b.config"},
                          {set_env, [{setup, [{a,3}]}]}]),
    [{apps,[kernel,stdlib,setup]},
     {env, [{setup, [{a,1}]}]},
     {set_env, [{setup, [{a,2}]}]},
     {set_env, [{setup, [{a,3}]}]}] =
        setup:read_config_script("c.config", nested, []).

to_file_(F, Term) ->
    {ok, Fd} = file:open(F, [write]),
    try io:fwrite(Fd, "~p.~n", [Term])
    after
        file:close(Fd)
    end.

-endif.
