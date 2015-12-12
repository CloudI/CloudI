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
-module(setup_gen).
-export([main/1,   % escript-style
         run/1,    % when called from within erlang
         help/0]). % prints help text.

-import(setup_lib, [abort/2]).

-define(if_verbose(Expr),
        case get(verbose) of
            true -> Expr;
            _    -> ok
        end).


main([]) ->
    help(),
    halt(1);
main([H]) when H=="-h"; H=="-help" ->
    help(),
    halt(0);
main(["-" ++ _|_] = Args) ->
    put(is_escript, true),
    Opts = try options(Args)
           catch
               error:E ->
                   abort(E, [])
           end,
    run(Opts);
main([Name, Config, Out| InArgs]) ->
    put(is_escript, true),
    Opts = try options(InArgs)
           catch
               error:E ->
                   abort(E, [])
           end,
    run([{name, Name}, {conf, Config}, {outdir, Out} | Opts]).

help() ->
    setup_lib:help().

%% @spec run(Options) -> ok
%% @doc Generates .rel file(s) and boot scripts for a given configuration.
%%
%% This function reads a configuration specification and generates the
%% files needed to start a node from an OTP boot script. Optionally, it can
%% also generate a 'setup' script, which contains the same applications, but
%% only loaded (except the `setup' application, if present, which is started).
%% This way, a node can be started with all paths set, and all environment
%% variables defined, such that a database can be created, and other setup
%% tasks be performed.
%%
%% Mandatory options:
%% * `{name, Name}'  - Name of the release (and of the .rel and .script files)
%% * `{outdir, Dir}' - Where to put the generated files. Dir is created if not
%%                     already present.
%% * `{conf, Conf}'  - Config file listing apps and perhaps other options
%%
%% Additional options:
%% * `{apps, [App]}' - List of applications to include in the release. Only the
%%                     first instance of this option is considered.
%% * `{add_apps, [App]}' - Adds applications to the ones given in the `apps'
%%                         option.
%% * `{include, ConfigFile}' - include options from the given file. The file
%%                             is processed using `file:script/2'.
%% * `{include_lib, ConfigFile}' - As above, but ConfigFile is named as with
%%                                 the `-include_lib(...)' directive in Erlang
%%                                 source code.
%% * ...
%% @end
%%
run(Options) ->
    %% dbg:tracer(),
    %% dbg:tpl(?MODULE,x),
    %% dbg:p(all,[c]),
    case lists:keyfind(verbose, 1, Options) of
        {_, true} -> put(verbose, true);
        _ -> ignore
    end,
    ?if_verbose(io:fwrite("Options = ~p~n", [Options])),
    Config = read_config(Options),
    ?if_verbose(io:fwrite("Config = ~p~n", [Config])),
    FullOpts = Options ++ Config,
    {Name, OutDir, RelDir, RelVsn, GenTarget} = name_and_target(FullOpts),
    ensure_dir(RelDir),
    Roots = roots(FullOpts),
    ?if_verbose(io:fwrite("Roots = ~p~n", [Roots])),
    check_config(Config),
    Env = env_vars(FullOpts),
    InstEnv = install_env(Env, FullOpts),
    add_paths(Roots, FullOpts),
    Apps0 = apps(FullOpts, Env),
    Rel = {release, {Name, RelVsn}, {erts, erts_vsn()}, [A || {A,_} <- Apps0]},
    ?if_verbose(io:fwrite("Rel: ~p~n", [Rel])),
    build_target_lib(GenTarget, OutDir, Apps0),
    copy_erts(GenTarget, OutDir),
    in_dir(RelDir,
           fun() ->
                   setup_lib:write_eterm("start.rel", Rel),
                   make_boot("start", GenTarget, Roots),
                   setup_lib:write_eterm("sys.config", Env),
                   if_install(FullOpts,
                              fun() ->
                                      InstRel = make_install_rel(Rel),
                                      setup_lib:write_eterm(
                                        "install.rel", InstRel),
                                      setup_lib:write_eterm(
                                        "install.config", InstEnv),
                                      make_boot("install", GenTarget, Roots)
                              end, ok),
                   setup_lib:write_eterm("setup_gen.eterm", FullOpts)
           end).

name_and_target(FullOpts) ->
    Name = option(name, FullOpts),
    case proplists:get_value(target, FullOpts, false) of
        false ->
            RelDir = option(outdir, FullOpts),
            RelVsn = rel_vsn(RelDir, FullOpts),
            {Name, RelDir, RelDir, RelVsn, false};
        TargetDir ->
            RelVsn = option(vsn, FullOpts),
            RelDir = filename:join([TargetDir, "releases", RelVsn]),
            case filelib:is_dir(TargetDir) of
                true  -> abort("Target directory exists: ~s~n", [TargetDir]);
                false -> ok
            end,
            {Name, TargetDir, RelDir, RelVsn, true}
    end.


build_target_lib(false, _, _) ->
    ok;
build_target_lib(true, Root, Apps) ->
    LibRoot = filename:join(Root, "lib"),
    file:make_dir(LibRoot),
    in_dir(LibRoot,
           fun() ->
                   lists:foreach(
                     fun({A, D}) ->
                             AppName = element(1, A),
                             AppVsn = element(2, A),
                             AppD = atom_to_list(AppName) ++ "-" ++ AppVsn,
                             Dir = filename:dirname(D),
                             copy_app(Dir, AppD)
                     end, Apps)
           end).

copy_erts(false, _) ->
    ok;
copy_erts(true, Root) ->
    {ok, [[ErlRoot]]} = init:get_argument(root),
    [Erts] = filelib:wildcard(filename:join(ErlRoot, "erts-*")),
    BaseName = filename:basename(Erts),
    file:make_dir(filename:join(Root, BaseName)),
    Cmd = "cp -r " ++ Erts ++ "/bin " ++ Root ++ "/" ++ BaseName ++ "/",
    os:cmd(Cmd).


copy_app(A0, To) ->
    file:make_dir(To),
    os:cmd("cp -r " ++ A0 ++ "/priv " ++ To),
    os:cmd("cp -r " ++ A0 ++ "/ebin " ++ To).


if_install(Options, F, Else) ->
    case proplists:get_value(install,Options,false) of
        true ->
            F();
        _ ->
            Else
    end.

options(["-target"       , D|T]) -> [{target,D}|options(T)];
options(["-name"         , N|T]) -> [{name, N}|options(T)];
options(["-root"         , D|T]) -> [{root, D}|options(T)];
options(["-out"          , D|T]) -> [{outdir, D}|options(T)];
options(["-relconf"      , F|T]) -> [{relconf, F}|options(T)];
options(["-conf"         , F|T]) -> [{conf, F}|options(T)];
%% options(["-target_subdir", D|T]) -> [{target_subdir, D}|options(T)];
options(["-install"])            -> [{install, true}];
options(["-install" | ["-" ++ _|_] = T]) -> [{install, true}|options(T)];
options(["-install"      , D|T]) -> [{install, mk_bool(D)}|options(T)];
options(["-sys"          , D|T]) -> [{sys, D}|options(T)];
options(["-vsn"          , D|T]) -> [{vsn, D}|options(T)];
options(["-pa"           , D|T]) -> [{pa, D}|options(T)];
options(["-pz"           , D|T]) -> [{pz, D}|options(T)];
options(["-v"               |T]) -> [{verbose, true}|options(T)];
options(["-V" ++ VarName, ExprStr | T]) ->
    Var = list_to_atom(VarName),
    Term = parse_term(ExprStr),
    [{var, Var, Term}|options(T)];
options([_Other|_] = L) ->
    abort("Unknown_option: ~p~n", [L]);
options([]) ->
    [].

mk_bool(T) when T=="true" ; T=="1" -> true;
mk_bool(F) when F=="false"; F=="0" -> false;
mk_bool(Other) ->
    abort("Expected truth value (~p)~n", [Other]).

parse_term(Str) ->
    case erl_scan:string(Str) of
        {ok, Ts, _} ->
            case erl_parse:parse_term(ensure_dot(Ts)) of
                {ok, T} ->
                    T;
                {error,_} = EParse ->
                    abort(EParse, [])
            end;
        {error,_,_} = EScan ->
            abort(EScan, [])
    end.

ensure_dot(Ts) ->
    case lists:reverse(Ts) of
        [{dot,_}|_] ->
            Ts;
        Rev ->
            lists:reverse([{dot,1}|Rev])
    end.

%% target_dir(RelDir, Config) ->
%%     D = case proplists:get_value(target_subdir, Config) of
%%          undefined ->
%%              RelDir;
%%          Sub ->
%%              filename:join(RelDir, Sub)
%%      end,
%%     ensure_dir(D),
%%     D.

ensure_dir(D) ->
    case filelib:is_dir(D) of
        true ->
            ok;
        false ->
            case filelib:ensure_dir(D) of
                ok ->
                    case file:make_dir(D) of
                        ok ->
                            ok;
                        MakeErr ->
                            abort("Could not create ~s (~p)~n", [D, MakeErr])
                    end;
                EnsureErr ->
                    abort("Parent of ~s could not be created or is not "
                          "writeable (~p)~n", [D, EnsureErr])
            end
    end.

read_config(Opts) ->
    case lists:keyfind(conf, 1, Opts) of
        false ->
            read_rel_config(Opts);
        {_, F} ->
            Name = option(name, Opts),
            setup:read_config_script(F, Name, Opts)
    end.

read_rel_config(Opts) ->
    case lists:keyfind(relconf, 1, Opts) of
        {relconf, F} ->
            Name = option(name, Opts),
            case file:consult(F) of
                {ok, Conf} ->
                    ?if_verbose(io:fwrite("Relconf = ~p~n", [Conf])),
                    SysConf = option(sys, Conf),
                    LibDirs = option(lib_dirs, SysConf),
                    TargetOpt = rel_conf_target_dir(Conf, Opts),
                    case [{V,As} || {rel,N,V,As} <- SysConf,
                                N == Name] of
                        [] ->
                            abort("No matching 'rel' (~w) in ~s~n", [Name, F]);
                        [{V,Apps}] ->
                            TargetOpt ++
                                [{vsn, V},
                                 {apps, Apps} | [{root, D} || D <- LibDirs]]
                    end;
                Error ->
                    abort("Error reading relconf ~s:~n"
                          "~p~n", [F, Error])
            end;
        false ->
            abort("No usable config file~n", [])
    end.

rel_conf_target_dir(Conf, Opts) ->
    case lists:keyfind(target, 1, Opts) of
        {_, _} ->
            %% The 'target' option overrides whatever is in the relconf
            [];
        false ->
            case lists:keyfind(target_dir, 1, Conf) of
                false ->
                    [];
                {_, TargetDir} ->
                    [{target, TargetDir}]
            end
    end.

roots(Opts) ->
    [R || {root, R} <- Opts].

check_config(Conf) ->
    _ = [mandatory(K, Conf) || K <- [apps]],
    ok.

option(K, Opts) ->
    case lists:keyfind(K, 1, Opts) of
        {_, V} ->
            V;
        false ->
            abort("Mandatory: -~s~n", [atom_to_list(K)])
    end.

env_vars(Options) ->
    Env0 = case proplists:get_value(sys, Options) of
               undefined ->
                   [];
               Sys ->
                   case file:consult(Sys) of
                       {ok, [E]} ->
                           E;
                       {error, Reason} ->
                           abort("Error reading ~s:~n"
                                 "~p~n", [Sys, Reason])
                   end
           end,
    SetupEnv = if_install(Options, fun() -> [{setup,
                                              [{conf,Options}]}]
                                   end, []),
    lists:foldl(
      fun(E, Acc) ->
              merge_env(E, Acc)
      end, Env0, [E || {env, E} <- Options] ++ [SetupEnv]).

install_env(Env, Options) ->
    Dist =
        case proplists:get_value(nodes, Options, []) of
            []  -> [];
            [_] -> [];
            [_,_|_] = Nodes ->
                [{sync_nodes_mandatory, Nodes},
                 {sync_nodes_timeout, infinity},
                 {distributed, [{setup, [hd(Nodes)]}]}]
        end,
    case lists:keyfind(kernel, 1, Env) of
        false ->
            [{kernel, Dist} | Env];
        {_, KEnv} ->
            Env1 = Dist ++
                [E || {K,_} = E <- KEnv,
                      not lists:member(K, [sync_nodes_optional,
                                           sync_nodes_mandatory,
                                           sync_nodes_timeout,
                                           distributed])],
            lists:keyreplace(kernel, 1, Env, {kernel, Env1})
    end.

merge_env(E, Env) ->
    lists:foldl(
      fun({App, AEnv}, Acc) ->
              case lists:keyfind(App, 1, Env) of
                  false ->
                      Acc ++ [{App, AEnv}];
                  {_, AEnv1} ->
                      New = {App, lists:foldl(
                                    fun({K,V}, Acc1) ->
                                            lists:keystore(K,1,Acc1,{K,V})
                                    end, AEnv1, AEnv)},
                      lists:keyreplace(App, 1, Acc, New)
              end
      end, Env, E).


mandatory(K, Conf) ->
    case lists:keymember(K, 1, Conf) of
        false ->
            abort("missing mandatory config item: ~p~n", [K]);
        true ->
            ok
    end.

in_dir(D, F) ->
    {ok, Old} = file:get_cwd(),
    try file:set_cwd(D) of
        ok ->
            ?if_verbose(io:fwrite("entering directory ~s~n", [D])),
            F();
        Error ->
            abort("Error entering rel dir (~p): ~p~n", [D,Error])
    after
        ok = file:set_cwd(Old)
    end.

-define(is_type(T), T==permanent;T==temporary;T==transient;T==load).

apps(Options, Env) ->
    %% Apps0 = proplists:get_value(apps, Options, [])
    %%     ++ lists:concat(proplists:get_all_values(add_apps, Options)),
    {AddApps, RemoveApps} = add_remove_apps(Options, Env),
    Apps0 = remove_apps(RemoveApps,
                        proplists:get_value(apps, Options, []) ++ AddApps),
    Apps1 = trim_duplicates(
              if_install(Options,
                         fun() ->
                                 ensure_setup(Apps0)
                         end, Apps0)),
    AppNames = app_names(Apps1),
    ?if_verbose(io:fwrite("Apps1 = ~p~n", [Apps1])),
    AppVsns = lists:flatmap(
                fun(A) when is_atom(A) ->
                        {V,D} = app_vsn(A),
                        [{{A, V}, D}];
                   ({A,V}) when is_list(V) ->
                        {V1,D} = app_vsn(A, V),
                        [{{A, V1}, D}];
                   ({A,Type}) when ?is_type(Type) ->
                        {V1,D} = app_vsn(A),
                        [{{A, V1, Type}, D}];
                   ({A,V,Type}) when ?is_type(Type) ->
                        {V1,D} = app_vsn(A, V),
                        [{{A, V1, Type}, D}];
                   ({A,V,Incl}) when is_list(Incl) ->
                        {V1, D} = app_vsn(A, V),
                        expand_included(Incl, AppNames)
                            ++ [{{A, V1, Incl}, D}];
                   ({A,V,Type,Incl}) when ?is_type(Type) ->
                        {V1, D} = app_vsn(A, V),
                        expand_included(Incl, AppNames)
                            ++ [{{A, V1, Type, Incl}, D}]
                end, sort_apps(Options, Apps1)),
    ?if_verbose(io:fwrite("AppVsns = ~p~n", [AppVsns])),
    %% setup_is_load_only(replace_versions(AppVsns, Apps1)).
    setup_is_load_only(AppVsns).

add_remove_apps(Options, _Env) ->
    lists:foldl(
      fun({add_apps, As}, {Incl, Excl}) ->
              {add_to_set(As, Incl), del_from_set(As, Excl)};
         ({remove_apps, As}, {Incl, Excl}) ->
              {del_from_set(As, Incl), add_to_set(app_names(As), Excl)};
         (_, Acc) ->
              Acc
      end, {[], []}, Options).

sort_apps(Options, Apps) ->
    lists:foldl(fun({sort_app, A, Before}, Acc) ->
                        case is_in_set(A, Acc) of
                            {true, App} ->
                                insert_before(Acc -- [App], App, Before);
                            false ->
                                abort("Cannot re-sort ~p - not found~n", [A])
                        end;
                   (_, Acc) ->
                        Acc
                end, Apps, Options).

insert_before([H|T], App, Before) ->
    case is_in_set(H, Before) of
        {true, _} ->
            [App, H|T];
        false ->
            [H|insert_before(T, App, Before)]
    end.

add_to_set(As, Set) ->
    lists:foldl(fun(A, Acc) ->
                        case is_in_set(A, Acc) of
                            {true, Prev} ->
                                lists:delete(Prev, Acc) ++ [A];
                            false ->
                                Acc ++ [A]
                        end
                end, Set, As).

del_from_set(As, Set) ->
    lists:foldl(fun(A, Acc) ->
                        case is_in_set(A, Acc) of
                            {true, Prev} ->
                                lists:delete(Prev, Acc);
                            false ->
                                Acc
                        end
                end, Set, As).

is_in_set(Entry, Set) ->
    A = if is_tuple(Entry) -> element(1, Entry);
           is_atom(Entry)  -> Entry
        end,
    name_in_set(A, Set).

app_name(A) when is_atom(A) -> A;
app_name(T) when is_tuple(T) -> element(1, T).

app_names(As) ->
    [app_name(A) || A <- As].

name_in_set(A, [A|_]) -> {true, A};
name_in_set(A, [H|_]) when element(1,H) == A -> {true, H};
name_in_set(A, [_|T]) ->
    name_in_set(A, T);
name_in_set(_, []) ->
    false.


remove_apps(Remove, Apps) ->
    lists:filter(
      fun(Entry) ->
              A = case Entry of
                      Am when is_atom(Am) -> Am;
                      T when is_tuple(T)  -> element(1,T)
                  end,
              not lists:member(A, Remove)
      end, Apps).



trim_duplicates([A|As0]) when is_atom(A) ->
    As1 = [Ax || Ax <- As0, Ax =/= A],
    case lists:keymember(A, 1, As0) of
        false ->
            [A|trim_duplicates(As1)];
        true ->
            %% a more well-defined entry exists; use that one.
            trim_duplicates(As1)
    end;
trim_duplicates([At|As0]) when is_tuple(At) ->
    %% Remove all exact duplicates
    As1 = [Ax || Ax <- As0, Ax =/= At],
    %% If other detailed entries (though not duplicates) exist, we should
    %% perhaps try to combine them. For now, let's just abort.
    case [Ay || Ay <- As1, element(1,Ay) == element(1,At)] of
        [] ->
            [At|trim_duplicates(As0)];
        [_|_] = Duplicates ->
            abort("Conflicting app entries: ~p~n", [[At|Duplicates]])
    end;
trim_duplicates([]) ->
    [].


expand_included(Incl, AppNames) ->
    R = case Incl -- AppNames of
            [] ->
                [];
            Implicit ->
                [{A, app_vsn(A), load} || A <- Implicit]
        end,
    ?if_verbose(io:fwrite("expand_included(~p, ~p) -> ~p~n",
                          [Incl, AppNames, R])),
    R.

ensure_setup([setup|_] = As) -> As;
ensure_setup([A|_] = As) when element(1,A) == setup -> As;
ensure_setup([H|T]) -> [H|ensure_setup(T)];
ensure_setup([]) ->
    [setup].

setup_is_load_only(Apps) ->
    lists:map(fun({{setup,V}, D}) ->
                      {{setup,V,load}, D};
                 (A) ->
                      A
              end, Apps).

add_paths(Roots, Opts) ->
    APaths = proplists:get_all_values(pa, Opts),
    _ = [ true = code:add_patha(P) || P <- APaths ],

    ZPaths = proplists:get_all_values(pz, Opts),
    _ = [ true = code:add_pathz(P) || P <- ZPaths ],

    Paths = case proplists:get_value(wild_roots, Opts, false) of
                true ->
                    lists:foldl(fun(R, Acc) ->
                                        expand_root(R, Acc)
                                end, [], Roots);
                false ->
                    lists:concat(
                      lists:map(fun(R) ->
                                        case filelib:wildcard(
                                               filename:join(R, "lib/*/ebin")) of
                                            [] ->
                                                filelib:wildcard(
                                                  filename:join(R, "*/ebin"))
                                        end
                                end, Roots))
            end,
    ?if_verbose(io:fwrite("Paths = ~p~n", [Paths])),
    Res = code:set_path(Paths ++ (code:get_path() -- Paths)),
    %% Res = code:add_pathsa(Paths -- code:get_path()),
    ?if_verbose(io:fwrite("add path Res = ~p~n", [Res])).

expand_root(R, Acc) ->
    case filename:basename(R) of
        "ebin" ->
            [R|Acc];
        _ ->
            case file:list_dir(R) of
                {ok, Fs} ->
                    lists:foldl(fun(F, Acc1) ->
                                        expand_root(filename:join(R, F), Acc1)
                                end, Acc, Fs);
                {error,enotdir} ->
                    Acc;
                {error,_} = E ->
                    ?if_verbose(io:fwrite("warning: ~p (~s)~n", [E, R])),
                    Acc
            end
    end.

rel_vsn(RelDir, Options) ->
    case proplists:get_value(vsn, Options) of
        undefined ->
            Dir =
                case RelDir of
                    "." ->
                        {ok,Cwd} = file:get_cwd(),
                        Cwd;
                    D ->
                        D
                end,
            filename:basename(Dir);
        V when is_list(V) ->
            V;
        Other ->
            abort("Invalid release version ~p~n", [Other])
    end.

erts_vsn() ->
    erlang:system_info(version).

app_vsn(A) ->
    app_vsn(A, latest).
    %% AName = if is_atom(A) -> A;
    %%            true -> element(1, A)
    %%         end,
    %% D = code:lib_dir(AName),
    %% AppFile = filename:join(D, "ebin/" ++ atom_to_list(AName) ++ ".app"),
    %% case file:consult(AppFile) of
    %%     {ok, [{application, _, Opts}]} ->
    %%         V = proplists:get_value(vsn, Opts),
    %%         ?if_verbose(io:fwrite("app_vsn(~p) -> ~p~n", [A,V])),
    %%         V;
    %%     Other ->
    %%         abort("Oops reading .app file (~p): ~p~n", [AppFile, Other])
    %% end.

app_vsn(A, V) ->
    AppStr = atom_to_list(A),
    Path = code:get_path(),
    Found = [D || D <- Path, is_app(AppStr, D)],
    Sorted = setup_lib:sort_vsns(lists:usort(Found), AppStr),
    ?if_verbose(io:fwrite("Sorted = ~p~n", [Sorted])),
    match_app_vsn(Sorted, V, AppStr).

match_app_vsn(Vsns, latest, App) ->
    %% element(1, lists:last(Vsns));
    case Vsns of
        [] ->
            abort("No version of ~s found~n", [App]);
        [_|_] ->
            lists:last(Vsns)
    end;
match_app_vsn(Vsns, V, App) when is_list(V) ->
    case [Pair || {V1, _} = Pair <- Vsns,
                V == V1] of
        [FoundV] ->
            FoundV;
        [] ->
            abort("Cannot find version ~s of ~s~n", [V, App])
    end.


is_app(A, D) ->
    case re:run(D, A ++ "[^/]*/ebin\$") of
        {match, _} ->
            true;
        nomatch ->
            false
    end.

%% replace_versions([App|Apps], [H|T]) ->
%%     A = element(1, App),
%%     V = element(2, App),
%%     Res =
%%         if is_atom(H) ->
%%                 A = H,  % assertion
%%                 {A, V};
%%            true ->
%%                 A = element(1, H), % assertion
%%                 setelement(2, H, V)
%%         end,
%%     [Res | replace_versions(Apps, T)];
%% replace_versions([], []) ->
%%     [].

make_boot(Rel, GenTarget, Roots) ->
    Path = path(Roots),
    Vars =
        if GenTarget -> [];
           true ->
                {Vs, _} = lists:mapfoldl(
                            fun(R, N) ->
                                    V = var_name(N),
                                    {{V, R}, N+1}
                            end, 1, Roots),
                Vs
        end,
    ?if_verbose(io:fwrite("Path = ~p~n", [Path])),
    Opts = if GenTarget -> [no_module_tests];
              true      -> [no_module_tests, local, {variables, Vars}]
           end,
    Res = systools:make_script(Rel, [{path, path(Roots)}|Opts]),
    ?if_verbose(io:fwrite("make_script() -> ~p~n", [Res])).


make_install_rel({release, R, Erts, Apps}) ->
    Apps1 =
        lists:map(
          fun({setup,V,load}) ->
                  {setup, V};
             (A) ->
                  case lists:member(element(1,A), [stdlib,kernel,sasl]) of
                      true ->
                          A;
                      false ->
                          case A of
                              {Nm,Vsn} ->
                                  {Nm,Vsn,load};
                              {Nm,Vsn,Inc} when is_list(Inc) ->
                                  {Nm,Vsn,load,Inc};
                              _ ->
                                  A
                          end
                  end
          end, Apps),
    %% Apps2 = case app_vsn(setup) of
    %%          undefined ->
    %%              Apps1;
    %%          V ->
    %%              Apps1 ++ [{setup, V}]
    %%      end,
    {release, R, Erts, Apps1}.


path(Roots) ->
    [filename:join(R, "lib/*/ebin") || R <- Roots].


var_name(N) ->
    "V" ++ integer_to_list(N).
