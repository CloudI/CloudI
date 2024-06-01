-module(setup_file).

-export([list_dir/1,
         open/2,
         close/1,
         read_file/1,
         consult/1,
         script/1,
         script/2]).

-include_lib("stdlib/include/zip.hrl").
-include_lib("kernel/include/file.hrl").

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% @doc Like {@link file:list_dir/1}, but supports paths into `zip' and `escript' archives
%%
%% This function works like `file:list_dir/1' on normal paths, but instead of failing
%% on paths that lead into archives, it does a fair job of entering the archive and
%% producing a result.
%% @end
-spec list_dir(Dir) -> {ok, Filenames} | {error, Reason} when
      Dir :: file:name_all(),
      Filenames :: [file:filename()],
      Reason :: file:posix()
              | badarg
              | {no_translation, Filename :: unicode:latin1_binary()}.
list_dir(Dir) ->
    case file:list_dir(Dir) of
        {error, enotdir} = Err ->
            case contains_zip_file(Dir) of
                {true, D} ->
                    list_dir_zip(D);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

%% @doc Like {@link file:read_file/1}, but supports paths into `zip' and `escript' archives
%%
%% This function works like `file:read_file/1' on normal paths, but instead of failing
%% on paths that lead into archives, it does a fair job of entering the archive and
%% producing a result.
%% @end
-spec read_file(Filename) -> {ok, Binary} | {error, Reason} when
      Filename :: file:name_all(),
      Binary   :: binary(),
      Reason   :: file:posix() | badarg | terminated | system_limit.
read_file(File) ->
    case file:read_file(File) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    read_file_zip(F);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

%% @doc Like {@link file:consult/1}, but supports paths into `zip' and `escript' archives
%%
%% This function works like `file:consult/1' on normal paths, but instead of failing
%% on paths that lead into archives, it does a fair job of entering the archive and
%% producing a result.
%% @end
-spec consult(Filename) -> {ok, Terms} | {error, Reason} when
      Filename :: file:name_all(),
      Terms :: [term()],
      Reason :: file:posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.
consult(File) ->
    case file:consult(File) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    consult_zip(F);
                false ->
                    Err
            end;
        Other ->
            Other
    end.


%% @doc Like {@link file:script/1}, but supports paths into `zip' and `escript' archives
%%
%% This function works like `file:script/1' on normal paths, but instead of failing
%% on paths that lead into archives, it does a fair job of entering the archive and
%% producing a result.
%% @end
-spec script(Filename) -> {ok, Value} | {error, Reason} when
      Filename :: file:name_all(),
      Value :: term(),
      Reason :: file:posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.
script(File) ->
    script(File, []).


%% @doc Like {@link file:script/2}, but supports paths into `zip' and `escript' archives
%%
%% This function works like `file:script/2' on normal paths, but instead of failing
%% on paths that lead into archives, it does a fair job of entering the archive and
%% producing a result.
%% @end
-spec script(Filename, Bindings) -> {ok, Value} | {error, Reason} when
      Filename :: file:name_all(),
      Bindings :: erl_eval:binding_struct(),
      Value :: term(),
      Reason :: file:posix() | badarg | terminated | system_limit
              | {Line :: integer(), Mod :: module(), Term :: term()}.
script(File, Bindings) ->
    case file:script(File, Bindings) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    eval_zip(F, Bindings);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

contains_zip_file(File) when is_atom(File) ->
    contains_zip_file(atom_to_binary(File, utf8));
contains_zip_file(File) ->
    case contains_ez_archive(File) of
        true -> {true, #{type => ez, file => File}};
        false ->
            contains_escript_as_dir(File)
    end.

contains_ez_archive(File) ->
    case re:run(File, "\\.ez", []) of
        {match, _} -> true;
        nomatch    -> false
    end.

contains_escript_as_dir(F) ->
    contains_escript_as_dir(fix_path_for_prim_loader(F), []).

contains_escript_as_dir(F, Acc) ->
    case file:read_link_info(F) of
        {ok, #file_info{type = regular}} ->
            case escript:extract(F, [compile_source]) of
                {ok, [{shebang, _}|Rest]} ->
                    case lists:keyfind(archive, 1, Rest) of
                        {archive, ArchiveBin} ->
                            RelF = case Acc of
                                       [] -> [];
                                       _  -> filename:join(Acc)
                                   end,
                            {true, #{ type => escript,
                                      archive_file => F,
                                      archive_bin => ArchiveBin,
                                      rel_filename => RelF }};
                        false ->
                            false
                    end;
                _ ->
                    false
            end;
        {error, enotdir} ->
            contains_escript_as_dir(filename:dirname(F), [filename:basename(F)|Acc]);
        _ ->
            false
    end.

consult_zip(#{} = ZI) ->
    case read_file_zip(ZI) of
        {ok, Bin} ->
            {ok, Fd} = open_bin(Bin, [read]),
            try consult_stream(Fd)
            after
                _ = close(Fd)
            end;
        {error, _} = Error ->
            Error
    end.

open(File, Opts) ->
    case file:open(File, Opts) of
        {error, enotdir} = Err ->
            case contains_zip_file(File) of
                {true, F} ->
                    open_zip(F, Opts);
                false ->
                    Err
            end;
        Other ->
            Other
    end.

open_zip(#{type := ez, file := File}, Opts) ->
    case check_opts(Opts) of
        ok ->
            case prim_loader_read_file(File) of
                {ok, Bin} ->
                    open_bin(Bin, Opts);
                Other ->
                    Other
            end;
        Error ->
            Error
    end.

open_bin(Bin, Opts) ->
    Opts1 = [O || O <- Opts,
                  O =/= raw], % will this work anyway?
    setup_file_io_server:start_link(self(), Bin, Opts1).

check_opts([O | _])
  when O == write;
       O == append;
       O == exclusive;
       O == delayed_write;
       O == directory ->
    {error, eacces};
check_opts([{delayed_write,_,_}|_]) ->
    {error, eacces};
check_opts([_|Opts]) ->
    check_opts(Opts);
check_opts([]) ->
    ok.

close(Fd) ->
    file:close(Fd).

consult_stream(Fd) ->
    _ = epp:set_encoding(Fd),
    consult_stream(Fd, 1, []).

consult_stream(Fd, Line, Acc) ->
    case io:read(Fd, '', Line) of
        {ok,Term,EndLine} ->
            consult_stream(Fd, EndLine, [Term|Acc]);
        {error,Error,_Line} ->
            {error,Error};
        {eof,_Line} ->
            {ok,lists:reverse(Acc)}
    end.

eval_zip(#{} = ZI, Bs) ->
    case read_file_zip(ZI) of
        {ok, Bin} ->
            {ok, Fd} = open_bin(Bin, [read]),
            try eval_stream(Fd, return, Bs)
            after
                _ = file:close(Fd)
            end;
        Error ->
            Error
    end.

%% Copied from file.erl (OTP 24.3.4.2)
eval_stream(Fd, Handling, Bs) ->
    _ = epp:set_encoding(Fd),
    eval_stream(Fd, Handling, 1, undefined, [], Bs).

eval_stream(Fd, H, Line, Last, E, Bs) ->
    eval_stream2(io:parse_erl_exprs(Fd, '', Line), Fd, H, Last, E, Bs).

eval_stream2({ok,Form,EndLine}, Fd, H, Last, E, Bs0) ->
    try erl_eval:exprs(Form, Bs0) of
        {value,V,Bs} ->
            eval_stream(Fd, H, EndLine, {V}, E, Bs)
    catch Class:Reason:StackTrace ->
            Error = {EndLine,?MODULE,{Class,Reason,StackTrace}},
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
%% - - - - 

list_dir_zip(#{type := ez, file := Dir}) ->
    prim_loader_list_dir(Dir);
list_dir_zip(#{type := escript, rel_filename := RelF} = ZI) ->
    Dir = case RelF of
              "." -> "";
              []  -> [];
              _   -> RelF ++ "/"
          end,
    with_escript_zip(
      fun(Handle) ->
              case zip:zip_list_dir(Handle) of
                  {ok, Files} ->
                      Res = lists:foldr(
                              fun(#zip_file{name = F}, Acc) ->
                                      case string:prefix(F, Dir) of
                                          nomatch -> Acc;
                                          []      -> Acc;
                                          Rest ->
                                              [H|_] = string:split(Rest, "/"),
                                              [H|Acc]
                                      end;
                                 (_, Acc) ->
                                      Acc
                              end, [], Files),
                      {ok, ordsets:from_list(Res)};
                  {error, _} = Error ->
                      Error
              end
      end, ZI).

prim_loader_list_dir(Dir0) ->
    Dir = fix_path_for_prim_loader(Dir0),
    case erl_prim_loader:list_dir(Dir) of
        error ->
            %% TODO: We could improve the error reasons here
            {error, enoent};
        {ok, _} = Ok ->
            Ok
    end.

read_file_zip(#{type := ez, file := File}) ->
    prim_loader_read_file(File);
read_file_zip(#{type := escript,
                rel_filename := RelF} = ZI) ->
    with_escript_zip(fun(Handle) ->
                             case zip:zip_get(RelF, Handle) of
                                 {ok, {_, Binary}} ->
                                     {ok, Binary};
                                 {error, _} = Error ->
                                     Error
                             end
                     end, ZI).

with_escript_zip(F, #{type := escript, archive_bin := ArchiveB}) ->
    case zip:zip_open(ArchiveB, [memory]) of
        {ok, Handle} ->
            try F(Handle)
            after
                zip:zip_close(Handle)
            end;
        {error,_} = OpenError ->
            OpenError
    end.


prim_loader_read_file(File0) ->
    File = fix_filename_for_prim_loader(File0),
    case erl_prim_loader:get_file(File) of
        {ok, Bin, _} ->
            {ok, Bin};
        error ->
            %% TODO: perhaps we can improve error reporting here
            {error, enoent}
    end.

%% the file module works with atom arguments, but let's ignore that.
fix_filename_for_prim_loader(F) when is_binary(F) ->
    binary_to_list(F);
fix_filename_for_prim_loader(F) when is_list(F) ->
    F.

%% file:list_dir/1 accepts a binary argument, incl trailing / (even multiple)
fix_path_for_prim_loader(Dir) ->
    re:replace(Dir, <<"/+$">>, <<>>, [{return,list}]).

-ifdef(TEST).

setup_file_test_() ->
    {foreach,
     fun() ->
             create_zip()
     end,
     fun(Zip) ->
             file:delete(Zip)
     end,
     [
      fun(Zip) ->
              [
               fun() -> t_consult(Zip) end,
               fun() -> t_script(Zip) end,
               fun() -> t_read_file(Zip) end,
               fun() -> t_read_file1(Zip) end,
               fun() -> t_list_dir(Zip) end,
               fun() -> t_list_dir1(Zip) end
              ]
      end]}.

setup_escript_file_test_() ->
    {foreach,
     fun() ->
             create_escript()
     end,
     fun(Zip) ->
             file:delete(Zip)
     end,
     [
      fun(Zip) ->
              [
               fun() -> t_consult(Zip) end,
               fun() -> t_script(Zip) end,
               fun() -> t_read_file(Zip) end,
               fun() -> t_read_file1(Zip) end,
               fun() -> t_list_dir(Zip) end,
               fun() -> t_list_dir1(Zip) end
              ]
      end]}.

create_zip() ->
    Z = "test.ez",
    {ok, Cwd} = file:get_cwd(),
    {ok, Bin1} = file:read_file("rebar.config"),
    {ok, Bin2} = file:read_file("rebar.config.script"),
    {ok, Bin3} = file:read_file("src/setup.app.src"),
    {ok, ZipF} = zip:create(Z, [{"rebar.config", Bin1},
                                {"rebar.config.script", Bin2},
                                {"src/setup.app.src", Bin3}],
                            [{cwd, Cwd}]),
    ZipF.

create_escript() ->
    F = "demo.escript",
    Source = ("%% Demo\nmain(_Args) ->\n"
              "    io:format(\"~p\",[erlang:system_info(schedulers)]).\n"),
    {ok, Bin} = escript:create(binary, [shebang, comment, {emu_args, "+S3"},
                                        {source, list_to_binary(Source)},
                                        {archive, ["rebar.config",
                                                   "rebar.config.script",
                                                   "src/setup.app.src"], []}]),
    ok = file:write_file(F, Bin),
    F.


t_consult(Zip) ->
    F = "rebar.config",
    {ok, Res} = file:consult(F),
    {ok, Res} = setup_file:consult(F),
    {ok, Res} = setup_file:consult(filename:join(Zip, F)),
    ok.

t_script(Zip) ->
    {ok, Cfg} = file:consult("rebar.config"),
    F = "rebar.config.script",
    Opts = [{'CONFIG', Cfg}],
    {ok, Res} = file:script(F, Opts),
    {ok, Res} = setup_file:script(F, Opts),
    {ok, Res} = setup_file:script(filename:join(Zip, F), Opts),
    ok.

t_read_file(Zip) ->
    F = "rebar.config",
    ZipF = filename:join(Zip, F),
    {ok, Bin} = file:read_file(F),
    {ok, Bin} = setup_file:read_file(F),
    {ok, Bin} = setup_file:read_file(ZipF),
    {ok, Bin} = setup_file:read_file(list_to_binary(ZipF)),
    {ok, Bin} = setup_file:read_file(list_to_atom(ZipF)),
    ok.

t_read_file1(Zip) ->
    F = "src/setup.app.src",
    ZipF = filename:join(Zip, F),
    {ok, Bin} = file:read_file(F),
    {ok, Bin} = setup_file:read_file(F),
    {ok, Bin} = setup_file:read_file(ZipF),
    {ok, Bin} = setup_file:read_file(list_to_binary(ZipF)),
    {ok, Bin} = setup_file:read_file(list_to_atom(ZipF)),
    ok.

t_list_dir(Zip) ->
    {ok, Fs} = file:list_dir("."),
    {ok, Fs} = setup_file:list_dir("."),
    {ok, Fs1} = setup_file:list_dir(Zip),
    {ok, Fs1} = setup_file:list_dir(Zip ++ "/"),
    [] = Fs1 -- Fs,
    ["rebar.config", "rebar.config.script", "src"] = lists:sort(Fs1),
    ok.

t_list_dir1(Zip) ->
    {ok, Fs} = file:list_dir("src"),
    {ok, Fs} = setup_file:list_dir("src"),
    ZipDir = filename:join(Zip, "src"),
    {ok, Fs1} = setup_file:list_dir(ZipDir),
    {ok, Fs1} = setup_file:list_dir(ZipDir ++ "/"),
    [] =  Fs1 -- Fs,
    ["setup.app.src"] = Fs1,
    ok.

-endif.
