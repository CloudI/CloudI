#! /usr/bin/env escript

main(_Args) ->
    CompileOpts = [debug_info],
    CheckFor = [{proper, 'PROPER', "proper.hrl"}, {eqc, 'EQC', "eqc.hrl"}],
    Opts = lists:foldl(fun({Mod, Define, Hrl}, Acc) ->
        case is_avail(Mod, Hrl) of
            false ->
                Acc;
            true ->
                [{d, Define} | Acc]
        end
    end, CompileOpts, CheckFor),
    MakeLine = {'*', Opts},
    
    FileName = filename:join(["test", "Emakefile"]),
    case file:open(FileName, [write]) of
        {ok, File} ->
            io:format(File, "~p.~n", [MakeLine]);
        Error ->
            io:format("Could not open file ~s: ~p~n", [FileName, Error])
    end.

is_avail(AppMod, Hrl) ->
    case code:lib_dir(AppMod) of
        {error, bad_name} ->
            false;
        Dir ->
            filelib:is_regular(filename:join([Dir, "include", Hrl]))
    end.


