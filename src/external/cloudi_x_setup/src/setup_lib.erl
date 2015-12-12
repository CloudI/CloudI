%% -*- erlang -*-
%%==============================================================================
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
%%==============================================================================
-module(setup_lib).

-export([is_string/1,
	 sort_vsns/2,
	 compare_vsns/2,
	 releases_dir/0,
	 write_eterm/2,
	 write_script/2,
	 abort/2, help/0]).

is_string(L) ->
    lists:all(fun(X) when 0 =< X, X =< 255 -> true;
                 (_) -> false
              end, L).

write_eterm(F, Term) ->
    case file:open(F, [write]) of
        {ok, Fd} ->
            try
                io:fwrite(Fd, "~p.~n", [Term])
            after
                ok = file:close(Fd)
            end;
        Error ->
            abort("Error writing file (~s): ~p~n", [F, Error])
    end.

write_script(F, Script) ->
    case file:open(F, [write]) of
        {ok, Fd} ->
            try
                [io:fwrite(Fd, "~p.~n", [Term]) || Term <- Script]
            after
                ok = file:close(Fd)
            end;
        Error ->
            abort("Error writing file (~s): ~p~n", [F, Error])
    end.

abort(Fmt, Args) ->
    E = io_lib:fwrite(Fmt, Args),
    case get(is_escript) of
        true ->
            io:fwrite(E),
            help(),
            halt(1);
        _ ->
            erlang:error(lists:flatten(E))
    end.

sort_vsns(Dirs, AppStr) ->
    AppF = AppStr ++ ".app",
    lists:sort(fun({Va,_}, {Vb,_}) ->
                       compare_vsns(Va, Vb)
               end,
               lists:foldr(
                 fun(D, Acc) ->
                         case file:consult(
                                filename:join(D, AppF)) of
                             {ok, [{_, _, Attrs}]} ->
                                 {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
                                 [{Vsn, D} | Acc];
                             _ ->
                                 Acc
                         end
                 end, [], Dirs)).

compare_vsns(V1, V2) ->
    ToS = fun(V) ->
                  [pad_x(X) || X <- string:tokens(V, ".")]
          end,
    ToS(V1) < ToS(V2).

pad_x(S) ->
    lists:duplicate(30 - length(S), $0) ++ [flip(C) || C <- S].

flip(C) when $a =< C, C =< $z -> $A + (C - $a);
flip(C) when $A =< C, C =< $Z -> $a + (C - $A);
flip(C) -> C.

%% Almost verbatim from release_handler:init/1
releases_dir() ->
    {ok, [[Root]]} = init:get_argument(root),
    {CliDir, _Masters} = is_client(),
    case application:get_env(sasl, releases_dir) of
	undefined ->
	    case os:getenv("RELDIR") of
		R when R==false; R==[] ->
		    if CliDir == false ->
			    filename:join([Root, "releases"]);
		       true ->
			    filename:join([CliDir, "releases"])
		    end;
		RELDIR ->
		    RELDIR
	    end;
	{ok, Dir} ->
	    Dir
    end.

%% Copy-pasted from release_handler.erl
is_client() ->
    case application:get_env(masters) of
	{ok, Masters} ->
	    Alive = is_alive(),
	    case atom_list(Masters) of
		true when Alive == true ->
		    case application:get_env(client_directory) of
			{ok, ClientDir} ->
			    case int_list(ClientDir) of
				true ->
				    {ClientDir, Masters};
				_ ->
				    exit({bad_parameter, client_directory,
					  ClientDir})
			    end;
			_ ->
			    {false, false}
		    end;
		_ ->
		    exit({bad_parameter, masters, Masters})
	    end;
	_ ->
	    {false, false}
    end.

atom_list([A|T]) when is_atom(A) -> atom_list(T);
atom_list([])                    -> true;
atom_list(_)                     -> false.

int_list([I|T]) when is_integer(I) -> int_list(T);
int_list([])                       -> true;
int_list(_)                        -> false.
%% ... end copy-paste


help() ->
    io:fwrite(
      "Usage: escript setup_gen.beam Name Conf Outdir [Options]~n"
      "   or:~n"
      "       escript setup_gen.beam Options~n~n"
      "Name  : Name of release (for .rel file)~n"
      "Conf  : Name of .conf file (file:script/2 format)~n"
      "Outdir: Where to write generated files~n~n"
      "-name Name : Name of release (for .rel file)~n"
      "-root Dir  : Installation root directories"
      " (multiple -root options allowed)~n"
      "-conf F    : setup-style Conf file~n"
      "-relconf F : RelTool-style config file~n"
      "-out OutDir: Where to write generated files~n"
      "-sys F     : Name of pre-existing sys.config file~n"
      "-vsn V     : System version (otherwise derived from outdir)~n"
      "-install B : B:: true|false - whether to create install boot script~n"
      "-v         : Verbose - generate lots of output~n"
      , []).
