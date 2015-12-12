-module(ex_gen_module).
-compile(export_all).

-compile({parse_transform, parse_trans_codegen}).

f() ->
    codegen:gen_module(test, [{render,0}, {source, 0}],
		       [
			{render, fun() ->
					 x end},
			{source, fun() ->
					 ok end}
		       ]).
