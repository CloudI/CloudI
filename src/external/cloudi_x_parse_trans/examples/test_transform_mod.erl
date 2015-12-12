-module(test_transform_mod).
-export([ex1/0]).

-include("codegen.hrl").

ex1() ->
    parse_trans_mod:transform_module(
      ex1, [fun(Fs, _Os) ->
		    parse_trans:export_function(int, 0, Fs)
	    end,
	    fun transform_ex1/2], [{pt_pp_src,true}]).

transform_ex1(Forms, _Opts) ->
    NewF = codegen:gen_function(add, fun(A, B) ->
					     A - B
				     end),
    parse_trans:replace_function(add, 2, NewF, Forms).
