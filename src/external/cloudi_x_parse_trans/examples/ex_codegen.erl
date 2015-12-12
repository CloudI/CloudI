-module(ex_codegen).

-compile({parse_transform, parse_trans_codegen}).

-export([f/1, g/2, h/0, i/0, j/2, k/0, k/1, gen/2, fs/0]).

%%-pt_pp_forms(true).
-pt_pp_src(true).

f(Name) ->
    codegen:gen_function(
      Name,
      fun(1,2,3) ->
	      foo;
	  (A,B,C) ->
	      {A,B,C}
      end).



g(Name, V) ->
    codegen:gen_function(
      Name,
      fun(L) ->
	      member({'$var',V}, L)
      end).

h() ->
    codegen:gen_function(
      funny,
      fun() ->
	      fun gen/2
      end).

i() ->
    codegen:exprs(fun(X) ->
			  case X of 1 ->
				  is_1;
			      Other ->
				  {is_other,Other}
			  end
		  end).

j(Name, Form) ->
    codegen:gen_function(
      Name,
      fun(L) ->
	      member({'$form',Form}, L)
      end).

x() ->
    [(fun(X) ->
	      X
      end)(X1) || X1 <- [1,2]].

k() ->
    codegen:gen_function(
      lcf,
      [fun({'$var',X}) ->
	       {'$var',Y}
       end || {X, Y}  <- [{1,a},{2,b},{3,c}]]).

k(L) ->
    codegen:gen_function(
      lcf,
      [fun({'$var',X}) ->
	       {'$var',Y}
       end || {X, Y}  <- L]).

gen(Name, X) ->
    codegen:gen_function(Name, fun(L) -> lists:member({'$var',X}, L) end).

fs() ->
    V = local_V,
    codegen:gen_functions(
      [{foo, fun() ->
		     foo
	     end},
       {h1, fun h/0},
       {g1, fun g/2},
       {bar, fun() ->
		     bar
	     end}]).
