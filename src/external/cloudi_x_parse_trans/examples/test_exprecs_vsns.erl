-module(test_exprecs_vsns).

-export([f/0]).
-export_records([r]).

-compile({parse_transform, exprecs}).

-record(r, {a,b,c}).
-record(r__1_2, {a,b}).
-record(r__1_1, {a,b,c,d}).


f() ->
    io:fwrite("'#info-r'(fields) -> ~p~n", ['#info-r'(fields)]),
    io:fwrite("'#info-r__1_1'(fields)' -> ~p (not exported)~n",
	      ['#info-r__1_1'(fields)]),
    io:fwrite("'#info-r__1_2'(fields)' -> ~p (not exported)~n",
	      ['#info-r__1_2'(fields)]),
    io:fwrite("'#convert-'(\"1_1\", {r,1,2,3,4}) -> ~p~n",
	      ['#convert-'("1_1", {r,1,2,3,4})]),
    io:fwrite("'#convert-'(\"1_2\", {r,1,2}) -> ~p~n",
	      ['#convert-'("1_2", {r,1,2})]).

