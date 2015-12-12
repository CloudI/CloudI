%% This is a test module
-module(test).
-compile({parse_transform, test_pt}).

-export([f/1]).
-export_records([r]).
-record(r, {a = [1,2,3] :: [integer()],
            b}).

-spec f(X) -> X.
f(X) ->
    X.
