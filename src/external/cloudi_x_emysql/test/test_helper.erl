-module(test_helper).

-export([test_u/0, test_p/0]).

%% @doc test_u/0 returns the canonical user name for testing.
%%   "hello_username" by default, "travis" if in travis.
%% @end
test_u() ->
    case os:getenv("TRAVIS") of
        "true" -> "travis";
        false -> "hello_username"
    end.

%% @doc test_p/0 returns the canonical password for testing.
%%   "hello_password" by default, "" if in travis.
%% @end
test_p() ->
    case os:getenv("TRAVIS") of
        "true" -> "";
        false -> "hello_password"
    end.