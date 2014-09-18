%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(emysql_app).
-behaviour(application).

-export([start/2, stop/1, modules/0, default_timeout/0, lock_timeout/0, pools/0, conn_test_period/0]).

-include("emysql.hrl").

start(_Type, _StartArgs) ->

    % case StartArgs of
    %   "%MAKETIME%" -> ok; % happens with rebar build
    %   _ -> io:format("Build time: ~p~n", StartArgs)
    % end,

    emysql_sup:start_link().

stop(_State) ->
	ok = lists:foreach(
		fun (Pool) -> emysql:remove_pool(Pool#pool.pool_id) end,
		emysql_conn_mgr:pools()).

modules() ->
	{ok, Modules} = application_controller:get_key(emysql, modules),
	Modules.

default_timeout() ->
    case application:get_env(emysql, default_timeout) of
        undefined -> ?TIMEOUT;
        {ok, Timeout} -> Timeout
    end.

lock_timeout() ->
    case application:get_env(emysql, lock_timeout) of
        undefined -> ?LOCK_TIMEOUT;
        {ok, Timeout} -> Timeout
    end.

pools() ->
    case application:get_env(emysql, pools) of
        {ok, Pools} when is_list(Pools) ->
            Pools;
        _ ->
            []
    end.


conn_test_period() ->
  case application:get_env(emysql, conn_test_period) of
    undefined -> ?CONN_TEST_PERIOD;
    {ok, Period} -> Period
  end.
