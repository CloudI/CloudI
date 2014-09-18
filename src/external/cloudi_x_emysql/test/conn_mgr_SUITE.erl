%%%-------------------------------------------------------------------
%%% File     : Emysql/test/conn_mgr_SUITE.erl
%%% Descr    : Suite #7 - Testing connection manager. 
%%% Authors  : R. Richardson, H. Diedrich
%%% Created  : 04/06/2012 ransomr
%%% Changed  : 04/07/2012 hd - added tests w/faster race provocation
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from Emysql/:
%%%     make test
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-module(conn_mgr_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/emysql.hrl").

%% Faster or more realistic testing of races of issue #9. Both are reliable.
-define(TRY_RACE, lock_and_pass_connection). % faster, focused on problem zone.
% -define(TRY_RACE, make_queries). % takes a minute, with real queries.

% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() -> 
    [ 
      stuck_waiting_1, % former two_procs
      stuck_waiting_2, 
      %pool_leak_1,     % former no_lock_timeout
      pool_leak_2,
      dying_client_does_not_lock_the_connection_out
    ].


%%--------------------------------------------------------------------
init_per_suite(Config) ->

	% if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(emysql),
    emysql:add_pool(test_pool, 1,
        test_helper:test_u(), test_helper:test_p(), "localhost", 3306,
        "hello_database", utf8),
    Config.
    
% clean up
%%--------------------------------------------------------------------
end_per_suite(_) ->
	ok.

init_per_testcase(dying_client_does_not_lock_the_connection_out, Config) ->
    LockTimeout = emysql_app:lock_timeout(),
    DefaultTimeout = emysql_app:default_timeout(),
    application:set_env(emysql, lock_timeout, 15000),
    application:set_env(emysql, default_timeout, 15000),
    [{old_sysconfig, [{lock_timeout, LockTimeout}, {default_timeout, DefaultTimeout}]} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(dying_client_does_not_lock_the_connection_out, Config) ->
    [ application:set_env(emysql, K, V) || {K, V} <- proplists:get_value(old_sysconfig, Config) ],
    Config;
end_per_testcase(_, Config) ->
    Config.

%% Test Case: Test two processes trying to share one connection
%% Test for Issue 9
%% Original name: two_procs(_)
%%--------------------------------------------------------------------
stuck_waiting_1(_) ->
    Num = 2,
    process_flag(trap_exit, true),
    [spawn_link(fun test_proc/0)
     || _ <- lists:seq(1,Num)],
    [receive
	 {'EXIT', _, Reason} -> 
	     normal = Reason
     end
     || _ <- lists:seq(1,Num)],
    ok.

%% Process that will do a bunch of requests against mysql
test_proc() ->
    [
     #result_packet{} = emysql:execute(test_pool, "describe hello_table;")
     || _ <- lists:seq(1,1000)
    ].
     
%% Test Case: Make sure that the pool is still usable after a lock timeout
%% Test for race on connection where the connection is sent to a process
%% at the exact same time it hits the timeout.
%% See ransomr's first comment on Issue 9
%% Original name: no_lock_timeout(_)
%%--------------------------------------------------------------------
pool_leak_1(_) ->
    Num = 2,
    OldEnv = application:get_env(emysql, lock_timeout),
    %% This is really hard to repro - the timing is very tricky.
    %% On my machine a timeout of 1 seems to reproduce it after about 10 attempts.
    %% Once it happens the connection is lost from the pool, so the pool needs to be reset.
    application:set_env(emysql, lock_timeout, 1),
    process_flag(trap_exit, true),
    [spawn_link(fun test_proc/0)
     || _ <- lists:seq(1,Num)],
    %% We expect one process to timeout, but the other should exit normally
    receive
	{'EXIT', _, Reason1} ->
	    connection_lock_timeout = Reason1
    end,
    receive
	{'EXIT', _, Reason2} ->
	    normal = Reason2
    end,

    case OldEnv of
	undefined ->
	    application:unset_env(emysql, lock_timeout);
	{ok, Timeout} ->
	    application:set_env(emysql, lock_timeout, Timeout)
    end,
    ok.
    
%% Test Case: Test two processes trying to share one connection.
%% Test for race Issue #9.
%% If one process times out, then it got stuck likely by the first of
%% the races described by Ransom in Issue #9.
%% This is a mod of Ransom's original two_proc test that replicates
%% the race more consistently on some machines. /hd
%%--------------------------------------------------------------------
stuck_waiting_2(_) ->
    
    % parellel processes, fast loops of sql command, timout factor vs need. 
    Processes = 2,
    Loops = 10000,
    LenienceFactor = Loops * 2,

    % find needed time for the sql command
    {Micro, _} = timer:tc(?MODULE, ?TRY_RACE, [1]),
    Timeout = trunc(Micro / 1000 * LenienceFactor) + 100,
    ct:log("Seen execution time: ~p �s (microseconds)~n", [Micro]),
    ct:log("Set timeout: ~p ms (milliseconds)~n", [Timeout]),

    %% Warn about time when run with actual MySQL query for TRY_RACE.
    Timeout > 5000 andalso
        ct:print("This race test will take approx. ~p seconds.~n", [round(Timeout/1000)]),
    
    % set timeout
    OldEnv = application:get_env(emysql, lock_timeout),
    application:set_env(emysql, lock_timeout, Timeout),
    process_flag(trap_exit, true),

    [spawn_link(conn_mgr_SUITE, ?TRY_RACE, [Loops])
     || _ <- lists:seq(1,Processes)],
    [receive
	 {'EXIT', _, normal} -> 
	    ct:log("One process complete, other could now use connections.", []);
	 {'EXIT', _, connection_lock_timeout} -> 
	    ct:log("One process exits stuck in timeout.", []),
	    ct:log("Timout was set to ~pms to be sure it can even outlast complete starvation by the other process and really indicatetes that even a free connection could be assigned to this process. Loops: ~p, used time for one sample SQL operation: ~p ms.", 
	    [Timeout, Loops, Micro/1000]),
	    exit(issue9_stuck_waiting);
	 {'EXIT', _, Reason} -> exit({unexpected_error, Reason})
     end
     || _ <- lists:seq(1,Processes)],

    case OldEnv of
	undefined ->
	    application:unset_env(emysql, lock_timeout);
	{ok, Timeout} ->
	    application:set_env(emysql, lock_timeout, Timeout)
    end,
    ok.
 
     
%% Test Case: Make sure that no connections are lost from the pool.
%% Test for race Issue #9.
%% Test for race on connection where the connection is sent to a 
%% process at the exact same time it hits the timeout.
%% This is a mod of Ransom's original pool_leak_1 test that 
%% replicates the race more consistently on some machines. /hd
%%--------------------------------------------------------------------
pool_leak_2(_) ->
    Processes = 100,
    Loops = 10,
    EmptyGb = gb_trees:empty(),
    
    %% find this output via test/index.html
    [Pool] = emysql_conn_mgr:pools(),
    ct:log("Pool available at start: ~p~n", [Pool#pool.available]),

    %% preliminary test
    [#emysql_connection{pool_id     = test_pool,
                        prepared    = EmptyGb,
                        locked_at   = undefined,
                        alive       = true,
                        test_period = 0}] = queue:to_list(Pool#pool.available),

    %% Brief timeout for test
    OldEnv = application:get_env(emysql, lock_timeout),
    application:set_env(emysql, lock_timeout, 1),

    %% Make 100 processes grap and release connections in parallel
    process_flag(trap_exit, true),
    [spawn_link(?MODULE, lock_and_pass_connection, [Loops]) 
        % Don't use ?TRY_RACE here, it races too rarely.
     || _ <- lists:seq(1,Processes)],
    [receive _ -> ok end 
     || _ <- lists:seq(1,Processes)],

    %% Test if pool still has one connection (and available)
    [Pool1] = emysql_conn_mgr:pools(),
    ct:log("Pool available after test: ~p~n", [Pool1#pool.available]),
    case queue:to_list(Pool1#pool.available) of
        [#emysql_connection{pool_id     = test_pool,
                            prepared    = EmptyGb,
                            locked_at   = undefined,
                            alive       = true,
                            test_period = 0}] -> ok;
        [] -> exit(issue9_pool_leak);
        E -> exit({unexpected_error, E})
    end,
    
    %% Refit default timout for other tests -- TODO: fix for all tests.
    case OldEnv of
	undefined ->
	    application:unset_env(emysql, lock_timeout);
	{ok, Timeout} ->
	    application:set_env(emysql, lock_timeout, Timeout)
    end,
    ok.

%% TRY_RACE: process that gets and releases a connection in fastest succession.
%% Substitute for make_queries/1, executing and also provoking races faster.
lock_and_pass_connection(Loops) ->
    [   case emysql_conn_mgr:wait_for_connection(test_pool) of
	        Connection ->
        	        emysql_conn_mgr:pass_connection(Connection)
        end
        || _ <- lists:seq(1,Loops)
    ].

%% Alternate payload of TRY_RACE. Less canceled down stand-in for above.
%% Process that will do a bunch of requests against mysql
make_queries(Loops) ->
    [ #result_packet{} = emysql:execute(test_pool, "describe hello_table")
     || _ <- lists:seq(1,Loops)
    ].

%% Test Case: Make sure that the pool is still usable after a waiting client dies
dying_client_does_not_lock_the_connection_out(_Config) ->
    #result_packet{} = emysql:execute(test_pool, "select 1"),
    F1 = fun() -> emysql:execute(test_pool, "select sleep(5);", 15000) end,
    Client = spawn(F1),
    timer:sleep(1000),

    exit(Client, kill),
    timer:sleep(1000),
    #result_packet{} = emysql:execute(test_pool, "select 1").
