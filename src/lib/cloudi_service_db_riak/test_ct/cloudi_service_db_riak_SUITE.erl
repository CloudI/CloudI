%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
-module(cloudi_service_db_riak_SUITE).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test callbacks
-export([t_get_put_sequence_1/1,
         t_secondary_index_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_RIAK_HOST, "127.0.0.1").
-define(DEFAULT_RIAK_PORT, 8087).
-define(BUCKET, "cloudi_tests_bucket").

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    test_condition([{group, get_put_sequence},
                    {group, secondary_index}]).

groups() ->
    [{get_put_sequence, [],
      [t_get_put_sequence_1]},
     {secondary_index, [],
      [t_secondary_index_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 10100}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
    {ok, [_]} = cloudi_service_api:services_add([
        {internal,
            "/",
            cloudi_service_db_riak,
            [{debug, true},
             {bucket, ?BUCKET}],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 5, 300, []}
        ], infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Context = cloudi:new(),
    ServiceName = "/" ++ ?BUCKET,
    {ok, Keys} = cloudi_service_db_riak:list_keys(Context, ServiceName,
                                                  [], undefined),
    [cloudi_service_db_riak:delete(Context, ServiceName,
                                   Key, undefined) || Key <- Keys],
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_get_put_sequence_1(_Config) ->
    Context = cloudi:new(),
    ServiceName = "/" ++ ?BUCKET,
    Value0 = <<"value0">>,
    {ok, Key0, Object0} = cloudi_service_db_riak:new(Context, ServiceName,
                                                     undefined, Value0,
                                                     [{object, true}],
                                                     undefined),
    true = is_binary(Key0),
    {ok, Key0, Value0} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key0, Value0, [],
                                                    undefined),
    {ok, Key0, Value0} = cloudi_service_db_riak:get(Context, ServiceName,
                                                    Object0, undefined),
    Value1 = <<"value1">>,
    Object1 = cloudi_service_db_riak:object_update(Object0, Value1),
    % can't see updated value on Object0,
    % vclock is not updated until after a put
    {ok, Key0, Value0} = cloudi_service_db_riak:get(Context, ServiceName,
                                                    Object1, undefined),

    {ok, Key0, Object2} = cloudi_service_db_riak:put(Context, ServiceName,
                                                     Key0, Object1,
                                                     [{object, true}],
                                                     undefined),
    {ok, Key0, Value1} = cloudi_service_db_riak:get(Context, ServiceName,
                                                    Key0, undefined),
    {ok, Key0, Value1} = cloudi_service_db_riak:get(Context, ServiceName,
                                                    Object2, undefined),
    Value2 = <<"value2">>,
    {ok, Key0, Object3} = cloudi_service_db_riak:put(Context, ServiceName,
                                                     Key0, Value2,
                                                     [{object, true}],
                                                     undefined),
    {ok, Key0, Value2} = cloudi_service_db_riak:get(Context, ServiceName,
                                                    Object3, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Object3,
                                       [{r, all}, {w, all}], undefined),
    {error, notfound} = cloudi_service_db_riak:get(Context, ServiceName,
                                                   Key0, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key0,
                                       [{r, all}, {w, all}], undefined),
    ok.

t_secondary_index_1(_Config) ->
    Context = cloudi:new(),
    ServiceName = "/" ++ ?BUCKET,
    Key0 = <<"key00">>,
    Value0 = <<"value0">>,
    Indexes0 = [{{binary_index, "even"}, [<<"true">>]},
                {{binary_index, "prime"}, [<<"false">>]},
                {{binary_index, "div_by_3"}, [<<"false">>]},
                {{integer_index, "greatest_divisor"}, [0]}],
    Key1 = <<"key01">>,
    Value1 = <<"value1">>,
    Indexes1 = [{{binary_index, "even"}, [<<"false">>]},
                {{binary_index, "prime"}, [<<"true">>]},
                {{binary_index, "div_by_3"}, [<<"false">>]},
                {{integer_index, "greatest_divisor"}, [1]}],
    Key2 = <<"key02">>,
    Value2 = <<"value2">>,
    Indexes2 = [{{binary_index, "even"}, [<<"true">>]},
                {{binary_index, "prime"}, [<<"true">>]},
                {{binary_index, "div_by_3"}, [<<"false">>]},
                {{integer_index, "greatest_divisor"}, [2]}],
    Key3 = <<"key03">>,
    Value3 = <<"value3">>,
    Indexes3 = [{{binary_index, "even"}, [<<"false">>]},
                {{binary_index, "prime"}, [<<"true">>]},
                {{binary_index, "div_by_3"}, [<<"true">>]},
                {{integer_index, "greatest_divisor"}, [1]}],
    Key4 = <<"key04">>,
    Value4 = <<"value4">>,
    Indexes4 = [{{binary_index, "even"}, [<<"true">>]},
                {{binary_index, "prime"}, [<<"false">>]},
                {{binary_index, "div_by_3"}, [<<"false">>]},
                {{integer_index, "greatest_divisor"}, [2]}],
    Key5 = <<"key05">>,
    Value5 = <<"value5">>,
    Indexes5 = [{{binary_index, "even"}, [<<"false">>]},
                {{binary_index, "prime"}, [<<"true">>]},
                {{binary_index, "div_by_3"}, [<<"false">>]},
                {{integer_index, "greatest_divisor"}, [1]}],
    Key6 = <<"key06">>,
    Value6 = <<"value6">>,
    Indexes6 = [{{binary_index, "even"}, [<<"true">>]},
                {{binary_index, "prime"}, [<<"false">>]},
                {{binary_index, "div_by_3"}, [<<"true">>]},
                {{integer_index, "greatest_divisor"}, [3]}],
    Key7 = <<"key07">>,
    Value7 = <<"value7">>,
    Indexes7 = [{{binary_index, "even"}, [<<"false">>]},
                {{binary_index, "prime"}, [<<"true">>]},
                {{binary_index, "div_by_3"}, [<<"false">>]},
                {{integer_index, "greatest_divisor"}, [1]}],
    Key8 = <<"key08">>,
    Value8 = <<"value8">>,
    Indexes8 = [{{binary_index, "even"}, [<<"true">>]},
                {{binary_index, "prime"}, [<<"false">>]},
                {{binary_index, "div_by_3"}, [<<"false">>]},
                {{integer_index, "greatest_divisor"}, [4]}],
    Key9 = <<"key09">>,
    Value9 = <<"value9">>,
    Indexes9 = [{{binary_index, "even"}, [<<"false">>]},
                {{binary_index, "prime"}, [<<"false">>]},
                {{binary_index, "div_by_3"}, [<<"true">>]},
                {{integer_index, "greatest_divisor"}, [3]}],
    Key10 = <<"key10">>,
    Value10 = <<"value10">>,
    Indexes10 = [{{binary_index, "even"}, [<<"true">>]},
                 {{binary_index, "prime"}, [<<"false">>]},
                 {{binary_index, "div_by_3"}, [<<"false">>]},
                 {{integer_index, "greatest_divisor"}, [5]}],
    {ok, Key0, Value0} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key0, Value0,
                                                    [{indexes, Indexes0}],
                                                    undefined),
    {ok, Key1, Value1} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key1, Value1,
                                                    [{indexes, Indexes1}],
                                                    undefined),
    {ok, Key2, Value2} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key2, Value2,
                                                    [{indexes, Indexes2}],
                                                    undefined),
    {ok, Key3, Value3} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key3, Value3,
                                                    [{indexes, Indexes3}],
                                                    undefined),
    {ok, Key4, Value4} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key4, Value4,
                                                    [{indexes, Indexes4}],
                                                    undefined),
    {ok, Key5, Value5} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key5, Value5,
                                                    [{indexes, Indexes5}],
                                                    undefined),
    {ok, Key6, Value6} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key6, Value6,
                                                    [{indexes, Indexes6}],
                                                    undefined),
    {ok, Key7, Value7} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key7, Value7,
                                                    [{indexes, Indexes7}],
                                                    undefined),
    {ok, Key8, Value8} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key8, Value8,
                                                    [{indexes, Indexes8}],
                                                    undefined),
    {ok, Key9, Value9} = cloudi_service_db_riak:new(Context, ServiceName,
                                                    Key9, Value9,
                                                    [{indexes, Indexes9}],
                                                    undefined),
    {ok, Key10, Value10} = cloudi_service_db_riak:new(Context, ServiceName,
                                                      Key10, Value10,
                                                      [{indexes, Indexes10}],
                                                      undefined),
    % requires that riak_kv uses riak_kv_eleveldb_backend
    % (http://docs.basho.com/riak/1.2.0/tutorials/choosing-a-backend/LevelDB/)
    {ok,
     KeyListEven,
     undefined,
     undefined} = cloudi_service_db_riak:
                  get_index_eq(Context, ServiceName,
                               {binary_index, "even"}, <<"true">>,
                               undefined),
    [<<"key00">>,<<"key02">>,<<"key04">>,
     <<"key06">>,<<"key08">>,<<"key10">>] = lists:sort(KeyListEven),
    {ok,
     KeyListPrime,
     undefined,
     undefined} = cloudi_service_db_riak:
                  get_index_eq(Context, ServiceName,
                               {binary_index, "prime"}, <<"true">>,
                               undefined),
    [<<"key01">>,<<"key02">>,<<"key03">>,
     <<"key05">>,<<"key07">>] = lists:sort(KeyListPrime),
    {ok,
     KeyListDivBy3,
     undefined,
     undefined} = cloudi_service_db_riak:
                  get_index_eq(Context, ServiceName,
                               {binary_index, "div_by_3"}, <<"true">>,
                               undefined),
    [<<"key03">>,<<"key06">>,<<"key09">>] = lists:sort(KeyListDivBy3),
    {ok,
     KeyListGreatestDivisor,
     undefined,
     undefined} = cloudi_service_db_riak:
                  get_index_range(Context, ServiceName,
                                  {integer_index, "greatest_divisor"},
                                  2, 3, undefined),
    [<<"key02">>,<<"key04">>,<<"key06">>,
     <<"key09">>] = lists:sort(KeyListGreatestDivisor),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key0, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key1, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key2, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key3, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key4, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key5, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key6, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key7, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key8, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key9, undefined),
    ok = cloudi_service_db_riak:delete(Context, ServiceName, Key10, undefined),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

test_condition(L) ->
    case gen_tcp:connect(?DEFAULT_RIAK_HOST, ?DEFAULT_RIAK_PORT, []) of
        {ok, Socket} ->
            catch gen_tcp:close(Socket),
            L;
        {error, econnrefused} ->
            error_logger:error_msg("unable to test ~p",
                                   [{?DEFAULT_RIAK_HOST,
                                     ?DEFAULT_RIAK_PORT}]),
            {skip, riak_dead};
        {error, Reason} ->
            error_logger:error_msg("unable to test ~p: ~p",
                                   [{?DEFAULT_RIAK_HOST,
                                     ?DEFAULT_RIAK_PORT}, Reason]),
            {skip, riak_dead}
    end.

