%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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
             {bucket, ?BUCKET},
             {ping, 1000}],
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
    Context0 = cloudi:new(),
    ServiceName = "/" ++ ?BUCKET,
    {{ok, Keys},
     Context1} = cloudi_service_db_riak:list_keys(Context0, ServiceName,
                                                  [], undefined),
    lists:foldl(fun(Key, ContextNext0) ->
        {_, ContextNext1} = cloudi_service_db_riak:delete(ContextNext0,
                                                          ServiceName,
                                                          Key, undefined),
        ContextNext1
    end, Context1, Keys),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_get_put_sequence_1(_Config) ->
    Context0 = cloudi:new(),
    ServiceName = "/" ++ ?BUCKET,
    Value0 = <<"value0">>,
    {{ok, Key0, Object0},
     Context1} = cloudi_service_db_riak:new(Context0, ServiceName,
                                            undefined, Value0,
                                            [{object, true}],
                                            undefined),
    true = is_binary(Key0),
    {{ok, Key0, Value0},
     Context2} = cloudi_service_db_riak:new(Context1, ServiceName,
                                            Key0, Value0, [],
                                            undefined),
    {{ok, Key0, Value0},
     Context3} = cloudi_service_db_riak:get(Context2, ServiceName,
                                            Object0, undefined),
    Value1 = <<"value1">>,
    Object1 = cloudi_service_db_riak:object_update(Object0, Value1),
    % can't see updated value on Object0,
    % vclock is not updated until after a put
    {{ok, Key0, Value0},
     Context4} = cloudi_service_db_riak:get(Context3, ServiceName,
                                            Object1, undefined),

    {{ok, Key0, Object2},
     Context5} = cloudi_service_db_riak:put(Context4, ServiceName,
                                            Key0, Object1,
                                            [{object, true}],
                                            undefined),
    {{ok, Key0, Value1},
     Context6} = cloudi_service_db_riak:get(Context5, ServiceName,
                                            Key0, undefined),
    {{ok, Key0, Value1},
     Context7} = cloudi_service_db_riak:get(Context6, ServiceName,
                                            Object2, undefined),
    Value2 = <<"value2">>,
    {{ok, Key0, Object3},
     Context8} = cloudi_service_db_riak:put(Context7, ServiceName,
                                            Key0, Value2,
                                            [{object, true}],
                                            undefined),
    {{ok, Key0, Value2},
     Context9} = cloudi_service_db_riak:get(Context8, ServiceName,
                                            Object3, undefined),
    {ok,
     Context10} = cloudi_service_db_riak:delete(Context9, ServiceName, Object3,
                                                [{r, all}, {w, all}],
                                                undefined),
    {{error, notfound},
     Context11} = cloudi_service_db_riak:get(Context10, ServiceName,
                                             Key0, undefined),
    {ok,
     _} = cloudi_service_db_riak:delete(Context11, ServiceName, Key0,
                                        [{r, all}, {w, all}],
                                        undefined),
    ok.

t_secondary_index_1(_Config) ->
    Context0 = cloudi:new(),
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
    {{ok, Key0, Value0},
     Context1} = cloudi_service_db_riak:new(Context0, ServiceName,
                                            Key0, Value0,
                                            [{indexes, Indexes0}],
                                            undefined),
    {{ok, Key1, Value1},
     Context2} = cloudi_service_db_riak:new(Context1, ServiceName,
                                            Key1, Value1,
                                            [{indexes, Indexes1}],
                                            undefined),
    {{ok, Key2, Value2},
     Context3} = cloudi_service_db_riak:new(Context2, ServiceName,
                                            Key2, Value2,
                                            [{indexes, Indexes2}],
                                            undefined),
    {{ok, Key3, Value3},
     Context4} = cloudi_service_db_riak:new(Context3, ServiceName,
                                            Key3, Value3,
                                            [{indexes, Indexes3}],
                                            undefined),
    {{ok, Key4, Value4},
     Context5} = cloudi_service_db_riak:new(Context4, ServiceName,
                                            Key4, Value4,
                                            [{indexes, Indexes4}],
                                            undefined),
    {{ok, Key5, Value5},
     Context6} = cloudi_service_db_riak:new(Context5, ServiceName,
                                            Key5, Value5,
                                            [{indexes, Indexes5}],
                                            undefined),
    {{ok, Key6, Value6},
     Context7} = cloudi_service_db_riak:new(Context6, ServiceName,
                                            Key6, Value6,
                                            [{indexes, Indexes6}],
                                            undefined),
    {{ok, Key7, Value7},
     Context8} = cloudi_service_db_riak:new(Context7, ServiceName,
                                            Key7, Value7,
                                            [{indexes, Indexes7}],
                                            undefined),
    {{ok, Key8, Value8},
     Context9} = cloudi_service_db_riak:new(Context8, ServiceName,
                                            Key8, Value8,
                                            [{indexes, Indexes8}],
                                            undefined),
    {{ok, Key9, Value9},
     Context10} = cloudi_service_db_riak:new(Context9, ServiceName,
                                             Key9, Value9,
                                             [{indexes, Indexes9}],
                                             undefined),
    {{ok, Key10, Value10},
     Context11} = cloudi_service_db_riak:new(Context10, ServiceName,
                                             Key10, Value10,
                                             [{indexes, Indexes10}],
                                             undefined),
    % requires that riak_kv uses riak_kv_eleveldb_backend
    % (http://docs.basho.com/riak/1.2.0/tutorials/choosing-a-backend/LevelDB/)
    {{ok,
      KeyListEven,
      undefined,
      undefined},
     Context12} = cloudi_service_db_riak:
                  get_index_eq(Context11, ServiceName,
                               {binary_index, "even"}, <<"true">>,
                               undefined),
    [<<"key00">>,<<"key02">>,<<"key04">>,
     <<"key06">>,<<"key08">>,<<"key10">>] = lists:sort(KeyListEven),
    {{ok,
      KeyListPrime,
      undefined,
      undefined},
     Context13} = cloudi_service_db_riak:
                  get_index_eq(Context12, ServiceName,
                               {binary_index, "prime"}, <<"true">>,
                               undefined),
    [<<"key01">>,<<"key02">>,<<"key03">>,
     <<"key05">>,<<"key07">>] = lists:sort(KeyListPrime),
    {{ok,
      KeyListDivBy3,
      undefined,
      undefined},
     Context14} = cloudi_service_db_riak:
                  get_index_eq(Context13, ServiceName,
                               {binary_index, "div_by_3"}, <<"true">>,
                               undefined),
    [<<"key03">>,<<"key06">>,<<"key09">>] = lists:sort(KeyListDivBy3),
    {{ok,
      KeyListGreatestDivisor,
      undefined,
      undefined},
     Context15} = cloudi_service_db_riak:
                  get_index_range(Context14, ServiceName,
                                  {integer_index, "greatest_divisor"},
                                  2, 3, undefined),
    [<<"key02">>,<<"key04">>,<<"key06">>,
     <<"key09">>] = lists:sort(KeyListGreatestDivisor),
    {ok,
     Context16} = cloudi_service_db_riak:delete(Context15,
                                                ServiceName, Key0,
                                                undefined),
    {ok,
     Context17} = cloudi_service_db_riak:delete(Context16,
                                                ServiceName, Key1,
                                                undefined),
    {ok,
     Context18} = cloudi_service_db_riak:delete(Context17,
                                                ServiceName, Key2,
                                                undefined),
    {ok,
     Context19} = cloudi_service_db_riak:delete(Context18,
                                                ServiceName, Key3,
                                                undefined),
    {ok,
     Context20} = cloudi_service_db_riak:delete(Context19,
                                                ServiceName, Key4,
                                                undefined),
    {ok,
     Context21} = cloudi_service_db_riak:delete(Context20,
                                                ServiceName, Key5,
                                                undefined),
    {ok,
     Context22} = cloudi_service_db_riak:delete(Context21,
                                                ServiceName, Key6,
                                                undefined),
    {ok,
     Context23} = cloudi_service_db_riak:delete(Context22,
                                                ServiceName, Key7,
                                                undefined),
    {ok,
     Context24} = cloudi_service_db_riak:delete(Context23,
                                                ServiceName, Key8,
                                                undefined),
    {ok,
     Context25} = cloudi_service_db_riak:delete(Context24,
                                                ServiceName, Key9,
                                                undefined),
    {ok,
     _} = cloudi_service_db_riak:delete(Context25,
                                        ServiceName, Key10,
                                        undefined),
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

