%%%-------------------------------------------------------------------
%%% File     : Emysql/test/emysql_util_SUITE.erl
%%% Descr    : Suite - testing mysql_util
%%% Author   : Mike Oxford <moxford@gmail.com>
%%% Created  : 07/01/2013
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from Emysql/:
%%%     make testutil
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------
-module(emysql_util_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include("../include/emysql.hrl").

% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() ->
    [
      {group, as_dict_group},
      {group, as_proplist_group},
      {group, as_record_group}
    ].

groups() ->
    [
     {as_dict_group, [], [
              dict_empty_test,
              dict_single_test,
              dict_multi_test
             ]},
     {as_proplist_group, [], [
                  proplist_empty_test,
                  proplist_single_test,
                  proplist_multi_test
                 ]},
     {as_record_group, [], [
                record_test
               ]}
    ].

%%--------------------------------------------------------------------
init_per_suite(Config) ->
    %% if this fails, focus on environment_SUITE to fix test setup.
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

%% Test Cases: Test if emysql_util:as_dict/1 works
%%--------------------------------------------------------------------
dict_empty_test(_) ->
    E = dict:from_list([]),
    E = emysql_util:as_dict(get_empty_test()),
    E = emysql:as_dict(get_empty_test()),
    ok.

dict_single_test(_) ->
    E = dict:from_list([{<<"HelloField">>,<<"Hello">>}]),
    E = emysql_util:as_dict(get_single_test()),
    E = emysql:as_dict(get_single_test()),
    ok.

dict_multi_test(_) ->
    E = dict:from_list([
            {<<"HelloField">>,<<"Hello">>},
            {<<"HiField">>,<<"Hi">>},
            {<<"ByeField">>,<<"Bye">>}
    ]),
    E = emysql_util:as_dict(get_multi_test()),
    E = emysql:as_dict(get_multi_test()),
    ok.

%% Test Cases: Test if emysql_util:as_proplist/1 works
%%--------------------------------------------------------------------
proplist_empty_test(_) ->
    [] = emysql_util:as_proplist(get_empty_test()),
    [] = emysql:as_proplist(get_empty_test()),
    ok.

proplist_single_test(_) ->
    Expect = [ [{<<"HelloField">>,<<"Hello">>}] ],
    Expect = emysql_util:as_proplist(get_single_test()),
    Expect = emysql:as_proplist(get_single_test()),
    ok.

proplist_multi_test(_) ->
    Expect = [ [{<<"HelloField">>,<<"Hello">>},{<<"HiField">>,<<"Hi">>},{<<"ByeField">>,<<"Bye">>}] ],
    Expect = emysql_util:as_proplist(get_multi_test()),
    Expect = emysql:as_proplist(get_multi_test()),
    ok.

%% Test Cases: Test if emysql_util:as_record/3 works
%%--------------------------------------------------------------------
%% this test is from a previous author and imported here
-record(person, {surname, name, phone, socks}).
record_test(_Config) ->
    emysql:execute(test_pool, <<"DROP TABLE IF EXISTS as_record_test;">>),
    emysql:execute(test_pool, <<"CREATE TABLE as_record_test (name varchar(60), surname varchar(60), socks int)">>),

    emysql:prepare(ins, <<"INSERT INTO as_record_test (name, surname, socks) VALUES (?, ?, ?)">>),
    emysql:execute(test_pool, ins, [<<"Maxim">>, <<"Komar">>, 3]),

                        % test
    Result = emysql:execute(test_pool, <<"select * from as_record_test">>),
    Expected = [#person{ surname = <<"Komar">>, name = <<"Maxim">>, socks=3 }],
    Expected = emysql_util:as_record(Result, person, record_info(fields, person)),
    Expected = emysql:as_record(Result, person, record_info(fields, person)),
    ok.
%%--------------------------------------------------------------------

get_empty_test() ->
    #result_packet {
        seq_num = 5,
        field_list = [#field {
                        seq_num = 2,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HelloField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 15,
                        flags = 1,
                        decimals = 31 }],
        rows = undefined,
        extra = <<>>}.

get_single_test() ->
    #result_packet {
        seq_num = 5,
        field_list = [#field {
                        seq_num = 2,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HelloField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 15,
                        flags = 1,
                        decimals = 31 }],
        rows = [[<<"Hello">>]],
        extra = <<>>}.

get_multi_test() ->
    #result_packet {
        seq_num = 7,
        field_list = [#field {
                        seq_num = 2,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HelloField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 15,
                        flags = 1,
                        decimals = 31 },
                      #field {
                        seq_num = 3,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"HiField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 6,
                        flags = 1,
                        decimals = 31 },
                      #field {
                        seq_num = 4,
                        catalog = <<"def">>,
                        db = <<>>,
                        table = <<>>,
                        org_table = <<>>,
                        name = <<"ByeField">>,
                        org_name = <<>>,
                        type = 253,
                        default = <<>>,
                        charset_nr = 33,
                        length = 9,
                        flags = 1,
                        decimals = 31 }],
         rows = [[<<"Hello">>,<<"Hi">>,<<"Bye">>]],
        extra = <<>>}.
