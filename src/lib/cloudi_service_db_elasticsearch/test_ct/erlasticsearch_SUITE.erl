%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Mahesh Paolini-Subramanya
%%% @doc Erlastic_search tests
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erlasticsearch_SUITE).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-compile(export_all).

-define(CHECKSPEC(M,F,N), true = proper:check_spec({M,F,N})).
-define(PROPTEST(A), true = proper:quickcheck(A())).

-define(NUMTESTS, 500).
-define(DOCUMENT_DEPTH, 5).
-define(THREE_SHARDS, <<"{\"settings\":{\"number_of_shards\":3}}">>).
-define(MAPPING_KEY, <<"some_type">>).
-define(MAPPING_VALUE, <<"boolean">>).
-define(MAPPING_DOC(Type), [{Type, [{<<"properties">>, [{?MAPPING_KEY, [{<<"type">>, ?MAPPING_VALUE}]}]}]}]).
-define(ALIASES_DOC(Index, Alias), [{<<"actions">>, [[{<<"add">>, [{<<"index">>, Index}, {<<"alias">>, Alias}]}]]}]).
-define(UPDATE_KEY, <<"udpate_key">>).
-define(UPDATE_VALUE, <<"udpate_value">>).
-define(UPDATE_DOC, [{<<"doc">>, [{?UPDATE_KEY, ?UPDATE_VALUE}]}]).
% Cloudi
-define(CLOUDI_CONF, "cloudi.conf").
-define(DB_PREFIX, "/dbpopulator/elasticsearch/").
-define(DB_TARGET, "testdb").
-define(TIMEOUT, 1000).

-define(DEFAULT_THRIFT_HOST, "localhost").
-define(DEFAULT_THRIFT_PORT, 9500).
-define(ES(Prefix, Target), {internal, Prefix, cloudi_service_db_elasticsearch, [{database, Target}, {connection_options, [{thrift_host, ?DEFAULT_THRIFT_HOST}, {thrift_port, ?DEFAULT_THRIFT_PORT}]}], immediate_closest, 5000, 5000, 5000, undefined, undefined, 1, 5, 300, []}).

-record(internal,
    {
        prefix             :: string(),
        module             :: atom() | file:filename(),
        args               :: list(),
        dest_refresh       :: cloudi_service_api:dest_refresh(),
        timeout_init       :: cloudi_service_api:timeout_milliseconds(),
        timeout_async      :: cloudi_service_api:timeout_milliseconds(),
        timeout_sync       :: cloudi_service_api:timeout_milliseconds(),
        dest_list_deny     :: cloudi_service_api:dest_list(),
        dest_list_allow    :: cloudi_service_api:dest_list(),
        count_process      :: pos_integer(),
        max_r              :: non_neg_integer(),
        max_t              :: cloudi_service_api:seconds(),
        options            :: cloudi_service_api:service_options_internal()
    }).
    

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap,{seconds,320}}].

init_per_suite(Config0) ->
    setup_environment(),

    Config1 = Config0,

    % General startup
    Config2 = start(Config1),

    % Cloudi config
    Config3 = setup_cloudi_services(Config2),
    Config3.


end_per_suite(Config) ->
    stop(Config),
    ok.


init_per_group(_GroupName, Config0) ->
    Index = random_name(<<"index_">>),
    IndexWithShards = join([Index, <<"with_shards">>], <<"_">>),
    Type = random_name(<<"type_">>),

    Config1 = [{index, Index}, 
               {index_with_shards, IndexWithShards},
               {type, Type}]
                ++ Config0,
    Config1.

end_per_group(_GroupName, Config) ->
    Index = ?config(index, Config),
    % To remove these indexes...
    Config1 = [{context, cloudi:new()} | Config],
    delete_this_index(Config1, Index),
    ok.


init_per_testcase(_TestCase, Config) ->
    [{context, cloudi:new()} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

test_condition(L) ->
    case gen_tcp:connect(?DEFAULT_THRIFT_HOST, ?DEFAULT_THRIFT_PORT, []) of
        {ok, Socket} ->
            catch gen_tcp:close(Socket),
            L;
        {error, econnrefused} ->
            error_logger:error_msg("unable to test ~p",
                                   [{?DEFAULT_THRIFT_HOST,
                                     ?DEFAULT_THRIFT_PORT}]),
            {skip, elasticsearch_dead};
        {error, Reason} ->
            error_logger:error_msg("unable to test ~p: ~p",
                                   [{?DEFAULT_THRIFT_HOST,
                                     ?DEFAULT_THRIFT_PORT}, Reason]),
            {skip, elasticsearch_dead}
    end.

groups() ->
    [
        {crud_index, [],
          [t_is_index_1,
           t_is_index_all,
           t_is_type_1,
           t_is_type_all,
           t_create_index, 
           t_create_index_with_shards,
           t_open_index
         ]},
        {crud_mapping, [{repeat, 5}],
       [t_put_mapping,
        t_get_mapping,
        t_delete_mapping
      ]},

        {aliases, [{repeat, 5}],
       [t_aliases,
        t_insert_alias_1,
        t_insert_alias_2,
        t_delete_alias,
        t_is_alias,
        t_get_alias
      ]},
        {index_helpers, [],
           [t_flush_1,
           t_flush_list,
           t_flush_all,
           t_refresh_1,
           t_refresh_list,
           t_refresh_all,
           t_optimize_1,
           t_optimize_list,
           t_optimize_all,
           t_segments_1,
           t_segments_list,
           t_segments_all,
           t_status_1,
           t_status_all,
           t_clear_cache_1,
           t_clear_cache_list,
           t_clear_cache_all
   
          ]},
       % These three _MUST_ be in this sequence, and by themselves
       {crud_doc, [],
         [ t_insert_doc, 
          t_get_doc, 
          t_update_doc, 
          t_delete_doc
         ]},
       {doc_helpers, [],
          [t_is_doc,
           t_mget_index,
           t_mget_type,
           t_mget_id
         ]},
        {test, [],
         [
            t_health
         ]},
        {cluster_helpers, [],
         [t_health,
          t_state,
          t_nodes_info,
          t_nodes_stats
         ]},
   
        {search, [],
         [t_search,
          t_count,
          t_delete_by_query_param,
          t_delete_by_query_doc
         ]}
    ].

all() ->
    test_condition([
%        {group, test}
        {group, crud_index},
        {group, crud_doc},
        {group, search},
        {group, index_helpers},
        {group, cluster_helpers},
        {group, doc_helpers},
        {group, crud_mapping},
        {group, aliases}
    ]).

t_health(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, Response} = cloudi:send_sync(Context, Target, {health}),
    true = is_200(Response).

t_state(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, Response} = cloudi:send_sync(Context, Target, {state, [{filter_nodes, true}]}),
    true = is_200(Response).

t_nodes_info(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, Response} = cloudi:send_sync(Context, Target, {nodes_info}),
    true = is_200(Response).

t_nodes_stats(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, Response} = cloudi:send_sync(Context, Target, {nodes_stats}),
    true = is_200(Response).

t_status_1(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    check_status_1(Config, Index),
    delete_all_indices(Config, Index).

t_status_all(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    check_status_all(Config, Index),
    delete_all_indices(Config, Index).

t_clear_cache_1(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    clear_cache_1(Config, Index),
    delete_all_indices(Config, Index).

t_clear_cache_list(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    clear_cache_list(Config, Index),
    delete_all_indices(Config, Index).

t_clear_cache_all(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    clear_cache_all(Config, Index),
    delete_all_indices(Config, Index).

t_put_mapping(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    MappingDoc = cloudi_x_jsx:encode(?MAPPING_DOC(Type)),
    {ok, Response} = cloudi:send_sync(Context, Target, {put_mapping, Index, Type, MappingDoc}),
    true = is_200(Response),
    delete_this_index(Config, Index).

t_get_mapping(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    MappingDoc = cloudi_x_jsx:encode(?MAPPING_DOC(Type)),
    {ok, Response1} = cloudi:send_sync(Context, Target, {put_mapping, Index, Type, MappingDoc}),
    true = is_200(Response1),
    {ok, Response2} = cloudi:send_sync(Context, Target, {get_mapping, Index, Type}),
    validate_mapping(Type, Response2),
    delete_this_index(Config, Index).

validate_mapping(Type, Response) ->
    {body, Data1} = lists:keyfind(body, 1, Response),
    Data2 = case is_binary(Data1) of
        true ->
            cloudi_x_jsx:decode(Data1);
        false ->
            Data1
    end,
    {Type, Data3} = lists:keyfind(Type, 1, Data2),
    {<<"properties">>, Data4} = lists:keyfind(<<"properties">>, 1, Data3),
    {?MAPPING_KEY, [{<<"type">>, ?MAPPING_VALUE}]} = lists:keyfind(?MAPPING_KEY, 1, Data4).

t_delete_mapping(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    MappingDoc = cloudi_x_jsx:encode(?MAPPING_DOC(Type)),
    {ok, Response1} = cloudi:send_sync(Context, Target, {put_mapping, Index, Type, MappingDoc}),
    true = is_200(Response1),
    {ok, Response2} = cloudi:send_sync(Context, Target, {delete_mapping, Index, Type}),
    true = is_200(Response2),
    delete_this_index(Config, Index).

t_aliases(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Alias = random_name(Index),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    AliasesDoc = ?ALIASES_DOC(Index, Alias),
    {ok, Response} = cloudi:send_sync(Context, Target, {aliases, AliasesDoc}),
    true = is_200(Response),
    delete_this_index(Config, Index).

t_insert_alias_1(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Alias = random_name(Index),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    {ok, Response} = cloudi:send_sync(Context, Target, {insert_alias, Index, Alias}),
    true = is_200(Response),
    delete_this_index(Config, Index).

t_insert_alias_2(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Alias = random_name(Index),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    Params = [{<<"routing">>, <<"1">>}],
    {ok, Response} = cloudi:send_sync(Context, Target, {insert_alias, Index, Alias, Params}),
    true = is_200(Response),
    delete_this_index(Config, Index).

t_delete_alias(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Alias = random_name(Index),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    {ok, Response1} = cloudi:send_sync(Context, Target, {insert_alias, Index, Alias}),
    true = is_200(Response1),
    {ok, Response2} = cloudi:send_sync(Context, Target, {delete_alias, Index, Alias}),
    true = is_200(Response2),
    delete_this_index(Config, Index).

t_is_alias(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Alias = random_name(Index),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    {ok, Response1} = cloudi:send_sync(Context, Target, {insert_alias, Index, Alias}),
    true = is_200(Response1),
    true = true_response(cloudi:send_sync(Context, Target, {is_alias, Index, Alias})),
    delete_this_index(Config, Index).

t_get_alias(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Alias = random_name(Index),
    cloudi:send_sync(Context, Target, {create_index, Index}),
    {ok, Response1} = cloudi:send_sync(Context, Target, {insert_alias, Index, Alias}),
    true = is_200(Response1),
    {ok, Response2} = cloudi:send_sync(Context, Target, {get_alias, Index, Alias}),
    validate_alias(Index, Alias, Response2),
    delete_this_index(Config, Index).

validate_alias(Index, Alias, Response) ->
    {body, Data1} = lists:keyfind(body, 1, Response),
    Data2 = case is_binary(Data1) of
        true ->
            cloudi_x_jsx:decode(Data1);
        false ->
            Data1
    end,
    {Index, Data3} = lists:keyfind(Index, 1, Data2),
    {<<"aliases">>, Data4} = lists:keyfind(<<"aliases">>, 1, Data3),
    {Alias, _} = lists:keyfind(Alias, 1, Data4).


t_is_index_1(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    are_indices_1(Config, Index),
    delete_all_indices(Config, Index).

t_is_index_all(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    are_indices_all(Config, Index),
    delete_all_indices(Config, Index).

t_is_type_1(Config) ->
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    build_data(Config, Index, Type),
    are_types_1(Config, Index, Type),
    clear_data(Config, Index).

t_is_type_all(Config) ->
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    build_data(Config, Index, Type),
    are_types_1(Config, Index, Type),
    clear_data(Config, Index).

t_flush_1(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                FullIndex = join([Index, BX], <<"_">>),
                {ok, Response} = cloudi:send_sync(Context, Target, {flush, FullIndex}),
                true = is_200(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_all_indices(Config, Index).

t_flush_list(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    Indexes = 
    lists:map(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                join([Index, BX], <<"_">>)
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    {ok, Response} = cloudi:send_sync(Context, Target, {flush, Indexes}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_flush_all(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    {ok, Response} = cloudi:send_sync(Context, Target, {flush}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_refresh_1(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                FullIndex = join([Index, BX], <<"_">>),
                {ok, Response} = cloudi:send_sync(Context, Target, {refresh, FullIndex}),
                true = is_200(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_all_indices(Config, Index).

t_refresh_list(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    Indexes = 
    lists:map(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                join([Index, BX], <<"_">>)
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    {ok, Response} = cloudi:send_sync(Context, Target, {refresh, Indexes}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_refresh_all(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    {ok, Response} = cloudi:send_sync(Context, Target, {refresh}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_optimize_1(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                FullIndex = join([Index, BX], <<"_">>),
                {ok, Response} = cloudi:send_sync(Context, Target, {optimize, FullIndex}),
                true = is_200(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_all_indices(Config, Index).

t_optimize_list(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    Indexes = 
    lists:map(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                join([Index, BX], <<"_">>)
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    {ok, Response} = cloudi:send_sync(Context, Target, {optimize, Indexes}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_optimize_all(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    {ok, Response} = cloudi:send_sync(Context, Target, {optimize}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_segments_1(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                FullIndex = join([Index, BX], <<"_">>),
                {ok, Response} = cloudi:send_sync(Context, Target, {segments, FullIndex}),
                true = is_200(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_all_indices(Config, Index).

t_segments_list(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    Indexes = 
    lists:map(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                join([Index, BX], <<"_">>)
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    {ok, Response} = cloudi:send_sync(Context, Target, {segments, Indexes}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_segments_all(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    create_indices(Config, Index),
    {ok, Response} = cloudi:send_sync(Context, Target, {segments}),
    true = is_200(Response),
    delete_all_indices(Config, Index).

t_create_index(Config) ->
    Index = ?config(index, Config),
    create_indices(Config, Index),
    delete_all_indices(Config, Index).

t_create_index_with_shards(Config) ->
    Index = ?config(index_with_shards, Config),
    create_indices(Config, Index),
    delete_all_indices(Config, Index).


t_open_index(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    insert_doc(Config),
    {ok, Response} = cloudi:send_sync(Context, Target, {close_index, Index}),
    true = is_200(Response),
    {ok, Response1} = cloudi:send_sync(Context, Target, {open_index, Index}),
    true = is_200(Response1),
    delete_doc(Config).

t_mget_id(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    Query = id_query(),
    {ok, Response} = cloudi:send_sync(Context, Target, {mget_doc, Index, Type, Query}),
    ?DOCUMENT_DEPTH  = docs_from_result(Response),
    delete_doc(Config).

t_mget_type(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    Query = id_query(Type),
    {ok, Response} = cloudi:send_sync(Context, Target, {mget_doc, Index, Query}),
    ?DOCUMENT_DEPTH  = docs_from_result(Response),
    delete_doc(Config).

t_mget_index(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    Query = id_query(Index, Type),
    {ok, Response} = cloudi:send_sync(Context, Target, {mget_doc, Index, Query}),
    ?DOCUMENT_DEPTH  = docs_from_result(Response),
    delete_doc(Config).

t_search(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    lists:foreach(fun(X) ->
                Query = param_query(X),
                {ok, Response} = cloudi:send_sync(Context, Target, {search, 
                                                                    Index, Type, <<>>, [{q, Query}]}),
                % The document is structured so that the number of top level
                % keys is as (?DOCUMENT_DEPTH + 1 - X)
                ?DOCUMENT_DEPTH  = hits_from_result(Response) + X - 1
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_doc(Config).

t_count(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    lists:foreach(fun(X) ->
                Query1 = param_query(X),
                Query2 = json_query(X),

                % query as parameter
                {ok, Response1} = cloudi:send_sync(Context, Target, {count, 
                                                                    Index, Type, <<>>, [{q, Query1}]}),
                % The document is structured so that the number of top level
                % keys is as (?DOCUMENT_DEPTH + 1 - X)
                ?DOCUMENT_DEPTH  = count_from_result(Response1) + X - 1,

                % query as doc
                {ok, Response2} = cloudi:send_sync(Context, Target, {count, 
                                                                     Index, Type, Query2, []}),
                % The document is structured so that the number of top level
                % keys is as (?DOCUMENT_DEPTH + 1 - X)
                ?DOCUMENT_DEPTH  = count_from_result(Response2) + X - 1

        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_doc(Config).

t_delete_by_query_param(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    Query1 = param_query(1),
    {ok, Response1} = cloudi:send_sync(Context, Target, {count, 
                                                         Index, Type, <<>>, [{q, Query1}]}),
    5 = count_from_result(Response1),
    {ok, DResponse1} = cloudi:send_sync(Context, Target, {delete_by_query, 
                                                         Index, Type, <<>>, [{q, Query1}]}),
    true = is_200(DResponse1),
    cloudi:send_sync(Context, Target, {flush, Index}),
    {ok, DResponse1a} = cloudi:send_sync(Context, Target, {count, 
                                                         Index, Type, <<>>, [{q, Query1}]}),
    0 = count_from_result(DResponse1a),

    % All Indices
    insert_doc(Config),
    {ok, ADResponse1} = cloudi:send_sync(Context, Target, {delete_by_query, 
                                                         <<>>, [{q, Query1}]},
                                         120000),
    true = is_200(ADResponse1),
    cloudi:send_sync(Context, Target, {flush, Index}),
    {ok, ADResponse1a} = cloudi:send_sync(Context, Target, {count, 
                                                         <<>>, [{q, Query1}]}),
    0 = count_from_result(ADResponse1a).

t_delete_by_query_doc(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    Query1 = param_query(1),
    Query2 = json_query(1),
    {ok, Response1} = cloudi:send_sync(Context, Target, {count, 
                                                         Index, Type, <<>>, [{q, Query1}]}),
    5 = count_from_result(Response1),
    {ok, DResponse1} = cloudi:send_sync(Context, Target, {delete_by_query, 
                                                          Index, Type, Query2, []}),
    true = is_200(DResponse1),
    cloudi:send_sync(Context, Target, {flush, Index}),
    {ok, DResponse1a} = cloudi:send_sync(Context, Target, {count, 
                                                         Index, Type, <<>>, [{q, Query1}]}),
    0 = count_from_result(DResponse1a),

    % All Indices
    insert_doc(Config),
    {ok, ADResponse1} = cloudi:send_sync(Context, Target, {delete_by_query, Query2}, 120000),
                                                         
    true = is_200(ADResponse1),
    cloudi:send_sync(Context, Target, {flush, Index}),
    {ok, ADResponse1a} = cloudi:send_sync(Context, Target, {count, 
                                                         <<>>, [{q, Query1}]}),
    0 = count_from_result(ADResponse1a).

t_insert_doc(Config) ->
    insert_doc(Config),
    delete_doc(Config).

t_delete_doc(Config) ->
    insert_doc(Config),
    delete_doc(Config).

t_is_doc(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                true = true_response(cloudi:send_sync(Context, Target, {is_doc, Index, Type, BX}))
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_doc(Config).

t_get_doc(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                {ok, Response} = cloudi:send_sync(Context, Target, {get_doc, Index, Type, BX}),
                true = is_200(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_doc(Config).

t_update_doc(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    insert_doc(Config),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                Doc = ?UPDATE_DOC,
                {ok, Response1} = cloudi:send_sync(Context, Target, {update_doc, Index, Type, BX, Doc}),
                true = is_200(Response1),
                {ok, Response2} = cloudi:send_sync(Context, Target, {get_doc, Index, Type, BX}),
                validate_update(Response2)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    delete_doc(Config).

validate_update(Response) ->
    {body, Data1} = lists:keyfind(body, 1, Response),
    Data2 = case is_binary(Data1) of
        true ->
            cloudi_x_jsx:decode(Data1);
        false ->
            Data1
    end,
    {_, Data3} = lists:keyfind(<<"_source">>, 1, Data2),
    {?UPDATE_KEY, ?UPDATE_VALUE} = lists:keyfind(?UPDATE_KEY, 1, Data3).


check_status_1(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                FullIndex = enumerated(Index, X),
                {ok, Response} = cloudi:send_sync(Context, Target, {status, FullIndex}),
                true = is_200(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)).

check_status_all(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    FullIndexList = 
    lists:map(fun(X) ->
                enumerated(Index, X)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    {ok, Response} = cloudi:send_sync(Context, Target, {status, FullIndexList}),
    true = is_200(Response).

clear_cache_1(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                FullIndex = enumerated(Index, X),
                {ok, Response1} = cloudi:send_sync(Context, Target, {clear_cache, FullIndex}),
                true = is_200(Response1),
                {ok, Response2} = cloudi:send_sync(Context, Target, {clear_cache, FullIndex, [{filter, true}]}),
                true = is_200(Response2)
        end, lists:seq(1, ?DOCUMENT_DEPTH)).

clear_cache_list(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    FullIndexList = 
    lists:map(fun(X) ->
                enumerated(Index, X)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    {ok, Response1} = cloudi:send_sync(Context, Target, {clear_cache, FullIndexList}),
    true = is_200(Response1),
    {ok, Response2} = cloudi:send_sync(Context, Target, {clear_cache, FullIndexList, [{filter, true}]}),
    true = is_200(Response2).

clear_cache_all(Config, _Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, Response1} = cloudi:send_sync(Context, Target, {clear_cache}),
    true = is_200(Response1),
    {ok, Response2} = cloudi:send_sync(Context, Target, {clear_cache, [], [{filter, true}]}),
    true = is_200(Response2).

are_indices_1(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                FullIndex = join([Index, BX], <<"_">>),
                true = true_response(cloudi:send_sync(Context, Target, {is_index, FullIndex}))
        end, lists:seq(1, ?DOCUMENT_DEPTH)).

are_indices_all(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    FullIndexList = 
    lists:map(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                join([Index, BX], <<"_">>)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    true = true_response(cloudi:send_sync(Context, Target, {is_index, FullIndexList})).

are_types_1(Config, Index, Type) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                FullIndex = enumerated(Index, X),
                lists:foreach(fun(Y) ->
                            FullType = enumerated(Type, Y),
                            true = true_response(cloudi:send_sync(Context, Target, {is_type, FullIndex, FullType}))
                    end, lists:seq(1, ?DOCUMENT_DEPTH))
        end, lists:seq(1, ?DOCUMENT_DEPTH)).

are_types_all(Config, Index, Type) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    FullIndexList = 
    lists:map(fun(X) ->
                enumerated(Index, X)
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    FullTypeList = 
    lists:map(fun(X) ->
                enumerated(Type, X)
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    % List of indices
    lists:foreach(fun(X) ->
                FullType = enumerated(Type, X),
                true = true_response(cloudi:send_sync(Context, Target, {is_type, FullIndexList, FullType}))
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    % List of types
    lists:foreach(fun(X) ->
                FullIndex = enumerated(Index, X),
                true = true_response(cloudi:send_sync(Context, Target, {is_type, FullIndex, FullTypeList}))
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    % List of indices and types
    true = true_response(cloudi:send_sync(Context, Target, FullIndexList, FullTypeList)).

build_data(Config, Index, Type) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                FullIndex = enumerated(Index, X),
                lists:foreach(fun(Y) ->
                            FullType = enumerated(Type, Y),
                            BX = list_to_binary(integer_to_list(X)),
                            cloudi:send_sync(Context, Target, {insert_doc, FullIndex, 
                                                               FullType, BX, json_document(X)}),
                            cloudi:send_sync(Context, Target, {flush})
                    end, lists:seq(1, ?DOCUMENT_DEPTH))
        end, lists:seq(1, ?DOCUMENT_DEPTH)).

clear_data(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                FullIndex = enumerated(Index, X),
                cloudi:send_sync(Context, Target, {delete_index, FullIndex})
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    cloudi:send_sync(Context, Target, {flush}).


create_indices(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                FullIndex = join([Index, BX], <<"_">>),
                cloudi:send_sync(Context, Target, {create_index, FullIndex}),
                cloudi:send_sync(Context, Target, {flush})
        end, lists:seq(1, ?DOCUMENT_DEPTH)).

delete_all_indices(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    Index = ?config(index, Config),
    IndexWithShards = ?config(index_with_shards, Config),
    delete_all_indices(Config, Index),
    delete_all_indices(Config, IndexWithShards),
    {ok, _Response} = cloudi:send_sync(Context, Target, {flush}).

% Optionally check to see if the indices exist before trying to delete
delete_all_indices(Config, Index) ->
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                FullIndex = join([Index, BX], <<"_">>), 
                delete_this_index(Config, FullIndex)
        end, lists:seq(1, ?DOCUMENT_DEPTH)).

delete_this_index(Config, Index) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    case true_response(cloudi:send_sync(Context, Target, {is_index, Index})) of
        true -> 
            {ok, Response} = cloudi:send_sync(Context, Target, {delete_index, Index}, 120000),
            true = is_200(Response);
        false -> true
    end.

insert_doc(Config) ->
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                {ok, Response} = cloudi:send_sync(Context, Target, {insert_doc, 
                                                              Index, Type, BX, json_document(X)}),
                true = is_200_or_201(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    cloudi:send_sync(Context, Target, {flush, Index}).

delete_doc(Config) ->
    Index = ?config(index, Config),
    Type = ?config(type, Config),
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    lists:foreach(fun(X) ->
                BX = list_to_binary(integer_to_list(X)),
                {ok, Response} = cloudi:send_sync(Context, Target, {delete_doc, 
                                                                    Index, Type, BX}),
                true = is_200(Response)
        end, lists:seq(1, ?DOCUMENT_DEPTH)),
    cloudi:send_sync(Context, Target, {flush, Index}).

json_document(N) ->
    cloudi_x_jsx:to_json(document(N)).

document(N) ->
    lists:foldl(fun(X, Acc) -> 
                Tuple = case X of
                    1 -> 
                        [{key(1), value(1)}];
                    X -> 
                        [{key(X), value(X)},
                         {sub(X), document(N-1)}]
                end,
               Tuple ++ Acc
        end, [], lists:seq(1, N)).

key(N) -> data_index(key, N).
value(N) -> data_index(value, N).
sub(N) -> data_index(sub, N).
enumerated(Item, N) when is_binary(Item) ->
    data_index(list_to_atom(binary_to_list(Item)), N).

-spec data_index(atom(), integer()) -> binary().
data_index(Data, Index) ->
    BData = list_to_binary(atom_to_list(Data)),
    BIndex = list_to_binary(integer_to_list(Index)),
    join([BData, BIndex], <<"_">>).

true_response({ok, Response}) ->
    case lists:keyfind(result, 1, Response) of
        {result, true} -> true;
        {result, <<"true">>} -> true;
        _ -> false
    end.

id_query() ->
    Ids = lists:map(fun(X) -> 
                    BX = list_to_binary(integer_to_list(X)),
                    [{<<"_id">>, BX}] 
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    cloudi_x_jsx:encode([{docs, Ids}]).

id_query(Type) ->
    Ids = lists:map(fun(X) -> 
                    BX = list_to_binary(integer_to_list(X)),
                    [{<<"_type">>, Type}, {<<"_id">>, BX}] 
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    cloudi_x_jsx:encode([{docs, Ids}]).

id_query(Index, Type) ->
    Ids = lists:map(fun(X) -> 
                    BX = list_to_binary(integer_to_list(X)),
                    [{<<"_index">>, Index}, {<<"_type">>, Type}, {<<"_id">>, BX}] 
            end, lists:seq(1, ?DOCUMENT_DEPTH)),
    cloudi_x_jsx:encode([{docs, Ids}]).


param_query(X) ->
    Key = key(X),
    Value = value(X),
    join([Key, Value], <<":">>).

json_query(X) ->
    Key = key(X),
    Value = value(X),
    cloudi_x_jsx:encode([{term, [{Key, Value}]}]).

decode_body(Body) when is_binary(Body) ->
    cloudi_x_jsx:decode(Body);
decode_body(Body) -> Body.

hits_from_result(Result) ->
    {body, Body} = lists:keyfind(body, 1, Result),
    DBody = decode_body(Body),
    case lists:keyfind(<<"hits">>, 1, DBody) of
        false -> throw(false);
        {_, Value} ->
            case lists:keyfind(<<"total">>, 1, Value) of
                false -> throw(false);
                {_, Data} -> Data
            end
    end.

docs_from_result(Result) ->
    {body, Body} = lists:keyfind(body, 1, Result),
    DBody = decode_body(Body),
    case lists:keyfind(<<"docs">>, 1, DBody) of
        false -> throw(false);
        {_, Value} ->
            length(Value)
    end.


count_from_result(Result) ->
    {body, Body} = lists:keyfind(body, 1, Result),
    DBody = decode_body(Body),
    case lists:keyfind(<<"count">>, 1, DBody) of
        false -> throw(false);
        {_, Value} -> Value
    end.


setup_environment() ->
    random:seed(erlang:now()).

setup_cloudi_services(Config) ->
    Prefix = random_name(?DB_PREFIX ++ "foo"),
    Db = random_name(?DB_TARGET),
    Target = Prefix ++ Db,
    timer:sleep(?TIMEOUT),
    cloudi_service_api:services_add([?ES(Prefix, Db)], ?TIMEOUT),
    timer:sleep(?TIMEOUT),
    [{db_prefix, Prefix}, {target, Target} | Config].


start(Config) ->
    setup_cloudi(Config),
    Config.


stop(Config) ->
    Prefix = ?config(db_prefix, Config),
    unload_cloudi_service([Prefix]),
    teardown_cloudi(Config),
    ok.


is_200({error, _} = Response) -> Response;
is_200(Response) ->
    case lists:keyfind(status, 1, Response) of
        {status, 200} -> true;
        {status, <<"200">>} -> true;
        _ -> Response
    end.

is_200_or_201({error, _} = Response) -> Response;
is_200_or_201(Response) ->
    case lists:keyfind(status, 1, Response) of
        {status, 200} -> true;
        {status, <<"200">>} -> true;
        {status, 201} -> true;
        {status, <<"201">>} -> true;
        _ -> Response
    end.



join(List, Sep) when is_list(List) ->
    list_to_binary(join_list_sep(List, Sep)).

join_list_sep([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join_list_sep([], _Sep) ->
    [].
join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    lists:reverse(Acc).

%% Need to fix with a correct record format
cloudi_services() ->
    {ok, Services} = cloudi_service_api:services(?TIMEOUT),
    [{Uuid, Internal#internal.prefix} || {Uuid, Internal} <- Services].

unload_cloudi_service(Prefixes) ->
    lists:foreach(fun(Prefix) ->
                lists:foreach(fun({Uuid, X}) ->
                            case Prefix of
                                X -> 
                                    cloudi_service_api:services_remove([Uuid], ?TIMEOUT);
                                _ -> ok
                            end end, cloudi_services()) end, Prefixes).

setup_cloudi(_Config) ->
    CloudIConfig = [{acl, []}, {services, []}, {nodes, []},
                    {logging, [{file, "cloudi.log"}]}],
    ok = cloudi_x_reltool_util:application_start(cloudi_core,
                                        [{configuration, CloudIConfig}],1000).


teardown_cloudi(_Config) ->
    cloudi_x_reltool_util:application_stop(cloudi_core).

random_name(Name) when is_binary(Name) ->
    random:seed(erlang:now()),
    Id = list_to_binary(integer_to_list(random:uniform(999999999))),
    <<Name/binary, "_", Id/binary>>;

random_name(Name) when is_list(Name) ->
    random:seed(erlang:now()),
    Id = integer_to_list(random:uniform(999999999)),
    Name ++ "_" ++ Id.

