%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc type definitions and records.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

-module(erlasticsearch).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(gen_server).

-include("erlasticsearch.hrl").

%% API
-export([start/0, start/1]).
-export([stop/0, stop/1]).
-export([start_link/1]).
-export([stop_pool/1]).
-export([start_pool/1, start_pool/2, start_pool/3]).
-export([registered_pool_name/1]).
-export([get_env/2, set_env/2]).
-export([join/2]).

%% ElasticSearch
% Tests
-export([is_index/2]).
-export([is_type/3]).
-export([is_doc/4]).

% Cluster helpers
-export([health/1]).
-export([state/1, state/2]).
-export([nodes_info/1, nodes_info/2, nodes_info/3]).
-export([nodes_stats/1, nodes_stats/2, nodes_stats/3]).

% Index CRUD
-export([create_index/2, create_index/3]).
-export([delete_index/1, delete_index/2]).
-export([open_index/2]).
-export([close_index/2]).

% Doc CRUD
-export([insert_doc/5, insert_doc/6]).
-export([update_doc/5, update_doc/6]).
-export([get_doc/4, get_doc/5]).
-export([mget_doc/2, mget_doc/3, mget_doc/4]).
-export([delete_doc/4, delete_doc/5]).
-export([search/4, search/5]).
-export([count/2, count/3, count/4, count/5]).
-export([delete_by_query/2, delete_by_query/3, delete_by_query/4, delete_by_query/5]).

%% Index helpers
-export([status/2]).
-export([refresh/1, refresh/2]).
-export([flush/1, flush/2]).
-export([optimize/1, optimize/2]).
-export([clear_cache/1, clear_cache/2, clear_cache/3]).
-export([segments/1, segments/2]).

-export([is_200/1, is_200_or_201/1]).

% Mapping CRUD
-export([put_mapping/4]).
-export([get_mapping/3]).
-export([delete_mapping/3]).

% ALIASES CRUD
-export([aliases/2]).
-export([insert_alias/3, insert_alias/4]).
-export([delete_alias/3]).
-export([is_alias/3]).
-export([get_alias/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(APP, ?MODULE).

-record(state, {
        pool_name                           :: pool_name(),
        binary_response = false             :: boolean(),
        connection_options = []             :: params(),
        connection                          :: connection()}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    reltool_util:application_start(?APP).

%% @doc To start up a 'simple' client
-spec start(params()) -> {ok, pid()}.
start(Options) when is_list(Options) ->
    gen_server:start(?MODULE, [Options], []).

%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    reltool_util:application_stop(?APP).

%% @doc Stop this gen_server
-spec stop(server_ref()) -> ok | error().
stop(ServerRef) ->
    gen_server:call(ServerRef, {stop}, infinity).


%% @doc Used by Poolboy, to start 'unregistered' gen_servers
start_link(ConnectionOptions) ->
    gen_server:start_link(?MODULE, [?DEFAULT_POOL_NAME, ConnectionOptions], []).

%% @doc Name used to register the pool server
-spec registered_pool_name(pool_name()) -> registered_pool_name().
registered_pool_name(PoolName) when is_binary(PoolName) ->
    binary_to_atom(<<?REGISTERED_NAME_PREFIX, PoolName/binary, ".pool">>, utf8);
registered_pool_name({Host, Port, PoolName}) ->
    BHost = binary_host(Host),
    BPort = binary_port(Port),
    binary_to_atom(<<?REGISTERED_NAME_PREFIX, BHost/binary, "_", BPort/binary, "_", PoolName/binary, ".pool">>, utf8).

%% @doc Start a poolboy instance
-spec start_pool(pool_name()) -> supervisor:startchild_ret().
start_pool(PoolName) ->
    PoolOptions = application:get_env(erlasticsearch, pool_options, ?DEFAULT_POOL_OPTIONS),
    ConnectionOptions = application:get_env(erlasticsearch, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_pool(PoolName, PoolOptions, ConnectionOptions).

%% @doc Start a poolboy instance
-spec start_pool(pool_name(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions) when is_list(PoolOptions) ->
    ConnectionOptions = application:get_env(erlasticsearch, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_pool(PoolName, PoolOptions, ConnectionOptions).

%% @doc Start a poolboy instance with appropriate Pool & Conn settings
-spec start_pool(pool_name(), params(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions, ConnectionOptions) when is_list(PoolOptions),
                                                          is_list(ConnectionOptions) ->
    erlasticsearch_poolboy_sup:start_pool(fq_server_ref(PoolName), PoolOptions, ConnectionOptions).

%% @doc Stop a poolboy instance
-spec stop_pool(pool_name()) -> ok | error().
stop_pool(PoolName) ->
    erlasticsearch_poolboy_sup:stop_pool(fq_server_ref(PoolName)).
%%

%% @doc Get the health the  ElasticSearch cluster
-spec health(destination()) -> response().
health(Destination) ->
    route_call(Destination, {health}, infinity).

%% @equiv state(Destination, []).
-spec state(destination()) -> response().
state(Destination) ->
    state(Destination, []).

%% @doc Get the state of the  ElasticSearch cluster
-spec state(destination(), params()) -> response().
state(Destination, Params) when is_list(Params) ->
    route_call(Destination, {state, Params}, infinity).

%% @equiv nodes_info(Destination, [], []).
-spec nodes_info(destination()) -> response().
nodes_info(Destination) ->
    nodes_info(Destination, [], []).

%% @equiv nodes_info(Destination, [NodeName], []).
-spec nodes_info(destination(), node_name()) -> response().
nodes_info(Destination, NodeName) when is_binary(NodeName) ->
    nodes_info(Destination, [NodeName], []);
%% @equiv nodes_info(Destination, NodeNames, []).
nodes_info(Destination, NodeNames) when is_list(NodeNames) ->
    nodes_info(Destination, NodeNames, []).

%% @doc Get the nodes_info of the  ElasticSearch cluster
-spec nodes_info(destination(), [node_name()], params()) -> response().
nodes_info(Destination, NodeNames, Params) when is_list(NodeNames), is_list(Params) ->
    route_call(Destination, {nodes_info, NodeNames, Params}, infinity).

%% @equiv nodes_stats(Destination, [], []).
-spec nodes_stats(destination()) -> response().
nodes_stats(Destination) ->
    nodes_stats(Destination, [], []).

%% @equiv nodes_stats(Destination, [NodeName], []).
-spec nodes_stats(destination(), node_name()) -> response().
nodes_stats(Destination, NodeName) when is_binary(NodeName) ->
    nodes_stats(Destination, [NodeName], []);
%% @equiv nodes_stats(Destination, NodeNames, []).
nodes_stats(Destination, NodeNames) when is_list(NodeNames) ->
    nodes_stats(Destination, NodeNames, []).

%% @doc Get the nodes_stats of the  ElasticSearch cluster
-spec nodes_stats(destination(), [node_name()], params()) -> response().
nodes_stats(Destination, NodeNames, Params) when is_list(NodeNames), is_list(Params) ->
    route_call(Destination, {nodes_stats, NodeNames, Params}, infinity).

%% @doc Get the status of an index/indices in the  ElasticSearch cluster
-spec status(destination(), index() | [index()]) -> response().
status(Destination, Index) when is_binary(Index) ->
    status(Destination, [Index]);
status(Destination, Indexes) when is_list(Indexes)->
    route_call(Destination, {status, Indexes}, infinity).

%% @equiv create_index(Destination, Index, <<>>)
-spec create_index(destination(), index()) -> response().
create_index(Destination, Index) when is_binary(Index) ->
    create_index(Destination, Index, <<>>).

%% @doc Create an index in the ElasticSearch cluster
-spec create_index(destination(), index(), doc()) -> response().
create_index(Destination, Index, Doc) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    route_call(Destination, {create_index, Index, Doc}, infinity).

%% @doc Delete all the indices in the ElasticSearch cluster
-spec delete_index(destination()) -> response().
delete_index(Destination) ->
    delete_index(Destination, ?ALL).

%% @doc Delete an index(es) in the ElasticSearch cluster
-spec delete_index(destination(), index() | [index()]) -> response().
delete_index(Destination, Index) when is_binary(Index) ->
    delete_index(Destination, [Index]);
delete_index(Destination, Index) when is_list(Index) ->
    route_call(Destination, {delete_index, Index}, infinity).

%% @doc Open an index in the ElasticSearch cluster
-spec open_index(destination(), index()) -> response().
open_index(Destination, Index) when is_binary(Index) ->
    route_call(Destination, {open_index, Index}, infinity).

%% @doc Close an index in the ElasticSearch cluster
-spec close_index(destination(), index()) -> response().
close_index(Destination, Index) when is_binary(Index) ->
    route_call(Destination, {close_index, Index}, infinity).

%% @doc Check if an index/indices exists in the ElasticSearch cluster
-spec is_index(destination(), index() | [index()]) -> response().
is_index(Destination, Index) when is_binary(Index) ->
    is_index(Destination, [Index]);
is_index(Destination, Indexes) when is_list(Indexes) ->
    route_call(Destination, {is_index, Indexes}, infinity).

%% @equiv count(Destination, ?ALL, [], Doc []).
-spec count(destination(), doc()) -> response().
count(Destination, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    count(Destination, ?ALL, [], Doc, []).

%% @equiv count(Destination, ?ALL, [], Doc, Params).
-spec count(destination(), doc(), params()) -> response().
count(Destination, Doc, Params) when (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(Destination, ?ALL, [], Doc, Params).

%% @equiv count(Destination, Index, [], Doc, Params).
-spec count(destination(), index() | [index()], doc(), params()) -> response().
count(Destination, Index, Doc, Params) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(Destination, [Index], [], Doc, Params);
count(Destination, Indexes, Doc, Params) when is_list(Indexes) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(Destination, Indexes, [], Doc, Params).

%% @doc Get the number of matches for a query
-spec count(destination(), index() | [index()], type() | [type()], doc(), params()) -> response().
count(Destination, Index, Type, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(Destination, [Index], [Type], Doc, Params);
count(Destination, Indexes, Type, Doc, Params) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(Destination, Indexes, [Type], Doc, Params);
count(Destination, Index, Types, Doc, Params) when is_binary(Index) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    count(Destination, [Index], Types, Doc, Params);
count(Destination, Indexes, Types, Doc, Params) when is_list(Indexes) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    route_call(Destination, {count, Indexes, Types, Doc, Params}, infinity).

%% @equiv delete_by_query(Destination, ?ALL, [], Doc []).
-spec delete_by_query(destination(), doc()) -> response().
delete_by_query(Destination, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    delete_by_query(Destination, ?ALL, [], Doc, []).

%% @equiv delete_by_query(Destination, ?ALL, [], Doc, Params).
-spec delete_by_query(destination(), doc(), params()) -> response().
delete_by_query(Destination, Doc, Params) when (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(Destination, ?ALL, [], Doc, Params).

%% @equiv delete_by_query(Destination, Index, [], Doc, Params).
-spec delete_by_query(destination(), index() | [index()], doc(), params()) -> response().
delete_by_query(Destination, Index, Doc, Params) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(Destination, [Index], [], Doc, Params);
delete_by_query(Destination, Indexes, Doc, Params) when is_list(Indexes) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(Destination, Indexes, [], Doc, Params).

%% @doc Get the number of matches for a query
-spec delete_by_query(destination(), index() | [index()], type() | [type()], doc(), params()) -> response().
delete_by_query(Destination, Index, Type, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(Destination, [Index], [Type], Doc, Params);
delete_by_query(Destination, Indexes, Type, Doc, Params) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(Destination, Indexes, [Type], Doc, Params);
delete_by_query(Destination, Index, Types, Doc, Params) when is_binary(Index) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    delete_by_query(Destination, [Index], Types, Doc, Params);
delete_by_query(Destination, Indexes, Types, Doc, Params) when is_list(Indexes) andalso is_list(Types) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    route_call(Destination, {delete_by_query, Indexes, Types, Doc, Params}, infinity).

%% @doc Check if a type exists in an index/indices in the ElasticSearch cluster
-spec is_type(destination(), index() | [index()], type() | [type()]) -> response().
is_type(Destination, Index, Type) when is_binary(Index), is_binary(Type) ->
    is_type(Destination, [Index], [Type]);
is_type(Destination, Indexes, Type) when is_list(Indexes), is_binary(Type) ->
    is_type(Destination, Indexes, [Type]);
is_type(Destination, Index, Types) when is_binary(Index), is_list(Types) ->
    is_type(Destination, [Index], Types);
is_type(Destination, Indexes, Types) when is_list(Indexes), is_list(Types) ->
    route_call(Destination, {is_type, Indexes, Types}, infinity).

%% @equiv insert_doc(Destination, Index, Type, Id, Doc, []).
-spec insert_doc(destination(), index(), type(), id(), doc()) -> response().
insert_doc(Destination, Index, Type, Id, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    insert_doc(Destination, Index, Type, Id, Doc, []).

%% @doc Insert a doc into the ElasticSearch cluster
-spec insert_doc(destination(), index(), type(), id(), doc(), params()) -> response().
insert_doc(Destination, Index, Type, Id, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    route_call(Destination, {insert_doc, Index, Type, Id, Doc, Params}, infinity).

%% @equiv update_doc(Destination, Index, Type, Id, Doc, []).
-spec update_doc(destination(), index(), type(), id(), doc()) -> response().
update_doc(Destination, Index, Type, Id, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    update_doc(Destination, Index, Type, Id, Doc, []).

%% @doc Insert a doc into the ElasticSearch cluster
-spec update_doc(destination(), index(), type(), id(), doc(), params()) -> response().
update_doc(Destination, Index, Type, Id, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    route_call(Destination, {update_doc, Index, Type, Id, Doc, Params}, infinity).

%% @doc Checks to see if the doc exists
-spec is_doc(destination(), index(), type(), id()) -> response().
is_doc(Destination, Index, Type, Id) when is_binary(Index), is_binary(Type) ->
    route_call(Destination, {is_doc, Index, Type, Id}, infinity).

%% @equiv get_doc(Destination, Index, Type, Id, []).
-spec get_doc(destination(), index(), type(), id()) -> response().
get_doc(Destination, Index, Type, Id) when is_binary(Index), is_binary(Type) ->
    get_doc(Destination, Index, Type, Id, []).

%% @doc Get a doc from the ElasticSearch cluster
-spec get_doc(destination(), index(), type(), id(), params()) -> response().
get_doc(Destination, Index, Type, Id, Params) when is_binary(Index), is_binary(Type), is_list(Params)->
    route_call(Destination, {get_doc, Index, Type, Id, Params}, infinity).

%% @equiv mget_doc(Destination, <<>>, <<>>, Doc)
-spec mget_doc(destination(), doc()) -> response().
mget_doc(Destination, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    mget_doc(Destination, <<>>, <<>>, Doc).

%% @equiv mget_doc(Destination, Index, <<>>, Doc)
-spec mget_doc(destination(), index(), doc()) -> response().
mget_doc(Destination, Index, Doc) when is_binary(Index) andalso (is_binary(Doc) orelse is_list(Doc))->
    mget_doc(Destination, Index, <<>>, Doc).

%% @doc Get a doc from the ElasticSearch cluster
-spec mget_doc(destination(), index(), type(), doc()) -> response().
mget_doc(Destination, Index, Type, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc))->
    route_call(Destination, {mget_doc, Index, Type, Doc}, infinity).

%% @equiv delete_doc(Destination, Index, Type, Id, []).
-spec delete_doc(destination(), index(), type(), id()) -> response().
delete_doc(Destination, Index, Type, Id) when is_binary(Index), is_binary(Type) ->
    delete_doc(Destination, Index, Type, Id, []).
%% @doc Delete a doc from the ElasticSearch cluster
-spec delete_doc(destination(), index(), type(), id(), params()) -> response().
delete_doc(Destination, Index, Type, Id, Params) when is_binary(Index), is_binary(Type), is_list(Params)->
    route_call(Destination, {delete_doc, Index, Type, Id, Params}, infinity).

%% @equiv search(Destination, Index, Type, Doc, []).
-spec search(destination(), index(), type(), doc()) -> response().
search(Destination, Index, Type, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc))->
    search(Destination, Index, Type, Doc, []).
%% @doc Search for docs in the ElasticSearch cluster
-spec search(destination(), index(), type(), doc(), params()) -> response().
search(Destination, Index, Type, Doc, Params) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) andalso is_list(Params) ->
    route_call(Destination, {search, Index, Type, Doc, Params}, infinity).

%% @equiv refresh(Destination, ?ALL).
%% @doc Refresh all indices
%-spec refresh(destination()) -> response().
refresh(Destination) ->
    refresh(Destination, ?ALL).

%% @doc Refresh one or more indices
%-spec refresh(destination(), index() | [index()]) -> response().
refresh(Destination, Index) when is_binary(Index) ->
    refresh(Destination, [Index]);
refresh(Destination, Indexes) when is_list(Indexes) ->
    route_call(Destination, {refresh, Indexes}, infinity).

%% @doc Flush all indices
%% @equiv flush(Destination, ?ALL).
-spec flush(destination()) -> response().
flush(Destination) ->
    flush(Destination, ?ALL).

%% @doc Flush one or more indices
-spec flush(destination(), index() | [index()]) -> response().
flush(Destination, Index) when is_binary(Index) ->
    flush(Destination, [Index]);
flush(Destination, Indexes) when is_list(Indexes) ->
    route_call(Destination, {flush, Indexes}, infinity).

%% @equiv optimize(Destination, ?ALL).
%% @doc Optimize all indices
-spec optimize(destination()) -> response().
optimize(Destination) ->
    optimize(Destination, ?ALL).

%% @doc Optimize one or more indices
-spec optimize(destination(), index() | [index()]) -> response().
optimize(Destination, Index) when is_binary(Index) ->
    optimize(Destination, [Index]);
optimize(Destination, Indexes) when is_list(Indexes) ->
    route_call(Destination, {optimize, Indexes}, infinity).

%% @equiv segments(Destination, ?ALL).
%% @doc Optimize all indices
-spec segments(destination()) -> response().
segments(Destination) ->
    segments(Destination, ?ALL).

%% @doc Optimize one or more indices
-spec segments(destination(), index() | [index()]) -> response().
segments(Destination, Index) when is_binary(Index) ->
    segments(Destination, [Index]);
segments(Destination, Indexes) when is_list(Indexes) ->
    route_call(Destination, {segments, Indexes}, infinity).

%% @equiv clear_cache(Destination, ?ALL, []).
%% @doc Clear all the caches
-spec clear_cache(destination()) -> response().
clear_cache(Destination) ->
    clear_cache(Destination, ?ALL, []).

%% @equiv clear_cache(Destination, Indexes, []).
-spec clear_cache(destination(), index() | [index()]) -> response().
clear_cache(Destination, Index) when is_binary(Index) ->
    clear_cache(Destination, [Index], []);
clear_cache(Destination, Indexes) when is_list(Indexes) ->
    clear_cache(Destination, Indexes, []).

%% @equiv clear_cache(Destination, Indexes, []).
-spec clear_cache(destination(), index() | [index()], params()) -> response().
clear_cache(Destination, Index, Params) when is_binary(Index), is_list(Params) ->
    clear_cache(Destination, [Index], Params);
clear_cache(Destination, Indexes, Params) when is_list(Indexes), is_list(Params) ->
    route_call(Destination, {clear_cache, Indexes, Params}, infinity).


%% @doc Insert a mapping into an ElasticSearch index
-spec put_mapping(destination(), index() | [index()], type(), doc()) -> response().
put_mapping(Destination, Index, Type, Doc) when is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    put_mapping(Destination, [Index], Type, Doc);
put_mapping(Destination, Indexes, Type, Doc) when is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    route_call(Destination, {put_mapping, Indexes, Type, Doc}, infinity).

%% @doc Get a mapping from an ElasticSearch index
-spec get_mapping(destination(), index() | [index()], type()) -> response().
get_mapping(Destination, Index, Type) when is_binary(Index) andalso is_binary(Type) ->
    get_mapping(Destination, [Index], Type);
get_mapping(Destination, Indexes, Type) when is_list(Indexes) andalso is_binary(Type) ->
    route_call(Destination, {get_mapping, Indexes, Type}, infinity).

%% @doc Delete a mapping from an ElasticSearch index
-spec delete_mapping(destination(), index() | [index()], type()) -> response().
delete_mapping(Destination, Index, Type) when is_binary(Index) andalso is_binary(Type) ->
    delete_mapping(Destination, [Index], Type);
delete_mapping(Destination, Indexes, Type) when is_list(Indexes) andalso is_binary(Type) ->
    route_call(Destination, {delete_mapping, Indexes, Type}, infinity).

%% @doc Operate on aliases (as compared to 'alias')
-spec aliases(destination(), doc()) -> response().
aliases(Destination, Doc) when (is_binary(Doc) orelse is_list(Doc)) ->
    route_call(Destination, {aliases, Doc}, infinity).

%% @doc Insert an alias (as compared to 'aliases')
-spec insert_alias(destination(), index(), index()) -> response().
insert_alias(Destination, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    route_call(Destination, {insert_alias, Index, Alias}, infinity).
%% @doc Insert an alias with options(as compared to 'aliases')
-spec insert_alias(destination(), index(), index(), doc()) -> response().
insert_alias(Destination, Index, Alias, Doc) when is_binary(Index) andalso is_binary(Alias) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    route_call(Destination, {insert_alias, Index, Alias, Doc}, infinity).

%% @doc Delete an alias (as compared to 'aliases')
-spec delete_alias(destination(), index(), index()) -> response().
delete_alias(Destination, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    route_call(Destination, {delete_alias, Index, Alias}, infinity).

%% @doc Checks if an alias exists (Alias can be a string with a wildcard)
-spec is_alias(destination(), index(), index()) -> response().
is_alias(Destination, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    route_call(Destination, {is_alias, Index, Alias}, infinity).

%% @doc Gets an alias(or more, based on the string)
-spec get_alias(destination(), index(), index()) -> response().
get_alias(Destination, Index, Alias) when is_binary(Index) andalso is_binary(Alias) ->
    route_call(Destination, {get_alias, Index, Alias}, infinity).

%% @doc The official way to get a value from this application's env.
%%      Will return Default if that key is unset.
-spec get_env(Key :: atom(), Default :: term()) -> term().
get_env(Key, Default) ->
    case application:get_env(?APP, Key) of
        {ok, Value} ->
            Value;
        _ ->
            Default
    end.
-spec is_200(response()) -> boolean().
is_200({error, _} = Response) -> Response;
is_200(Response) ->
    case lists:keyfind(status, 1, Response) of
        {status, 200} -> true;
        {status, <<"200">>} -> true;
        _ -> false
    end.

-spec is_200_or_201(response()) -> boolean().
is_200_or_201({error, _} = Response) -> Response;
is_200_or_201(Response) ->
    case lists:keyfind(status, 1, Response) of
        {status, 200} -> true;
        {status, <<"200">>} -> true;
        {status, 201} -> true;
        {status, <<"201">>} -> true;
        _ -> false
    end.

%% @doc Join a a list of strings into one string, adding a separator between
%%      each string.
-spec join([binary()], Sep::binary()) -> binary().
join(List, Sep) when is_list(List) ->
    list_to_binary(join_list_sep(List, Sep)).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([PoolName, Options0]) ->
    {DecodeResponse, ConnectionOptions} = case lists:keytake(binary_response, 1, Options0) of
        {value, {binary_response, Decode}, Options1} -> {Decode, Options1};
        false -> {true, Options0}
    end,
    Connection = connection(ConnectionOptions),
    {ok, #state{pool_name = PoolName, 
                binary_response = DecodeResponse,
                connection_options = ConnectionOptions,
                connection = Connection}}.

handle_call({stop}, _From, State) ->
    thrift_client:close(State#state.connection),
    {stop, normal, ok, State};

handle_call({_Request = health}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_health(),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = state, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_state(Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = nodes_info, NodeNames, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_nodes_info(NodeNames, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = nodes_stats, NodeNames, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_nodes_stats(NodeNames, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = status, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_status(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = create_index, Index, Doc}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_create_index(Index, Doc),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = delete_index, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_delete_index(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = open_index, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_open_index(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = close_index, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_close_index(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = count, Index, Type, Doc, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_count(Index, Type, Doc, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = delete_by_query, Index, Type, Doc, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_delete_by_query(Index, Type, Doc, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = is_index, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_is_index(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    Result = make_boolean_response(Response, State),
    {reply, Result, State#state{connection = Connection1}};

handle_call({_Request = is_type, Index, Type}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_is_type(Index, Type),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    Result = make_boolean_response(Response, State),

    {reply, Result, State#state{connection = Connection1}};

handle_call({_Request = insert_doc, Index, Type, Id, Doc, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_insert_doc(Index, Type, Id, Doc, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = update_doc, Index, Type, Id, Doc, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_update_doc(Index, Type, Id, Doc, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = get_doc, Index, Type, Id, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_get_doc(Index, Type, Id, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = mget_doc, Index, Type, Doc}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_mget_doc(Index, Type, Doc),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = is_doc, Index, Type, Id}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_is_doc(Index, Type, Id),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    Result = make_boolean_response(Response, State),
    {reply, Result, State#state{connection = Connection1}};

handle_call({_Request = delete_doc, Index, Type, Id, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_delete_doc(Index, Type, Id, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = search, Index, Type, Doc, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_search(Index, Type, Doc, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = refresh, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_refresh(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = flush, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_flush(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = optimize, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_optimize(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = segments, Index}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_segments(Index),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = clear_cache, Index, Params}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_clear_cache(Index, Params),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = put_mapping, Indexes, Type, Doc}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_put_mapping(Indexes, Type, Doc),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = get_mapping, Indexes, Type}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_get_mapping(Indexes, Type),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = delete_mapping, Indexes, Type}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_delete_mapping(Indexes, Type),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = aliases, Doc}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_aliases(Doc),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = insert_alias, Index, Alias}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_insert_alias(Index, Alias),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = insert_alias, Index, Alias, Doc}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_insert_alias(Index, Alias, Doc),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = delete_alias, Index, Alias}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_delete_alias(Index, Alias),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call({_Request = is_alias, Index, Alias}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_is_alias(Index, Alias),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    Result = make_boolean_response(Response, State),
    {reply, Result, State#state{connection = Connection1}};

handle_call({_Request = get_alias, Index, Alias}, _From, State = #state{connection = Connection0}) ->
    RestRequest = rest_request_get_alias(Index, Alias),
    {Connection1, Response} = process_request(Connection0, RestRequest, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call(_Request, _From, State) ->
    thrift_client:close(State#state.connection),
    {stop, unhandled_call, State}.

handle_cast(_Request, State) ->
    thrift_client:close(State#state.connection),
    {stop, unhandled_info, State}.

handle_info(_Info, State) ->
    thrift_client:close(State#state.connection),
    {stop, unhandled_info, State}.

terminate(_Reason, State) ->
    thrift_client:close(State#state.connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%% @doc Build a new connection
-spec connection(params()) -> connection().
connection(ConnectionOptions) ->
    ThriftHost = proplists:get_value(thrift_host, ConnectionOptions, ?DEFAULT_THRIFT_HOST),
    ThriftPort = proplists:get_value(thrift_port, ConnectionOptions, ?DEFAULT_THRIFT_PORT),
    ThriftOptions = case lists:keyfind(thrift_options, 1, ConnectionOptions) of
        {thrift_options, Options} -> Options;
        false -> []
    end,
    {ok, Connection} = thrift_client_util:new(ThriftHost, ThriftPort, elasticsearch_thrift, ThriftOptions),
    Connection.


%% @doc Sets the value of the configuration parameter Key for this application.
-spec set_env(Key :: atom(), Value :: term()) -> ok.
set_env(Key, Value) ->
    application:set_env(?APP, Key, Value).

%% @doc Process the request over thrift
%%      In case the network blipped and the thrift connection vanishes,
%%      this will retry the request (w/ a new thrift connection)
%%      before choking
-spec process_request(connection(), rest_request(), #state{}) -> {connection(), response()}.
process_request(Connection, Request, State) ->
    try process_request_1(_Retry = true, Connection, Request, State)
    catch
        Exception:Reason ->
            case {Exception, Reason} of
                {throw, {retry_request, true}} ->
                    process_request_1(false, undefined, Request, State);
                _ ->
                    {undefined, ?CONNECTION_REFUSED}
            end
    end.

%% @doc Actually perform the request
-spec process_request_1(boolean(), connection(), rest_request(), #state{}) -> {connection(), response()}.
process_request_1(Retry, undefined, Request, State = #state{connection_options = ConnectionOptions,
                                                   binary_response = BinaryResponse}) ->
    Connection = connection(ConnectionOptions),
    {Connection1, RestResponse} = do_request(Retry, Connection, {'execute', [Request]}, State),
    {Connection1, process_response(BinaryResponse, RestResponse)};
process_request_1(Retry, Connection, Request, State = #state{binary_response = BinaryResponse}) ->
    {Connection1, RestResponse} = do_request(Retry, Connection, {'execute', [Request]}, State),
    {Connection1, process_response(BinaryResponse, RestResponse)}.

-spec do_request(boolean(), connection(), {'execute', [rest_request()]}, #state{}) -> {connection(),  {ok, rest_response()} | error() | exception()}.
do_request(Retry, Connection, {Function, Args}, _State) ->
    Args2 =
        case Args of
            [#restRequest{body=Body}] when is_binary(Body) ->
                Args;
            [A=#restRequest{body=Body}] when is_list(Body) ->
                [A#restRequest{body=jsx:encode(Body)}]
        end,
    try thrift_client:call(Connection, Function, Args2) of
        {Connection1, Response = {ok, _}} ->
            {Connection1, Response};
        {Connection1, Response = {error, _}} ->
            {Connection1, Response}
    catch
        Exception:Reason ->
            case {Exception, Reason} of
                {throw, {Connection1, Response = {exception, _}}} ->
                    {Connection1, Response};
                % Thrift client closes the connection
                {error, {case_clause, {error, closed}}} ->
                    if Retry =:= false -> {undefined, ?CONNECTION_REFUSED};
                        true -> throw({retry_request, Retry})
                    end;
                {error, {case_clause,{error, econnrefused}}} ->
                    if Retry =:= false -> {undefined, ?CONNECTION_REFUSED};
                        true -> throw({retry_request, Retry})
                    end;
                {error, badarg} ->
                    {Connection, {error, badarg}}

            end
    end.


-spec process_response(boolean(), {ok, rest_response()} | error() | exception()) -> response().
process_response(_, {error, _} = Response) ->
    Response;
process_response(true, {ok, #restResponse{status = Status, body = undefined}}) ->
    [{status, erlang:integer_to_binary(Status)}];
process_response(false, {ok, #restResponse{status = Status, body = undefined}}) ->
    [{status, Status}];
process_response(true, {ok, #restResponse{status = Status, body = Body}}) ->
    [{status, erlang:integer_to_binary(Status)}, {body, Body}];
process_response(false, {ok, #restResponse{status = Status, body = Body}}) ->
    [{status, Status}, {body, jsx:decode(Body)}].


%% @doc Build a new rest request
rest_request_health() ->
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = ?HEALTH}.

rest_request_state(Params) when is_list(Params) ->
    Uri = make_uri([?STATE], Params),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

rest_request_nodes_info(NodeNames, Params) when is_list(NodeNames),
                                                is_list(Params) ->
    NodeNameList = join(NodeNames, <<",">>),
    Uri = make_uri([?NODES, NodeNameList], Params),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

rest_request_nodes_stats(NodeNames, Params) when is_list(NodeNames),
                                                is_list(Params) ->
    NodeNameList = join(NodeNames, <<",">>),
    Uri = make_uri([?NODES, NodeNameList, ?STATS], Params),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

rest_request_status(Index) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, ?STATUS], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

rest_request_create_index(Index, Doc) when is_binary(Index) andalso
                                              (is_binary(Doc) orelse is_list(Doc)) ->
    #restRequest{method = ?elasticsearch_Method_PUT,
                 uri = Index,
                 body = Doc}.

rest_request_delete_index(Index) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    #restRequest{method = ?elasticsearch_Method_DELETE,
                 uri = IndexList}.

rest_request_open_index(Index) when is_binary(Index) ->
    Uri = join([Index, ?OPEN], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri}.

rest_request_close_index(Index) when is_binary(Index) ->
    Uri = join([Index, ?CLOSE], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri}.

rest_request_count(Index, Type, Doc, Params) when is_list(Index) andalso
                                        is_list(Type) andalso
                                        (is_binary(Doc) orelse is_list(Doc)) andalso
                                        is_list(Params) ->
    IndexList = join(Index, <<",">>),
    TypeList = join(Type, <<",">>),
    Uri = make_uri([IndexList, TypeList, ?COUNT], Params),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri,
                 body = Doc}.

rest_request_delete_by_query(Index, Type, Doc, Params) when is_list(Index) andalso
                                        is_list(Type) andalso
                                        (is_binary(Doc) orelse is_list(Doc)) andalso
                                        is_list(Params) ->
    IndexList = join(Index, <<",">>),
    TypeList = join(Type, <<",">>),
    Uri = make_uri([IndexList, TypeList, ?QUERY], Params),
    #restRequest{method = ?elasticsearch_Method_DELETE,
                 uri = Uri,
                 body = Doc}.

rest_request_is_index(Index) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    #restRequest{method = ?elasticsearch_Method_HEAD,
                 uri = IndexList}.

rest_request_is_type(Index, Type) when is_list(Index),
                                        is_list(Type) ->
    IndexList = join(Index, <<",">>),
    TypeList = join(Type, <<",">>),
    Uri = join([IndexList, TypeList], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_HEAD,
                 uri = Uri}.

rest_request_insert_doc(Index, Type, undefined, Doc, Params) when is_binary(Index) andalso
                                                      is_binary(Type) andalso
                                                      (is_binary(Doc) orelse is_list(Doc)) andalso
                                                      is_list(Params) ->
    Uri = make_uri([Index, Type], Params),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri,
                 body = Doc};

rest_request_insert_doc(Index, Type, Id, Doc, Params) when is_binary(Index) andalso
                                                      is_binary(Type) andalso
                                                      is_binary(Id) andalso
                                                      (is_binary(Doc) orelse is_list(Doc)) andalso
                                                      is_list(Params) ->
    Uri = make_uri([Index, Type, Id], Params),
    #restRequest{method = ?elasticsearch_Method_PUT,
                 uri = Uri,
                 body = Doc}.

rest_request_update_doc(Index, Type, Id, Doc, Params) when is_binary(Index) andalso
                                                      is_binary(Type) andalso
                                                      is_binary(Id) andalso
                                                      (is_binary(Doc) orelse is_list(Doc)) andalso
                                                      is_list(Params) ->
    Uri = make_uri([Index, Type, Id, ?UPDATE], Params),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri,
                 body = Doc}.

rest_request_is_doc(Index, Type, Id) when is_binary(Index),
                                                   is_binary(Type),
                                                   is_binary(Id) ->
    Uri = make_uri([Index, Type, Id], []),
    #restRequest{method = ?elasticsearch_Method_HEAD,
                 uri = Uri}.

rest_request_get_doc(Index, Type, Id, Params) when is_binary(Index),
                                                   is_binary(Type),
                                                   is_binary(Id),
                                                   is_list(Params) ->
    Uri = make_uri([Index, Type, Id], Params),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

rest_request_mget_doc(Index, Type, Doc) when is_binary(Index) andalso
                                                   is_binary(Type) andalso
                                                   (is_binary(Doc) orelse is_list(Doc)) ->
    Uri = make_uri([Index, Type, ?MGET], []),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri,
                 body = Doc}.

rest_request_delete_doc(Index, Type, Id, Params) when is_binary(Index),
                                                   is_binary(Type),
                                                   is_binary(Id),
                                                   is_list(Params) ->
    Uri = make_uri([Index, Type, Id], Params),
    #restRequest{method = ?elasticsearch_Method_DELETE,
                 uri = Uri}.

rest_request_search(Index, Type, Doc, Params) when is_binary(Index) andalso
                                                   is_binary(Type) andalso
                                                   (is_binary(Doc) orelse is_list(Doc)) andalso
                                                   is_list(Params) ->
    Uri = make_uri([Index, Type, ?SEARCH], Params),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri,
                 body = Doc}.

rest_request_refresh(Index) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, ?REFRESH], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri}.

rest_request_flush(Index) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, ?FLUSH], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri}.

rest_request_optimize(Index) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, ?OPTIMIZE], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri}.

rest_request_segments(Index) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, ?SEGMENTS], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

rest_request_clear_cache(Index, Params) when is_list(Index) ->
    IndexList = join(Index, <<",">>),
    Uri = make_uri([IndexList, ?CLEAR_CACHE], Params),
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri}.

rest_request_put_mapping(Index, Type, Doc) when is_list(Index),
                                                is_binary(Type),
                                                (is_binary(Doc) orelse is_list(Doc)) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, Type, ?MAPPING], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_PUT,
                 uri = Uri,
                 body = Doc}.

rest_request_get_mapping(Index, Type) when is_list(Index),
                                           is_binary(Type) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, Type, ?MAPPING], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

rest_request_delete_mapping(Index, Type) when is_list(Index),
                                              is_binary(Type) ->
    IndexList = join(Index, <<",">>),
    Uri = join([IndexList, Type, ?MAPPING], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_DELETE,
                 uri = Uri}.

rest_request_aliases(Doc) when is_binary(Doc) orelse is_list(Doc) ->
    Uri = ?ALIASES,
    #restRequest{method = ?elasticsearch_Method_POST,
                 uri = Uri,
                 body = Doc}.

rest_request_insert_alias(Index, Alias) when is_binary(Index),
                                             is_binary(Alias) ->
    Uri = join([Index, ?ALIAS, Alias], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_PUT,
                 uri = Uri}.

rest_request_insert_alias(Index, Alias, Doc) when is_binary(Index),
                                                  is_binary(Alias),
                                                  (is_binary(Doc) orelse is_list(Doc)) ->
    Uri = join([Index, ?ALIAS, Alias], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_PUT,
                 uri = Uri,
                 body = Doc}.

rest_request_delete_alias(Index, Alias) when is_binary(Index),
                                             is_binary(Alias) ->
    Uri = join([Index, ?ALIAS, Alias], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_DELETE,
                 uri = Uri}.

rest_request_is_alias(Index, Alias) when is_binary(Index),
                                         is_binary(Alias) ->
    Uri = join([Index, ?ALIAS, Alias], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_HEAD,
                 uri = Uri}.

rest_request_get_alias(Index, Alias) when is_binary(Index),
                                          is_binary(Alias) ->
    Uri = join([Index, ?ALIAS, Alias], <<"/">>),
    #restRequest{method = ?elasticsearch_Method_GET,
                 uri = Uri}.

%% @doc Send the request to the gen_server
-spec route_call(destination(), tuple(), timeout()) -> response().
% When this comes back from Poolboy, ServerRef is a pid
%       optionally, atom for internal testing
route_call(ServerRef, Command, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    gen_server:call(ServerRef, Command, Timeout);
%% @doc Send the request to poolboy
route_call(Destination, Command, Timeout) ->
    pool_call(fq_server_ref(Destination), Command, Timeout).

-spec pool_call(fq_server_ref(), tuple(), timeout()) ->response().
pool_call(FqServerRef, Command, Timeout) ->
    PoolId = registered_pool_name(FqServerRef),
    TransactionFun = fun() ->
            poolboy:transaction(PoolId, fun(Worker) ->
                        gen_server:call(Worker, Command, Timeout)
                end) end,
    try
        TransactionFun()
    % If the pool doesnt' exist, the keyspace has not been set before
    catch
        exit:{noproc, _} ->
            start_pool(FqServerRef),
            TransactionFun()
    end.

-spec join_list_sep([binary()], binary()) -> [any()].
join_list_sep([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join_list_sep([], _Sep) ->
    [].
join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    lists:reverse(Acc).

%% @doc Make a complete URI based on the tokens and props
make_uri(BaseList, PropList) ->
    Base = join(BaseList, <<"/">>),
    case PropList of
        [] ->
            Base;
        PropList ->
            Props = uri_params_encode(PropList),
            join([Base, Props], <<"?">>)
    end.

% Thanks to https://github.com/tim/erlang-oauth.git
-spec uri_params_encode([tuple()]) -> string().
uri_params_encode(Params) ->
  intercalate("&", [uri_join([K, V], "=") || {K, V} <- Params]).

uri_join(Values, Separator) ->
  string:join([uri_encode(Value) || Value <- Values], Separator).

intercalate(Sep, Xs) ->
  lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) ->
  [];
intersperse(_, [X]) ->
  [X];
intersperse(Sep, [X | Xs]) ->
  [X, Sep | intersperse(Sep, Xs)].

uri_encode(Term) when is_binary(Term) ->
    binary_to_list(Term);
uri_encode(Term) when is_integer(Term) ->
  integer_to_list(Term);
uri_encode(Term) when is_atom(Term) ->
  uri_encode(atom_to_list(Term));
uri_encode(Term) when is_list(Term) ->
  uri_encode(lists:reverse(Term, []), []).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

uri_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
  NewAcc = [$%, dec2hex(X bsr 4), dec2hex(X band 16#0f) | Acc],
  uri_encode(T, NewAcc);
uri_encode([], Acc) ->
  Acc.

-compile({inline, [{dec2hex, 1}]}).

dec2hex(N) when N >= 10 andalso N =< 15 ->
  N + $A - 10;
dec2hex(N) when N >= 0 andalso N =< 9 ->
  N + $0.

-spec make_boolean_response(response(), #state{}) -> response().
make_boolean_response({error, _} = Response, _) -> Response;
make_boolean_response(Response, #state{binary_response = false}) when is_list(Response) ->
    case is_200(Response) of
        true -> [{result, true} | Response];
        false -> [{result, false} | Response]
    end;
make_boolean_response(Response, #state{binary_response = true}) when is_list(Response) ->
    case is_200(Response) of
        true -> [{result, <<"true">>} | Response];
        false -> [{result, <<"false">>} | Response]
    end.

%% @doc Fully qualify a server ref w/ a thrift host/port
-spec fq_server_ref(destination()) -> fq_server_ref().
fq_server_ref({Host, Port, Name}) when is_list(Name) -> {Host, Port, list_to_binary(Name)};
fq_server_ref({Host, Port, Name}) when is_binary(Name) -> {Host, Port, Name};
fq_server_ref(Destination) when is_list(Destination) -> {undefined, undefined, list_to_binary(Destination)};
fq_server_ref(Destination) when is_binary(Destination) -> {undefined, undefined, Destination}.

%% If thrift host is passed in, use it
binary_host(Host) when is_list(Host) -> list_to_binary(Host);
binary_host(undefined) -> <<"">>.

%% If thrift port is passed in, use it
binary_port(Port) when is_integer(Port) -> list_to_binary(integer_to_list(Port));
binary_port(undefined) -> <<"">>.
