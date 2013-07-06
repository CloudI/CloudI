%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI ErlasticSearch Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes
%%%         software developed by Mahesh Paolini-Subramanya
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright 2013 Mahesh Paolini-Subramanya
%%% @version 1.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_elasticsearch).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(cloudi_service).

%% external interface

%% couchdb API
-export([is_index/3, 
         is_type/4,
         is_doc/5, 
         % Cluster helpers
         health/2,
         state/2, state/3,
         nodes_info/2, nodes_info/3, nodes_info/4,
         nodes_stats/2, nodes_stats/3, nodes_stats/4, 
         % Index CRUD
         create_index/3, create_index/4,
         delete_index/2, delete_index/3,
         open_index/3,
         close_index/3, 
         % Doc CRUD
         insert_doc/6, insert_doc/7,
         get_doc/5, get_doc/6,
         mget_doc/3, mget_doc/4, mget_doc/5,
         delete_doc/5, delete_doc/6,
         search/5, search/6,
         count/3, count/4, count/5, count/6,
         delete_by_query/3, delete_by_query/4, delete_by_query/5, delete_by_query/6, 
         %% Index helpers
         status/3,
         refresh/2, refresh/3,
         flush/2, flush/3,
         optimize/2, optimize/3,
         clear_cache/2, clear_cache/3, clear_cache/4,
         segments/2, segments/3]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_DATABASE, undefined).
-define(DEFAULT_THRIFT_HOST, "localhost").
-define(DEFAULT_THRIFT_PORT, 9500).
-define(DEFAULT_POOL_NAME, <<"default_cloudi_elasticsearch_pool">>).
-define(DEFAULT_POOL_OPTIONS, [{size, 5},
                               {max_overflow, 10}
                              ]).

-define(DEFAULT_CONNECTION_OPTIONS, [{thrift_host, ?DEFAULT_THRIFT_HOST},
                                     {thrift_port, ?DEFAULT_THRIFT_PORT}
                                    ]).

-define(ALL, <<"_all">>).



-type error()           :: {error, Reason :: term()}.
-type connection()      :: any().
-type node_name()       :: binary().
-type index()           :: binary().
-type type()            :: binary().
-type id()              :: binary() | undefined.
-type doc()             :: binary().
-type params()          :: [tuple()].
-type pool_name()       :: binary().
-type dispatcher()      :: cloudi_service:dispatcher().
-type name()            :: cloudi_service:service_name().
-type database()        :: atom() | string() | binary().

-record(state, {
        pool_name           :: pool_name(),
        binary_pool_name    :: pool_name(),
        connection          :: connection(),
        binary_connection   :: connection(),
        database            :: database()
        }).

-type response()        :: [tuple()] | error().


%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%% @doc Get the health the  ElasticSearch cluster
-spec health(dispatcher(), name()) -> {ok, response()} | {error, any()}.
health(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    cloudi_service:send_sync(Dispatcher, Name, {health}).

%% _equiv state(Dispatcher, Name, []).
-spec state(dispatcher(), name()) -> {ok, response()} | {error, any()}.
state(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    state(Dispatcher, Name, []).

%% @doc Get the state of the  ElasticSearch cluster
-spec state(dispatcher(), name(), params()) -> {ok, response()} | {error, any()}.
state(Dispatcher, Name, Params) when is_pid(Dispatcher), is_list(Name), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {state, Params}).

%% _equiv nodes_info(Dispatcher, Name, [], []).
-spec nodes_info(dispatcher(), name()) -> {ok, response()} | {error, any()}.
nodes_info(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    nodes_info(Dispatcher, Name, [], []).

%% _equiv nodes_info(Dispatcher, Name, [NodeName], []).
-spec nodes_info(dispatcher(), name(), node_name()) -> {ok, response()} | {error, any()}.
nodes_info(Dispatcher, Name, NodeName) when is_pid(Dispatcher), is_list(Name), is_binary(NodeName) ->
    nodes_info(Dispatcher, Name, [NodeName], []);
%% _equiv nodes_info(Dispatcher, Name, NodeNames, []).
nodes_info(Dispatcher, Name, NodeNames) when is_pid(Dispatcher), is_list(Name), is_list(NodeNames) ->
    nodes_info(Dispatcher, Name, NodeNames, []).

%% @doc Get the nodes_info of the  ElasticSearch cluster
-spec nodes_info(dispatcher(), name(), [node_name()], params()) -> {ok, response()} | {error, any()}.
nodes_info(Dispatcher, Name, NodeNames, Params) when is_pid(Dispatcher), is_list(Name), is_list(NodeNames), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {nodes_info, NodeNames, Params}).

%% _equiv nodes_stats(Dispatcher, Name, [], []).
-spec nodes_stats(dispatcher(), name()) -> {ok, response()} | {error, any()}.
nodes_stats(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    nodes_stats(Dispatcher, Name, [], []).

%% _equiv nodes_stats(Dispatcher, Name, [NodeName], []).
-spec nodes_stats(dispatcher(), name(), node_name()) -> {ok, response()} | {error, any()}.
nodes_stats(Dispatcher, Name, NodeName) when is_pid(Dispatcher), is_list(Name), is_binary(NodeName) ->
    nodes_stats(Dispatcher, Name, [NodeName], []);
%% _equiv nodes_stats(Dispatcher, Name, NodeNames, []).
nodes_stats(Dispatcher, Name, NodeNames) when is_pid(Dispatcher), is_list(Name), is_list(NodeNames) ->
    nodes_stats(Dispatcher, Name, NodeNames, []).

%% @doc Get the nodes_stats of the  ElasticSearch cluster
-spec nodes_stats(dispatcher(), name(), [node_name()], params()) -> {ok, response()} | {error, any()}.
nodes_stats(Dispatcher, Name, NodeNames, Params) when is_pid(Dispatcher), is_list(Name), is_list(NodeNames), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {nodes_stats, NodeNames, Params}).

%% @doc Get the status of an index/indices in the  ElasticSearch cluster
-spec status(dispatcher(), name(), index() | [index()]) -> {ok, response()} | {error, any()}.
status(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    status(Dispatcher, Name, [Index]);
status(Dispatcher, Name, Indexes) when is_pid(Dispatcher), is_list(Name), is_list(Indexes)->
    cloudi_service:send_sync(Dispatcher, Name, {status, Indexes}).

%% _equiv create_index(Dispatcher, Name, Index, <<>>)
-spec create_index(dispatcher(), name(), index()) -> {ok, response()} | {error, any()}.
create_index(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    create_index(Dispatcher, Name, Index, <<>>).

%% @doc Create an index in the ElasticSearch cluster
-spec create_index(dispatcher(), name(), index(), doc()) -> {ok, response()} | {error, any()}.
create_index(Dispatcher, Name, Index, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Doc) ->
    cloudi_service:send_sync(Dispatcher, Name, {create_index, Index, Doc}).

%% @doc Delete all the indices in the ElasticSearch cluster
-spec delete_index(dispatcher(), name()) -> {ok, response()} | {error, any()}.
delete_index(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    delete_index(Dispatcher, Name, ?ALL).

%% @doc Delete an index(es) in the ElasticSearch cluster
-spec delete_index(dispatcher(), name(), index() | [index()]) -> {ok, response()} | {error, any()}.
delete_index(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    delete_index(Dispatcher, Name, [Index]);
delete_index(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_list(Index) ->
    cloudi_service:send_sync(Dispatcher, Name, {delete_index, Index}).

%% @doc Open an index in the ElasticSearch cluster
-spec open_index(dispatcher(), name(), index()) -> {ok, response()} | {error, any()}.
open_index(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    cloudi_service:send_sync(Dispatcher, Name, {open_index, Index}).

%% @doc Close an index in the ElasticSearch cluster
-spec close_index(dispatcher(), name(), index()) -> {ok, response()} | {error, any()}.
close_index(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    cloudi_service:send_sync(Dispatcher, Name, {close_index, Index}).

%% @doc Check if an index/indices exists in the ElasticSearch cluster
-spec is_index(dispatcher(), name(), index() | [index()]) -> {ok, boolean()} | {error, any()}.
is_index(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    is_index(Dispatcher, Name, [Index]);
is_index(Dispatcher, Name, Indexes) when is_pid(Dispatcher), is_list(Name), is_list(Indexes) ->
    cloudi_service:send_sync(Dispatcher, Name, {is_index, Indexes}).

%% _equiv count(Dispatcher, Name, ?ALL, [], Doc []).
-spec count(dispatcher(), name(), doc()) -> {ok, boolean()} | {error, any()}.
count(Dispatcher, Name, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Doc) ->
    count(Dispatcher, Name, ?ALL, [], Doc, []).

%% _equiv count(Dispatcher, Name, ?ALL, [], Doc, Params).
-spec count(dispatcher(), name(), doc(), params()) -> {ok, boolean()} | {error, any()}.
count(Dispatcher, Name, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Doc), is_list(Params) ->
    count(Dispatcher, Name, ?ALL, [], Doc, Params).

%% _equiv count(Dispatcher, Name, Index, [], Doc, Params).
-spec count(dispatcher(), name(), index() | [index()], doc(), params()) -> {ok, boolean()} | {error, any()}.
count(Dispatcher, Name, Index, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Doc), is_list(Params) ->
    count(Dispatcher, Name, [Index], [], Doc, Params);
count(Dispatcher, Name, Indexes, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_binary(Doc), is_list(Params) ->
    count(Dispatcher, Name, Indexes, [], Doc, Params).

%% @doc Get the number of matches for a query
-spec count(dispatcher(), name(), index() | [index()], type() | [type()], doc(), params()) -> {ok, boolean()} | {error, any()}.
count(Dispatcher, Name, Index, Type, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_binary(Doc), is_list(Params) ->
    count(Dispatcher, Name, [Index], [Type], Doc, Params);
count(Dispatcher, Name, Indexes, Type, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_binary(Type), is_binary(Doc), is_list(Params) ->
    count(Dispatcher, Name, Indexes, [Type], Doc, Params);
count(Dispatcher, Name, Index, Types, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_list(Types), is_binary(Doc), is_list(Params) ->
    count(Dispatcher, Name, [Index], Types, Doc, Params);
count(Dispatcher, Name, Indexes, Types, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_list(Types), is_binary(Doc), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {count, Indexes, Types, Doc, Params}).

%% _equiv delete_by_query(Dispatcher, Name, ?ALL, [], Doc []).
-spec delete_by_query(dispatcher(), name(), doc()) -> {ok, boolean()} | {error, any()}.
delete_by_query(Dispatcher, Name, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Doc) ->
    delete_by_query(Dispatcher, Name, ?ALL, [], Doc, []).

%% _equiv delete_by_query(Dispatcher, Name, ?ALL, [], Doc, Params).
-spec delete_by_query(dispatcher(), name(), doc(), params()) -> {ok, boolean()} | {error, any()}.
delete_by_query(Dispatcher, Name, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Doc), is_list(Params) ->
    delete_by_query(Dispatcher, Name, ?ALL, [], Doc, Params).

%% _equiv delete_by_query(Dispatcher, Name, Index, [], Doc, Params).
-spec delete_by_query(dispatcher(), name(), index() | [index()], doc(), params()) -> {ok, boolean()} | {error, any()}.
delete_by_query(Dispatcher, Name, Index, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Doc), is_list(Params) ->
    delete_by_query(Dispatcher, Name, [Index], [], Doc, Params);
delete_by_query(Dispatcher, Name, Indexes, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_binary(Doc), is_list(Params) ->
    delete_by_query(Dispatcher, Name, Indexes, [], Doc, Params).

%% @doc Get the number of matches for a query
-spec delete_by_query(dispatcher(), name(), index() | [index()], type() | [type()], doc(), params()) -> {ok, boolean()} | {error, any()}.
delete_by_query(Dispatcher, Name, Index, Type, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_binary(Doc), is_list(Params) ->
    delete_by_query(Dispatcher, Name, [Index], [Type], Doc, Params);
delete_by_query(Dispatcher, Name, Indexes, Type, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_binary(Type), is_binary(Doc), is_list(Params) ->
    delete_by_query(Dispatcher, Name, Indexes, [Type], Doc, Params);
delete_by_query(Dispatcher, Name, Index, Types, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_list(Types), is_binary(Doc), is_list(Params) ->
    delete_by_query(Dispatcher, Name, [Index], Types, Doc, Params);
delete_by_query(Dispatcher, Name, Indexes, Types, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_list(Types), is_binary(Doc), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {delete_by_query, Indexes, Types, Doc, Params}).

%% @doc Check if a type exists in an index/indices in the ElasticSearch cluster
-spec is_type(dispatcher(), name(), index() | [index()], type() | [type()]) -> {ok, boolean()} | {error, any()}.
is_type(Dispatcher, Name, Index, Type) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type) ->
    is_type(Dispatcher, Name, [Index], [Type]);
is_type(Dispatcher, Name, Indexes, Type) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_binary(Type) ->
    is_type(Dispatcher, Name, Indexes, [Type]);
is_type(Dispatcher, Name, Index, Types) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_list(Types) ->
    is_type(Dispatcher, Name, [Index], Types);
is_type(Dispatcher, Name, Indexes, Types) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_list(Types) ->
    cloudi_service:send_sync(Dispatcher, Name, {is_type, Indexes, Types}).

%% _equiv insert_doc(Index, Type, Id, Doc, []).
-spec insert_doc(dispatcher(), name(), index(), type(), id(), doc()) -> {ok, response()} | {error, any()}.
insert_doc(Dispatcher, Name, Index, Type, Id, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_binary(Doc) ->
    insert_doc(Dispatcher, Name, Index, Type, Id, Doc, []).

%% @doc Insert a doc into the ElasticSearch cluster
-spec insert_doc(dispatcher(), name(), index(), type(), id(), doc(), params()) -> {ok, response()} | {error, any()}.
insert_doc(Dispatcher, Name, Index, Type, Id, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_binary(Doc), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {insert_doc, Index, Type, Id, Doc, Params}).

%% @doc Checks to see if the doc exists
-spec is_doc(dispatcher(), name(), index(), type(), id()) -> {ok, response()} | {error, any()}.
is_doc(Dispatcher, Name, Index, Type, Id) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type) ->
    cloudi_service:send_sync(Dispatcher, Name, {is_doc, Index, Type, Id}).

%% _equiv get_doc(Dispatcher, Name, Index, Type, Id, []).
-spec get_doc(dispatcher(), name(), index(), type(), id()) -> {ok, response()} | {error, any()}.
get_doc(Dispatcher, Name, Index, Type, Id) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type) ->
    get_doc(Dispatcher, Name, Index, Type, Id, []).

%% @doc Get a doc from the ElasticSearch cluster
-spec get_doc(dispatcher(), name(), index(), type(), id(), params()) -> {ok, response()} | {error, any()}.
get_doc(Dispatcher, Name, Index, Type, Id, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_list(Params)->
    cloudi_service:send_sync(Dispatcher, Name, {get_doc, Index, Type, Id, Params}).

%% _equiv mget_doc(Dispatcher, Name, <<>>, <<>>, Doc)
-spec mget_doc(dispatcher(), name(), doc()) -> {ok, response()} | {error, any()}.
mget_doc(Dispatcher, Name, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Doc) ->
    mget_doc(Dispatcher, Name, <<>>, <<>>, Doc).

%% _equiv mget_doc(Dispatcher, Name, Index, <<>>, Doc)
-spec mget_doc(dispatcher(), name(), index(), doc()) -> {ok, response()} | {error, any()}.
mget_doc(Dispatcher, Name, Index, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Doc)->
    mget_doc(Dispatcher, Name, Index, <<>>, Doc).

%% @doc Get a doc from the ElasticSearch cluster
-spec mget_doc(dispatcher(), name(), index(), type(), doc()) -> {ok, response()} | {error, any()}.
mget_doc(Dispatcher, Name, Index, Type, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_binary(Doc)->
    cloudi_service:send_sync(Dispatcher, Name, {mget_doc, Index, Type, Doc}).

%% _equiv delete_doc(Dispatcher, Name, Index, Type, Id, []).
-spec delete_doc(dispatcher(), name(), index(), type(), id()) -> {ok, response()} | {error, any()}.
delete_doc(Dispatcher, Name, Index, Type, Id) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type) ->
    delete_doc(Dispatcher, Name, Index, Type, Id, []).
%% @doc Delete a doc from the ElasticSearch cluster
-spec delete_doc(dispatcher(), name(), index(), type(), id(), params()) -> {ok, response()} | {error, any()}.
delete_doc(Dispatcher, Name, Index, Type, Id, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_list(Params)->
    cloudi_service:send_sync(Dispatcher, Name, {delete_doc, Index, Type, Id, Params}).

%% _equiv search(Dispatcher, Name, Index, Type, Doc, []).
-spec search(dispatcher(), name(), index(), type(), doc()) -> {ok, response()} | {error, any()}.
search(Dispatcher, Name, Index, Type, Doc) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_binary(Doc)->
    search(Dispatcher, Name, Index, Type, Doc, []).
%% @doc Search for docs in the ElasticSearch cluster
-spec search(dispatcher(), name(), index(), type(), doc(), params()) -> {ok, response()} | {error, any()}.
search(Dispatcher, Name, Index, Type, Doc, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_binary(Type), is_binary(Doc), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {search, Index, Type, Doc, Params}).

%% _equiv refresh(Dispatcher, Name, ?ALL).
%% @doc Refresh all indices
-spec refresh(dispatcher(), name()) -> {ok, response()} | {error, any()}.
refresh(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    refresh(Dispatcher, Name, ?ALL).

%% @doc Refresh one or more indices
-spec refresh(dispatcher(), name(), index() | [index()]) -> {ok, response()} | {error, any()}.
refresh(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    refresh(Dispatcher, Name, [Index]);
refresh(Dispatcher, Name, Indexes) when is_pid(Dispatcher), is_list(Name), is_list(Indexes) ->
    cloudi_service:send_sync(Dispatcher, Name, {refresh, Indexes}).

%% @doc Flush all indices
%% _equiv flush(Dispatcher, Name, ?ALL).
-spec flush(dispatcher(), name()) -> {ok, response()} | {error, any()}.
flush(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    flush(Dispatcher, Name, ?ALL).

%% @doc Flush one or more indices
-spec flush(dispatcher(), name(), index() | [index()]) -> {ok, response()} | {error, any()}.
flush(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    flush(Dispatcher, Name, [Index]);
flush(Dispatcher, Name, Indexes) when is_pid(Dispatcher), is_list(Name), is_list(Indexes) ->
    cloudi_service:send_sync(Dispatcher, Name, {flush, Indexes}).

%% _equiv optimize(Dispatcher, Name, ?ALL).
%% @doc Optimize all indices
-spec optimize(dispatcher(), name()) -> {ok, response()} | {error, any()}.
optimize(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    optimize(Dispatcher, Name, ?ALL).

%% @doc Optimize one or more indices
-spec optimize(dispatcher(), name(), index() | [index()]) -> {ok, response()} | {error, any()}.
optimize(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    optimize(Dispatcher, Name, [Index]);
optimize(Dispatcher, Name, Indexes) when is_pid(Dispatcher), is_list(Name), is_list(Indexes) ->
    cloudi_service:send_sync(Dispatcher, Name, {optimize, Indexes}).

%% _equiv segments(Dispatcher, Name, ?ALL).
%% @doc Optimize all indices
-spec segments(dispatcher(), name()) -> {ok, response()} | {error, any()}.
segments(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    segments(Dispatcher, Name, ?ALL).

%% @doc Optimize one or more indices
-spec segments(dispatcher(), name(), index() | [index()]) -> {ok, response()} | {error, any()}.
segments(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    segments(Dispatcher, Name, [Index]);
segments(Dispatcher, Name, Indexes) when is_pid(Dispatcher), is_list(Name), is_list(Indexes) ->
    cloudi_service:send_sync(Dispatcher, Name, {segments, Indexes}).

%% _equiv clear_cache(Dispatcher, Name, ?ALL, []).
%% @doc Clear all the caches
-spec clear_cache(dispatcher(), name()) -> {ok, response()} | {error, any()}.
clear_cache(Dispatcher, Name) when is_pid(Dispatcher), is_list(Name) ->
    clear_cache(Dispatcher, Name, ?ALL, []).

%% _equiv clear_cache(Dispatcher, Name, Indexes, []).
-spec clear_cache(dispatcher(), name(), index() | [index()]) -> {ok, response()} | {error, any()}.
clear_cache(Dispatcher, Name, Index) when is_pid(Dispatcher), is_list(Name), is_binary(Index) ->
    clear_cache(Dispatcher, Name, [Index], []);
clear_cache(Dispatcher, Name, Indexes) when is_pid(Dispatcher), is_list(Name), is_list(Indexes) ->
    clear_cache(Dispatcher, Name, Indexes, []).

%% _equiv clear_cache(Dispatcher, Name, Indexes, []).
-spec clear_cache(dispatcher(), name(), index() | [index()], params()) -> {ok, response()} | {error, any()}.
clear_cache(Dispatcher, Name, Index, Params) when is_pid(Dispatcher), is_list(Name), is_binary(Index), is_list(Params) ->
    clear_cache(Dispatcher, Name, [Index], Params);
clear_cache(Dispatcher, Name, Indexes, Params) when is_pid(Dispatcher), is_list(Name), is_list(Indexes), is_list(Params) ->
    cloudi_service:send_sync(Dispatcher, Name, {clear_cache, Indexes, Params}).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
            {pool_name, ?DEFAULT_POOL_NAME},
            {pool_options, ?DEFAULT_POOL_OPTIONS},
            {connection_options, ?DEFAULT_CONNECTION_OPTIONS},
            {database, ?DEFAULT_DATABASE}],
    [PoolName, PoolOptions, ConnectionOptions0, DatabaseName] =
        cloudi_proplists:take_values(Defaults, Args),

    BinaryPoolName = <<PoolName/binary, "_binary">>,
    ConnectionOptions1 = remove_binary_response_option(ConnectionOptions0),
    ConnectionOptions = [{binary_response, false} | ConnectionOptions1],
    BinaryConnectionOptions = [{binary_response, true} | ConnectionOptions1],

    case cloudi_x_erlasticsearch:start_pool(PoolName, PoolOptions, ConnectionOptions) of
        {ok, Connection} ->
            case cloudi_x_erlasticsearch:start_pool(BinaryPoolName, PoolOptions, BinaryConnectionOptions) of
                {ok, BinaryConnection} -> 
                    cloudi_service:subscribe(Dispatcher, DatabaseName),
                    {ok, #state{pool_name = PoolName,
                                binary_pool_name = BinaryPoolName,
                                connection = Connection,
                                binary_connection = BinaryConnection,
                                database = DatabaseName}};
                {error, Reason} ->
                    cloudi_x_erlasticsearch:stop_pool(PoolName),
                    {stop, Reason}
            end;
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{pool_name = PoolName,
                                     binary_pool_name = BinaryPoolName
                                    } = State,
                              _Dispatcher) ->
    case Request of
        Command when is_binary(Command) ->
            {reply, do_query(Command, BinaryPoolName, Timeout), State};
        {'health'} -> 
            {reply, cloudi_x_erlasticsearch:health({pool, PoolName}), State};
        {'state'} ->
            {reply, cloudi_x_erlasticsearch:state({pool, PoolName}, []), State};
        {'state', Params} -> 
            {reply, cloudi_x_erlasticsearch:state({pool, PoolName}, Params), State};
        {'nodes_info'} ->
            {reply, cloudi_x_erlasticsearch:nodes_info({pool, PoolName}, [], []), State};
        {'nodes_info', NodeNames} ->
            {reply, cloudi_x_erlasticsearch:nodes_info({pool, PoolName}, NodeNames, []), State};
        {'nodes_info', NodeNames, Params} -> 
            {reply, cloudi_x_erlasticsearch:nodes_info({pool, PoolName}, NodeNames, Params), State};
        {'nodes_stats'} ->
            {reply, cloudi_x_erlasticsearch:nodes_stats({pool, PoolName}, [], []), State};
        {'nodes_stats', NodeNames} ->
            {reply, cloudi_x_erlasticsearch:nodes_stats({pool, PoolName}, NodeNames, []), State};
        {'nodes_stats', NodeNames, Params} -> 
            {reply, cloudi_x_erlasticsearch:nodes_stats({pool, PoolName}, NodeNames, Params), State};
        {'status', Indexes} -> 
            {reply, cloudi_x_erlasticsearch:status({pool, PoolName}, Indexes), State};
        {'create_index', Index} ->
            {reply, cloudi_x_erlasticsearch:create_index({pool, PoolName}, Index, <<>>), State};
        {'create_index', Index, Doc} -> 
            {reply, cloudi_x_erlasticsearch:create_index({pool, PoolName}, Index, Doc), State};
        {'delete_index'} ->
            {reply, cloudi_x_erlasticsearch:delete_index({pool, PoolName}, ?ALL), State};
        {'delete_index', Index} -> 
            {reply, cloudi_x_erlasticsearch:delete_index({pool, PoolName}, Index), State};
        {'open_index', Index} -> 
            {reply, cloudi_x_erlasticsearch:open_index({pool, PoolName}, Index), State};
        {'close_index', Index} -> 
            {reply, cloudi_x_erlasticsearch:close_index({pool, PoolName}, Index), State};
        {'is_index', Indexes} -> 
            {reply, cloudi_x_erlasticsearch:is_index({pool, PoolName}, Indexes), State};
        {'count', Doc} ->
            {reply, cloudi_x_erlasticsearch:count({pool, PoolName}, ?ALL, [], Doc, []), State};
        {'count', Doc, Params} ->
            {reply, cloudi_x_erlasticsearch:count({pool, PoolName}, ?ALL, [], Doc, Params), State};
        {'count', Indexes, Doc, Params} -> 
            {reply, cloudi_x_erlasticsearch:count({pool, PoolName}, Indexes, [], Doc, Params), State};
        {'count', Indexes, Types, Doc, Params} -> 
            {reply, cloudi_x_erlasticsearch:count({pool, PoolName}, Indexes, Types, Doc, Params), State};
        {'delete_by_query', Doc} ->
            {reply, cloudi_x_erlasticsearch:delete_by_query({pool, PoolName}, ?ALL, [], Doc, []), State};
        {'delete_by_query', Doc, Params} -> 
            {reply, cloudi_x_erlasticsearch:delete_by_query({pool, PoolName}, ?ALL, [], Doc, Params), State};
        {'delete_by_query', Indexes, Doc, Params} -> 
            {reply, cloudi_x_erlasticsearch:delete_by_query({pool, PoolName}, Indexes, [], Doc, Params), State};
        {'delete_by_query', Indexes, Types, Doc, Params} -> 
            {reply, cloudi_x_erlasticsearch:delete_by_query({pool, PoolName}, Indexes, Types, Doc, Params), State};
        {'is_type', Indexes, Types} -> 
            {reply, cloudi_x_erlasticsearch:is_type({pool, PoolName}, Indexes, Types), State};
        {'insert_doc', Index, Type, Id, Doc} ->
            {reply, cloudi_x_erlasticsearch:insert_doc({pool, PoolName}, Index, Type, Id, Doc, []), State};
        {'insert_doc', Index, Type, Id, Doc, Params} -> 
            {reply, cloudi_x_erlasticsearch:insert_doc({pool, PoolName}, Index, Type, Id, Doc, Params), State};
        {'is_doc', Index, Type, Id} -> 
            {reply, cloudi_x_erlasticsearch:is_doc({pool, PoolName}, Index, Type, Id), State};
        {'get_doc', Index, Type, Id} ->
            {reply, cloudi_x_erlasticsearch:get_doc({pool, PoolName}, Index, Type, Id, []), State};
        {'get_doc', Index, Type, Id, Params} -> 
            {reply, cloudi_x_erlasticsearch:get_doc({pool, PoolName}, Index, Type, Id, Params), State};
        {'mget_doc', Index, Doc} -> 
            {reply, cloudi_x_erlasticsearch:mget_doc({pool, PoolName}, Index, <<>>, Doc), State};
        {'mget_doc', Index, Type, Doc} -> 
            {reply, cloudi_x_erlasticsearch:mget_doc({pool, PoolName}, Index, Type, Doc), State};
        {'delete_doc', Index, Type, Id} ->
            {reply, cloudi_x_erlasticsearch:delete_doc({pool, PoolName}, Index, Type, Id, []), State};
        {'delete_doc', Index, Type, Id, Params} -> 
            {reply, cloudi_x_erlasticsearch:delete_doc({pool, PoolName}, Index, Type, Id, Params), State};
        {'search', Index, Type, Doc} ->
            {reply, cloudi_x_erlasticsearch:search({pool, PoolName}, Index, Type, Doc, []), State};
        {'search', Index, Type, Doc, Params} -> 
            {reply, cloudi_x_erlasticsearch:search({pool, PoolName}, Index, Type, Doc, Params), State};
        {'refresh'} ->
            {reply, cloudi_x_erlasticsearch:refresh({pool, PoolName}, ?ALL), State};
        {'refresh', Indexes} ->
            {reply, cloudi_x_erlasticsearch:refresh({pool, PoolName}, Indexes), State};
        {'flush'} ->
            {reply, cloudi_x_erlasticsearch:flush({pool, PoolName}, ?ALL), State};
        {'flush', Indexes} ->
            {reply, cloudi_x_erlasticsearch:flush({pool, PoolName}, Indexes), State};
        {'optimize'} ->
            {reply, cloudi_x_erlasticsearch:optimize({pool, PoolName}, ?ALL), State};
        {'optimize', Indexes} ->
            {reply, cloudi_x_erlasticsearch:optimize({pool, PoolName}, Indexes), State};
        {'segments'} ->
            {reply, cloudi_x_erlasticsearch:segments({pool, PoolName}, ?ALL), State};
        {'segments', Indexes} ->
            {reply, cloudi_x_erlasticsearch:segments({pool, PoolName}, Indexes), State};
        {'clear_cache'} ->
            {reply, cloudi_x_erlasticsearch:clear_cache({pool, PoolName}, ?ALL, []), State};
        {'clear_cache', Indexes} ->
            {reply, cloudi_x_erlasticsearch:clear_cache({pool, PoolName}, Indexes, []), State};
        {'clear_cache', Indexes, Params} ->
            {reply, cloudi_x_erlasticsearch:clear_cache({pool, PoolName}, Indexes, Params), State}
    end.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{pool_name = PoolName,
                                   binary_pool_name = BinaryPoolName}) ->
    cloudi_x_erlasticsearch:stop_pool(PoolName),
    cloudi_x_erlasticsearch:stop_pool(BinaryPoolName),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, BinaryPoolName, _Timeout) ->
    try (case cloudi_string:binary_to_term(Query) of
		        {'health'} -> 
		            cloudi_x_erlasticsearch:health({pool, BinaryPoolName});
                {'state'} ->
                    cloudi_x_erlasticsearch:state({pool, BinaryPoolName}, []);
		        {'state', Params} -> 
		            cloudi_x_erlasticsearch:state({pool, BinaryPoolName}, Params);
		        {'nodes_info'} ->
		            cloudi_x_erlasticsearch:nodes_info({pool, BinaryPoolName}, [], []);
		        {'nodes_info', NodeNames} ->
		            cloudi_x_erlasticsearch:nodes_info({pool, BinaryPoolName}, NodeNames, []);
		        {'nodes_info', NodeNames, Params} -> 
		            cloudi_x_erlasticsearch:nodes_info({pool, BinaryPoolName}, NodeNames, Params);
		        {'nodes_stats'} ->
		            cloudi_x_erlasticsearch:nodes_stats({pool, BinaryPoolName}, [], []);
		        {'nodes_stats', NodeNames} ->
		            cloudi_x_erlasticsearch:nodes_stats({pool, BinaryPoolName}, NodeNames, []);
		        {'nodes_stats', NodeNames, Params} -> 
		            cloudi_x_erlasticsearch:nodes_stats({pool, BinaryPoolName}, NodeNames, Params);
		        {'status', Indexes} -> 
		            cloudi_x_erlasticsearch:status({pool, BinaryPoolName}, Indexes);
		        {'create_index', Index} ->
		            cloudi_x_erlasticsearch:create_index({pool, BinaryPoolName}, Index, <<>>);
		        {'create_index', Index, Doc} -> 
		            cloudi_x_erlasticsearch:create_index({pool, BinaryPoolName}, Index, Doc);
		        {'delete_index'} ->
		            cloudi_x_erlasticsearch:delete_index({pool, BinaryPoolName}, ?ALL);
		        {'delete_index', Index} -> 
		            cloudi_x_erlasticsearch:delete_index({pool, BinaryPoolName}, Index);
		        {'open_index', Index} -> 
		            cloudi_x_erlasticsearch:open_index({pool, BinaryPoolName}, Index);
		        {'close_index', Index} -> 
		            cloudi_x_erlasticsearch:close_index({pool, BinaryPoolName}, Index);
		        {'is_index', Indexes} -> 
		            cloudi_x_erlasticsearch:is_index({pool, BinaryPoolName}, Indexes);
		        {'count', Doc} ->
		            cloudi_x_erlasticsearch:count({pool, BinaryPoolName}, ?ALL, [], Doc, []);
		        {'count', Doc, Params} ->
		            cloudi_x_erlasticsearch:count({pool, BinaryPoolName}, ?ALL, [], Doc, Params);
		        {'count', Indexes, Doc, Params} -> 
		            cloudi_x_erlasticsearch:count({pool, BinaryPoolName}, Indexes, [], Doc, Params);
		        {'count', Indexes, Types, Doc, Params} -> 
		            cloudi_x_erlasticsearch:count({pool, BinaryPoolName}, Indexes, Types, Doc, Params);
		        {'delete_by_query', Doc} ->
                    cloudi_x_erlasticsearch:delete_by_query({pool, BinaryPoolName}, ?ALL, [], Doc, []);
		        {'delete_by_query', Doc, Params} -> 
		            cloudi_x_erlasticsearch:delete_by_query({pool, BinaryPoolName}, ?ALL, [], Doc, Params);
		        {'delete_by_query', Indexes, Doc, Params} -> 
		            cloudi_x_erlasticsearch:delete_by_query({pool, BinaryPoolName}, Indexes, [], Doc, Params);
		        {'delete_by_query', Indexes, Types, Doc, Params} -> 
		            cloudi_x_erlasticsearch:delete_by_query({pool, BinaryPoolName}, Indexes, Types, Doc, Params);
		        {'is_type', Indexes, Types} -> 
		            cloudi_x_erlasticsearch:is_type({pool, BinaryPoolName}, Indexes, Types);
		        {'insert_doc', Index, Type, Id, Doc} ->
		            cloudi_x_erlasticsearch:insert_doc({pool, BinaryPoolName}, Index, Type, Id, Doc, []);
		        {'insert_doc', Index, Type, Id, Doc, Params} -> 
		            cloudi_x_erlasticsearch:insert_doc({pool, BinaryPoolName}, Index, Type, Id, Doc, Params);
		        {'is_doc', Index, Type, Id} -> 
		            cloudi_x_erlasticsearch:is_doc({pool, BinaryPoolName}, Index, Type, Id);
		        {'get_doc', Index, Type, Id} ->
		            cloudi_x_erlasticsearch:get_doc({pool, BinaryPoolName}, Index, Type, Id, []);
		        {'get_doc', Index, Type, Id, Params} -> 
		            cloudi_x_erlasticsearch:get_doc({pool, BinaryPoolName}, Index, Type, Id, Params);
		        {'mget_doc', Index, Doc} -> 
		            cloudi_x_erlasticsearch:mget_doc({pool, BinaryPoolName}, Index, <<>>, Doc);
		        {'mget_doc', Index, Type, Doc} -> 
		            cloudi_x_erlasticsearch:mget_doc({pool, BinaryPoolName}, Index, Type, Doc);
		        {'delete_doc', Index, Type, Id} ->
		            cloudi_x_erlasticsearch:delete_doc({pool, BinaryPoolName}, Index, Type, Id, []);
		        {'delete_doc', Index, Type, Id, Params} -> 
		            cloudi_x_erlasticsearch:delete_doc({pool, BinaryPoolName}, Index, Type, Id, Params);
		        {'search', Index, Type, Doc} ->
		            cloudi_x_erlasticsearch:search({pool, BinaryPoolName}, Index, Type, Doc, []);
		        {'search', Index, Type, Doc, Params} -> 
		            cloudi_x_erlasticsearch:search({pool, BinaryPoolName}, Index, Type, Doc, Params);
		        {'refresh'} ->
		            cloudi_x_erlasticsearch:refresh({pool, BinaryPoolName}, ?ALL);
		        {'refresh', Indexes} ->
		            cloudi_x_erlasticsearch:refresh({pool, BinaryPoolName}, Indexes);
		        {'flush'} ->
		            cloudi_x_erlasticsearch:flush({pool, BinaryPoolName}, ?ALL);
		        {'flush', Indexes} ->
		            cloudi_x_erlasticsearch:flush({pool, BinaryPoolName}, Indexes);
		        {'optimize'} ->
		            cloudi_x_erlasticsearch:optimize({pool, BinaryPoolName}, ?ALL);
		        {'optimize', Indexes} ->
		            cloudi_x_erlasticsearch:optimize({pool, BinaryPoolName}, Indexes);
		        {'segments'} ->
		            cloudi_x_erlasticsearch:segments({pool, BinaryPoolName}, ?ALL);
		        {'segments', Indexes} ->
		            cloudi_x_erlasticsearch:segments({pool, BinaryPoolName}, Indexes);
		        {'clear_cache'} ->
		            cloudi_x_erlasticsearch:clear_cache({pool, BinaryPoolName}, ?ALL, []);
		        {'clear_cache', Indexes} ->
		            cloudi_x_erlasticsearch:clear_cache({pool, BinaryPoolName}, Indexes, []);
		        {'clear_cache', Indexes, Params} ->
                    cloudi_x_erlasticsearch:clear_cache({pool, BinaryPoolName}, Indexes, Params);
                _ ->
                    {error, invalid_call}
            end) of
        {error, invalid_call} ->
            ?LOG_DEBUG("Invalid elasticsearch command tuple ~p",
                       [binary_to_list(Query)]),
            <<>>;
        Response ->
            response_to_single_binary(Response)

    catch
        _:Reason ->
            ?LOG_DEBUG("exception when processing "
                       "elasticsearch command tuple ~p: ~p",
                       [binary_to_list(Query), Reason]),
            <<>>
    end.

%% @doc Incoming options should not request binary_response
%%      (since we have two pools!)
-spec remove_binary_response_option(params()) -> params().
remove_binary_response_option(Options) ->
    case lists:keytake(binary_response, 1, Options) of 
        {value, _, Result} -> Result;
        false -> Options
    end.



%% @doc Convert the response to a single binary JSON blob
-spec response_to_single_binary(response()) -> binary().
response_to_single_binary(Response) ->
    response_to_single_binary(Response, []).

-spec response_to_single_binary(response(), iolist()) -> binary().
response_to_single_binary([], Acc) -> 
    iolist_to_binary([<<"{">>, lists:reverse(Acc), <<"}">>]);
response_to_single_binary([{status, Status} | []], Acc) ->
    response_to_single_binary([], [[<<"\"status\":">>, Status] | Acc]);
response_to_single_binary([{status, Status} | Tail], Acc) ->
    response_to_single_binary(Tail, [[<<"\"status\":">>, Status, <<",">>] | Acc]);
response_to_single_binary([{result, Result} | []], Acc) ->
    response_to_single_binary([], [[<<"\"result\":">>, Result] | Acc]);
response_to_single_binary([{result, Result} | Tail], Acc) ->
    response_to_single_binary(Tail, [[<<"\"result\":">>, Result, <<",">>] | Acc]);
response_to_single_binary([{body, Body} | []], Acc) ->
    response_to_single_binary([], [[<<"\"body\":">>, Body] | Acc]);
response_to_single_binary([{body, Body} | Tail], Acc) ->
    response_to_single_binary(Tail, [[<<"\"body\":">>, Body, <<",">>] | Acc]);
response_to_single_binary([_ | Tail], Acc) ->
    response_to_single_binary(Tail, [Acc]).

