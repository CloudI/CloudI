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
%%% @version 1.3.0 {@date} {@time}
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
         update_doc/6, update_doc/7,
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
         segments/2, segments/3,
         % Mapping CRUD
         put_mapping/5,
         get_mapping/4,
         delete_mapping/4,
         % Aliases CRUD
         aliases/3,
         insert_alias/4, insert_alias/5,
         delete_alias/4,
         is_alias/4,
         get_alias/4]).

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
-define(DEFAULT_POOL_OPTIONS, [{size, 1},
                               {max_overflow, 0}
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
-type dispatcher()      :: cloudi_service:dispatcher() | cloudi:context().
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
-spec health(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
health(Dispatcher, Name)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {health}).

%% _equiv state(Dispatcher, Name, []).
-spec state(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
state(Dispatcher, Name)
    when is_list(Name) ->
    state(Dispatcher, Name, []).

%% @doc Get the state of the  ElasticSearch cluster
-spec state(dispatcher(), name(), params()) ->
    {ok, response()} |
    {error, any()}.
state(Dispatcher, Name, Params)
    when is_list(Name), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {state, Params}).

%% _equiv nodes_info(Dispatcher, Name, [], []).
-spec nodes_info(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
nodes_info(Dispatcher, Name) when is_list(Name) ->
    nodes_info(Dispatcher, Name, [], []).

%% _equiv nodes_info(Dispatcher, Name, [NodeName], []).
-spec nodes_info(dispatcher(), name(), node_name()) ->
    {ok, response()} |
    {error, any()}.
nodes_info(Dispatcher, Name, NodeName)
    when is_list(Name), is_binary(NodeName) ->
    nodes_info(Dispatcher, Name, [NodeName], []);
%% _equiv nodes_info(Dispatcher, Name, NodeNames, []).
nodes_info(Dispatcher, Name, NodeNames)
    when is_list(Name), is_list(NodeNames) ->
    nodes_info(Dispatcher, Name, NodeNames, []).

%% @doc Get the nodes_info of the  ElasticSearch cluster
-spec nodes_info(dispatcher(), name(), [node_name()], params()) ->
    {ok, response()} |
    {error, any()}.
nodes_info(Dispatcher, Name, NodeNames, Params)
    when is_list(Name), is_list(NodeNames), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {nodes_info, NodeNames, Params}).

%% _equiv nodes_stats(Dispatcher, Name, [], []).
-spec nodes_stats(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
nodes_stats(Dispatcher, Name) when is_list(Name) ->
    nodes_stats(Dispatcher, Name, [], []).

%% _equiv nodes_stats(Dispatcher, Name, [NodeName], []).
-spec nodes_stats(dispatcher(), name(), node_name()) ->
    {ok, response()} |
    {error, any()}.
nodes_stats(Dispatcher, Name, NodeName)
    when is_list(Name), is_binary(NodeName) ->
    nodes_stats(Dispatcher, Name, [NodeName], []);
%% _equiv nodes_stats(Dispatcher, Name, NodeNames, []).
nodes_stats(Dispatcher, Name, NodeNames)
    when is_list(Name), is_list(NodeNames) ->
    nodes_stats(Dispatcher, Name, NodeNames, []).

%% @doc Get the nodes_stats of the  ElasticSearch cluster
-spec nodes_stats(dispatcher(), name(), [node_name()], params()) ->
    {ok, response()} |
    {error, any()}.
nodes_stats(Dispatcher, Name, NodeNames, Params)
    when is_list(Name), is_list(NodeNames), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {nodes_stats, NodeNames, Params}).

%% @doc Get the status of an index/indices in the  ElasticSearch cluster
-spec status(dispatcher(), name(), index() | [index()]) ->
    {ok, response()} |
    {error, any()}.
status(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    status(Dispatcher, Name, [Index]);
status(Dispatcher, Name, Indexes)
    when is_list(Name), is_list(Indexes)->
    cloudi:send_sync(Dispatcher, Name,
                     {status, Indexes}).

%% _equiv create_index(Dispatcher, Name, Index, <<>>)
-spec create_index(dispatcher(), name(), index()) ->
    {ok, response()} |
    {error, any()}.
create_index(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    create_index(Dispatcher, Name, Index, <<>>).

%% @doc Create an index in the ElasticSearch cluster
-spec create_index(dispatcher(), name(), index(), doc()) ->
    {ok, response()} |
    {error, any()}.
create_index(Dispatcher, Name, Index, Doc)
    when is_list(Name), is_binary(Index), (is_binary(Doc) orelse is_list(Doc)) ->
    cloudi:send_sync(Dispatcher, Name,
                     {create_index, Index, Doc}).

%% @doc Delete all the indices in the ElasticSearch cluster
-spec delete_index(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
delete_index(Dispatcher, Name)
    when is_list(Name) ->
    delete_index(Dispatcher, Name, ?ALL).

%% @doc Delete an index(es) in the ElasticSearch cluster
-spec delete_index(dispatcher(), name(), index() | [index()]) ->
    {ok, response()} |
    {error, any()}.
delete_index(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    delete_index(Dispatcher, Name, [Index]);
delete_index(Dispatcher, Name, Index)
    when is_list(Name), is_list(Index) ->
    cloudi:send_sync(Dispatcher, Name,
                     {delete_index, Index}).

%% @doc Open an index in the ElasticSearch cluster
-spec open_index(dispatcher(), name(), index()) ->
    {ok, response()} |
    {error, any()}.
open_index(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    cloudi:send_sync(Dispatcher, Name,
                     {open_index, Index}).

%% @doc Close an index in the ElasticSearch cluster
-spec close_index(dispatcher(), name(), index()) ->
    {ok, response()} |
    {error, any()}.
close_index(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    cloudi:send_sync(Dispatcher, Name,
                     {close_index, Index}).

%% @doc Check if an index/indices exists in the ElasticSearch cluster
-spec is_index(dispatcher(), name(), index() | [index()]) ->
    {ok, boolean()} |
    {error, any()}.
is_index(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    is_index(Dispatcher, Name, [Index]);
is_index(Dispatcher, Name, Indexes)
    when is_list(Name), is_list(Indexes) ->
    cloudi:send_sync(Dispatcher, Name, {is_index, Indexes}).

%% _equiv count(Dispatcher, Name, ?ALL, [], Doc []).
-spec count(dispatcher(), name(), doc()) ->
    {ok, boolean()} |
    {error, any()}.
count(Dispatcher, Name, Doc)
    when is_list(Name), (is_binary(Doc) orelse is_list(Doc)) ->
    count(Dispatcher, Name, ?ALL, [], Doc, []).

%% _equiv count(Dispatcher, Name, ?ALL, [], Doc, Params).
-spec count(dispatcher(), name(), doc(), params()) ->
    {ok, boolean()} |
    {error, any()}.
count(Dispatcher, Name, Doc, Params)
    when is_list(Name), (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    count(Dispatcher, Name, ?ALL, [], Doc, Params).

%% _equiv count(Dispatcher, Name, Index, [], Doc, Params).
-spec count(dispatcher(), name(), index() | [index()], doc(), params()) ->
    {ok, boolean()} |
    {error, any()}.
count(Dispatcher, Name, Index, Doc, Params)
    when is_list(Name), is_binary(Index), (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    count(Dispatcher, Name, [Index], [], Doc, Params);
count(Dispatcher, Name, Indexes, Doc, Params)
    when is_list(Name), is_list(Indexes), (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    count(Dispatcher, Name, Indexes, [], Doc, Params).

%% @doc Get the number of matches for a query
-spec count(dispatcher(), name(), index() | [index()],
            type() | [type()], doc(), params()) ->
    {ok, boolean()} |
    {error, any()}.
count(Dispatcher, Name, Index, Type, Doc, Params)
    when is_list(Name), is_binary(Index), is_binary(Type),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    count(Dispatcher, Name, [Index], [Type], Doc, Params);
count(Dispatcher, Name, Indexes, Type, Doc, Params)
    when is_list(Name), is_list(Indexes), is_binary(Type),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    count(Dispatcher, Name, Indexes, [Type], Doc, Params);
count(Dispatcher, Name, Index, Types, Doc, Params)
    when is_list(Name), is_binary(Index), is_list(Types),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    count(Dispatcher, Name, [Index], Types, Doc, Params);
count(Dispatcher, Name, Indexes, Types, Doc, Params)
    when is_list(Name), is_list(Indexes), is_list(Types),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {count, Indexes, Types, Doc, Params}).

%% _equiv delete_by_query(Dispatcher, Name, ?ALL, [], Doc []).
-spec delete_by_query(dispatcher(), name(), doc()) ->
    {ok, boolean()} |
    {error, any()}.
delete_by_query(Dispatcher, Name, Doc)
    when is_list(Name), (is_binary(Doc) orelse is_list(Doc)) ->
    delete_by_query(Dispatcher, Name, ?ALL, [], Doc, []).

%% _equiv delete_by_query(Dispatcher, Name, ?ALL, [], Doc, Params).
-spec delete_by_query(dispatcher(), name(), doc(), params()) ->
    {ok, boolean()} |
    {error, any()}.
delete_by_query(Dispatcher, Name, Doc, Params)
    when is_list(Name), (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(Dispatcher, Name, ?ALL, [], Doc, Params).

%% _equiv delete_by_query(Dispatcher, Name, Index, [], Doc, Params).
-spec delete_by_query(dispatcher(), name(), index() | [index()],
                      doc(), params()) ->
    {ok, boolean()} |
    {error, any()}.
delete_by_query(Dispatcher, Name, Index, Doc, Params)
    when is_list(Name), is_binary(Index), (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(Dispatcher, Name, [Index], [], Doc, Params);
delete_by_query(Dispatcher, Name, Indexes, Doc, Params)
    when is_list(Name), is_list(Indexes), (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(Dispatcher, Name, Indexes, [], Doc, Params).

%% @doc Get the number of matches for a query
-spec delete_by_query(dispatcher(), name(), index() | [index()],
                      type() | [type()], doc(), params()) ->
    {ok, boolean()} |
    {error, any()}.
delete_by_query(Dispatcher, Name, Index, Type, Doc, Params)
    when is_list(Name), is_binary(Index), is_binary(Type),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(Dispatcher, Name, [Index], [Type], Doc, Params);
delete_by_query(Dispatcher, Name, Indexes, Type, Doc, Params)
    when is_list(Name), is_list(Indexes), is_binary(Type),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(Dispatcher, Name, Indexes, [Type], Doc, Params);
delete_by_query(Dispatcher, Name, Index, Types, Doc, Params)
    when is_list(Name), is_binary(Index), is_list(Types),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    delete_by_query(Dispatcher, Name, [Index], Types, Doc, Params);
delete_by_query(Dispatcher, Name, Indexes, Types, Doc, Params)
    when is_list(Name), is_list(Indexes), is_list(Types),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {delete_by_query, Indexes, Types, Doc, Params}).

%% @doc Check if a type exists in an index/indices in the ElasticSearch cluster
-spec is_type(dispatcher(), name(), index() | [index()], type() | [type()]) ->
    {ok, boolean()} |
    {error, any()}.
is_type(Dispatcher, Name, Index, Type)
    when is_list(Name), is_binary(Index), is_binary(Type) ->
    is_type(Dispatcher, Name, [Index], [Type]);
is_type(Dispatcher, Name, Indexes, Type)
    when is_list(Name), is_list(Indexes), is_binary(Type) ->
    is_type(Dispatcher, Name, Indexes, [Type]);
is_type(Dispatcher, Name, Index, Types)
    when is_list(Name), is_binary(Index), is_list(Types) ->
    is_type(Dispatcher, Name, [Index], Types);
is_type(Dispatcher, Name, Indexes, Types)
    when is_list(Name), is_list(Indexes), is_list(Types) ->
    cloudi:send_sync(Dispatcher, Name,
                     {is_type, Indexes, Types}).

%% _equiv insert_doc(Index, Type, Id, Doc, []).
-spec insert_doc(dispatcher(), name(), index(), type(), id(), doc()) ->
    {ok, response()} |
    {error, any()}.
insert_doc(Dispatcher, Name, Index, Type, Id, Doc)
    when is_list(Name), is_binary(Index), is_binary(Type), (is_binary(Doc) orelse is_list(Doc)) ->
    insert_doc(Dispatcher, Name, Index, Type, Id, Doc, []).

%% @doc Insert a doc into the ElasticSearch cluster
-spec insert_doc(dispatcher(), name(), index(), type(),
                 id(), doc(), params()) ->
    {ok, response()} |
    {error, any()}.
insert_doc(Dispatcher, Name, Index, Type, Id, Doc, Params)
    when is_list(Name), is_binary(Index), is_binary(Type),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {insert_doc, Index, Type, Id, Doc, Params}).

%% _equiv update_doc(Index, Type, Id, Doc, []).
-spec update_doc(dispatcher(), name(), index(), type(), id(), doc()) ->
    {ok, response()} |
    {error, any()}.
update_doc(Dispatcher, Name, Index, Type, Id, Doc)
    when is_list(Name), is_binary(Index), is_binary(Type), (is_binary(Doc) orelse is_list(Doc)) ->
    update_doc(Dispatcher, Name, Index, Type, Id, Doc, []).

%% @doc Insert a doc into the ElasticSearch cluster
-spec update_doc(dispatcher(), name(), index(), type(),
                 id(), doc(), params()) ->
    {ok, response()} |
    {error, any()}.
update_doc(Dispatcher, Name, Index, Type, Id, Doc, Params)
    when is_list(Name), is_binary(Index), is_binary(Type),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {update_doc, Index, Type, Id, Doc, Params}).

%% @doc Checks to see if the doc exists
-spec is_doc(dispatcher(), name(), index(), type(), id()) ->
    {ok, response()} |
    {error, any()}.
is_doc(Dispatcher, Name, Index, Type, Id)
    when is_list(Name), is_binary(Index), is_binary(Type) ->
    cloudi:send_sync(Dispatcher, Name,
                     {is_doc, Index, Type, Id}).

%% _equiv get_doc(Dispatcher, Name, Index, Type, Id, []).
-spec get_doc(dispatcher(), name(), index(), type(), id()) ->
    {ok, response()} |
    {error, any()}.
get_doc(Dispatcher, Name, Index, Type, Id)
    when is_list(Name), is_binary(Index), is_binary(Type) ->
    get_doc(Dispatcher, Name, Index, Type, Id, []).

%% @doc Get a doc from the ElasticSearch cluster
-spec get_doc(dispatcher(), name(), index(), type(), id(), params()) ->
    {ok, response()} |
    {error, any()}.
get_doc(Dispatcher, Name, Index, Type, Id, Params)
    when is_list(Name), is_binary(Index), is_binary(Type), is_list(Params)->
    cloudi:send_sync(Dispatcher, Name,
                     {get_doc, Index, Type, Id, Params}).

%% _equiv mget_doc(Dispatcher, Name, <<>>, <<>>, Doc)
-spec mget_doc(dispatcher(), name(), doc()) ->
    {ok, response()} |
    {error, any()}.
mget_doc(Dispatcher, Name, Doc)
    when is_list(Name), (is_binary(Doc) orelse is_list(Doc)) ->
    mget_doc(Dispatcher, Name, <<>>, <<>>, Doc).

%% _equiv mget_doc(Dispatcher, Name, Index, <<>>, Doc)
-spec mget_doc(dispatcher(), name(), index(), doc()) ->
    {ok, response()} |
    {error, any()}.
mget_doc(Dispatcher, Name, Index, Doc)
    when is_list(Name), is_binary(Index), (is_binary(Doc) orelse is_list(Doc)) ->
    mget_doc(Dispatcher, Name, Index, <<>>, Doc).

%% @doc Get a doc from the ElasticSearch cluster
-spec mget_doc(dispatcher(), name(), index(), type(), doc()) ->
    {ok, response()} |
    {error, any()}.
mget_doc(Dispatcher, Name, Index, Type, Doc)
    when is_list(Name), is_binary(Index), is_binary(Type), (is_binary(Doc) orelse is_list(Doc)) ->
    cloudi:send_sync(Dispatcher, Name,
                     {mget_doc, Index, Type, Doc}).

%% _equiv delete_doc(Dispatcher, Name, Index, Type, Id, []).
-spec delete_doc(dispatcher(), name(), index(), type(), id()) ->
    {ok, response()} |
    {error, any()}.
delete_doc(Dispatcher, Name, Index, Type, Id)
    when is_list(Name), is_binary(Index), is_binary(Type) ->
    delete_doc(Dispatcher, Name, Index, Type, Id, []).

%% @doc Delete a doc from the ElasticSearch cluster
-spec delete_doc(dispatcher(), name(), index(), type(), id(), params()) ->
    {ok, response()} |
    {error, any()}.
delete_doc(Dispatcher, Name, Index, Type, Id, Params)
    when is_list(Name), is_binary(Index), is_binary(Type), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {delete_doc, Index, Type, Id, Params}).

%% _equiv search(Dispatcher, Name, Index, Type, Doc, []).
-spec search(dispatcher(), name(), index(), type(), doc()) ->
    {ok, response()} |
    {error, any()}.
search(Dispatcher, Name, Index, Type, Doc)
    when is_list(Name), is_binary(Index), is_binary(Type), (is_binary(Doc) orelse is_list(Doc)) ->
    search(Dispatcher, Name, Index, Type, Doc, []).

%% @doc Search for docs in the ElasticSearch cluster
-spec search(dispatcher(), name(), index(), type(), doc(), params()) ->
    {ok, response()} |
    {error, any()}.
search(Dispatcher, Name, Index, Type, Doc, Params)
    when is_list(Name), is_binary(Index), is_binary(Type),
         (is_binary(Doc) orelse is_list(Doc)), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {search, Index, Type, Doc, Params}).

%% _equiv refresh(Dispatcher, Name, ?ALL).
%% @doc Refresh all indices
-spec refresh(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
refresh(Dispatcher, Name) when is_list(Name) ->
    refresh(Dispatcher, Name, ?ALL).

%% @doc Refresh one or more indices
-spec refresh(dispatcher(), name(), index() | [index()]) ->
    {ok, response()} |
    {error, any()}.
refresh(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    refresh(Dispatcher, Name, [Index]);
refresh(Dispatcher, Name, Indexes)
    when is_list(Name), is_list(Indexes) ->
    cloudi:send_sync(Dispatcher, Name,
                     {refresh, Indexes}).

%% @doc Flush all indices
%% _equiv flush(Dispatcher, Name, ?ALL).
-spec flush(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
flush(Dispatcher, Name) when is_list(Name) ->
    flush(Dispatcher, Name, ?ALL).

%% @doc Flush one or more indices
-spec flush(dispatcher(), name(), index() | [index()]) ->
    {ok, response()} |
    {error, any()}.
flush(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    flush(Dispatcher, Name, [Index]);
flush(Dispatcher, Name, Indexes)
    when is_list(Name), is_list(Indexes) ->
    cloudi:send_sync(Dispatcher, Name,
                     {flush, Indexes}).

%% _equiv optimize(Dispatcher, Name, ?ALL).
%% @doc Optimize all indices
-spec optimize(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
optimize(Dispatcher, Name)
    when is_list(Name) ->
    optimize(Dispatcher, Name, ?ALL).

%% @doc Optimize one or more indices
-spec optimize(dispatcher(), name(), index() | [index()]) ->
    {ok, response()} |
    {error, any()}.
optimize(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    optimize(Dispatcher, Name, [Index]);
optimize(Dispatcher, Name, Indexes)
    when is_list(Name), is_list(Indexes) ->
    cloudi:send_sync(Dispatcher, Name,
                     {optimize, Indexes}).

%% _equiv segments(Dispatcher, Name, ?ALL).
%% @doc Optimize all indices
-spec segments(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
segments(Dispatcher, Name)
    when is_list(Name) ->
    segments(Dispatcher, Name, ?ALL).

%% @doc Optimize one or more indices
-spec segments(dispatcher(), name(), index() | [index()]) ->
    {ok, response()} |
    {error, any()}.
segments(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    segments(Dispatcher, Name, [Index]);
segments(Dispatcher, Name, Indexes)
    when is_list(Name), is_list(Indexes) ->
    cloudi:send_sync(Dispatcher, Name,
                     {segments, Indexes}).

%% _equiv clear_cache(Dispatcher, Name, ?ALL, []).
%% @doc Clear all the caches
-spec clear_cache(dispatcher(), name()) ->
    {ok, response()} |
    {error, any()}.
clear_cache(Dispatcher, Name)
    when is_list(Name) ->
    clear_cache(Dispatcher, Name, ?ALL, []).

%% _equiv clear_cache(Dispatcher, Name, Indexes, []).
-spec clear_cache(dispatcher(), name(), index() | [index()]) ->
    {ok, response()} |
    {error, any()}.
clear_cache(Dispatcher, Name, Index)
    when is_list(Name), is_binary(Index) ->
    clear_cache(Dispatcher, Name, [Index], []);
clear_cache(Dispatcher, Name, Indexes)
    when is_list(Name), is_list(Indexes) ->
    clear_cache(Dispatcher, Name, Indexes, []).

%% _equiv clear_cache(Dispatcher, Name, Indexes, []).
-spec clear_cache(dispatcher(), name(), index() | [index()], params()) ->
    {ok, response()} | {error, any()}.
clear_cache(Dispatcher, Name, Index, Params)
    when is_list(Name), is_binary(Index), is_list(Params) ->
    clear_cache(Dispatcher, Name, [Index], Params);
clear_cache(Dispatcher, Name, Indexes, Params)
    when is_list(Name), is_list(Indexes), is_list(Params) ->
    cloudi:send_sync(Dispatcher, Name,
                     {clear_cache, Indexes, Params}).
%% @doc Insert a mapping into an ElasticSearch index
-spec put_mapping(dispatcher(), name(), index() | [index()], type(), doc()) -> {ok, response()} | {error, any()}.
put_mapping(Dispatcher, Name, Index, Type, Doc) when is_list(Name), is_binary(Index) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    put_mapping(Dispatcher, Name, [Index], Type, Doc);
put_mapping(Dispatcher, Name, Indexes, Type, Doc) when is_list(Name), is_list(Indexes) andalso is_binary(Type) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    cloudi:send_sync(Dispatcher, Name, {put_mapping, Indexes, Type, Doc}).

%% @doc Get a mapping from an ElasticSearch index
-spec get_mapping(dispatcher(), name(), index() | [index()], type()) -> {ok, response()} | {error, any()}.
get_mapping(Dispatcher, Name, Index, Type) when is_list(Name), is_binary(Index) andalso is_binary(Type) ->
    get_mapping(Dispatcher, Name, [Index], Type);
get_mapping(Dispatcher, Name, Indexes, Type) when  is_list(Name),is_list(Indexes) andalso is_binary(Type) ->
    cloudi:send_sync(Dispatcher, Name, {get_mapping, Indexes, Type}).

%% @doc Delete a mapping from an ElasticSearch index
-spec delete_mapping(dispatcher(), name(), index() | [index()], type()) -> {ok, response()} | {error, any()}.
delete_mapping(Dispatcher, Name, Index, Type) when is_list(Name), is_binary(Index) andalso is_binary(Type) ->
    delete_mapping(Dispatcher, Name, [Index], Type);
delete_mapping(Dispatcher, Name, Indexes, Type) when  is_list(Name), is_list(Indexes) andalso is_binary(Type) ->
    cloudi:send_sync(Dispatcher, Name, {delete_mapping, Indexes, Type}).

%% @doc Operate on aliases (as compared to 'alias')
-spec aliases(dispatcher(), name(), doc()) -> {ok, response()} | {error, any()}.
aliases(Dispatcher, Name, Doc) when is_list(Name), is_list(Name), (is_binary(Doc) orelse is_list(Doc)) ->
    cloudi:send_sync(Dispatcher, Name, {aliases, Doc}).

%% @doc Insert an alias (as compared to 'aliases')
-spec insert_alias(dispatcher(), name(), index(), index()) -> {ok, response()} | {error, any()}.
insert_alias(Dispatcher, Name, Index, Alias) when is_list(Name), is_binary(Index) andalso is_binary(Alias) ->
    cloudi:send_sync(Dispatcher, Name, {insert_alias, Index, Alias}).
%% @doc Insert an alias with options(as compared to 'aliases')
-spec insert_alias(dispatcher(), name(), index(), index(), doc()) -> {ok, response()} | {error, any()}.
insert_alias(Dispatcher, Name, Index, Alias, Doc) when  is_list(Name),is_binary(Index) andalso is_binary(Alias) andalso (is_binary(Doc) orelse is_list(Doc)) ->
    cloudi:send_sync(Dispatcher, Name, {insert_alias, Index, Alias, Doc}).

%% @doc Delete an alias (as compared to 'aliases')
-spec delete_alias(dispatcher(), name(), index(), index()) -> {ok, response()} | {error, any()}.
delete_alias(Dispatcher, Name, Index, Alias) when is_list(Name), is_binary(Index) andalso is_binary(Alias) ->
    cloudi:send_sync(Dispatcher, Name, {delete_alias, Index, Alias}).

%% @doc Checks if an alias exists (Alias can be a string with a wildcard)
-spec is_alias(dispatcher(), name(), index(), index()) -> {ok, response()} | {error, any()}.
is_alias(Dispatcher, Name, Index, Alias) when is_list(Name), is_binary(Index) andalso is_binary(Alias) ->
    cloudi:send_sync(Dispatcher, Name, {is_alias, Index, Alias}).

%% @doc Gets an alias(or more, based on the string)
-spec get_alias(dispatcher(), name(), index(), index()) -> {ok, response()} | {error, any()}.
get_alias(Dispatcher, Name, Index, Alias) when is_list(Name), is_binary(Index) andalso is_binary(Alias) ->
    cloudi:send_sync(Dispatcher, Name, {get_alias, Index, Alias}).

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
        Command ->
            {reply, process_query(PoolName, Command), State}
    end.


cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, undefined) ->
    ok;
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
    try 
        Command =  cloudi_string:binary_to_term(Query),
        process_query(BinaryPoolName, Command)
    of
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

process_query(PoolName, {'health'}) -> 
    cloudi_x_erlasticsearch:health(PoolName);
process_query(PoolName, {'state'}) ->
    cloudi_x_erlasticsearch:state(PoolName, []);
process_query(PoolName, {'state', Params}) -> 
    cloudi_x_erlasticsearch:state(PoolName, Params);
process_query(PoolName, {'nodes_info'}) ->
    cloudi_x_erlasticsearch:nodes_info(PoolName, [], []);
process_query(PoolName, {'nodes_info', NodeNames}) ->
    cloudi_x_erlasticsearch:nodes_info(PoolName, NodeNames, []);
process_query(PoolName, {'nodes_info', NodeNames, Params}) -> 
    cloudi_x_erlasticsearch:nodes_info(PoolName, NodeNames, Params);
process_query(PoolName, {'nodes_stats'}) ->
    cloudi_x_erlasticsearch:nodes_stats(PoolName, [], []);
process_query(PoolName, {'nodes_stats', NodeNames}) ->
    cloudi_x_erlasticsearch:nodes_stats(PoolName, NodeNames, []);
process_query(PoolName, {'nodes_stats', NodeNames, Params}) -> 
    cloudi_x_erlasticsearch:nodes_stats(PoolName, NodeNames, Params);
process_query(PoolName, {'status', Indexes}) -> 
    cloudi_x_erlasticsearch:status(PoolName, Indexes);
process_query(PoolName, {'create_index', Index}) ->
    cloudi_x_erlasticsearch:create_index(PoolName, Index, <<>>);
process_query(PoolName, {'create_index', Index, Doc}) -> 
    cloudi_x_erlasticsearch:create_index(PoolName, Index, Doc);
process_query(PoolName, {'delete_index'}) ->
    cloudi_x_erlasticsearch:delete_index(PoolName, ?ALL);
process_query(PoolName, {'delete_index', Index}) -> 
    cloudi_x_erlasticsearch:delete_index(PoolName, Index);
process_query(PoolName, {'open_index', Index}) -> 
    cloudi_x_erlasticsearch:open_index(PoolName, Index);
process_query(PoolName, {'close_index', Index}) -> 
    cloudi_x_erlasticsearch:close_index(PoolName, Index);
process_query(PoolName, {'is_index', Indexes}) -> 
    cloudi_x_erlasticsearch:is_index(PoolName, Indexes);
process_query(PoolName, {'count', Doc}) ->
    cloudi_x_erlasticsearch:count(PoolName, ?ALL, [], Doc, []);
process_query(PoolName, {'count', Doc, Params}) ->
    cloudi_x_erlasticsearch:count(PoolName, ?ALL, [], Doc, Params);
process_query(PoolName, {'count', Indexes, Doc, Params}) -> 
    cloudi_x_erlasticsearch:count(PoolName, Indexes, [], Doc, Params);
process_query(PoolName, {'count', Indexes, Types, Doc, Params}) -> 
    cloudi_x_erlasticsearch:count(PoolName, Indexes, Types, Doc, Params);
process_query(PoolName, {'delete_by_query', Doc}) ->
    cloudi_x_erlasticsearch:delete_by_query(PoolName, ?ALL, [], Doc, []);
process_query(PoolName, {'delete_by_query', Doc, Params}) -> 
    cloudi_x_erlasticsearch:delete_by_query(PoolName, ?ALL, [], Doc, Params);
process_query(PoolName, {'delete_by_query', Indexes, Doc, Params}) -> 
    cloudi_x_erlasticsearch:delete_by_query(PoolName, Indexes, [], Doc, Params);
process_query(PoolName, {'delete_by_query', Indexes, Types, Doc, Params}) -> 
    cloudi_x_erlasticsearch:delete_by_query(PoolName, Indexes, Types, Doc, Params);
process_query(PoolName, {'is_type', Indexes, Types}) -> 
    cloudi_x_erlasticsearch:is_type(PoolName, Indexes, Types);
process_query(PoolName, {'insert_doc', Index, Type, Id, Doc}) ->
    cloudi_x_erlasticsearch:insert_doc(PoolName, Index, Type, Id, Doc, []);
process_query(PoolName, {'insert_doc', Index, Type, Id, Doc, Params}) -> 
    cloudi_x_erlasticsearch:insert_doc(PoolName, Index, Type, Id, Doc, Params);
process_query(PoolName, {'update_doc', Index, Type, Id, Doc}) ->
    cloudi_x_erlasticsearch:update_doc(PoolName, Index, Type, Id, Doc, []);
process_query(PoolName, {'update_doc', Index, Type, Id, Doc, Params}) -> 
    cloudi_x_erlasticsearch:update_doc(PoolName, Index, Type, Id, Doc, Params);
process_query(PoolName, {'is_doc', Index, Type, Id}) -> 
    cloudi_x_erlasticsearch:is_doc(PoolName, Index, Type, Id);
process_query(PoolName, {'get_doc', Index, Type, Id}) ->
    cloudi_x_erlasticsearch:get_doc(PoolName, Index, Type, Id, []);
process_query(PoolName, {'get_doc', Index, Type, Id, Params}) -> 
    cloudi_x_erlasticsearch:get_doc(PoolName, Index, Type, Id, Params);
process_query(PoolName, {'mget_doc', Index, Doc}) -> 
    cloudi_x_erlasticsearch:mget_doc(PoolName, Index, <<>>, Doc);
process_query(PoolName, {'mget_doc', Index, Type, Doc}) -> 
    cloudi_x_erlasticsearch:mget_doc(PoolName, Index, Type, Doc);
process_query(PoolName, {'delete_doc', Index, Type, Id}) ->
    cloudi_x_erlasticsearch:delete_doc(PoolName, Index, Type, Id, []);
process_query(PoolName, {'delete_doc', Index, Type, Id, Params}) -> 
    cloudi_x_erlasticsearch:delete_doc(PoolName, Index, Type, Id, Params);
process_query(PoolName, {'search', Index, Type, Doc}) ->
    cloudi_x_erlasticsearch:search(PoolName, Index, Type, Doc, []);
process_query(PoolName, {'search', Index, Type, Doc, Params}) -> 
    cloudi_x_erlasticsearch:search(PoolName, Index, Type, Doc, Params);
process_query(PoolName, {'refresh'}) ->
    cloudi_x_erlasticsearch:refresh(PoolName, ?ALL);
process_query(PoolName, {'refresh', Indexes}) ->
    cloudi_x_erlasticsearch:refresh(PoolName, Indexes);
process_query(PoolName, {'flush'}) ->
    cloudi_x_erlasticsearch:flush(PoolName, ?ALL);
process_query(PoolName, {'flush', Indexes}) ->
    cloudi_x_erlasticsearch:flush(PoolName, Indexes);
process_query(PoolName, {'optimize'}) ->
    cloudi_x_erlasticsearch:optimize(PoolName, ?ALL);
process_query(PoolName, {'optimize', Indexes}) ->
    cloudi_x_erlasticsearch:optimize(PoolName, Indexes);
process_query(PoolName, {'segments'}) ->
    cloudi_x_erlasticsearch:segments(PoolName, ?ALL);
process_query(PoolName, {'segments', Indexes}) ->
    cloudi_x_erlasticsearch:segments(PoolName, Indexes);
process_query(PoolName, {'clear_cache'}) ->
    cloudi_x_erlasticsearch:clear_cache(PoolName, ?ALL, []);
process_query(PoolName, {'clear_cache', Indexes}) ->
    cloudi_x_erlasticsearch:clear_cache(PoolName, Indexes, []);
process_query(PoolName, {'clear_cache', Indexes, Params}) ->
    cloudi_x_erlasticsearch:clear_cache(PoolName, Indexes, Params);
process_query(PoolName, {'put_mapping', Indexes, Type, Doc}) ->
    cloudi_x_erlasticsearch:put_mapping(PoolName, Indexes, Type, Doc);
process_query(PoolName, {'get_mapping', Indexes, Type}) ->
    cloudi_x_erlasticsearch:get_mapping(PoolName, Indexes, Type);
process_query(PoolName, {'delete_mapping', Indexes, Type}) ->
    cloudi_x_erlasticsearch:delete_mapping(PoolName, Indexes, Type);
process_query(PoolName, {'aliases', Doc}) ->
    cloudi_x_erlasticsearch:aliases(PoolName, Doc);
process_query(PoolName, {'insert_alias', Index, Alias}) ->
    cloudi_x_erlasticsearch:insert_alias(PoolName, Index, Alias);
process_query(PoolName, {'insert_alias', Index, Alias, Doc}) ->
    cloudi_x_erlasticsearch:insert_alias(PoolName, Index, Alias, Doc);
process_query(PoolName, {'delete_alias', Index, Alias}) ->
    cloudi_x_erlasticsearch:delete_alias(PoolName, Index, Alias);
process_query(PoolName, {'is_alias', Index, Alias}) ->
    cloudi_x_erlasticsearch:is_alias(PoolName, Index, Alias);
process_query(PoolName, {'get_alias', Index, Alias}) ->
    cloudi_x_erlasticsearch:get_alias(PoolName, Index, Alias).

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

