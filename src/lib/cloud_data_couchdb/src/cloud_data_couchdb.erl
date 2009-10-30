%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi CouchDB Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%%         This product includes software developed by Michael Truog
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2009 Michael Truog
%%% @version 0.0.8 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_data_couchdb).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).
-behaviour(cloud_data_interface).

%% external interface

%% couchdb API
-export([create_database/1, create_database/2,
         delete_database/1, delete_database/2,
         database_info/1, database_info/2,
         server_info/1, server_info/2,
         retrieve_all_dbs/1, retrieve_all_dbs/2,
         create_attachment/4, create_attachment/5,
         create_document/2, create_document/3,
         create_document_id/3, create_document_id/4,
         create_documents/2, create_documents/3,
         document_revision/2, document_revision/3,
         retrieve_document/2, retrieve_document/3,
         update_document/3, update_document/4,
         update_document_rev/4, update_document_rev/5,
         replace_document/3, replace_document/4,
         replace_document_rev/4, replace_document_rev/5,
         delete_document/2, delete_document/3,
         delete_document_rev/3, delete_document_rev/4,
         delete_documents/2, delete_documents/3,
         create_views/3, create_views/4,
         create_view/4, create_view/5,
         create_view_type/5, create_view_type/6,
         invoke_view/3, invoke_view/4,
         invoke_view_keys/4, invoke_view_keys/5]).

%% do_queries_group/5 interface
-export([do_queries_sequentially/2]).

%% cloud_data_interface callbacks
-export([start_link/2, handle_stop/1, handle_do_queries/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").
-include("cloud_types.hrl").

-define(DEFAULT_HOST_NAME, "127.0.0.1").
-define(DEFAULT_PORT,      5984).
-define(DEFAULT_TIMEOUT,   20000). % 20 seconds
-define(DEFAULT_DATABASE,  "default"). % if the configuration does not specify

-record(state,
    {
    data_title = undefined,
    hostname = undefined,
    port = undefined,
    connection = undefined,
    database = undefined,
    timeout = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create the database.===
%% @end
%%-------------------------------------------------------------------------

-spec create_database(DataTitle :: atom()) ->
    'ok' |
    {'error', any()}.

-spec create_database(DataTitle :: atom(),
                      Timeout :: 'undefined' | pos_integer()) ->
    'ok' |
    {'error', any()}.

create_database(DataTitle) ->
    create_database(DataTitle, undefined).

create_database(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {create_database, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete the database.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_database(DataTitle :: atom()) ->
    'ok' |
    {'error', any()}.

-spec delete_database(DataTitle :: atom(),
                      Timeout :: 'undefined' | pos_integer()) ->
    'ok' |
    {'error', any()}.

delete_database(DataTitle) ->
    delete_database(DataTitle, undefined).

delete_database(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {delete_database, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get information about the database.===
%% @end
%%-------------------------------------------------------------------------

-spec database_info(DataTitle :: atom()) ->
    {'ok', list()} |
    {'error', any()}.

-spec database_info(DataTitle :: atom(),
                    Timeout :: 'undefined' | pos_integer()) ->
    {'ok', list()} |
    {'error', any()}.

database_info(DataTitle) ->
    database_info(DataTitle, undefined).

database_info(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {database_info, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get information about the server.===
%% @end
%%-------------------------------------------------------------------------

-spec server_info(DataTitle :: atom()) ->
    {'ok', list()} |
    {'error', any()}.

-spec server_info(DataTitle :: atom(),
                  Timeout :: 'undefined' | pos_integer()) ->
    {'ok', list()} |
    {'error', any()}.

server_info(DataTitle) ->
    server_info(DataTitle, undefined).

server_info(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {server_info, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Retrieve a list of all the databases.===
%% @end
%%-------------------------------------------------------------------------

-spec retrieve_all_dbs(DataTitle :: atom()) ->
    {'ok', list()} |
    {'error', any()}.

-spec retrieve_all_dbs(DataTitle :: atom(),
                       Timeout :: 'undefined' | pos_integer()) ->
    {'ok', list()} |
    {'error', any()}.

retrieve_all_dbs(DataTitle) ->
    retrieve_all_dbs(DataTitle, undefined).

retrieve_all_dbs(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {retrieve_all_dbs, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a document attachment from a file.===
%% The file is referenced by its filename.
%% @end
%%-------------------------------------------------------------------------

-spec create_attachment(DataTitle :: atom(),
                        DocumentID :: string(),
                        File :: string(),
                        ContentType :: string()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec create_attachment(DataTitle :: atom(),
                        DocumentID :: string(),
                        File :: string(),
                        ContentType :: string(),
                        Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

create_attachment(DataTitle, DocumentID, File, ContentType) ->
    create_attachment(DataTitle, DocumentID, File, ContentType, undefined).

create_attachment(DataTitle, DocumentID, File, ContentType, Timeout)
    when is_list(DocumentID), is_list(File),
         is_list(ContentType) ->
    gen_server:call(DataTitle,
        {create_attachment, DocumentID, File, ContentType, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a document and allow the server to create the document id.===
%% @end
%%-------------------------------------------------------------------------

-spec create_document(DataTitle :: atom(),
                      Doc :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec create_document(DataTitle :: atom(),
                      Doc :: list(),
                      Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

create_document(DataTitle, Doc) ->
    create_document(DataTitle, Doc, undefined).

create_document(DataTitle, Doc, Timeout)
    when is_list(Doc) ->
    gen_server:call(DataTitle,
        {create_document, Doc, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a document with a specific document id.===
%% @end
%%-------------------------------------------------------------------------

-spec create_document_id(DataTitle :: atom(),
                         DocumentID :: string(),
                         Doc :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec create_document_id(DataTitle :: atom(),
                         DocumentID :: string(),
                         Doc :: list(),
                         Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

create_document_id(DataTitle, DocumentID, Doc) ->
    create_document_id(DataTitle, DocumentID, Doc, undefined).

create_document_id(DataTitle, DocumentID, Doc, Timeout)
    when is_list(DocumentID), is_list(Doc) ->
    gen_server:call(DataTitle,
        {create_document, DocumentID, Doc, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create many documents.===
%% @end
%%-------------------------------------------------------------------------

-spec create_documents(DataTitle :: atom(),
                       Documents :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec create_documents(DataTitle :: atom(),
                       Documents :: list(),
                       Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

create_documents(DataTitle, Documents) ->
    create_documents(DataTitle, Documents, undefined).

create_documents(DataTitle, Documents, Timeout)
    when is_list(Documents) ->
    gen_server:call(DataTitle,
        {create_documents, Documents, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current revision of a document.===
%% @end
%%-------------------------------------------------------------------------

-spec document_revision(DataTitle :: atom(),
                        DocumentID :: string()) ->
    {'ok', {any(), any()}} |
    {'error', any()}.

-spec document_revision(DataTitle :: atom(),
                        DocumentID :: string(),
                        Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {any(), any()}} |
    {'error', any()}.

document_revision(DataTitle, DocumentID) ->
    document_revision(DataTitle, DocumentID, undefined).

document_revision(DataTitle, DocumentID, Timeout)
    when is_list(DocumentID) ->
    gen_server:call(DataTitle,
        {document_revision, DocumentID, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec retrieve_document(DataTitle :: atom(),
                        DocumentID :: string()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec retrieve_document(DataTitle :: atom(),
                        DocumentID :: string(),
                        Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

retrieve_document(DataTitle, DocumentID) ->
    retrieve_document(DataTitle, DocumentID, undefined).

retrieve_document(DataTitle, DocumentID, Timeout)
    when is_list(DocumentID) ->
    gen_server:call(DataTitle,
        {retrieve_document, DocumentID, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec update_document(DataTitle :: atom(),
                      DocumentID :: string(),
                      Doc :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec update_document(DataTitle :: atom(),
                      DocumentID :: string(),
                      Doc :: list(),
                      Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

update_document(DataTitle, DocumentID, Doc) ->
    update_document(DataTitle, DocumentID, Doc, undefined).

update_document(DataTitle, DocumentID, Doc, Timeout)
    when is_list(DocumentID), is_list(Doc) ->
    gen_server:call(DataTitle,
        {update_document, DocumentID, Doc, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a specific revision of a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec update_document_rev(DataTitle :: atom(),
                          DocumentID :: string(),
                          Rev :: string(),
                          Doc :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec update_document_rev(DataTitle :: atom(),
                          DocumentID :: string(),
                          Rev :: string(),
                          Doc :: list(),
                          Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

update_document_rev(DataTitle, DocumentID, Rev, Doc) ->
    update_document_rev(DataTitle, DocumentID, Rev, Doc, undefined).

update_document_rev(DataTitle, DocumentID, Rev, Doc, Timeout)
    when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
    gen_server:call(DataTitle,
        {update_document, DocumentID, Rev, Doc, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Replace a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec replace_document(DataTitle :: atom(),
                      DocumentID :: string(),
                      Doc :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec replace_document(DataTitle :: atom(),
                      DocumentID :: string(),
                      Doc :: list(),
                      Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

replace_document(DataTitle, DocumentID, Doc) ->
    replace_document(DataTitle, DocumentID, Doc, undefined).

replace_document(DataTitle, DocumentID, Doc, Timeout)
    when is_list(DocumentID), is_list(Doc) ->
    gen_server:call(DataTitle,
        {replace_document, DocumentID, Doc, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Replace a specific revision of a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec replace_document_rev(DataTitle :: atom(),
                           DocumentID :: string(),
                           Rev :: string(),
                           Doc :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec replace_document_rev(DataTitle :: atom(),
                           DocumentID :: string(),
                           Rev :: string(),
                           Doc :: list(),
                           Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

replace_document_rev(DataTitle, DocumentID, Rev, Doc) ->
    replace_document_rev(DataTitle, DocumentID, Rev, Doc, undefined).

replace_document_rev(DataTitle, DocumentID, Rev, Doc, Timeout)
    when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
    gen_server:call(DataTitle,
        {replace_document, DocumentID, Rev, Doc, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_document(DataTitle :: atom(),
                      DocumentID :: string()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec delete_document(DataTitle :: atom(),
                      DocumentID :: string(),
                      Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

delete_document(DataTitle, DocumentID) ->
    delete_document(DataTitle, DocumentID, undefined).

delete_document(DataTitle, DocumentID, Timeout)
    when is_list(DocumentID) ->
    gen_server:call(DataTitle,
        {delete_document, DocumentID, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a specific revision of a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_document_rev(DataTitle :: atom(),
                          DocumentID :: string(),
                          Rev :: string()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec delete_document_rev(DataTitle :: atom(),
                          DocumentID :: string(),
                          Rev :: string(),
                          Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

delete_document_rev(DataTitle, DocumentID, Rev) ->
    delete_document_rev(DataTitle, DocumentID, Rev, undefined).

delete_document_rev(DataTitle, DocumentID, Rev, Timeout)
    when is_list(DocumentID), is_list(Rev) ->
    gen_server:call(DataTitle,
        {delete_document, DocumentID, Rev, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete many documents.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_documents(DataTitle :: atom(),
                       Documents :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec delete_documents(DataTitle :: atom(),
                       Documents :: list(),
                       Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

delete_documents(DataTitle, Documents) ->
    delete_documents(DataTitle, Documents, undefined).

delete_documents(DataTitle, Documents, Timeout)
    when is_list(Documents) ->
    gen_server:call(DataTitle,
        {delete_documents, Documents, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create many views.===
%% @end
%%-------------------------------------------------------------------------

-spec create_views(DataTitle :: atom(),
                   DocName :: string(),
                   ViewList :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec create_views(DataTitle :: atom(),
                   DocName :: string(),
                   ViewList :: list(),
                   Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

create_views(DataTitle, DocName, ViewList) ->
    create_views(DataTitle, DocName, ViewList, undefined).

create_views(DataTitle, DocName, ViewList, Timeout)
    when is_list(DocName), is_list(ViewList) ->
    gen_server:call(DataTitle,
        {create_view, DocName, ViewList, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a view.===
%% @end
%%-------------------------------------------------------------------------

-spec create_view(DataTitle :: atom(),
                  DocName :: string(),
                  ViewName :: string(),
                  Data :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec create_view(DataTitle :: atom(),
                  DocName :: string(),
                  ViewName :: string(),
                  Data :: list(),
                  Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

create_view(DataTitle, DocName, ViewName, Data) ->
    create_view(DataTitle, DocName, ViewName, Data, undefined).

create_view(DataTitle, DocName, ViewName, Data, Timeout)
    when is_list(DocName), is_list(ViewName), is_list(Data) ->
    gen_server:call(DataTitle,
        {create_view, DocName, ViewName, Data, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a view of a unique type.===
%% @end
%%-------------------------------------------------------------------------

-spec create_view_type(DataTitle :: atom(),
                       DocName :: string(),
                       Type :: string(),
                       ViewName :: string(),
                       Data :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec create_view_type(DataTitle :: atom(),
                       DocName :: string(),
                       Type :: string(),
                       ViewName :: string(),
                       Data :: list(),
                       Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

create_view_type(DataTitle, DocName,
                 Type, ViewName, Data) ->
    create_view_type(DataTitle, DocName,
                     Type, ViewName, Data, undefined).

create_view_type(DataTitle, DocName, Type, ViewName, Data, Timeout)
    when is_list(DocName), is_list(Type),
         is_list(ViewName), is_list(Data) ->
    gen_server:call(DataTitle,
        {create_view, DocName,
         Type, ViewName, Data, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invoke a view.===
%% @end
%%-------------------------------------------------------------------------

-spec invoke_view(DataTitle :: atom(),
                  DocName :: string(),
                  ViewName :: string()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec invoke_view(DataTitle :: atom(),
                  DocName :: string(),
                  ViewName :: string(),
                  Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

invoke_view(DataTitle, DocName, ViewName) ->
    invoke_view(DataTitle, DocName, ViewName, undefined).

invoke_view(DataTitle, DocName, ViewName, Timeout)
    when is_list(DocName), is_list(ViewName) ->
    gen_server:call(DataTitle,
        {invoke_view, DocName, ViewName, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invoke a view with a list of keys.===
%% @end
%%-------------------------------------------------------------------------

-spec invoke_view_keys(DataTitle :: atom(),
                       DocName :: string(),
                       ViewName :: string(),
                       Keys :: list()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

-spec invoke_view_keys(DataTitle :: atom(),
                       DocName :: string(),
                       ViewName :: string(),
                       Keys :: list(),
                       Timeout :: 'undefined' | pos_integer()) ->
    {'ok', {'json', {'struct', list()}}} |
    {'error', any()}.

invoke_view_keys(DataTitle, DocName, ViewName, Keys) ->
    invoke_view_keys(DataTitle, DocName, ViewName, Keys, undefined).

invoke_view_keys(DataTitle, DocName, ViewName, Keys, Timeout)
    when is_list(DocName), is_list(ViewName),
         is_list(Keys) ->
    gen_server:call(DataTitle,
        {invoke_view, DocName, ViewName, Keys, Timeout}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from cloud_data_interface
%%%------------------------------------------------------------------------

-spec start_link(DataTitle :: atom(),
                 Arguments :: list({atom(), string(), list({_,_})})) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(DataTitle, Arguments) when is_atom(DataTitle), is_list(Arguments) ->
    gen_server:start_link({local, DataTitle}, ?MODULE,
        [DataTitle, Arguments], []).

-spec handle_stop(DataTitle :: atom()) -> any().

handle_stop(DataTitle) when is_atom(DataTitle) ->
    gen_server:call(DataTitle, stop).

-spec handle_do_queries(DataTitle :: atom(),
                        QueryList :: data_list()) ->
    {'ok', data_list()} |
    {'error', data_list()}.

handle_do_queries(DataTitle, QueryList)
    when is_atom(DataTitle), is_list(QueryList) ->
    % depend on DEFAULT_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {do_queries, QueryList}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([DataTitle, Arguments]) when is_atom(DataTitle), is_list(Arguments) ->
    init_state(DataTitle, Arguments).
handle_call({'create_database', undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_database_c,
        [Connection, HostName, Port,
         Database, Timeout], State);
handle_call({'create_database', Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_database_c,
        [Connection, HostName, Port,
         Database, Timeout], State);
handle_call({'delete_database', undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, delete_database_c,
        [Connection, HostName, Port,
         Database, Timeout], State);
handle_call({'delete_database', Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, delete_database_c,
        [Connection, HostName, Port,
         Database, Timeout], State);
handle_call({'database_info', undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, database_info_c,
        [Connection, HostName, Port,
         Database, Timeout], State);
handle_call({'database_info', Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, database_info_c,
        [Connection, HostName, Port,
         Database, Timeout], State);
handle_call({'server_info', undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, server_info_c,
        [Connection, HostName, Port,
         Timeout], State);
handle_call({'server_info', Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, server_info_c,
        [Connection, HostName, Port,
         Timeout], State);
handle_call({'retrieve_all_dbs', undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, retrieve_all_dbs_c,
        [Connection, HostName, Port,
         Timeout], State);
handle_call({'retrieve_all_dbs', Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, retrieve_all_dbs_c,
        [Connection, HostName, Port,
         Timeout], State);
handle_call({'create_attachment', DocumentID, File, ContentType, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_attachment_c,
        [Connection, HostName, Port,
         Database, DocumentID, File, ContentType, Timeout], State);
handle_call({'create_attachment', DocumentID, File, ContentType, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_attachment_c,
        [Connection, HostName, Port,
         Database, DocumentID, File, ContentType, Timeout], State);
handle_call({'create_document', Doc, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_document_c,
        [Connection, HostName, Port,
         Database, Doc, Timeout], State);
handle_call({'create_document', Doc, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_document_c,
        [Connection, HostName, Port,
         Database, Doc, Timeout], State);
handle_call({'create_document', DocumentID, Doc, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Doc, Timeout], State);
handle_call({'create_document', DocumentID, Doc, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Doc, Timeout], State);
handle_call({'create_documents', Documents, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_documents_c,
        [Connection, HostName, Port,
         Database, Documents, Timeout], State);
handle_call({'create_documents', Documents, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_documents_c,
        [Connection, HostName, Port,
         Database, Documents, Timeout], State);
handle_call({'document_revision', DocumentID, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, document_revision_c,
        [Connection, HostName, Port,
         Database, DocumentID, Timeout], State);
handle_call({'document_revision', DocumentID, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, document_revision_c,
        [Connection, HostName, Port,
         Database, DocumentID, Timeout], State);
handle_call({'retrieve_document', DocumentID, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, retrieve_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Timeout], State);
handle_call({'retrieve_document', DocumentID, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, retrieve_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Timeout], State);
handle_call({'update_document', DocumentID, Doc, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, update_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Doc, Timeout], State);
handle_call({'update_document', DocumentID, Doc, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, update_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Doc, Timeout], State);
handle_call({'update_document', DocumentID, Rev, Doc, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, update_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Rev, Doc, Timeout], State);
handle_call({'update_document', DocumentID, Rev, Doc, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, update_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Rev, Doc, Timeout], State);
handle_call({'replace_document', DocumentID, Doc, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, replace_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Doc, Timeout], State);
handle_call({'replace_document', DocumentID, Doc, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, replace_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Doc, Timeout], State);
handle_call({'replace_document', DocumentID, Rev, Doc, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, replace_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Rev, Doc, Timeout], State);
handle_call({'replace_document', DocumentID, Rev, Doc, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, replace_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Rev, Doc, Timeout], State);
handle_call({'delete_document', DocumentID, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, delete_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Timeout], State);
handle_call({'delete_document', DocumentID, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, delete_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Timeout], State);
handle_call({'delete_document', DocumentID, Rev, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, delete_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Rev, Timeout], State);
handle_call({'delete_document', DocumentID, Rev, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, delete_document_c,
        [Connection, HostName, Port,
         Database, DocumentID, Rev, Timeout], State);
handle_call({'delete_documents', Documents, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, delete_documents_c,
        [Connection, HostName, Port,
         Database, Documents, Timeout], State);
handle_call({'delete_documents', Documents, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, delete_documents_c,
        [Connection, HostName, Port,
         Database, Documents, Timeout], State);
handle_call({'create_view', DocName, ViewList, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewList, Timeout], State);
handle_call({'create_view', DocName, ViewList, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewList, Timeout], State);
handle_call({'create_view', DocName, ViewName, Data, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewName, Data, Timeout], State);
handle_call({'create_view', DocName, ViewName, Data, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewName, Data, Timeout], State);
handle_call({'create_view', DocName, Type, ViewName, Data, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, create_view_c,
        [Connection, HostName, Port,
         Database, DocName, Type, ViewName, Data, Timeout], State);
handle_call({'create_view', DocName, Type, ViewName, Data, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, create_view_c,
        [Connection, HostName, Port,
         Database, DocName, Type, ViewName, Data, Timeout], State);
handle_call({'invoke_view', DocName, ViewName, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, invoke_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewName, Timeout], State);
handle_call({'invoke_view', DocName, ViewName, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, invoke_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewName, Timeout], State);
handle_call({'invoke_view', DocName, ViewName, Keys, undefined}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database,
                   timeout = Timeout} = State) ->
    reply_with_result(ecouchdb, invoke_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewName, Keys, Timeout], State);
handle_call({'invoke_view', DocName, ViewName, Keys, Timeout}, _,
            #state{hostname = HostName,
                   port = Port,
                   connection = Connection,
                   database = Database} = State)
    when is_integer(Timeout), Timeout > 0 ->
    reply_with_result(ecouchdb, invoke_view_c,
        [Connection, HostName, Port,
         Database, DocName, ViewName, Keys, Timeout], State);
handle_call(stop, _,
            #state{data_title = DataTitle,
                   connection = Connection} = State) ->
    ecouchdb:close(Connection),
    {stop, atom_to_list(DataTitle) ++ " was requested to stop", State};
handle_call({do_queries, QueryList}, _,
            #state{data_title = DataTitle} = State) ->
    Response = cloud_data_interface:do_queries_group(QueryList,
        cloud_data_couchdb, do_queries_sequentially, State, DataTitle),
    {reply, Response, State, hibernate};
handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.
handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.
handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.
terminate(_, #state{connection = Connection}) ->
    ecouchdb:close(Connection),
    ok.
code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% initialize the client state
init_state(DataTitle, Args)
    when is_atom(DataTitle), is_list(Args) ->
    Defaults = [
        {hostname, ?DEFAULT_HOST_NAME},
        {port, ?DEFAULT_PORT},
        {timeout, ?DEFAULT_TIMEOUT},
        {database, ?DEFAULT_DATABASE}],
    [HostName, Port, Timeout, Database, []] =
        proplists_extensions:take_values(Defaults, Args),
    case ecouchdb:connect(HostName, Port, Timeout) of
        {ok, Connection} ->
            {ok, #state{data_title = DataTitle,
                        hostname = HostName,
                        port = Port,
                        connection = Connection,
                        database = Database,
                        timeout = Timeout}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% check the result to determine if a new connection exists
reply_with_result(M, F, A, State) ->
    case erlang:apply(M, F, A) of
        {Result, Result, NewConnection} ->
            {reply, Result, State#state{connection = NewConnection}};
        {ResultState, ResultData, NewConnection} ->
            {reply, {ResultState, ResultData},
                    State#state{connection = NewConnection}};
        {Result, Result} ->
            {reply, Result, State};
        {ResultState, ResultData} ->
            {reply, {ResultState, ResultData}, State}
    end.

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Connection, HostName, Port, Database, Timeout) ->
    try (case string_extensions:binary_to_term(Query) of
            {'create_database'} ->
                ecouchdb:create_database_c(Connection, HostName, Port,
                                           Database, Timeout);
            'create_database' ->
                ecouchdb:create_database_c(Connection, HostName, Port,
                                           Database, Timeout);
            {'delete_database'} ->
                ecouchdb:delete_database_c(Connection, HostName, Port,
                                           Database, Timeout);
            'delete_database' ->
                ecouchdb:delete_database_c(Connection, HostName, Port,
                                           Database, Timeout);
            % database_info
            % server_info
            % retrieve_all_dbs
            {'create_attachment', DocumentID, File, ContentType}
                when is_list(DocumentID), is_list(File),
                     is_list(ContentType) ->
                ecouchdb:create_attachment_c(Connection, HostName, Port,
                                             Database, DocumentID, File,
                                             ContentType, Timeout);
            {'create_document', DocumentID, Doc}
                when is_list(DocumentID), is_list(Doc) ->
                ecouchdb:create_document_c(Connection, HostName, Port,
                                           Database, DocumentID, Doc, Timeout);
            % create_documents
            % document_revision
            % retrieve_document
            {'update_document', DocumentID, Doc}
                when is_list(DocumentID), is_list(Doc) ->
                ecouchdb:update_document_c(Connection, HostName, Port,
                                           Database, DocumentID, Doc, Timeout);
            {'update_document', DocumentID, Rev, Doc}
                when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
                ecouchdb:update_document_c(Connection, HostName, Port,
                                           Database, DocumentID, Rev,
                                           Doc, Timeout);
            {'replace_document', DocumentID, Doc}
                when is_list(DocumentID), is_list(Doc) ->
                ecouchdb:replace_document_c(Connection, HostName, Port,
                                            Database, DocumentID,
                                            Doc, Timeout);
            {'replace_document', DocumentID, Rev, Doc}
                when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
                ecouchdb:replace_document_c(Connection, HostName, Port,
                                            Database, DocumentID, Rev,
                                            Doc, Timeout);
            {'delete_document', DocumentID}
                when is_list(DocumentID) ->
                ecouchdb:delete_document_c(Connection, HostName, Port,
                                           Database, DocumentID, Timeout);
            {'delete_document', DocumentID, Rev}
                when is_list(DocumentID), is_list(Rev) ->
                ecouchdb:delete_document_c(Connection, HostName, Port,
                                           Database, DocumentID, Rev, Timeout);
            % delete_documents
            {'create_view', DocName, ViewList}
                when is_list(DocName), is_list(ViewList) ->
                ecouchdb:create_view_c(Connection, HostName, Port, Database,
                                       DocName, ViewList, Timeout);
            {'create_view', DocName, ViewName, Data}
                when is_list(DocName), is_list(ViewName), is_list(Data) ->
                ecouchdb:create_view_c(Connection, HostName, Port, Database,
                                       DocName, ViewName, Data, Timeout);
            {'create_view', DocName, Type, ViewName, Data}
                when is_list(DocName), is_list(Type),
                     is_list(ViewName), is_list(Data) ->
                ecouchdb:create_view_c(Connection, HostName, Port, Database,
                                       DocName, Type, ViewName, Data, Timeout);
            {'invoke_view', DocName, ViewName}
                when is_list(DocName), is_list(ViewName) ->
                ecouchdb:invoke_view_c(Connection, HostName, Port, Database,
                                       DocName, ViewName, Timeout);
            {'invoke_view', DocName,   ViewName, Keys}
                when is_list(DocName), is_list(ViewName), is_list(Keys) ->
                ecouchdb:invoke_view_c(Connection, HostName, Port, Database,
                                       DocName, ViewName, Keys, Timeout);
            _ ->
                {error, invalid_call}
    
        end) of
        {error, invalid_call} ->
            ?LOG_DEBUG("Invalid couchdb command tuple ~p",
                       [binary_to_list(Query)]),
            false;
        _ ->
            true
    catch
        _:Reason ->
            ?LOG_DEBUG("exception when processing "
                       "couchdb command tuple ~p: ~p",
                       [binary_to_list(Query), Reason]),
            false
    end.

%% do all queries in the list until an error is encountered
%% return the remaining list if there is an error, else an empty list
do_queries_sequentially(QueryList, #state{hostname = HostName,
                                          port = Port,
                                          connection = Connection,
                                          database = Database,
                                          timeout = Timeout})
    when is_list(QueryList) ->
    lists:dropwhile(fun(Query) ->
        do_query(Query, Connection, HostName, Port, Database, Timeout)
    end, QueryList).

