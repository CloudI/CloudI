%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI CouchDB Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2015 Michael Truog
%%% @version 1.5.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_couchdb).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% couchdb API
-export([create_database/2, create_database/3,
         delete_database/2, delete_database/3,
         database_info/2, database_info/3,
         server_info/2, server_info/3,
         retrieve_all_dbs/2, retrieve_all_dbs/3,
         create_attachment/5, create_attachment/6,
         create_document/3, create_document/4,
         create_document_id/4, create_document_id/5,
         create_documents/3, create_documents/4,
         document_revision/3, document_revision/4,
         retrieve_document/3, retrieve_document/4,
         update_document/4, update_document/5,
         update_document_rev/5, update_document_rev/6,
         replace_document/4, replace_document/5,
         replace_document_rev/5, replace_document_rev/6,
         delete_document/3, delete_document/4,
         delete_document_rev/4, delete_document_rev/5,
         delete_documents/3, delete_documents/4,
         create_views/4, create_views/5,
         create_view/5, create_view/6,
         create_view_type/6, create_view_type/7,
         invoke_view/4, invoke_view/5,
         invoke_view_keys/5, invoke_view_keys/6]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_HOST_NAME,       "127.0.0.1").
-define(DEFAULT_PORT,                   5984).
-define(DEFAULT_TIMEOUT,               20000). % ms
-define(DEFAULT_DATABASE,          "default").

-record(state,
    {
        hostname = undefined,
        port = undefined,
        connection = undefined,
        database = undefined,
        timeout = undefined
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type external_response(Result) ::
    {{ok, Result}, NewAgent :: agent()} |
    {{error, cloudi:error_reason_sync()}, NewAgent :: agent()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create the database.===
%% @end
%%-------------------------------------------------------------------------

-spec create_database(Agent :: agent(),
                      Name :: service_name()) ->
    external_response(any()).

create_database(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     create_database).

-spec create_database(Agent :: agent(),
                      Name :: service_name(),
                      Timeout :: timeout_milliseconds()) ->
    external_response(any()).

create_database(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     create_database, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete the database.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_database(Agent :: agent(),
                      Name :: service_name()) ->
    external_response(any()).

delete_database(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     delete_database).

-spec delete_database(Agent :: agent(),
                      Name :: service_name(),
                      Timeout :: timeout_milliseconds()) ->
    external_response(any()).

delete_database(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     delete_database, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get information about the database.===
%% @end
%%-------------------------------------------------------------------------

-spec database_info(Agent :: agent(),
                    Name :: service_name()) ->
    external_response(any()).

database_info(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     database_info).

-spec database_info(Agent :: agent(),
                    Name :: service_name(),
                    Timeout :: timeout_milliseconds()) ->
    external_response(any()).

database_info(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     database_info, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get information about the server.===
%% @end
%%-------------------------------------------------------------------------

-spec server_info(Agent :: agent(),
                  Name :: service_name()) ->
    external_response(any()).

server_info(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     server_info).

-spec server_info(Agent :: agent(),
                  Name :: service_name(),
                  Timeout :: timeout_milliseconds()) ->
    external_response(any()).

server_info(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     server_info, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Retrieve a list of all the databases.===
%% @end
%%-------------------------------------------------------------------------

-spec retrieve_all_dbs(Agent :: agent(),
                       Name :: service_name()) ->
    external_response(any()).

retrieve_all_dbs(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     retrieve_all_dbs).

-spec retrieve_all_dbs(Agent :: agent(),
                       Name :: service_name(),
                       Timeout :: timeout_milliseconds()) ->
    external_response(any()).

retrieve_all_dbs(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     retrieve_all_dbs, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a document attachment from a file.===
%% The file is referenced by its filename.
%% @end
%%-------------------------------------------------------------------------

-spec create_attachment(Agent :: agent(),
                        Name :: service_name(),
                        DocumentID :: string(),
                        File :: string(),
                        ContentType :: string()) ->
    external_response({'json', {'struct', list()}}).

create_attachment(Agent, Name, DocumentID, File, ContentType)
    when is_list(DocumentID), is_list(File), is_list(ContentType) ->
    cloudi:send_sync(Agent, Name,
                     {create_attachment, DocumentID,
                      File, ContentType}).

-spec create_attachment(Agent :: agent(),
                        Name :: service_name(),
                        DocumentID :: string(),
                        File :: string(),
                        ContentType :: string(),
                        Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

create_attachment(Agent, Name, DocumentID, File, ContentType, Timeout)
    when is_list(DocumentID), is_list(File), is_list(ContentType) ->
    cloudi:send_sync(Agent, Name,
                     {create_attachment, DocumentID,
                      File, ContentType}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a document and allow the server to create the document id.===
%% @end
%%-------------------------------------------------------------------------

-spec create_document(Agent :: agent(),
                      Name :: service_name(),
                      Doc :: list()) ->
    external_response({'json', {'struct', list()}}).

create_document(Agent, Name, Doc)
    when is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {create_document, Doc}).

-spec create_document(Agent :: agent(),
                      Name :: service_name(),
                      Doc :: list(),
                      Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

create_document(Agent, Name, Doc, Timeout)
    when is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {create_document, Doc}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a document with a specific document id.===
%% @end
%%-------------------------------------------------------------------------

-spec create_document_id(Agent :: agent(),
                         Name :: service_name(),
                         DocumentID :: string(),
                         Doc :: list()) ->
    external_response({'json', {'struct', list()}}).

create_document_id(Agent, Name, DocumentID, Doc)
    when is_list(DocumentID), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {create_document, DocumentID, Doc}).

-spec create_document_id(Agent :: agent(),
                         Name :: service_name(),
                         DocumentID :: string(),
                         Doc :: list(),
                         Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

create_document_id(Agent, Name, DocumentID, Doc, Timeout)
    when is_list(DocumentID), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {create_document, DocumentID, Doc}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create many documents.===
%% @end
%%-------------------------------------------------------------------------

-spec create_documents(Agent :: agent(),
                       Name :: service_name(),
                       Documents :: list()) ->
    external_response({'json', {'struct', list()}}).

create_documents(Agent, Name, Documents)
    when is_list(Documents) ->
    cloudi:send_sync(Agent, Name,
                     {create_documents, Documents}).

-spec create_documents(Agent :: agent(),
                       Name :: service_name(),
                       Documents :: list(),
                       Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

create_documents(Agent, Name, Documents, Timeout)
    when is_list(Documents) ->
    cloudi:send_sync(Agent, Name,
                     {create_documents, Documents}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the current revision of a document.===
%% @end
%%-------------------------------------------------------------------------

-spec document_revision(Agent :: agent(),
                        Name :: service_name(),
                        DocumentID :: string()) ->
    external_response({any(), any()}).

document_revision(Agent, Name, DocumentID)
    when is_list(DocumentID) ->
    cloudi:send_sync(Agent, Name,
                     {document_revision, DocumentID}).

-spec document_revision(Agent :: agent(),
                        Name :: service_name(),
                        DocumentID :: string(),
                        Timeout :: timeout_milliseconds()) ->
    external_response({any(), any()}).

document_revision(Agent, Name, DocumentID, Timeout)
    when is_list(DocumentID) ->
    cloudi:send_sync(Agent, Name,
                     {document_revision, DocumentID}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec retrieve_document(Agent :: agent(),
                        Name :: service_name(),
                        DocumentID :: string()) ->
    external_response({'json', {'struct', list()}}).

retrieve_document(Agent, Name, DocumentID)
    when is_list(DocumentID) ->
    cloudi:send_sync(Agent, Name,
                     {retrieve_document, DocumentID}).

-spec retrieve_document(Agent :: agent(),
                        Name :: service_name(),
                        DocumentID :: string(),
                        Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

retrieve_document(Agent, Name, DocumentID, Timeout)
    when is_list(DocumentID) ->
    cloudi:send_sync(Agent, Name,
                     {retrieve_document, DocumentID}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec update_document(Agent :: agent(),
                      Name :: service_name(),
                      DocumentID :: string(),
                      Doc :: list()) ->
    external_response({'json', {'struct', list()}}).

update_document(Agent, Name, DocumentID, Doc)
    when is_list(DocumentID), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {update_document, DocumentID, Doc}).

-spec update_document(Agent :: agent(),
                      Name :: service_name(),
                      DocumentID :: string(),
                      Doc :: list(),
                      Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

update_document(Agent, Name, DocumentID, Doc, Timeout)
    when is_list(DocumentID), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {update_document, DocumentID, Doc}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update a specific revision of a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec update_document_rev(Agent :: agent(),
                          Name :: service_name(),
                          DocumentID :: string(),
                          Rev :: string(),
                          Doc :: list()) ->
    external_response({'json', {'struct', list()}}).

update_document_rev(Agent, Name, DocumentID, Rev, Doc)
    when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {update_document, DocumentID, Rev, Doc}).

-spec update_document_rev(Agent :: agent(),
                          Name :: service_name(),
                          DocumentID :: string(),
                          Rev :: string(),
                          Doc :: list(),
                          Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

update_document_rev(Agent, Name, DocumentID, Rev, Doc, Timeout)
    when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {update_document, DocumentID, Rev, Doc}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Replace a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec replace_document(Agent :: agent(),
                       Name :: service_name(),
                       DocumentID :: string(),
                       Doc :: list()) ->
    external_response({'json', {'struct', list()}}).

replace_document(Agent, Name, DocumentID, Doc)
    when is_list(DocumentID), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {replace_document, DocumentID, Doc}).

-spec replace_document(Agent :: agent(),
                       Name :: service_name(),
                       DocumentID :: string(),
                       Doc :: list(),
                       Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

replace_document(Agent, Name, DocumentID, Doc, Timeout)
    when is_list(DocumentID), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {replace_document, DocumentID, Doc}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Replace a specific revision of a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec replace_document_rev(Agent :: agent(),
                           Name :: service_name(),
                           DocumentID :: string(),
                           Rev :: string(),
                           Doc :: list()) ->
    external_response({'json', {'struct', list()}}).

replace_document_rev(Agent, Name, DocumentID, Rev, Doc)
    when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
    cloudi:send_sync(Agent, Name,
                     {replace_document, DocumentID, Rev, Doc}).

-spec replace_document_rev(Agent :: agent(),
                           Name :: service_name(),
                           DocumentID :: string(),
                           Rev :: string(),
                           Doc :: list(),
                           Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

replace_document_rev(Agent, Name, DocumentID, Rev, Doc, Timeout)
    when is_list(Name),
         is_list(DocumentID), is_list(Rev), is_list(Doc), is_integer(Timeout) ->
    cloudi:send_sync(Agent, Name,
                     {replace_document, DocumentID, Rev, Doc}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_document(Agent :: agent(),
                      Name :: service_name(),
                      DocumentID :: string()) ->
    external_response({'json', {'struct', list()}}).

delete_document(Agent, Name, DocumentID)
    when is_list(DocumentID) ->
    cloudi:send_sync(Agent, Name,
                     {delete_document, DocumentID}).

-spec delete_document(Agent :: agent(),
                      Name :: service_name(),
                      DocumentID :: string(),
                      Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

delete_document(Agent, Name, DocumentID, Timeout)
    when is_list(DocumentID) ->
    cloudi:send_sync(Agent, Name,
                     {delete_document, DocumentID}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a specific revision of a document based on its id.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_document_rev(Agent :: agent(),
                          Name :: service_name(),
                          DocumentID :: string(),
                          Rev :: string()) ->
    external_response({'json', {'struct', list()}}).

delete_document_rev(Agent, Name, DocumentID, Rev)
    when is_list(DocumentID), is_list(Rev) ->
    cloudi:send_sync(Agent, Name,
                     {delete_document, DocumentID, Rev}).

-spec delete_document_rev(Agent :: agent(),
                          Name :: service_name(),
                          DocumentID :: string(),
                          Rev :: string(),
                          Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

delete_document_rev(Agent, Name, DocumentID, Rev, Timeout)
    when is_list(DocumentID), is_list(Rev) ->
    cloudi:send_sync(Agent, Name,
                     {delete_document, DocumentID, Rev}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete many documents.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_documents(Agent :: agent(),
                       Name :: service_name(),
                       Documents :: list()) ->
    external_response({'json', {'struct', list()}}).

delete_documents(Agent, Name, Documents)
    when is_list(Documents) ->
    cloudi:send_sync(Agent, Name,
                     {delete_documents, Documents}).

-spec delete_documents(Agent :: agent(),
                       Name :: service_name(),
                       Documents :: list(),
                       Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

delete_documents(Agent, Name, Documents, Timeout)
    when is_list(Documents) ->
    cloudi:send_sync(Agent, Name,
                     {delete_documents, Documents}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create many views.===
%% @end
%%-------------------------------------------------------------------------

-spec create_views(Agent :: agent(),
                   Name :: service_name(),
                   DocName :: string(),
                   ViewList :: list()) ->
    external_response({'json', {'struct', list()}}).

create_views(Agent, Name, DocName, ViewList)
    when is_list(DocName), is_list(ViewList) ->
    cloudi:send_sync(Agent, Name,
                     {create_view, DocName, ViewList}).

-spec create_views(Agent :: agent(),
                   Name :: service_name(),
                   DocName :: string(),
                   ViewList :: list(),
                   Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

create_views(Agent, Name, DocName, ViewList, Timeout)
    when is_list(DocName), is_list(ViewList) ->
    cloudi:send_sync(Agent, Name,
                     {create_view, DocName, ViewList}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a view.===
%% @end
%%-------------------------------------------------------------------------

-spec create_view(Agent :: agent(),
                  Name :: service_name(),
                  DocName :: string(),
                  ViewName :: string(),
                  Data :: list()) ->
    external_response({'json', {'struct', list()}}).

create_view(Agent, Name, DocName, ViewName, Data)
    when is_list(DocName), is_list(ViewName), is_list(Data) ->
    cloudi:send_sync(Agent, Name,
                     {create_view, DocName, ViewName, Data}).

-spec create_view(Agent :: agent(),
                  Name :: service_name(),
                  DocName :: string(),
                  ViewName :: string(),
                  Data :: list(),
                  Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

create_view(Agent, Name, DocName, ViewName, Data, Timeout)
    when is_list(DocName), is_list(ViewName), is_list(Data) ->
    cloudi:send_sync(Agent, Name,
                     {create_view, DocName, ViewName, Data}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a view of a unique type.===
%% @end
%%-------------------------------------------------------------------------

-spec create_view_type(Agent :: agent(),
                       Name :: service_name(),
                       DocName :: string(),
                       Type :: string(),
                       ViewName :: string(),
                       Data :: list()) ->
    external_response({'json', {'struct', list()}}).

create_view_type(Agent, Name, DocName, Type, ViewName, Data)
    when is_list(DocName), is_list(Type),
         is_list(ViewName), is_list(Data) ->
    cloudi:send_sync(Agent, Name,
                     {create_view, DocName, Type, ViewName, Data}).

-spec create_view_type(Agent :: agent(),
                       Name :: service_name(),
                       DocName :: string(),
                       Type :: string(),
                       ViewName :: string(),
                       Data :: list(),
                       Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

create_view_type(Agent, Name, DocName, Type, ViewName, Data, Timeout)
    when is_list(DocName), is_list(Type),
         is_list(ViewName), is_list(Data) ->
    cloudi:send_sync(Agent, Name,
                     {create_view, DocName, Type, ViewName, Data}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invoke a view.===
%% @end
%%-------------------------------------------------------------------------

-spec invoke_view(Agent :: agent(),
                  Name :: service_name(),
                  DocName :: string(),
                  ViewName :: string()) ->
    external_response({'json', {'struct', list()}}).

invoke_view(Agent, Name, DocName, ViewName)
    when is_list(DocName), is_list(ViewName) ->
    cloudi:send_sync(Agent, Name,
                     {invoke_view, DocName, ViewName}).

-spec invoke_view(Agent :: agent(),
                  Name :: service_name(),
                  DocName :: string(),
                  ViewName :: string(),
                  Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

invoke_view(Agent, Name, DocName, ViewName, Timeout)
    when is_list(DocName), is_list(ViewName) ->
    cloudi:send_sync(Agent, Name,
                     {invoke_view, DocName, ViewName}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invoke a view with a list of keys.===
%% @end
%%-------------------------------------------------------------------------

-spec invoke_view_keys(Agent :: agent(),
                       Name :: service_name(),
                       DocName :: string(),
                       ViewName :: string(),
                       Keys :: list()) ->
    external_response({'json', {'struct', list()}}).

invoke_view_keys(Agent, Name, DocName, ViewName, Keys)
    when is_list(DocName), is_list(ViewName), is_list(Keys) ->
    cloudi:send_sync(Agent, Name,
                     {invoke_view, DocName, ViewName, Keys}).

-spec invoke_view_keys(Agent :: agent(),
                       Name :: string(),
                       DocName :: string(),
                       ViewName :: string(),
                       Keys :: list(),
                       Timeout :: timeout_milliseconds()) ->
    external_response({'json', {'struct', list()}}).

invoke_view_keys(Agent, Name, DocName, ViewName, Keys, Timeout)
    when is_list(DocName), is_list(ViewName), is_list(Keys) ->
    cloudi:send_sync(Agent, Name,
                     {invoke_view, DocName, ViewName, Keys}, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {hostname, ?DEFAULT_HOST_NAME},
        {port, ?DEFAULT_PORT},
        {timeout, ?DEFAULT_TIMEOUT},
        {database, ?DEFAULT_DATABASE}],
    [HostName, Port, Timeout, DatabaseName] =
        cloudi_proplists:take_values(Defaults, Args),
    case cloudi_x_ecouchdb:connect(HostName, Port, Timeout) of
        {ok, Connection} ->
            cloudi_service:subscribe(Dispatcher, DatabaseName),
            {ok, #state{hostname = HostName,
                        port = Port,
                        connection = Connection,
                        database = DatabaseName}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{hostname = HostName,
                                     port = Port,
                                     connection = Connection,
                                     database = Database} = State,
                              _Dispatcher) ->
    case Request of
        %Command when is_binary(Command) ->
        %    reply_external(do_query(Command, Connection, HostName,
        %                            Port, Database, Timeout),
        %                   Request, State);
        'create_database' ->
            reply_internal(cloudi_x_ecouchdb:
                           create_database_c(Connection, HostName,
                                             Port, Database, Timeout),
                           Request, State);
        'delete_database' ->
            reply_internal(cloudi_x_ecouchdb:
                           delete_database_c(Connection, HostName,
                                             Port, Database, Timeout),
                           Request, State);
        'database_info' ->
            reply_internal(cloudi_x_ecouchdb:
                           database_info_c(Connection, HostName,
                                           Port, Database, Timeout),
                           Request, State);
        'server_info' ->
            reply_internal(cloudi_x_ecouchdb:
                           server_info_c(Connection, HostName, Port, Timeout),
                           Request, State);
        'retrieve_all_dbs' ->
            reply_internal(cloudi_x_ecouchdb:
                           retrieve_all_dbs_c(Connection, HostName,
                                              Port, Timeout),
                           Request, State);
        {'create_attachment', DocumentID, File, ContentType} ->
            reply_internal(cloudi_x_ecouchdb:
                           create_attachment_c(Connection, HostName,
                                               Port, Database,
                                               DocumentID, File,
                                               ContentType, Timeout),
                           Request, State);
        {'create_document', Doc} ->
            reply_internal(cloudi_x_ecouchdb:
                           create_document_c(Connection, HostName,
                                             Port, Database, Doc, Timeout),
                           Request, State);
        {'create_document', DocumentID, Doc} ->
            reply_internal(cloudi_x_ecouchdb:
                           create_document_c(Connection, HostName,
                                             Port, Database,
                                             DocumentID, Doc, Timeout),
                           Request, State);
        {'create_documents', Documents} ->
            reply_internal(cloudi_x_ecouchdb:
                           create_documents_c(Connection, HostName,
                                              Port, Database, Documents,
                                              Timeout),
                           Request, State);
        {'document_revision', DocumentID} ->
            reply_internal(cloudi_x_ecouchdb:
                           document_revision_c(Connection, HostName,
                                               Port, Database, DocumentID,
                                               Timeout),
                           Request, State);
        {'retrieve_document', DocumentID} ->
            reply_internal(cloudi_x_ecouchdb:
                           retrieve_document_c(Connection, HostName,
                                               Port, Database,
                                               DocumentID, Timeout),
                           Request, State);
        {'update_document', DocumentID, Doc} ->
            reply_internal(cloudi_x_ecouchdb:
                           update_document_c(Connection, HostName,
                                             Port, Database,
                                             DocumentID, Doc, Timeout),
                           Request, State);
        {'update_document', DocumentID, Rev, Doc} ->
            reply_internal(cloudi_x_ecouchdb:
                           update_document_c(Connection, HostName,
                                             Port, Database,
                                             DocumentID, Rev, Doc, Timeout),
                           Request, State);
        {'replace_document', DocumentID, Doc} ->
            reply_internal(cloudi_x_ecouchdb:
                           replace_document_c(Connection, HostName,
                                              Port, Database,
                                              DocumentID, Doc, Timeout),
                           Request, State);
        {'replace_document', DocumentID, Rev, Doc} ->
            reply_internal(cloudi_x_ecouchdb:
                           replace_document_c(Connection, HostName,
                                              Port, Database,
                                              DocumentID, Rev, Doc, Timeout),
                           Request, State);
        {'delete_document', DocumentID} ->
            reply_internal(cloudi_x_ecouchdb:
                           delete_document_c(Connection, HostName,
                                             Port, Database,
                                             DocumentID, Timeout),
                           Request, State);
        {'delete_document', DocumentID, Rev} ->
            reply_internal(cloudi_x_ecouchdb:
                           delete_document_c(Connection, HostName,
                                             Port, Database,
                                             DocumentID, Rev, Timeout),
                           Request, State);
        {'delete_documents', Documents} ->
            reply_internal(cloudi_x_ecouchdb:
                           delete_documents_c(Connection, HostName,
                                              Port, Database,
                                              Documents, Timeout),
                           Request, State);
        {'create_view', DocName, ViewList} ->
            reply_internal(cloudi_x_ecouchdb:
                           create_view_c(Connection, HostName, Port,
                                         Database, DocName, ViewList, Timeout),
                           Request, State);
        {'create_view', DocName, ViewName, Data} ->
            reply_internal(cloudi_x_ecouchdb:
                           create_view_c(Connection, HostName, Port,
                                         Database, DocName, ViewName,
                                         Data, Timeout),
                           Request, State);
        {'create_view', DocName, Type, ViewName, Data} ->
            reply_internal(cloudi_x_ecouchdb:
                           create_view_c(Connection, HostName, Port,
                                         Database, DocName, Type,
                                         ViewName, Data, Timeout),
                           Request, State);
        {'invoke_view', DocName, ViewName} ->
            reply_internal(cloudi_x_ecouchdb:
                           invoke_view_c(Connection, HostName, Port,
                                         Database, DocName, ViewName, Timeout),
                           Request, State);
        {'invoke_view', DocName, ViewName, Keys} ->
            reply_internal(cloudi_x_ecouchdb:
                           invoke_view_c(Connection, HostName, Port,
                                         Database, DocName, ViewName,
                                         Keys, Timeout),
                           Request, State)
    end.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout,
                         undefined) ->
    ok;
cloudi_service_terminate(_Reason, _Timeout,
                         #state{connection = Connection}) ->
    cloudi_x_ecouchdb:close(Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

reply_internal({Output, Output, NewConnection}, _, State) ->
    {reply, Output, State#state{connection = NewConnection}};
reply_internal({OutputState, OutputData, NewConnection}, _, State) ->
    {reply, {OutputState, OutputData}, State#state{connection = NewConnection}};
reply_internal({Output, Output}, _, State) ->
    {reply, Output, State};
reply_internal({OutputState, OutputData}, _, State) ->
    {reply, {OutputState, OutputData}, State}.

%reply_external({Output, Output, NewConnection}, Input, State) ->
%    reply_binary(cloudi_string:term_to_binary(Output),
%                 Input, State#state{connection = NewConnection});
%reply_external({OutputState, OutputData, NewConnection}, Input, State) ->
%    reply_binary(cloudi_string:term_to_binary({OutputState, OutputData}),
%                 Input, State#state{connection = NewConnection});
%reply_external({Output, Output}, Input, State) ->
%    reply_binary(cloudi_string:term_to_binary(Output), Input, State);
%reply_external({OutputState, OutputData}, Input, State) ->
%    reply_binary(cloudi_string:term_to_binary({OutputState, OutputData}),
%                 Input, State);
%reply_external(Output, Input, State) when is_binary(Output) ->
%    reply_binary(Output, Input, State).
%
%reply_binary(Output, Input, State) ->
%    {reply, cloudi_response:new(Input, Output), State}.
%
%% do a single query and return a boolean to determine if the query succeeded
%do_query(Query, Connection, HostName, Port, Database, Timeout) ->
%    try (case cloudi_string:binary_to_term(Query) of
%        'create_database' ->
%            cloudi_x_ecouchdb:
%            create_database_c(Connection, HostName, Port,
%                              Database, Timeout);
%        'delete_database' ->
%            cloudi_x_ecouchdb:
%            delete_database_c(Connection, HostName, Port,
%                              Database, Timeout);
%        % database_info
%        % server_info
%        % retrieve_all_dbs
%        {'create_attachment', DocumentID, File, ContentType}
%            when is_list(DocumentID), is_list(File),
%                 is_list(ContentType) ->
%            cloudi_x_ecouchdb:
%            create_attachment_c(Connection, HostName, Port,
%                                Database, DocumentID, File,
%                                ContentType, Timeout);
%        {'create_document', DocumentID, Doc}
%            when is_list(DocumentID), is_list(Doc) ->
%            cloudi_x_ecouchdb:
%            create_document_c(Connection, HostName, Port,
%                              Database, DocumentID, Doc, Timeout);
%        % create_documents
%        % document_revision
%        % retrieve_document
%        {'update_document', DocumentID, Doc}
%            when is_list(DocumentID), is_list(Doc) ->
%            cloudi_x_ecouchdb:
%            update_document_c(Connection, HostName, Port,
%                              Database, DocumentID, Doc, Timeout);
%        {'update_document', DocumentID, Rev, Doc}
%            when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
%            cloudi_x_ecouchdb:
%            update_document_c(Connection, HostName, Port,
%                              Database, DocumentID, Rev,
%                              Doc, Timeout);
%        {'replace_document', DocumentID, Doc}
%            when is_list(DocumentID), is_list(Doc) ->
%            cloudi_x_ecouchdb:
%            replace_document_c(Connection, HostName, Port,
%                               Database, DocumentID, Doc, Timeout);
%        {'replace_document', DocumentID, Rev, Doc}
%            when is_list(DocumentID), is_list(Rev), is_list(Doc) ->
%            cloudi_x_ecouchdb:
%            replace_document_c(Connection, HostName, Port,
%                               Database, DocumentID, Rev, Doc, Timeout);
%        {'delete_document', DocumentID}
%            when is_list(DocumentID) ->
%            cloudi_x_ecouchdb:
%            delete_document_c(Connection, HostName, Port,
%                              Database, DocumentID, Timeout);
%        {'delete_document', DocumentID, Rev}
%            when is_list(DocumentID), is_list(Rev) ->
%            cloudi_x_ecouchdb:
%            delete_document_c(Connection, HostName, Port,
%                              Database, DocumentID, Rev, Timeout);
%        % delete_documents
%        {'create_view', DocName, ViewList}
%            when is_list(DocName), is_list(ViewList) ->
%            cloudi_x_ecouchdb:
%            create_view_c(Connection, HostName, Port, Database,
%                          DocName, ViewList, Timeout);
%        {'create_view', DocName, ViewName, Data}
%            when is_list(DocName), is_list(ViewName), is_list(Data) ->
%            cloudi_x_ecouchdb:
%            create_view_c(Connection, HostName, Port, Database,
%                          DocName, ViewName, Data, Timeout);
%        {'create_view', DocName, Type, ViewName, Data}
%            when is_list(DocName), is_list(Type),
%                 is_list(ViewName), is_list(Data) ->
%            cloudi_x_ecouchdb:
%            create_view_c(Connection, HostName, Port, Database,
%                          DocName, Type, ViewName, Data, Timeout);
%        {'invoke_view', DocName, ViewName}
%            when is_list(DocName), is_list(ViewName) ->
%            cloudi_x_ecouchdb:
%            invoke_view_c(Connection, HostName, Port, Database,
%                          DocName, ViewName, Timeout);
%        {'invoke_view', DocName,   ViewName, Keys}
%            when is_list(DocName), is_list(ViewName), is_list(Keys) ->
%            cloudi_x_ecouchdb:
%            invoke_view_c(Connection, HostName, Port, Database,
%                          DocName, ViewName, Keys, Timeout);
%        _ ->
%            {error, invalid_call}
%        end) of
%        {error, invalid_call} ->
%            ?LOG_DEBUG("Invalid couchdb command tuple ~p",
%                       [erlang:binary_to_list(Query)]),
%            <<>>;
%        Result ->
%            Result
%    catch
%        _:Reason ->
%            ?LOG_DEBUG("exception when processing "
%                       "couchdb command tuple ~p: ~p",
%                       [erlang:binary_to_list(Query), Reason]),
%            <<>>
%    end.

