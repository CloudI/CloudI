%% Copyright (c) 2009
%% Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>
%% Michael Truog <mjtruog [at] gmail (dot) com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%
%%
%% @author Dmitrii 'Mamut' Dimandt <dmitrii@dmitriid.com>
%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%% @copyright 2009 Dmitrii 'Mamut' Dimandt, et al.
%% @version 0.2
%% @doc A simple stupid wrapper for ecouchdb_conn. Requires mochijson2
%%
%% This module was created for the purpose of further simplifying access
%% to an already simple CouchDB interface.
%%
%% This code is available as Open Source Software under the MIT license.

-module(ecouchdb).

%% external interface
-export([connect/3,
         close/1,
         create_database_c/5, create_database/4,
         delete_database_c/5, delete_database/4,
         database_info_c/5, database_info/4,
         server_info_c/4, server_info/3,
         retrieve_all_dbs_c/4, retrieve_all_dbs/3,
         create_attachment_c/8, create_attachment/7,
         create_document_c/6, create_document_c/7,
         create_document/5, create_document/6,
         create_documents_c/6, create_documents/5,
         document_revision_c/6, document_revision/5,
         retrieve_document_c/6, retrieve_document/5,
         update_document_c/7, update_document_c/8,
         update_document/6, update_document/7,
         replace_document_c/7, replace_document_c/8,
         replace_document/6, replace_document/7,
         delete_document_c/6, delete_document_c/7,
         delete_document/5, delete_document/6,
         delete_documents_c/6, delete_documents/5,
         create_view_c/7, create_view_c/8, create_view_c/9,
         create_view/6, create_view/7, create_view/8,
         invoke_view_c/7, invoke_view_c/8,
         invoke_view/6, invoke_view/7]).

%% convenience functions for couchdb data
-export([get_value/2,
         set_value/2, set_value/3,
         fold/2,
         empty/0,
         extend/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

connect(Server, ServerPort, Timeout)
    when is_list(Server), is_integer(ServerPort), is_integer(Timeout) ->
    ecouchdb_conn:connect(Server, ServerPort, Timeout).

close(Connection)
    when is_port(Connection) ->
    ecouchdb_conn:disconnect(Connection).

create_database_c(Connection, Server, ServerPort, Database, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_database(Connection, Server, ServerPort,
                                  Database, Timeout).

create_database(Server, ServerPort, Database, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_database(undefined, Server, ServerPort,
                                  Database, Timeout).

delete_database_c(Connection, Server, ServerPort, Database, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:delete_database(Connection, Server, ServerPort,
                                  Database, Timeout).

delete_database(Server, ServerPort, Database, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:delete_database(undefined, Server, ServerPort,
                                  Database, Timeout).

database_info_c(Connection, Server, ServerPort, Database, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:database_info(Connection, Server, ServerPort,
                                Database, Timeout).

database_info(Server, ServerPort, Database, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:database_info(undefined, Server, ServerPort,
                                Database, Timeout).

server_info_c(Connection, Server, ServerPort, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:server_info(Connection, Server, ServerPort,
                              Timeout).

server_info(Server, ServerPort, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:server_info(undefined, Server, ServerPort,
                              Timeout).

retrieve_all_dbs_c(Connection, Server, ServerPort, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:retrieve_all_dbs(Connection, Server, ServerPort,
                                   Timeout).

retrieve_all_dbs(Server, ServerPort, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:retrieve_all_dbs(undefined, Server, ServerPort,
                                   Timeout).

create_attachment_c(Connection, Server, ServerPort, Database,
                    DocumentID, File, ContentType, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_attachment(Connection, Server, ServerPort,
                                    Database, DocumentID, File,
                                    ContentType, Timeout).

create_attachment(Server, ServerPort, Database,
                  DocumentID, File, ContentType, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_attachment(undefined, Server, ServerPort,
                                    Database, DocumentID, File,
                                    ContentType, Timeout).

create_document_c(Connection, Server, ServerPort, Database, Doc, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_document(Connection, Server, ServerPort,
                                  Database, undefined, Doc, Timeout).

create_document_c(Connection, Server, ServerPort,
                  Database, DocumentID, Doc, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_document(Connection, Server, ServerPort,
                                  Database, DocumentID, Doc, Timeout).

create_document(Server, ServerPort, Database, Doc, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_document(undefined, Server, ServerPort,
                                  Database, undefined, Doc, Timeout).

create_document(Server, ServerPort, Database, DocumentID, Doc, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_document(undefined, Server, ServerPort,
                                  Database, DocumentID, Doc, Timeout).

create_documents_c(Connection, Server, ServerPort,
                   Database, Documents, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_documents(Connection, Server, ServerPort,
                                   Database, Documents, Timeout).

create_documents(Server, ServerPort, Database, Documents, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_documents(undefined, Server, ServerPort,
                                   Database, Documents, Timeout).

document_revision_c(Connection, Server, ServerPort,
                    Database, DocumentID, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:document_revision(Connection, Server, ServerPort,
                                    Database, DocumentID, Timeout).

document_revision(Server, ServerPort, Database, DocumentID, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:document_revision(undefined, Server, ServerPort,
                                    Database, DocumentID, Timeout).

retrieve_document_c(Connection, Server, ServerPort,
                    Database, DocumentID, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:retrieve_document(Connection, Server, ServerPort,
                                    Database, DocumentID, Timeout).

retrieve_document(Server, ServerPort, Database, DocumentID, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:retrieve_document(undefined, Server, ServerPort,
                                    Database, DocumentID, Timeout).

%% @doc Update only several fields in a document.
%% Leave all other fields unmodified
update_document_c(Connection, Server, ServerPort,
                  Database, DocumentID, Doc, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(Connection, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}, NewConnection} ->
            case replace_document_c(NewConnection, Server, ServerPort, 
                                    Database, DocumentID, get_rev(Document),
                                    update_doc_fields(Document, Doc),
                                    ecouchdb_conn:reduce_timeout(Start,
                                                                 Timeout)) of
                {ResultState, ResultData} ->
                    {ResultState, ResultData, NewConnection};
                Result ->
                    Result
            end;
        {ok, {json, Document}} ->
            replace_document_c(Connection, Server, ServerPort, Database,
                               DocumentID, get_rev(Document),
                               update_doc_fields(Document, Doc),
                               ecouchdb_conn:reduce_timeout(Start, Timeout));
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

update_document_c(Connection, Server, ServerPort,
                  Database, DocumentID, Rev, Doc, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(Connection, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}, NewConnection} ->
            case replace_document_c(NewConnection, Server, ServerPort, 
                                    Database, DocumentID, Rev,
                                    update_doc_fields(Document, Doc),
                                    ecouchdb_conn:reduce_timeout(Start,
                                                                 Timeout)) of
                {ResultState, ResultData} ->
                    {ResultState, ResultData, NewConnection};
                Result ->
                    Result
            end;
        {ok, {json, Document}} ->
            replace_document_c(Connection, Server, ServerPort,
                               Database, DocumentID, Rev,
                               update_doc_fields(Document, Doc),
                               ecouchdb_conn:reduce_timeout(Start, Timeout));
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

update_document(Server, ServerPort, Database, DocumentID, Doc, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(undefined, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}} ->
            replace_document(Server, ServerPort, Database,
                             DocumentID, get_rev(Document),
                             update_doc_fields(Document, Doc),
                             ecouchdb_conn:reduce_timeout(Start, Timeout));
        {error, _} = Error ->
            Error
    end.

update_document(Server, ServerPort, Database, DocumentID, Rev, Doc, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(undefined, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}} ->
            replace_document(Server, ServerPort, Database,
                             DocumentID, Rev, update_doc_fields(Document, Doc),
                             ecouchdb_conn:reduce_timeout(Start, Timeout));
        {error, _} = Error ->
            Error
    end.

%% @doc Replace the doc by a new doc
%% (default ecouchdb_conn and couchdb behaviour for updates)
replace_document_c(Connection, Server, ServerPort,
                   Database, DocumentID, Doc, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(Connection, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}, NewConnection} ->
            case replace_document_c(NewConnection, Server, ServerPort, 
                                    Database, DocumentID,
                                    get_rev(Document), Doc,
                                    ecouchdb_conn:reduce_timeout(Start,
                                                                 Timeout)) of
                {ResultState, ResultData} ->
                    {ResultState, ResultData, NewConnection};
                Result ->
                    Result
            end;
        {ok, {json, Document}} ->
            replace_document_c(Connection, Server, ServerPort, Database,
                               DocumentID, get_rev(Document), Doc,
                               ecouchdb_conn:reduce_timeout(Start, Timeout));
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

replace_document_c(Connection, Server, ServerPort,
                   Database, DocumentID, Rev, Doc, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:update_document(Connection, Server, ServerPort,
                                  Database, DocumentID,
                                  [{<<"_rev">>, list_to_binary(Rev)} | Doc], 
                                  Timeout).

replace_document(Server, ServerPort, Database, DocumentID, Doc, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(undefined, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}} ->
            replace_document(Server, ServerPort, Database,
                             DocumentID, get_rev(Document), Doc,
                             ecouchdb_conn:reduce_timeout(Start, Timeout));
        {error, _} = Error ->
            Error
    end.

replace_document(Server, ServerPort, Database, DocumentID, Rev, Doc, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:update_document(undefined, Server, ServerPort,
                                  Database, DocumentID,
                                  [{<<"_rev">>, list_to_binary(Rev)} | Doc], 
                                  Timeout).

%% @doc Delete a document
delete_document_c(Connection, Server, ServerPort, Database, DocumentID, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(Connection, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}, NewConnection} ->
            case ecouchdb_conn:delete_document(NewConnection,
                                               Server, ServerPort, 
                                               Database, DocumentID,
                                               get_rev(Document),
                                               ecouchdb_conn:reduce_timeout(
                                                   Start, Timeout)) of
                {ResultState, ResultData} ->
                    {ResultState, ResultData, NewConnection};
                Result ->
                    Result
            end;
        {ok, {json, Document}} ->
            ecouchdb_conn:delete_document(Connection, Server, ServerPort, 
                                          Database, DocumentID,
                                          get_rev(Document),
                                          ecouchdb_conn:reduce_timeout(
                                              Start, Timeout));
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

delete_document_c(Connection, Server, ServerPort,
                  Database, DocumentID, Rev, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:delete_document(Connection, Server, ServerPort, Database,
                                  DocumentID, Rev, Timeout).

delete_document(Server, ServerPort, Database, DocumentID, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    Start = erlang:now(),
    case ecouchdb_conn:retrieve_document(undefined, Server, ServerPort,
                                         Database, DocumentID, Timeout) of
        {ok, {json, Document}} ->
            ecouchdb_conn:delete_document(undefined, Server, ServerPort,
                                          Database, DocumentID,
                                          get_rev(Document),
                                          ecouchdb_conn:reduce_timeout(
                                              Start, Timeout));
        {error, _} = Error ->
            Error
    end.

delete_document(Server, ServerPort, Database, DocumentID, Rev, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:delete_document(undefined, Server, ServerPort,
                                  Database, DocumentID, Rev, Timeout).

delete_documents_c(Connection, Server, ServerPort, Database, Documents, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:delete_documents(Connection, Server, ServerPort,
                                   Database, Documents, Timeout).

delete_documents(Server, ServerPort, Database, Documents, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:delete_documents(undefined, Server, ServerPort, Database,
                                   Documents, Timeout).

%% @doc Create a view
create_view_c(Connection, Server, ServerPort,
              Database, DocName, ViewList, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort),
         is_list(ViewList) ->
    ecouchdb_conn:create_view(Connection, Server, ServerPort, Database, DocName,
                              "javascript", ViewList, Timeout).
    
create_view_c(Connection, Server, ServerPort,
              Database, DocName, ViewName, Data, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_view(Connection, Server, ServerPort, Database, DocName,
                              "javascript", [{ViewName, Data}], Timeout).

create_view(Server, ServerPort, Database, DocName, ViewList, Timeout)
    when is_list(Server), is_integer(ServerPort), is_list(ViewList) ->
    ecouchdb_conn:create_view(undefined, Server, ServerPort, Database, DocName,
                              "javascript", ViewList, Timeout).
    
create_view_c(Connection, Server, ServerPort,
              Database, DocName, Type, ViewName, Data, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_view(Connection, Server, ServerPort,
                              Database, DocName, Type,
                              [{ViewName, Data}], Timeout).

create_view(Server, ServerPort, Database, DocName, ViewName, Data, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_view(undefined, Server, ServerPort, Database, DocName,
                              "javascript", [{ViewName, Data}], Timeout).

create_view(Server, ServerPort, Database, DocName, Type,
            ViewName, Data, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:create_view(undefined, Server, ServerPort,
                              Database, DocName, Type,
                              [{ViewName, Data}], Timeout).

%% @doc Invoke a view
invoke_view_c(Connection, Server, ServerPort,
              Database, DocName, ViewName, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:invoke_view(Connection, Server, ServerPort, Database,
                              DocName, ViewName, [], Timeout).

invoke_view_c(Connection, Server, ServerPort,
              Database, DocName, ViewName, Keys, Timeout)
    when is_port(Connection), is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:invoke_view(Connection, Server, ServerPort, Database,
                              DocName, ViewName, Keys, Timeout).

invoke_view(Server, ServerPort, Database, DocName, ViewName, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:invoke_view(undefined, Server, ServerPort, Database,
                              DocName, ViewName, [], Timeout).

invoke_view(Server, ServerPort, Database, DocName, ViewName, Keys, Timeout)
    when is_list(Server), is_integer(ServerPort) ->
    ecouchdb_conn:invoke_view(undefined, Server, ServerPort, Database,
                              DocName, ViewName, Keys, Timeout).

%%%------------------------------------------------------------------------
%%% Convenience functions for couchdb data
%%%------------------------------------------------------------------------

%% @doc Get the specified (set of) attribute(s)
get_value(Path, Struct) when is_list(Path) ->
    get_val(Path, Struct);
get_value(Key, {struct, L}) when is_binary(Key) ->
    proplists:get_value(Key, L).

%% @private
get_val([Key], Struct) ->
    get_value(Key, Struct);
get_val([Key | T], Struct) ->
    case get_value(Key, Struct) of
        List when is_list(List) ->
            [get_val(T, X) || X <- List];
        NewStruct when is_tuple(NewStruct) ->
            get_val(T, NewStruct)
    end.

%% @doc Set the specified (set of) attribute(s)
set_value(Path, Value, Struct) when is_list(Path) ->
    [H | T] = lists:reverse(Path),
    set_val(T, Struct, {struct, [{H, Value}]});
set_value(Key, Value, Struct) when is_binary(Key) ->
    extend(Struct, {struct, [{Key, Value}]}).

%% @private
set_val([], Struct, Result) ->
    extend(Struct, Result);
set_val([Key | T], Struct, Result) ->
    set_val(T, Struct, {struct, [{Key, Result}]}).

%% @doc To be used with the fold function
set_value(Key, Value) when is_binary(Key) ->
    fun(Struct) -> set_value(Key, vals(Value), Struct) end.

%% @private
vals(B) when is_binary(B) -> B;
vals(I) when is_integer(I) -> I;
vals(L) when is_list(L) -> list_to_binary(L);
vals(A) when is_atom(A) -> vals(atom_to_list(A)).

%% @doc Apply a list of set-functions on an initial object.
fold([H|T], Struct) -> fold(T, H(Struct));
fold([], Struct) -> Struct.

%% @doc Return an empty object
empty() -> {struct, []}.

%% @doc Extend a json obj with one or more json obj (add new leaves and modify the existing ones).
extend(S1, []) -> S1;
extend(S1, [S|T]) ->
    NewS = extend(S1, S),
    extend(NewS, T);
extend({struct, L1}, {struct, L2}) ->
    extend(L1, L2, []).

%% @private
extend(L1, [], Result) ->
    {struct, lists:append(Result,L1)};
extend(L1, [{K, {struct, ChildL2}} | T], Result) ->
    case proplists:get_value(K, L1) of
        {struct, ChildL1} ->
            NewL1 = proplists:delete(K, L1),
            extend(NewL1, T,
                [{K, extend({struct, ChildL1}, {struct, ChildL2})} | Result]);
        _ ->
            NewL1 = proplists:delete(K, L1),
            extend(NewL1, T, [{K, {struct, ChildL2}} | Result])
    end;
extend(L1, [{K, V} | T], Result) ->
    NewL1 = proplists:delete(K, L1),
    extend(NewL1, T, [{K,V} | Result]).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%get_value_internal(Doc, Key) ->
%    get_value_internal(Doc, Key, "").
get_value_internal({struct, L}, Key, DefaultValue) ->
    Values = proplists:get_value(<<"value">>, L, []),
    case Values of
        [] ->
            proplists:get_value(Key, L, DefaultValue);
        {struct, ValueList} ->
            proplists:get_value(Key, ValueList, DefaultValue)
    end;
get_value_internal({json, {struct, ValueList}}, Key, DefaultValue) ->
    proplists:get_value(Key, ValueList, DefaultValue);
get_value_internal(_, _Key, DefaultValue) ->
    DefaultValue.
    
%get_id(Doc) ->
%    get_id(Doc, "").
%get_id({struct, L}, DefaultValue) ->
%    binary_to_list(proplists:get_value(<<"id">>, L, DefaultValue));
%get_id({json, {struct, L}}, DefaultValue) ->
%    binary_to_list(proplists:get_value(<<"id">>, L, DefaultValue));
%get_id(_, DefaultValue) ->
%    DefaultValue.

get_rev(Doc) ->
    get_rev(Doc, "").
get_rev(Doc, DefaultValue) ->
    binary_to_list(get_value_internal(Doc, <<"_rev">>, DefaultValue)).
    
%get_total({json, {struct, L}}) ->
%    proplists:get_value(<<"total_rows">>, L, 0);
%get_total(_Any) ->
%    0.
%
%get_offset({json, {struct, L}}) ->
%    proplists:get_value(<<"offset">>, L, 0);
%get_offset(_Any) ->
%    0.
%
%get_rows({json, {struct, L}}) ->
%    proplists:get_value(<<"rows">>, L, []);
%get_rows(_Any) ->
%    [].

%% @private
%% Update document fields wih new values.
%% @see update_document
update_doc_fields({struct, OldDoc}, {struct, NewDoc}) ->
    update_doc_fields(OldDoc, NewDoc);
update_doc_fields({struct, OldDoc}, NewFields)
    when is_list(NewFields) ->
    update_doc_fields(OldDoc, NewFields);
update_doc_fields(OldDoc, {struct, NewDoc}) ->
    update_doc_fields(OldDoc, NewDoc);
update_doc_fields(OldDoc, []) ->
    OldDoc;
update_doc_fields(OldDoc, [{Key, Value}|T]) ->
    case proplists:get_value(Key, OldDoc) of
        undefined ->
            update_doc_fields([{Key, Value}] ++ OldDoc, T);
        _ ->
            NewDoc = proplists:delete(Key, OldDoc),
            update_doc_fields([{Key, Value}] ++ NewDoc, T)
    end.

