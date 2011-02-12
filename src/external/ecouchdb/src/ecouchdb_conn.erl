%% Copyright (c) 2008
%% Nick Gerakines <nick@gerakines.net>
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
%% Change Log:
%% * v0.2.4 mtruog
%%   - removed testing defaults
%%   - added timeouts and error handling
%%   - allow persistent connection api usage
%%   - now using http parsing through gen_tcp
%%   - http status code determines ok/error tuple result with parsed JSON
%%   - comment dead code, fixed tests and cleaned up
%% * v2009-01-23 ngerakines
%%   - Importing functionality from etnt_. GitHub merge didn't work.
%%   - Started adding etap tests.
%% * v0.2.3 2008-10-26: ngerakines
%%   - Added ability to delete databases.
%%   - Added function to fetch Document by ID and return it's Document
%%     ID and revision.
%%   - Added experimental function to create design documents based on
%%     a .js file's contents.
%%   - Fixed bug in parse_view/1 when error is returned.
%% * v0.2.2 2008-10-25: ngerakines
%%   - Applied a patch from Pablo Sortino <psortino@novamens.com> that
%%     provides bulk document creation.
%%   - Created accessor function to create a new document with an ID.
%% * v0.2.1 2008-10-05: ngerakines
%%   - Complete rewrite with reduced module size.
%%   - Moved away from the rfc4627 module and to mochijson2.
%%   - Moved away from urlencode dependancies from yaws to mochiweb.
%%   - Overall the module 'does less'.
%%   - Moved away from the gen_server model.
%% 
%% @author Nick Gerakines <nick@gerakines.net>
%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%% @copyright 2008 Nick Gerakines, et al.
%% @version 0.2.4
%% @doc A simple CouchDB client.
%% 
%% This module was created for the purpose of creating a very small and light
%% CouchDB interface. It supports a limited number of API methods and features.
%% 
%% This module was built for use in the I Play WoW Facebook Application and
%% website. Support is limited and features will be added as needed by the
%% developer/website.
%% 
%% This code is available as Open Source Software under the MIT license.
%% 
%% Updates at http://github.com/ngerakines/erlang_couchdb/
-module(ecouchdb_conn).

-author("Michael Truog <mjtruog [at] gmail (dot) com>").

%% external interface
-export([connect/3,
         disconnect/1,
         create_database/5,
         delete_database/5,
         database_info/5,
         server_info/4,
         retrieve_all_dbs/4,
         create_attachment/8,
         create_document/7,
         create_documents/6,
         document_revision/6,
         retrieve_document/6, retrieve_document/7,
         update_document/7,
         delete_document/7,
         delete_documents/6,
         create_view/8, create_view/9,
         invoke_view/8,
         invoke_multikey_view/9,
         %load_view/7,
         %parse_view/2,
         get_revs/6,
         fetch_ids/5]).
%% external helper functions
-export([reduce_timeout/2]).

-define(REQUEST_RETRY_MAX, 1).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%% @doc connect to create a persistent connection
connect(Server, ServerPort, Timeout)
    when is_list(Server), is_integer(ServerPort), is_integer(Timeout) ->
    case gen_tcp:connect(Server, ServerPort,
                         [binary, {active, false}, {packet, http},
                          {keepalive, true},
                          {send_timeout, Timeout}], Timeout) of
        {ok, _} = Success ->
            Success;
        {error, _} = Error ->
            Error
    end.

%% @doc disconnect the persistent connection
disconnect(Connection) ->
    gen_tcp:close(Connection).

%% @doc Create a new database.
create_database(Connection, Server, ServerPort, Database, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_integer(Timeout) ->
    case http_request(Connection, Server, ServerPort,
                      "PUT", build_uri(Database), <<>>, Timeout) of
        {ok, {json, {struct, [{<<"ok">>, true}]}}, NewConnection} ->
            {ok, ok, NewConnection};
        {ok, {json, {struct, [{<<"ok">>, true}]}}} ->
            {ok, ok};
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

%% @doc Delete a database.
delete_database(Connection, Server, ServerPort, Database, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_integer(Timeout) ->
    case http_request(Connection, Server, ServerPort,
                      "DELETE", build_uri(Database), <<>>, Timeout) of
        {ok, {json, {struct, [{<<"ok">>, true}]}}, NewConnection} ->
            {ok, ok, NewConnection};
        {ok, {json, {struct, [{<<"ok">>, true}]}}} ->
            {ok, ok};
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

%% @doc Get info about a database.
database_info(Connection, Server, ServerPort, Database, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_integer(Timeout) ->
    case http_request(Connection, Server, ServerPort,
                      "GET", build_uri(Database), <<>>, Timeout) of
        {ok, {json, {struct, Info}}, NewConnection} ->
            {ok, Info, NewConnection};
        {ok, {json, {struct, Info}}} ->
            {ok, Info};
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

%% @doc Get info about a server.
server_info(Connection, Server, ServerPort, Timeout)
    when is_list(Server), is_integer(ServerPort), is_integer(Timeout) ->
    case http_request(Connection, Server, ServerPort,
                      "GET", build_uri(), <<>>, Timeout) of
        {ok, {json, {struct, Welcome}}, NewConnection} ->
            {ok, Welcome, NewConnection};
        {ok, {json, {struct, Welcome}}} ->
            {ok, Welcome};
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

%% @doc Retieve all the databases
retrieve_all_dbs(Connection, Server, ServerPort, Timeout)
    when is_list(Server), is_integer(ServerPort), is_integer(Timeout) ->
    case http_request(Connection, Server, ServerPort,
                      "GET", "/_all_dbs", <<>>, Timeout) of
        {ok, {json, Database}, NewConnection} ->
            {ok, Database, NewConnection};
        {ok, {json, Database}} ->
            {ok, Database};
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

%% @doc Create a new attachment document.
create_attachment(Connection, Server, ServerPort, Database,
                  DocumentID, File, ContentType, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(File), is_integer(Timeout) ->
    {ok, Body} = file:read_file(File),
    http_request(Connection, Server, ServerPort,
                 "PUT", build_uri(Database, DocumentID ++ "/attachment"),
                 ContentType, Body, Timeout).

%%
%% @doc Create a new document. This function will create a document with a
%% list of attributes and leaves it up to the server to create an id for it.
%% The attributes should be a list of binary key/value tuples.
create_document(Connection, Server, ServerPort, Database, undefined,
                Attributes, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(Attributes), is_integer(Timeout) ->
    create_document(Connection, Server, ServerPort, Database, undefined,
                    {struct, Attributes}, Timeout);
create_document(Connection, Server, ServerPort, Database, undefined,
                {struct, _} = Obj, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_integer(Timeout) ->
    http_request(Connection, Server, ServerPort,
                 "POST", build_uri(Database),
                 list_to_binary(mochijson2:encode(Obj)), Timeout);

%%
%% @doc Create a new document with a specific document ID. This is just an
%% accessor function to update_document/4 when the intent is to create a 
%% new document.
create_document(Connection, Server, ServerPort, Database, DocumentID,
                [], Timeout) ->
    update_document(Connection, Server, ServerPort, Database, DocumentID,
                    [{<<"document_created_empty">>, <<"true">>}], Timeout);
create_document(Connection, Server, ServerPort, Database, DocumentID,
                Attributes, Timeout) ->
    update_document(Connection, Server, ServerPort, Database,
                    DocumentID, Attributes, Timeout).

%% @doc Create many documents in bulk.
%% This function created and submitted by Pablo Sortino, applied on 2008-10-25.
create_documents(Connection, Server, ServerPort, Database, Documents, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(Documents), is_integer(Timeout) ->
    BulkCreate = {struct, [
        {<<"docs">>, [
            {struct, Doc} || Doc <- Documents
        ]}
    ]},
    http_request(Connection, Server, ServerPort,
                 "POST", build_uri(Database, "_bulk_docs"),
                 list_to_binary(mochijson2:encode(BulkCreate)), Timeout).

%% @doc Return a tuple containing a document id and the document's latest
%% revision.
document_revision(Connection, Server, ServerPort, Database, DocID, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(DocID), is_integer(Timeout) ->
    case http_request(Connection, Server, ServerPort,
                      "GET", build_uri(Database, DocID, []), <<>>, Timeout) of
        {ok, {json, {struct, Props}}, NewConnection} ->
            {ok,
             {proplists:get_value(<<"_id">>, Props, undefined),
              proplists:get_value(<<"_rev">>, Props, undefined)},
             NewConnection};
        {ok, {json, {struct, Props}}} ->
            {ok,
             {proplists:get_value(<<"_id">>, Props, undefined),
              proplists:get_value(<<"_rev">>, Props, undefined)}};
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

%% @doc Fetches a document by it's id.
retrieve_document(Connection, Server, ServerPort,
                  Database, DocID, Timeout) ->
    retrieve_document(Connection, Server, ServerPort,
                      Database, DocID, [], Timeout).

%% @doc Fetches a document by it's id and also some attributes. Attributes
%% should be a list of non binary key/value pair tuples.
retrieve_document(Connection, Server, ServerPort,
                  Database, DocID, Attributes, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(DocID),
         is_list(Attributes), is_integer(Timeout) ->
    http_request(Connection, Server, ServerPort,
                 "GET", build_uri(Database, DocID, Attributes), <<>>, Timeout).

%% @doc Sets the attributes for a document with an idea. This function is a
%% bit misleading because it can be used to update an existing document
%% or create a new one with a specified id. If this function is used to
%% update a document the attributes list must contain a '_rev' key/value
%% pair tuple.
update_document(Connection, Server, ServerPort, Database,
                DocID, {struct,_} = Obj, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(DocID), is_integer(Timeout) ->
    http_request(Connection, Server, ServerPort,
                 "PUT", build_uri(Database, DocID),
                 list_to_binary(mochijson2:encode(Obj)), Timeout);
update_document(Connection, Server, ServerPort, Database,
                DocID, Attributes, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(DocID),
         is_list(Attributes), is_integer(Timeout) ->
    update_document(Connection, Server, ServerPort, Database,
                    DocID, {struct, Attributes}, Timeout).

%% @doc Deletes a given document by id and revision.
delete_document(Connection, Server, ServerPort,
                Database, DocID, Revision, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(DocID), is_list(Revision),
         is_integer(Timeout) ->
    http_request(Connection, Server, ServerPort,
                 "DELETE", build_uri(Database, DocID, [{"rev", Revision}]),
                 <<>>, Timeout).

%% @doc Delete a bunch of documents with a _bulk_docs request.
delete_documents(Connection, Server, ServerPort, Database, Documents, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(Documents), is_integer(Timeout) ->
    BulkDelete = {struct, [
        {<<"docs">>, [
            {struct, [{<<"_id">>, Id},
                      {<<"_rev">>, Rev},
                      {<<"_deleted">>, true}]} || {Id, Rev} <- Documents]}
    ]},
    http_request(Connection, Server, ServerPort,
                 "POST", build_uri(Database, "_bulk_docs"),
                 list_to_binary(mochijson2:encode(BulkDelete)), Timeout).

%% @doc Creates a design document. See create_view/6 for more.
create_view(Connection, Server, ServerPort, Database,
            ViewClass, Language, Views, Timeout) ->
    create_view(Connection, Server, ServerPort, Database,
                ViewClass, Language, Views, [], Timeout).

%% @doc Creates or updates a design document. The Views parameter should be
%% a list of tuples representing the view's data. When updating an existing
%% view please be sure to include the _rev field in the Attributes
%% parameter.
create_view(Connection, Server, ServerPort, Database,
            ViewClass, Language, Views, Attributes, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(ViewClass), is_list(Language),
         is_list(Views), is_list(Attributes), is_integer(Timeout) ->
    Design = [
        {<<"_id">>, list_to_binary("_design/" ++ ViewClass)},
        {<<"language">>, Language},
        {<<"views">>, {struct, [
            begin
                case View of
                    {Name, Map} -> 
                        {Name, {struct, [{<<"map">>, Map}]}};
                    {Name, Map, Reduce} ->
                        {Name, {struct, [{<<"map">>, Map},
                                         {<<"reduce">>, Reduce}]}}
                end
            end || View <- Views
        ]}}
    | Attributes],
    http_request(Connection, Server, ServerPort,
                 "PUT", build_uri(Database, "_design/" ++ ViewClass),
                 list_to_binary(mochijson2:encode({struct, Design})), Timeout).

%% @doc Executes a view with or without some attributes as modifiers.
invoke_view(Connection, Server, ServerPort, Database,
            ViewClass, ViewId, Attributes, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(ViewClass), is_list(ViewId),
         is_list(Attributes), is_integer(Timeout) ->
    http_request(Connection, Server, ServerPort,
                 "GET", view_uri(Database, ViewClass, ViewId, Attributes),
                 <<>>, Timeout).

invoke_multikey_view(Connection, Server, ServerPort, Database,
                     ViewClass, ViewId, Keys, Attributes, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Database), is_list(ViewClass), is_list(ViewId),
         is_list(Keys), is_list(Attributes), is_integer(Timeout) ->
    http_request(Connection, Server, ServerPort,
                 "POST", view_uri(Database, ViewClass, ViewId, Attributes),
                 list_to_binary(mochijson2:encode({struct, [{keys, Keys}]})),
                 Timeout).

%% doc Create a design document based on a file's contents.
%% Warning! This function is experimental.
%% end
%load_view(Connection, Server, ServerPort, Database, ViewName, File, Timeout) ->
%    {ok, FH2} = file:open(File, [read, binary]),
%    {ok, Data2} = file:read(FH2, 9999),
%    create_document(
%        Server, ServerPort, Database,
%        "_design/" ++ ViewName,
%        [{<<"language">>, <<"javascript">>}, {<<"views">>, mochijson2:decode(Data2)}],
%        Timeout
%    ).

%% doc Return a list of document ids for a given view.
%parse_view({json, {struct, [{<<"error">>, _Code}, {_, _Reason}]}}) ->
%    {0, 0, []};
%parse_view({json, Structure}) ->
%    {struct, Properties} = Structure,
%    TotalRows = proplists:get_value(<<"total_rows">>, Properties, 0),
%    Offset = proplists:get_value(<<"offset">>, Properties, 0),
%    Data = proplists:get_value(<<"rows">>, Properties, []),
%    Ids = [begin
%        {struct, Bits} = Rec,
%        Id = proplists:get_value(<<"id">>, Bits),
%        case proplists:get_value(<<"value">>, Bits, []) of
%            [] -> Id;
%            {struct, RowValues} -> {Id, RowValues};
%            _ -> Id
%        end
%    end || Rec <- Data],
%    {TotalRows, Offset, Ids};
%parse_view(_Other) ->
%    {0, 0, []}.

get_revs(Connection, Server, ServerPort, Db, ID, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Db), is_list(ID), is_integer(Timeout) ->
    case http_request(Connection, Server, ServerPort,"GET",
                      lists:concat(["/", Db, "/", ID, "?", "revs_info=true"]),
                      <<>>, Timeout) of
        {ok, {json, {struct, PropList}}, NewConnection} ->
            case proplists:get_value(<<"_revs_info">>, PropList, []) of
                [] ->
                    {ok, [], NewConnection};
                RevList ->
                    {ok, get_revision_list(RevList), NewConnection}
            end;
        {ok, {json, {struct, PropList}}} ->
            case proplists:get_value(<<"_revs_info">>, PropList, []) of
                [] ->
                    {ok, []};
                RevList ->
                    {ok, get_revision_list(RevList)}
            end;
        {error, _, _} = Error ->
            Error;
        {error, _} = Error ->
            Error
    end.

%% @doc Fetch a number of UUIDs from a CouchDB server.
fetch_ids(Connection, Server, ServerPort, Limit, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_list(Limit), is_integer(Timeout) ->
    http_request(Connection, Server, ServerPort,
                 "POST", build_uri(lists:concat(["_uuids?count=", Limit])),
                 <<>>, Timeout).

reduce_timeout({MegaSeconds1, Seconds1, MicroSeconds1}, Timeout)
    when is_integer(Timeout) ->
    {MegaSeconds2, Seconds2, MicroSeconds2} = erlang:now(),
    Seconds = (MegaSeconds2 - MegaSeconds1) * 1000000000 +
              (Seconds2 - Seconds1) * 1000,
    Elapsed = if
        Seconds == 0, MicroSeconds2 < MicroSeconds1 ->
            erlang:trunc((MicroSeconds2 + 1000000 - MicroSeconds1) * 0.001);
        true ->
            erlang:trunc(Seconds + (MicroSeconds2 - MicroSeconds1) * 0.001)
    end,
    if
        Elapsed >= Timeout ->
            % in case special significance is given to 0, use 1
            1;
        true ->
            Timeout - Elapsed
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

http_request(Connection, Server, ServerPort,
             Type, Url, Body, Timeout) ->
    http_request(Connection, Server, ServerPort,
                 Type, Url, undefined, Body, Timeout).

http_request(undefined, Server, ServerPort,
             Type, Url, ContentType, Body, Timeout)
    when is_list(Server), is_integer(ServerPort),
         is_binary(Body)->
    Start = erlang:now(),
    case connect(Server, ServerPort, Timeout) of
        {ok, Socket} ->
            Result = http_request_on_socket(Socket,
                reduce_timeout(Start, Timeout), ?REQUEST_RETRY_MAX,
                true, build_request(Type, Url, ContentType, Body, true),
                Server, ServerPort),
            gen_tcp:close(Socket),
            Result;
        {error, _} = Error ->
            Error
    end;
http_request(Socket, Server, ServerPort,
             Type, Url, ContentType, Body, Timeout)
    when is_port(Socket), is_list(Server), is_integer(ServerPort),
         is_binary(Body) ->
    http_request_on_socket(Socket, Timeout, ?REQUEST_RETRY_MAX,
        false, build_request(Type, Url, ContentType, Body, false),
        Server, ServerPort).

http_request_on_socket(Socket, Timeout, RetryCount,
                       AutoClose, Request, Server, ServerPort)
    when is_port(Socket) ->
    Start = erlang:now(),
    case gen_tcp:send(Socket, Request) of
        ok ->
            http_response(Socket, reduce_timeout(Start, Timeout), RetryCount,
                          AutoClose, Request, Server, ServerPort);
        {error, PosixState} when PosixState == closed;
                                 PosixState == enotconn ->
            gen_tcp:close(Socket),
            http_request_retry(Socket, reduce_timeout(Start, Timeout),
                               RetryCount,
                               AutoClose, Request, Server, ServerPort);
        {error, _} = Error ->
            Error
    end.

http_response(Socket, Timeout, RetryCount,
              AutoClose, Request, Server, ServerPort) ->
    Start = erlang:now(),
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, {http_response, _, HttpStatusCode, _}} ->
            http_response_receive(Socket, HttpStatusCode, false,
                                  reduce_timeout(Start, Timeout), RetryCount,
                                  AutoClose, Request, Server, ServerPort);
        {ok, _} = InvalidHttpData ->
            {error, {invalid_http, InvalidHttpData}};
        {error, PosixState} when PosixState == closed;
                                 PosixState == enotconn ->
            http_request_retry(Socket, reduce_timeout(Start, Timeout),
                               RetryCount,
                               AutoClose, Request, Server, ServerPort);
        {error, _} = Error ->
            Error
    end.

http_request_retry(_, _, 0, _, _, _, _) ->
    {error, closed};
http_request_retry(OldSocket, Timeout, RetryCount,
                   AutoClose, Request, Server, ServerPort)
    when is_boolean(AutoClose) ->
    Start = erlang:now(),
    case connect(Server, ServerPort, Timeout) of
        {ok, Socket} ->
            gen_tcp:close(OldSocket),
            case http_request_on_socket(Socket, reduce_timeout(Start, Timeout),
                                        RetryCount - 1, AutoClose, Request,
                                        Server, ServerPort) of
                {ResultState, ResultContents}
                    when AutoClose == false ->
                    {ResultState, ResultContents, Socket};
                Result ->
                    gen_tcp:close(Socket),
                    Result
            end;
        {error, _} = Error ->
            Error
    end.
    
http_response_receive(Socket, HttpStatusCode, HeaderDone, Timeout, RetryCount,
                      AutoClose, Request, Server, ServerPort)
    when is_port(Socket), is_integer(HttpStatusCode),
         is_boolean(HeaderDone), is_integer(Timeout) ->
    Start = erlang:now(),
    case gen_tcp:recv(Socket, 0, Timeout) of
        {ok, {http_header, _, _, _, _}} ->
            http_response_receive(Socket, HttpStatusCode, HeaderDone,
                                  reduce_timeout(Start, Timeout), RetryCount,
                                  AutoClose, Request, Server, ServerPort);
        {ok, http_eoh} ->
            http_response_receive(Socket, HttpStatusCode, true,
                                  reduce_timeout(Start, Timeout), RetryCount,
                                  AutoClose, Request, Server, ServerPort);
        % dialyzer thinks that
        % {ok, {http_error, _}} is returned, but in reality I get
        % {error, {http_error, _}}, not sure why
        {_, {http_error, HttpString}} when HeaderDone == true ->
            JSON = decode_json(HttpString),
            if
                JSON == error ->
                    {error, {invalid_json, HttpString}};
                HttpStatusCode >= 200, HttpStatusCode =< 299 ->
                    {ok, JSON};
                true ->
                    {error, JSON}
            end;
        {ok, _} = InvalidHttpData ->
            {error, {invalid_http, InvalidHttpData}};
        {error, PosixState} when PosixState == closed;
                                 PosixState == enotconn ->
            http_request_retry(Socket, reduce_timeout(Start, Timeout),
                               RetryCount,
                               AutoClose, Request, Server, ServerPort);
        {error, _} = Error ->
            Error
    end.

%% @private
%% Attempt to decode a JSON body into Erlang structures.
decode_json(Body) ->
    try mochijson2:decode(Body) of
        {struct, _} = Response ->
            {json, Response};
        Response when is_atom(Response); is_binary(Response);
                      is_integer(Response); is_float(Response);
                      is_list(Response) ->
            {json, Response}
    catch
        _:_ -> error
    end.

%% @private
%% Build the HTTP request for the Type of request, the URI and
%% optionally a body. If there is a body then find it's length and send that
%% as well. The content-type is hard-coded because this client will never
%% send anything other than json.
build_request(Type, URI, ContentType, Body, AutoClose)
    when is_list(Type), is_list(URI), is_binary(Body), is_boolean(AutoClose) ->
    HttpVersion = if
        AutoClose ->
            "1.0";
        true ->
            "1.1"
    end,
    ContentTypeStr = if
        ContentType == undefined ->
            "application/json";
        true ->
            ContentType
    end,
    erlang:iolist_to_binary([
       Type, " ", URI, " HTTP/", HttpVersion, "\r\n"
       "Content-Length: ", integer_to_list(erlang:iolist_size(Body)), "\r\n"
       "Content-Type: ", ContentTypeStr, "\r\n\r\n", Body]).

%% @private
%% The build_uri/0, /1, /2, /3 and view_uri/4 functions are used to create
%% the URI's mapping to databases, documents and design documents. Some URIs
%% also have query string parameters which are computed here as well.
%% NOTE: Converting the property list representing query string parameters
%% to the actual query string is done by mochiweb_util:urlencode/1. Make
%% sure that module is in the Erlang lib path.
build_uri() ->
    lists:concat(["/"]).

%% @private
build_uri(Database) ->
    lists:concat(["/", Database]).

%% @private
build_uri(Database, Request) ->
    lists:concat(["/", Database, "/", Request]).

%% @private
build_uri(Database, Request, Attributes) ->
    lists:concat(["/", Database, "/", Request,
        build_querystring(Attributes)]).

%% @private
view_uri(Database, ViewName, ViewId, Args) ->
    lists:concat(["/", Database, "/_design/",
        ViewName, "/_view/", ViewId, build_querystring(Args)]).

%% @private
build_querystring([]) -> [];
build_querystring(PropList) ->
    lists:concat(["?", mochiweb_util:urlencode(PropList)]).

get_revision_list(RevList) ->
    get_revision_list(RevList, []).

get_revision_list([{struct, PropList}|T], Acc) ->
    Rev = binary_to_list(proplists:get_value(
        <<"rev">>, PropList, <<"">>)),
    Status = binary_to_list(proplists:get_value(
        <<"status">>, PropList, <<"">>)),
    get_revision_list(T, Acc ++ [{Rev, Status}]);
get_revision_list([], Acc) ->
    Acc.

