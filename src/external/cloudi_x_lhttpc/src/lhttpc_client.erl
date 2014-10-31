%%% -*- coding: latin-1 -*-
%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%%% @private
%%% @author Oscar Hellström <oscar@hellstrom.st>
%%% @doc This module implements the HTTP request handling. This should normally
%%% not be called directly since it should be spawned by the lhttpc module.
%%% @end
%%------------------------------------------------------------------------------
-module(lhttpc_client).

-export([request/9]).

-include("lhttpc_types.hrl").
-include("lhttpc.hrl").

-define(CONNECTION_HDR(HDRS, DEFAULT),
    string:to_lower(lhttpc_lib:header_value("connection", HDRS, DEFAULT))).

-record(client_state, {
        host :: string(),
        port = 80 :: port_num(),
        ssl = false :: boolean(),
        method :: string(),
        request :: iolist(),
        request_headers :: headers(),
        socket,
        connect_timeout = infinity :: timeout(),
        connect_options = [] :: [any()],
        attempts :: integer(),
        requester :: pid(),
        partial_upload = false :: boolean(),
        chunked_upload = false :: boolean(),
        upload_window :: non_neg_integer() | infinity,
        partial_download = false :: boolean(),
        download_window = infinity :: timeout(),
        part_size :: non_neg_integer() | infinity,
        %% in case of infinity we read whatever data we can get from
        %% the wire at that point or in case of chunked one chunk
        proxy :: undefined | #lhttpc_url{},
        proxy_ssl_options = [] :: [any()],
        proxy_setup = false :: boolean()
    }).

%%==============================================================================
%% Exported functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @spec (From, Host, Port, Ssl, Path, Method, Hdrs, RequestBody, Options) -> ok
%%    From = pid()
%%    Host = string()
%%    Port = integer()
%%    Ssl = boolean()
%%    Method = atom() | string()
%%    Hdrs = [Header]
%%    Header = {string() | atom(), string()}
%%    Body = iolist()
%%    Options = [Option]
%%    Option = {connect_timeout, Milliseconds}
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec request(pid(), string(), port_num(), boolean(), string(),
        method(), headers(), iolist(), options()) -> ok.
request(From, Host, Port, Ssl, Path, Method, Hdrs, Body, Options) ->
    Result = try
        execute(From, Host, Port, Ssl, Path, Method, Hdrs, Body, Options)
    catch
        Reason ->
            {response, self(), {error, Reason}};
        error:closed ->
            {response, self(), {error, connection_closed}};
        error:Reason ->
            Stack = erlang:get_stacktrace(),
            {response, self(), {error, {Reason, Stack}}}
    end,
    case Result of
        {response, _, {ok, {no_return, _}}} -> ok;
        _Else                               -> From ! Result
    end,
    % Don't send back {'EXIT', self(), normal} if the process
    % calling us is trapping exits
    unlink(From),
    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc This function fills in the Client record used in the requests and obtains
%% the socket from an existing pool or creates a new pool if needed. If the
%% socket used is new, it also makes the pool gen_server its controlling process.
%% @end
%%------------------------------------------------------------------------------
execute(From, Host, Port, Ssl, Path, Method, Hdrs0, Body, Options) ->
    UploadWindowSize = proplists:get_value(partial_upload, Options),
    PartialUpload = proplists:is_defined(partial_upload, Options),
    PartialDownload = proplists:is_defined(partial_download, Options),
    PartialDownloadOptions = proplists:get_value(partial_download, Options, []),
    NormalizedMethod = lhttpc_lib:normalize_method(Method),
    Proxy = case proplists:get_value(proxy, Options) of
        undefined ->
            undefined;
        ProxyUrl when is_list(ProxyUrl), not Ssl ->
            % The point of HTTP CONNECT proxying is to use TLS tunneled in
            % a plain HTTP/1.1 connection to the proxy (RFC2817).
            throw(origin_server_not_https);
        ProxyUrl when is_list(ProxyUrl) ->
            lhttpc_lib:parse_url(ProxyUrl)
    end,
    Hdrs = lhttpc_lib:canonical_headers(Hdrs0),
    {ChunkedUpload, Request} = lhttpc_lib:format_request(Path, NormalizedMethod,
        Hdrs, Host, Port, Body, PartialUpload),
    %SocketRequest = {socket, self(), Host, Port, Ssl},
    Pool = proplists:get_value(pool, Options, whereis(lhttpc_manager)),
    %% Get a socket for the pool or exit
    %Socket = lhttpc_manager:ensure_call(Pool, SocketRequest, Options),
    Socket = lhttpc_manager:ensure_call(Pool, self(), Host, Port, Ssl, Options),
    State = #client_state{
        host = Host,
        port = Port,
        ssl = Ssl,
        method = NormalizedMethod,
        request = Request,
        requester = From,
        request_headers = Hdrs,
        socket = Socket,
        connect_timeout = proplists:get_value(connect_timeout, Options,
            infinity),
        connect_options = proplists:get_value(connect_options, Options, []),
        attempts = 1 + proplists:get_value(send_retry, Options, 1),
        partial_upload = PartialUpload,
        upload_window = UploadWindowSize,
        chunked_upload = ChunkedUpload,
        partial_download = PartialDownload,
        download_window = proplists:get_value(window_size,
            PartialDownloadOptions, infinity),
        part_size = proplists:get_value(part_size,
            PartialDownloadOptions, infinity),
        proxy = Proxy,
        proxy_setup = (Socket =/= undefined),
        proxy_ssl_options = proplists:get_value(proxy_ssl_options, Options, [])
    },
    Response = case send_request(State) of
        {R, undefined} ->
            {ok, R};
        {R, NewSocket} ->
            % The socket we ended up doing the request over is returned
            % here, it might be the same as Socket, but we don't know.
            % I've noticed that we don't want to give send sockets that we
            % can't change the controlling process for to the manager. This
            % really shouldn't fail, but it could do if:
            % * The socket was closed remotely already
            % * Due to an error in this module (returning dead sockets for
            %   instance)
            ok = lhttpc_manager:client_done(Pool, Host, Port, Ssl, NewSocket),
            {ok, R}
    end,
    {response, self(), Response}.

%%------------------------------------------------------------------------------
%% @private
%% @doc This function creates a new socket connection if needed, and it also
%% handles the proxy connection.
%% @end
%%------------------------------------------------------------------------------
send_request(#client_state{attempts = 0}) ->
    % Don't try again if the number of allowed attempts is 0.
    throw(connection_closed);
%we need a socket.
send_request(#client_state{socket = undefined} = State) ->
    {Host, Port, Ssl} = request_first_destination(State),
    Timeout = State#client_state.connect_timeout,
    ConnectOptions0 = State#client_state.connect_options,
    ConnectOptions = case (not lists:member(inet, ConnectOptions0)) andalso
                         (not lists:member(inet6, ConnectOptions0)) andalso
                         is_ipv6_host(Host) of
        true ->
            [inet6 | ConnectOptions0];
        false ->
            ConnectOptions0
    end,
    SocketOptions = [binary, {packet, http}, {active, false} | ConnectOptions],
    try lhttpc_sock:connect(Host, Port, SocketOptions, Timeout, Ssl) of
        {ok, Socket} ->
            send_request(State#client_state{socket = Socket});
        {error, etimedout} ->
            % TCP stack decided to give up
            throw(connect_timeout);
        {error, timeout} ->
            throw(connect_timeout);
        {error, 'record overflow'} ->
            throw(ssl_error);
        {error, Reason} ->
            erlang:error(Reason)
    catch
        exit:{{{badmatch, {error, {asn1, _}}}, _}, _} ->
            throw(ssl_decode_error);
        Type:Error ->
                    error_logger:error_msg("Socket connection error: ~p ~p, ~p",
                                           [Type, Error, erlang:get_stacktrace()])
    end;
send_request(#client_state{proxy = #lhttpc_url{}, proxy_setup = false} = State) ->
% use a proxy.
    #lhttpc_url{
        user = User,
        password = Passwd,
        is_ssl = Ssl
    } = State#client_state.proxy,
    #client_state{
        host = DestHost,
        port = Port,
        socket = Socket
    } = State,
    Host = case inet_parse:address(DestHost) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            % IPv6 address literals are enclosed by square brackets (RFC2732)
            [$[, DestHost, $], $:, integer_to_list(Port)];
        _ ->
            [DestHost, $:, integer_to_list(Port)]
    end,
    ConnectRequest = [
        "CONNECT ", Host, " HTTP/1.1\r\n",
        "Host: ", Host, "\r\n",
        case User of
            "" ->
                "";
            _ ->
                ["Proxy-Authorization: Basic ",
                    base64:encode(User ++ ":" ++ Passwd), "\r\n"]
        end,
        "\r\n"
    ],
    case lhttpc_sock:send(Socket, ConnectRequest, Ssl) of
        ok ->
            read_proxy_connect_response(State, nil, nil);
        {error, closed} ->
            lhttpc_sock:close(Socket, Ssl),
            throw(proxy_connection_closed);
        {error, Reason} ->
            lhttpc_sock:close(Socket, Ssl),
            erlang:error(Reason)
    end;
send_request(State) ->
%already have socket
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    Request = State#client_state.request,
    case lhttpc_sock:send(Socket, Request, Ssl) of
        ok ->
            if
                % {partial_upload, WindowSize} is used.
                State#client_state.partial_upload     -> partial_upload(State);
                not State#client_state.partial_upload -> read_response(State)
            end;
        {error, closed} ->
            lhttpc_sock:close(Socket, Ssl),
            NewState = State#client_state{
                socket = undefined,
                attempts = State#client_state.attempts - 1
            },
            send_request(NewState);
        {error, Reason} ->
            lhttpc_sock:close(Socket, Ssl),
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
request_first_destination(#client_state{proxy = #lhttpc_url{} = Proxy}) ->
    {Proxy#lhttpc_url.host, Proxy#lhttpc_url.port, Proxy#lhttpc_url.is_ssl};
request_first_destination(#client_state{host = Host, port = Port, ssl = Ssl}) ->
    {Host, Port, Ssl}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_proxy_connect_response(State, StatusCode, StatusText) ->
    Socket = State#client_state.socket,
    ProxyIsSsl = (State#client_state.proxy)#lhttpc_url.is_ssl,
    case lhttpc_sock:recv(Socket, ProxyIsSsl) of
        {ok, {http_response, _Vsn, Code, Reason}} ->
            read_proxy_connect_response(State, Code, Reason);
        {ok, {http_header, _, _Name, _, _Value}} ->
            read_proxy_connect_response(State, StatusCode, StatusText);
        {ok, http_eoh} when StatusCode >= 100, StatusCode =< 199 ->
            % RFC 2616, section 10.1:
            % A client MUST be prepared to accept one or more
            % 1xx status responses prior to a regular
            % response, even if the client does not expect a
            % 100 (Continue) status message. Unexpected 1xx
            % status responses MAY be ignored by a user agent.
            read_proxy_connect_response(State, nil, nil);
        {ok, http_eoh} when StatusCode >= 200, StatusCode < 300 ->
            % RFC2817, any 2xx code means success.
            ConnectOptions = State#client_state.connect_options,
            SslOptions = State#client_state.proxy_ssl_options,
            Timeout = State#client_state.connect_timeout,
            State2 = case ssl:connect(Socket, SslOptions ++ ConnectOptions, Timeout) of
                {ok, SslSocket} ->
                    State#client_state{socket = SslSocket, proxy_setup = true};
                {error, Reason} ->
                    lhttpc_sock:close(Socket, ProxyIsSsl),
                    erlang:error({proxy_connection_failed, Reason})
            end,
            send_request(State2);
        {ok, http_eoh} ->
            throw({proxy_connection_refused, StatusCode, StatusText});
        {error, closed} ->
            lhttpc_sock:close(Socket, ProxyIsSsl),
            throw(proxy_connection_closed);
        {error, Reason} ->
            erlang:error({proxy_connection_failed, Reason})
    end.


%%------------------------------------------------------------------------------
%% @private
%% @doc Called when {partial_upload, WindowSize} is used. The user can send
%% messages using functions in lhttpc module
%% @end
%%------------------------------------------------------------------------------
partial_upload(State) ->
    Response = {ok, {self(), State#client_state.upload_window}},
    State#client_state.requester ! {response, self(), Response},
    partial_upload_loop(State#client_state{attempts = 1, request = undefined}).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
partial_upload_loop(State = #client_state{requester = Pid}) ->
    receive
        {trailers, Pid, Trailers} ->
            send_trailers(State, Trailers),
            read_response(State);
        {body_part, Pid, http_eob} ->
            send_body_part(State, http_eob),
            read_response(State);
        {body_part, Pid, Data} ->
            send_body_part(State, Data),
            Pid ! {ack, self()},
            partial_upload_loop(State)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_body_part(State = #client_state{socket = Socket, ssl = Ssl}, BodyPart) ->
    Data = encode_body_part(State, BodyPart),
    check_send_result(State, lhttpc_sock:send(Socket, Data, Ssl)).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
send_trailers(State = #client_state{chunked_upload = true}, Trailers) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    Data = [<<"0\r\n">>, lhttpc_lib:format_hdrs(Trailers)],
    check_send_result(State, lhttpc_sock:send(Socket, Data, Ssl));
send_trailers(#client_state{chunked_upload = false}, _Trailers) ->
    erlang:error(trailers_not_allowed).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
encode_body_part(#client_state{chunked_upload = true}, http_eob) ->
    <<"0\r\n\r\n">>; % We don't send trailers after http_eob
encode_body_part(#client_state{chunked_upload = false}, http_eob) ->
    <<>>;
encode_body_part(#client_state{chunked_upload = true}, Data) ->
    Size = list_to_binary(erlang:integer_to_list(iolist_size(Data), 16)),
    [Size, <<"\r\n">>, Data, <<"\r\n">>];
encode_body_part(#client_state{chunked_upload = false}, Data) ->
    Data.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_send_result(_State, ok) ->
    ok;
check_send_result(#client_state{socket = Sock, ssl = Ssl}, {error, Reason}) ->
    lhttpc_sock:close(Sock, Ssl),
    throw(Reason).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec read_response(#client_state{}) -> {any(), socket()} | no_return().
read_response(#client_state{socket = Socket, ssl = Ssl} = State) ->
    lhttpc_sock:setopts(Socket, [{packet, http}], Ssl),
    read_response(State, nil, {nil, nil}, []).

%%------------------------------------------------------------------------------
%% @private
%% @doc @TODO This does not handle redirects at the moment.
%% @end
%%------------------------------------------------------------------------------
-spec read_response(#client_state{}, {integer(), integer()} | 'nil', http_status(),
       any()) -> {any(), socket()} | no_return().
read_response(State, Vsn, {StatusCode, _} = Status, Hdrs) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, {http_response, NewVsn, NewStatusCode, Reason}} ->
            NewStatus = {NewStatusCode, Reason},
            read_response(State, NewVsn, NewStatus, Hdrs);
        {ok, {http_header, _, Name, _, Value}} ->
            Header = lhttpc_lib:canonical_header({Name, Value}),
            read_response(State, Vsn, Status, [Header | Hdrs]);
        {ok, http_eoh} when StatusCode >= 100, StatusCode =< 199 ->
            % RFC 2616, section 10.1:
            % A client MUST be prepared to accept one or more
            % 1xx status responses prior to a regular
            % response, even if the client does not expect a
            % 100 (Continue) status message. Unexpected 1xx
            % status responses MAY be ignored by a user agent.
            read_response(State, nil, {nil, nil}, []);
        {ok, http_eoh} ->
            lhttpc_sock:setopts(Socket, [{packet, raw}], Ssl),
            Response = handle_response_body(State, Vsn, Status, Hdrs),
            NewHdrs = element(2, Response),
            ReqHdrs = State#client_state.request_headers,
            NewSocket = maybe_close_socket(Socket, Ssl, Vsn, ReqHdrs, NewHdrs),
            {Response, NewSocket};
        {error, closed} ->
            % Either we only noticed that the socket was closed after we
            % sent the request, the server closed it just after we put
            % the request on the wire or the server has some issues and is
            % closing connections without sending responses.
            % If this the first attempt to send the request, we will try again.
            lhttpc_sock:close(Socket, Ssl),
            NewState = State#client_state{
                socket = undefined,
                attempts = State#client_state.attempts - 1
            },
            send_request(NewState);
        {ok, {http_error, _} = Reason} ->
            erlang:error(Reason);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @doc Handles the reading of the response body.
%% @end
%%------------------------------------------------------------------------------
-spec handle_response_body(#client_state{}, {integer(), integer()},
                http_status(), headers()) -> {http_status(), headers(), body()} |
                                             {http_status(), headers()}.
handle_response_body(#client_state{partial_download = false} = State, Vsn,
        Status, Hdrs) ->
%when {partial_download, PartialDownloadOptions} option is NOT used.
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    Method = State#client_state.method,
    {Body, NewHdrs} = case has_body(Method, element(1, Status), Hdrs) of
                          true  -> read_body(Vsn, Hdrs, Ssl, Socket, body_type(Hdrs));
                          false -> {<<>>, Hdrs}
                      end,
    {Status, NewHdrs, Body};
handle_response_body(#client_state{partial_download = true} = State, Vsn,
        Status, Hdrs) ->
%when {partial_download, PartialDownloadOptions} option is used.
    Method = State#client_state.method,
    case has_body(Method, element(1, Status), Hdrs) of
        true ->
            Response = {ok, {Status, Hdrs, self()}},
            State#client_state.requester ! {response, self(), Response},
            MonRef = erlang:monitor(process, State#client_state.requester),
            Res = read_partial_body(State, Vsn, Hdrs, body_type(Hdrs)),
            erlang:demonitor(MonRef, [flush]),
            Res;
        false ->
            {Status, Hdrs, undefined}
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec has_body(method(), integer(), headers()) -> boolean().
has_body("HEAD", _, _) ->
    % HEAD responses aren't allowed to include a body
    false;
has_body("OPTIONS", _, Hdrs) ->
    % OPTIONS can include a body, if Content-Length or Transfer-Encoding
    % indicates it
    ContentLength = lhttpc_lib:header_value("content-length", Hdrs),
    TransferEncoding = lhttpc_lib:header_value("transfer-encoding", Hdrs),
    case {ContentLength, TransferEncoding} of
        {undefined, undefined} -> false;
        {_, _}                 -> true
    end;
has_body(_, 204, _) ->
    false; % RFC 2616 10.2.5: 204 No Content
has_body(_, 304, _) ->
    false; % RFC 2616 10.3.5: 304 Not Modified
has_body(_, _, _) ->
    true. % All other responses are assumed to have a body

%%------------------------------------------------------------------------------
%% @private
%% @doc  Find out how to read the entity body from the request.
% * If we have a Content-Length, just use that and read the complete
%   entity.
% * If Transfer-Encoding is set to chunked, we should read one chunk at
%   the time
% * If neither of this is true, we need to read until the socket is
%   closed (AFAIK, this was common in versions before 1.1).
%% @end
%%------------------------------------------------------------------------------
-spec body_type(headers()) -> 'chunked' | 'infinite' | {fixed_length, integer()}.
body_type(Hdrs) ->
    case lhttpc_lib:header_value("content-length", Hdrs) of
        undefined ->
            TransferEncoding = string:to_lower(
                lhttpc_lib:header_value("transfer-encoding", Hdrs, "undefined")
            ),
            case TransferEncoding of
                "chunked" -> chunked;
                _         -> infinite
            end;
        ContentLength ->
            {fixed_length, list_to_integer(ContentLength)}
    end.

%%------------------------------------------------------------------------------
%%% @private
%%% @doc Called when {partial_download, PartialDownloadOptions} option is used.
%%% @end
%%------------------------------------------------------------------------------
read_partial_body(State, _Vsn, Hdrs, chunked) ->
    Window = State#client_state.download_window,
    read_partial_chunked_body(State, Hdrs, Window, 0, [], 0);
read_partial_body(State, Vsn, Hdrs, infinite) ->
    check_infinite_response(Vsn, Hdrs),
    read_partial_infinite_body(State, Hdrs, State#client_state.download_window);
read_partial_body(State, _Vsn, Hdrs, {fixed_length, ContentLength}) ->
    read_partial_finite_body(State, Hdrs, ContentLength,
        State#client_state.download_window).

%%------------------------------------------------------------------------------
%%% @private
%%% @doc Called when {partial_download, PartialDownloadOptions} option is NOT used.
%%% @end
%%------------------------------------------------------------------------------
read_body(_Vsn, Hdrs, Ssl, Socket, chunked) ->
    read_chunked_body(Socket, Ssl, Hdrs, []);
read_body(Vsn, Hdrs, Ssl, Socket, infinite) ->
    check_infinite_response(Vsn, Hdrs),
    read_infinite_body(Socket, Hdrs, Ssl);
read_body(_Vsn, Hdrs, Ssl, Socket, {fixed_length, ContentLength}) ->
    read_length(Hdrs, Ssl, Socket, ContentLength).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_partial_finite_body(State = #client_state{}, Hdrs, 0, _Window) ->
    reply_end_of_body(State, [], Hdrs);
read_partial_finite_body(State = #client_state{requester = To}, Hdrs,
        ContentLength, 0) ->
    receive
        {ack, To} ->
            read_partial_finite_body(State, Hdrs, ContentLength, 1);
        {'DOWN', _, process, To, _} ->
            exit(normal)
    end;
read_partial_finite_body(State, Hdrs, ContentLength, Window) when Window >= 0->
    case read_body_part(State, ContentLength) of
        {ok, Bin} ->
            State#client_state.requester ! {body_part, self(), Bin},
            To = State#client_state.requester,
            receive
                {ack, To} ->
                    Length = ContentLength - iolist_size(Bin),
                    read_partial_finite_body(State, Hdrs, Length, Window);
                {'DOWN', _, process, To, _} ->
                    exit(normal)
            after 0 ->
                    Length = ContentLength - iolist_size(Bin),
                    read_partial_finite_body(State, Hdrs, Length, lhttpc_lib:dec(Window))
            end;
        {error, Reason} ->
            State#client_state.requester ! {error, self(), Reason},
            exit(normal)
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_body_part(#client_state{part_size = infinity} = State, _ContentLength) ->
    lhttpc_sock:recv(State#client_state.socket, State#client_state.ssl);
read_body_part(#client_state{part_size = PartSize} = State, ContentLength)
        when PartSize =< ContentLength ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    PartSize = State#client_state.part_size,
    lhttpc_sock:recv(Socket, PartSize, Ssl);
read_body_part(#client_state{part_size = PartSize} = State, ContentLength)
        when PartSize > ContentLength ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    lhttpc_sock:recv(Socket, ContentLength, Ssl).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_length(Hdrs, Ssl, Socket, Length) ->
    case lhttpc_sock:recv(Socket, Length, Ssl) of
        {ok, Data} ->
            {Data, Hdrs};
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_partial_chunked_body(State, Hdrs, Window, BufferSize, Buffer, 0) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    PartSize = State#client_state.part_size,
    case read_chunk_size(Socket, Ssl) of
        0 ->
            reply_chunked_part(State, Buffer, Window),
            {Trailers, NewHdrs} = read_trailers(Socket, Ssl, [], Hdrs),
            reply_end_of_body(State, Trailers, NewHdrs);
        ChunkSize when PartSize =:= infinity ->
            Chunk = read_chunk(Socket, Ssl, ChunkSize),
            NewWindow = reply_chunked_part(State, [Chunk | Buffer], Window),
            read_partial_chunked_body(State, Hdrs, NewWindow, 0, [], 0);
        ChunkSize when BufferSize + ChunkSize >= PartSize ->
            {Chunk, RemSize} = read_partial_chunk(Socket, Ssl,
                PartSize - BufferSize, ChunkSize),
            NewWindow = reply_chunked_part(State, [Chunk | Buffer], Window),
            read_partial_chunked_body(State, Hdrs, NewWindow, 0, [], RemSize);
        ChunkSize ->
            Chunk = read_chunk(Socket, Ssl, ChunkSize),
            read_partial_chunked_body(State, Hdrs, Window,
                BufferSize + ChunkSize, [Chunk | Buffer], 0)
    end;
read_partial_chunked_body(State, Hdrs, Window, BufferSize, Buffer, RemSize) ->
    Socket = State#client_state.socket,
    Ssl = State#client_state.ssl,
    PartSize = State#client_state.part_size,
    if
        BufferSize + RemSize >= PartSize ->
            {Chunk, NewRemSize} =
                read_partial_chunk(Socket, Ssl, PartSize - BufferSize, RemSize),
            NewWindow = reply_chunked_part(State, [Chunk | Buffer], Window),
            read_partial_chunked_body(State, Hdrs, NewWindow, 0, [],
                NewRemSize);
        BufferSize + RemSize < PartSize ->
            Chunk = read_chunk(Socket, Ssl, RemSize),
            read_partial_chunked_body(State, Hdrs, Window, BufferSize + RemSize,
                [Chunk | Buffer], 0)
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_chunk_size(Socket, Ssl) ->
    lhttpc_sock:setopts(Socket, [{packet, line}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, ChunkSizeExt} ->
            chunk_size(ChunkSizeExt);
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
reply_chunked_part(_State, [], Window) ->
    Window;
reply_chunked_part(State = #client_state{requester = Pid}, Buff, 0) ->
    receive
        {ack, Pid} ->
            reply_chunked_part(State, Buff, 1);
        {'DOWN', _, process, Pid, _} ->
            exit(normal)
    end;
reply_chunked_part(#client_state{requester = Pid}, Buffer, Window) ->
    Pid ! {body_part, self(), list_to_binary(lists:reverse(Buffer))},
    receive
        {ack, Pid} ->  Window;
        {'DOWN', _, process, Pid, _} -> exit(normal)
    after 0 ->
        lhttpc_lib:dec(Window)
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_chunked_body(Socket, Ssl, Hdrs, Chunks) ->
    case read_chunk_size(Socket, Ssl) of
        0 ->
            Body = list_to_binary(lists:reverse(Chunks)),
            {_, NewHdrs} = read_trailers(Socket, Ssl, [], Hdrs),
            {Body, NewHdrs};
        Size ->
            Chunk = read_chunk(Socket, Ssl, Size),
            read_chunked_body(Socket, Ssl, Hdrs, [Chunk | Chunks])
    end.

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
chunk_size(Bin) ->
    erlang:list_to_integer(lists:reverse(chunk_size(Bin, [])), 16).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
chunk_size(<<$;, _/binary>>, Chars) ->
    Chars;
chunk_size(<<"\r\n", _/binary>>, Chars) ->
    Chars;
chunk_size(<<$\s, Binary/binary>>, Chars) ->
    %% Facebook's HTTP server returns a chunk size like "6  \r\n"
    chunk_size(Binary, Chars);
chunk_size(<<Char, Binary/binary>>, Chars) ->
    chunk_size(Binary, [Char | Chars]).

%%------------------------------------------------------------------------------
%%% @private
%%------------------------------------------------------------------------------
read_partial_chunk(Socket, Ssl, ChunkSize, ChunkSize) ->
    {read_chunk(Socket, Ssl, ChunkSize), 0};
read_partial_chunk(Socket, Ssl, Size, ChunkSize) ->
    lhttpc_sock:setopts(Socket, [{packet, raw}], Ssl),
    case lhttpc_sock:recv(Socket, Size, Ssl) of
        {ok, Chunk} ->
            {Chunk, ChunkSize - Size};
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_chunk(Socket, Ssl, Size) ->
    lhttpc_sock:setopts(Socket, [{packet, raw}], Ssl),
    case lhttpc_sock:recv(Socket, Size + 2, Ssl) of
        {ok, <<Chunk:Size/binary, "\r\n">>} ->
            Chunk;
        {ok, Data} ->
            erlang:error({invalid_chunk, Data});
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec read_trailers(socket(), boolean(), any(), any()) ->
                           {any(), any()} | no_return().
read_trailers(Socket, Ssl, Trailers, Hdrs) ->
    lhttpc_sock:setopts(Socket, [{packet, httph}], Ssl),
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, http_eoh} ->
            {Trailers, Hdrs};
        {ok, {http_header, _, Name, _, Value}} ->
            Header = {lhttpc_lib:maybe_atom_to_list(Name), Value},
            read_trailers(Socket, Ssl, [Header | Trailers], [Header | Hdrs]);
        {error, {http_error, Data}} ->
            erlang:error({bad_trailer, Data})
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec reply_end_of_body(#client_state{}, any(), any()) -> {'no_return', any()}.
reply_end_of_body(#client_state{requester = Requester}, Trailers, Hdrs) ->
    Requester ! {http_eob, self(), Trailers},
    {no_return, Hdrs}.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
read_partial_infinite_body(State = #client_state{requester = To}, Hdrs, 0) ->
    receive
        {ack, To} ->
            read_partial_infinite_body(State, Hdrs, 1);
        {'DOWN', _, process, To, _} ->
            exit(normal)
    end;
read_partial_infinite_body(State = #client_state{requester = To}, Hdrs, Window)
        when Window >= 0 ->
    case read_infinite_body_part(State) of
        http_eob -> reply_end_of_body(State, [], Hdrs);
        Bin ->
            State#client_state.requester ! {body_part, self(), Bin},
            receive
                {ack, To} ->
                    read_partial_infinite_body(State, Hdrs, Window);
                {'DOWN', _, process, To, _} ->
                    exit(normal)
            after 0 ->
                read_partial_infinite_body(State, Hdrs, lhttpc_lib:dec(Window))
            end
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec read_infinite_body_part(#client_state{}) -> bodypart() | no_return().
read_infinite_body_part(#client_state{socket = Socket, ssl = Ssl}) ->
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, Data} ->
            Data;
        {error, closed} ->
            http_eob;
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
check_infinite_response({1, Minor}, Hdrs) when Minor >= 1 ->
    HdrValue = lhttpc_lib:header_value("connection", Hdrs, "keep-alive"),
    case string:to_lower(HdrValue) of
        "close" -> ok;
        _       -> erlang:error(no_content_length)
    end;
check_infinite_response(_, Hdrs) ->
    HdrValue = lhttpc_lib:header_value("connection", Hdrs, "close"),
    case string:to_lower(HdrValue) of
        "keep-alive" -> erlang:error(no_content_length);
        _            -> ok
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec read_infinite_body(socket(), headers(), boolean()) ->
                        {binary(), headers()} | no_return().
read_infinite_body(Socket, Hdrs, Ssl) ->
    read_until_closed(Socket, <<>>, Hdrs, Ssl).

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec read_until_closed(socket(), binary(), any(), boolean()) ->
                        {binary(), any()} | no_return().
read_until_closed(Socket, Acc, Hdrs, Ssl) ->
    case lhttpc_sock:recv(Socket, Ssl) of
        {ok, Body} ->
            NewAcc = <<Acc/binary, Body/binary>>,
            read_until_closed(Socket, NewAcc, Hdrs, Ssl);
        {error, closed} ->
            {Acc, Hdrs};
        {error, Reason} ->
            erlang:error(Reason)
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
maybe_close_socket(Socket, Ssl, {1, Minor}, ReqHdrs, RespHdrs) when Minor >= 1->
    ClientConnection = ?CONNECTION_HDR(ReqHdrs, "keep-alive"),
    ServerConnection = ?CONNECTION_HDR(RespHdrs, "keep-alive"),
    if
        ClientConnection =:= "close"; ServerConnection =:= "close" ->
            lhttpc_sock:close(Socket, Ssl),
            undefined;
        ClientConnection =/= "close", ServerConnection =/= "close" ->
            Socket
    end;
maybe_close_socket(Socket, Ssl, _, ReqHdrs, RespHdrs) ->
    ClientConnection = ?CONNECTION_HDR(ReqHdrs, "keep-alive"),
    ServerConnection = ?CONNECTION_HDR(RespHdrs, "close"),
    if
        ClientConnection =:= "close"; ServerConnection =/= "keep-alive" ->
            lhttpc_sock:close(Socket, Ssl),
            undefined;
        ClientConnection =/= "close", ServerConnection =:= "keep-alive" ->
            Socket
    end.

%%------------------------------------------------------------------------------
%% @private
%%------------------------------------------------------------------------------
-spec is_ipv6_host(host()) -> boolean().
is_ipv6_host(Host) ->
    case inet_parse:address(Host) of
        {ok, {_, _, _, _, _, _, _, _}} ->
            true;
        {ok, {_, _, _, _}} ->
            false;
        _ ->
            % Prefer IPv4 over IPv6.
            case inet:getaddr(Host, inet) of
                {ok, _} ->
                    false;
                _ ->
                    case inet:getaddr(Host, inet6) of
                        {ok, _} ->
                            true;
                        _ ->
                            false
                    end
            end
    end.
