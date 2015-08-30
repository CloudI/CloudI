%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cowboy CloudI HTTP Handler==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_http_cowboy_handler).
-author('mjtruog [at] gmail (dot) com').

%-behaviour(cloudi_x_cowboy_http_handler).
%-behaviour(cloudi_x_cowboy_websocket_handler).

%% external interface

%% cloudi_x_cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         info/3,
         terminate/3]).

%% cloudi_x_cowboy_websocket_handler callbacks
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").
-include("cloudi_http_cowboy_handler.hrl").

-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.

-record(websocket_state,
    {
        % for service requests entering CloudI
        websocket_connect_trans_id   :: undefined | cloudi_service:trans_id(),
        name_incoming                :: string(),
        name_outgoing                :: cloudi_service:service_name(),
        request_info                 :: list({binary(), binary()}) | binary(),
        % for a service request exiting CloudI
        response_pending = false     :: boolean(),
        response_timer               :: reference(),
        request_pending              :: cloudi:message_service_request(),
        response_lookup :: dict_proxy(any(),
                                      {cloudi:message_service_request(),
                                       reference()}) | undefined,
        recv_timeouts :: dict_proxy(cloudi:trans_id(),
                                    reference()) | undefined,
        queued :: cloudi_x_pqueue4:cloudi_x_pqueue4(
                      cloudi:message_service_request()) | undefined
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_x_cowboy_http_handler
%%%------------------------------------------------------------------------

init(_Transport, Req0,
     #cowboy_state{use_websockets = UseWebSockets} = State)
    when UseWebSockets =:= true; UseWebSockets =:= exclusively ->
    case upgrade_request(Req0) of
        {websocket, Req1} ->
            {upgrade, protocol, cloudi_x_cowboy_websocket, Req1,
             State#cowboy_state{use_websockets = true}};
        {undefined, Req1} ->
            if
                UseWebSockets =:= exclusively ->
                    {shutdown, Req1, State};
                true ->
                    {ok, Req1, State}
            end;
        {Upgrade, Req1} ->
            ?LOG_ERROR("Unknown protocol: ~p", [Upgrade]),
            {shutdown, Req1, State}
    end;
init(_Transport, Req,
     #cowboy_state{use_websockets = false} = State) ->
    {ok, Req, State}.

handle(Req0,
       #cowboy_state{output_type = OutputType,
                     content_type_forced = ContentTypeForced,
                     content_types_accepted = ContentTypesAccepted,
                     set_x_forwarded_for = SetXForwardedFor,
                     status_code_timeout = StatusCodeTimeout,
                     use_host_prefix = UseHostPrefix,
                     use_client_ip_prefix = UseClientIpPrefix,
                     use_method_suffix = UseMethodSuffix
                     } = State) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun request_time_start/0, []),
    {Method, Req1} = cloudi_x_cowboy_req:method(Req0),
    {HeadersIncoming0, Req2} = cloudi_x_cowboy_req:headers(Req1),
    {QsVals, Req3} = cloudi_x_cowboy_req:qs_vals(Req2),
    {PathRaw, Req4} = cloudi_x_cowboy_req:path(Req3),
    {{ClientIpAddr, ClientPort} = Client,
     Req5} = cloudi_x_cowboy_req:peer(Req4),
    {NameIncoming, ReqN} = service_name_incoming(UseClientIpPrefix,
                                                 UseHostPrefix,
                                                 PathRaw,
                                                 Client,
                                                 Req5),
    RequestAccepted = if
        ContentTypesAccepted =:= undefined ->
            true;
        true ->
            header_accept_check(HeadersIncoming0, ContentTypesAccepted)
    end,
    if
        RequestAccepted =:= false ->
            HttpCode = 406,
            {ok, Req} = cloudi_x_cowboy_req:reply(HttpCode,
                                                  ReqN),
            ?LOG_WARN_APPLY(fun request_time_end_error/5,
                            [HttpCode, Method, NameIncoming,
                             RequestStartMicroSec, not_acceptable]),
            {ok, Req, State};
        RequestAccepted =:= true ->
            NameOutgoing = if
                UseMethodSuffix =:= false ->
                    NameIncoming;
                Method =:= <<"GET">> ->
                    NameIncoming ++ "/get";
                Method =:= <<"POST">> ->
                    NameIncoming ++ "/post";
                Method =:= <<"PUT">> ->
                    NameIncoming ++ "/put";
                Method =:= <<"DELETE">> ->
                    NameIncoming ++ "/delete";
                Method =:= <<"HEAD">> ->
                    NameIncoming ++ "/head";
                Method =:= <<"OPTIONS">> ->
                    NameIncoming ++ "/options";
                Method =:= <<"PATCH">> ->
                    NameIncoming ++ "/connect";
                Method =:= <<"TRACE">> ->
                    NameIncoming ++ "/trace";
                Method =:= <<"CONNECT">> ->
                    NameIncoming ++ "/connect";
                true ->
                    % handle custom methods, if they occur
                    NameIncoming ++ [$/ |
                        string:to_lower(erlang:binary_to_list(Method))]
            end,
            PeerShort = erlang:list_to_binary(inet_parse:ntoa(ClientIpAddr)),
            PeerLong = cloudi_ip_address:to_binary(ClientIpAddr),
            PeerPort = erlang:integer_to_binary(ClientPort),
            HeadersIncoming1 = [{<<"peer">>, PeerShort},
                                {<<"peer-port">>, PeerPort},
                                {<<"source-address">>, PeerLong},
                                {<<"source-port">>, PeerPort},
                                {<<"url-path">>, PathRaw} |
                                HeadersIncoming0],
            HeadersIncomingN = if
                SetXForwardedFor =:= true ->
                    case lists:keyfind(<<"x-forwarded-for">>, 1,
                                       HeadersIncoming0) of
                        false ->
                            [{<<"x-forwarded-for">>, PeerShort} |
                             HeadersIncoming1];
                        _ ->
                            HeadersIncoming1
                            
                    end;
                SetXForwardedFor =:= false ->
                    HeadersIncoming1
            end,
            Body = if
                Method =:= <<"GET">> ->
                    if
                        (OutputType =:= external) orelse
                        (OutputType =:= binary) orelse (OutputType =:= list) ->
                            get_query_string_external(QsVals);
                        OutputType =:= internal ->
                            QsVals
                    end;
                (Method =:= <<"POST">>) orelse
                (Method =:= <<"PUT">>) ->
                    case header_content_type(HeadersIncoming0) of
                        <<"application/zip">> ->
                            'application_zip';
                        <<"multipart/", _/binary>> ->
                            'multipart';
                        _ ->
                            'normal'
                    end;
                (Method =:= <<"DELETE">>) orelse
                (Method =:= <<"HEAD">>) orelse
                (Method =:= <<"OPTIONS">>) orelse
                (Method =:= <<"TRACE">>) orelse
                (Method =:= <<"CONNECT">>) ->
                    <<>>;
                true ->
                    'normal'
            end,
            case handle_request(NameOutgoing, HeadersIncomingN,
                                Body, ReqN, State) of
                {{cowboy_response, HeadersOutgoing, Response},
                 ReqN0, NewState} ->
                    {HttpCode,
                     Req} = handle_response(NameIncoming, HeadersOutgoing,
                                            Response, ReqN0, OutputType,
                                            ContentTypeForced),
                    ?LOG_TRACE_APPLY(fun request_time_end_success/5,
                                     [HttpCode, Method,
                                      NameIncoming, NameOutgoing,
                                      RequestStartMicroSec]),
                    {ok, Req, NewState};
                {{cowboy_error, timeout},
                 ReqN0, NewState} ->
                    HttpCode = StatusCodeTimeout,
                    {ok, Req} = if
                        HttpCode =:= 405 ->
                            % currently not providing a list of valid methods
                            % (a different HTTP status code is a better
                            %  choice, since this service name may not exist)
                            HeadersOutgoing = [{<<"allow">>, <<"">>}],
                            cloudi_x_cowboy_req:reply(HttpCode,
                                                      HeadersOutgoing,
                                                      ReqN0);
                        true ->
                            cloudi_x_cowboy_req:reply(HttpCode,
                                                      ReqN0)
                    end,
                    ?LOG_WARN_APPLY(fun request_time_end_error/5,
                                    [HttpCode, Method, NameIncoming,
                                     RequestStartMicroSec, timeout]),
                    {ok, Req, NewState};
                {{cowboy_error, Reason},
                 ReqN0, NewState} ->
                    HttpCode = 500,
                    {ok, Req} = cloudi_x_cowboy_req:reply(HttpCode,
                                                          ReqN0),
                    ?LOG_WARN_APPLY(fun request_time_end_error/5,
                                    [HttpCode, Method, NameIncoming,
                                     RequestStartMicroSec, Reason]),
                    {ok, Req, NewState}
            end
    end.

info(Message, Req, State) ->
    ?LOG_WARN("ignored ~p", [Message]),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

websocket_init(_Transport, Req0,
               #cowboy_state{scope = Scope,
                             prefix = Prefix,
                             timeout_websocket = TimeoutWebSocket,
                             output_type = OutputType,
                             set_x_forwarded_for = SetXForwardedFor,
                             websocket_connect = WebSocketConnect,
                             websocket_ping = WebSocketPing,
                             websocket_protocol = WebSocketProtocol,
                             websocket_name_unique = WebSocketNameUnique,
                             websocket_subscriptions = WebSocketSubscriptions,
                             use_websockets = true,
                             use_host_prefix = UseHostPrefix,
                             use_client_ip_prefix = UseClientIpPrefix,
                             use_method_suffix = UseMethodSuffix
                             } = State) ->
    {Method, Req1} = cloudi_x_cowboy_req:method(Req0),
    {HeadersIncoming0, Req2} = cloudi_x_cowboy_req:headers(Req1),
    {PathRaw, Req3} = cloudi_x_cowboy_req:path(Req2),
    {{ClientIpAddr, ClientPort} = Client,
     Req4} = cloudi_x_cowboy_req:peer(Req3),
    {NameIncoming, ReqN} = service_name_incoming(UseClientIpPrefix,
                                                 UseHostPrefix,
                                                 PathRaw,
                                                 Client,
                                                 Req4),
    NameOutgoing = if
        UseMethodSuffix =:= false ->
            NameIncoming;
        Method =:= <<"GET">> ->
            NameIncoming ++ "/get"
    end,
    % can not turn-off the /websocket suffix, since it would otherwise
    % cause a conflict with service requests coming from HTTP into CloudI
    % when UseMethodSuffix == false
    PathRawStr = erlang:binary_to_list(PathRaw),
    NameWebSocket = PathRawStr ++ "/websocket",
    % service requests are only received if they relate to
    % the service's prefix
    SubscribeWebSocket = lists:prefix(Prefix, NameWebSocket),
    HeadersIncoming1 = if
        SubscribeWebSocket =:= true ->
            [{<<"service-name">>, erlang:list_to_binary(NameWebSocket)} |
             HeadersIncoming0];
        SubscribeWebSocket =:= false ->
            HeadersIncoming0
    end,
    PeerShort = erlang:list_to_binary(inet_parse:ntoa(ClientIpAddr)),
    PeerLong = cloudi_ip_address:to_binary(ClientIpAddr),
    PeerPort = erlang:integer_to_binary(ClientPort),
    HeadersIncoming2 = [{<<"peer">>, PeerShort},
                        {<<"peer-port">>, PeerPort},
                        {<<"source-address">>, PeerLong},
                        {<<"source-port">>, PeerPort},
                        {<<"url-path">>, PathRaw} | HeadersIncoming1],
    HeadersIncomingN = if
        SetXForwardedFor =:= true ->
            case lists:keyfind(<<"x-forwarded-for">>, 1, HeadersIncoming0) of
                false ->
                    [{<<"x-forwarded-for">>, PeerShort} | HeadersIncoming2];
                _ ->
                    HeadersIncoming2
                    
            end;
        SetXForwardedFor =:= false ->
            HeadersIncoming2
    end,
    RequestInfo = if
        (OutputType =:= external) orelse (OutputType =:= binary) ->
            headers_external_incoming(HeadersIncomingN);
        (OutputType =:= internal) orelse (OutputType =:= list) ->
            HeadersIncomingN
    end,
    WebSocketPingStatus = if
        WebSocketPing =:= undefined ->
            undefined;
        is_integer(WebSocketPing) ->
            erlang:send_after(WebSocketPing, self(),
                              {websocket_ping, WebSocketPing}),
            received
    end,
    ResponseLookup = if
        WebSocketProtocol /= undefined ->
            dict:new();
        true ->
            undefined
    end,
    RecvTimeouts = if
        WebSocketProtocol =:= undefined ->
            dict:new();
        true ->
            undefined
    end,
    Queued = if
        WebSocketProtocol =:= undefined ->
            cloudi_x_pqueue4:new();
        true ->
            undefined
    end,
    if
        SubscribeWebSocket =:= true ->
            % initiate an asynchronous close if the websocket must be unique
            OldConnectionMonitors = if
                WebSocketNameUnique =:= true ->
                    case cloudi_x_cpg:get_members(Scope, NameWebSocket,
                                                  infinity) of
                        {ok, _, OldConnections} ->
                            lists:map(fun(OldConnection) ->
                                cloudi_service_http_cowboy:close(OldConnection),
                                erlang:monitor(process, OldConnection)
                            end, OldConnections);
                        {error, _} ->
                            []
                    end;
                WebSocketNameUnique =:= false ->
                    []
            end,
            % service requests are only received if they relate to
            % the service's prefix
            ok = cloudi_x_cpg:join(Scope, NameWebSocket, self(), infinity),
            % block on the websocket close if the connection must be unique
            if
                WebSocketNameUnique =:= true ->
                    lists:foreach(fun(OldConnectionMonitor) ->
                        receive
                            {'DOWN', OldConnectionMonitor, process, _, _} ->
                                ok
                        end
                    end, OldConnectionMonitors);
                WebSocketNameUnique =:= false ->
                    ok
            end,
            if
                WebSocketSubscriptions =:= undefined ->
                    ok;
                true ->
                    % match websocket_subscriptions to determine if a
                    % more subscriptions should occur, possibly
                    % using parameters in a pattern template
                    % for the subscription
                    case cloudi_x_trie:find_match(PathRawStr,
                                                  WebSocketSubscriptions) of
                        error ->
                            ok;
                        {ok, Pattern, Functions} ->
                            Parameters = cloudi_service:
                                         service_name_parse(PathRawStr,
                                                            Pattern),
                            websocket_subscriptions(Functions, Parameters,
                                                    Scope)
                    end
            end;
        SubscribeWebSocket =:= false ->
            ok
    end,
    {ok, ReqN,
     websocket_connect_check(WebSocketConnect,
                             State#cowboy_state{
                                 websocket_ping = WebSocketPingStatus,
                                 websocket_subscriptions = undefined,
                                 websocket_state = #websocket_state{
                                     name_incoming = NameIncoming,
                                     name_outgoing = NameOutgoing,
                                     request_info = RequestInfo,
                                     response_lookup = ResponseLookup,
                                     recv_timeouts = RecvTimeouts,
                                     queued = Queued}}), TimeoutWebSocket}.

websocket_handle({ping, _Payload}, Req, State) ->
    % cowboy automatically responds with pong
    {ok, Req, State};

websocket_handle({pong, _Payload}, Req, State) ->
    {ok, Req, State#cowboy_state{websocket_ping = received}};

websocket_handle({WebSocketResponseType, ResponseBinary}, Req,
                 #cowboy_state{output_type = OutputType,
                               websocket_protocol = undefined,
                               use_websockets = true,
                               websocket_state = #websocket_state{
                                   request_info = ResponseInfo,
                                   response_pending = true,
                                   response_timer = ResponseTimer,
                                   request_pending = T
                                   } = WebSocketState
                               } = State)
    when WebSocketResponseType =:= text;
         WebSocketResponseType =:= binary ->
    Response = if
        (OutputType =:= external) orelse (OutputType =:= internal) orelse
        (OutputType =:= binary) ->
            ResponseBinary;
        (OutputType =:= list) ->
            erlang:binary_to_list(ResponseBinary)
    end,
    websocket_handle_outgoing_response(T, ResponseTimer,
                                       ResponseInfo, Response),
    websocket_process_queue(Req,
                            State#cowboy_state{websocket_state =
                                WebSocketState#websocket_state{
                                    response_pending = false,
                                    response_timer = undefined,
                                    request_pending = undefined}
                                });

websocket_handle({WebSocketRequestType, RequestBinary}, Req,
                 #cowboy_state{dispatcher = Dispatcher,
                               context = Context,
                               output_type = OutputType,
                               websocket_protocol = undefined,
                               use_websockets = true,
                               websocket_state = #websocket_state{
                                   name_incoming = NameIncoming,
                                   name_outgoing = NameOutgoing,
                                   request_info = RequestInfo,
                                   response_pending = false}
                               } = State)
    when WebSocketRequestType =:= text;
         WebSocketRequestType =:= binary ->
    Request = if
        (OutputType =:= external) orelse (OutputType =:= internal) orelse
        (OutputType =:= binary) ->
            RequestBinary;
        (OutputType =:= list) ->
            erlang:binary_to_list(RequestBinary)
    end,
    ResponseBinaryF = fun(Data) ->
        true = (((OutputType =:= external) orelse
                 (OutputType =:= internal)) andalso
                (is_binary(Data) orelse is_list(Data))) orelse
               ((OutputType =:= binary) andalso
                is_binary(Data)) orelse
               ((OutputType =:= list) andalso
                is_list(Data)),
        Data
    end,
    websocket_handle_incoming_request(Dispatcher, Context, NameOutgoing,
                                      RequestInfo, Request, ResponseBinaryF,
                                      WebSocketRequestType, Req,
                                      NameIncoming, State);

websocket_handle({WebSocketRequestType, RequestBinary}, Req,
                 #cowboy_state{dispatcher = Dispatcher,
                               context = Context,
                               websocket_protocol = WebSocketProtocol,
                               use_websockets = true,
                               websocket_state = #websocket_state{
                                   name_incoming = NameIncoming,
                                   name_outgoing = NameOutgoing,
                                   request_info = Info,
                                   response_pending = false,
                                   response_lookup = ResponseLookup
                                   } = WebSocketState
                                } = State)
    when WebSocketRequestType =:= text;
         WebSocketRequestType =:= binary ->
    {LookupID,
     LookupData, Value} = case WebSocketProtocol(incoming, RequestBinary) of
        {incoming, Request} ->
            {undefined, undefined, Request};
        {ID, Response} ->
            case dict:find(ID, ResponseLookup) of
                {ok, ResponseData} ->
                    {ID, ResponseData, Response};
                error ->
                    {undefined, timeout, undefined}
            end
    end,
    case LookupData of
        undefined ->
            % an incoming service request
            ResponseF = fun(ProtocolData) ->
                {_, Data} = WebSocketProtocol(outgoing, ProtocolData),
                Data
            end,
            websocket_handle_incoming_request(Dispatcher, Context, NameOutgoing,
                                              Info, Value, ResponseF,
                                              WebSocketRequestType, Req,
                                              NameIncoming, State);
        timeout ->
            % a response arrived but has already timed-out
            {ok, Req, State};
        {T, ResponseTimer} ->
            % a response to an outgoing service request that has finished
            websocket_handle_outgoing_response(T, ResponseTimer,
                                               Info, Value),
            {ok, Req,
             State#cowboy_state{websocket_state =
                 WebSocketState#websocket_state{
                     response_lookup = dict:erase(LookupID, ResponseLookup)}
                 }}
    end.

websocket_info({response_timeout, ID}, Req,
               #cowboy_state{use_websockets = true,
                             websocket_state = #websocket_state{
                                 response_pending = false,
                                 response_lookup = ResponseLookup
                                 } = WebSocketState
                             } = State) ->
    {ok, Req,
     State#cowboy_state{websocket_state =
         WebSocketState#websocket_state{
             response_lookup = dict:erase(ID, ResponseLookup)}
         }};

websocket_info(response_timeout, Req,
               #cowboy_state{websocket_protocol = undefined,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 response_pending = true} = WebSocketState
                             } = State) ->
    websocket_process_queue(Req,
                            State#cowboy_state{websocket_state =
                                WebSocketState#websocket_state{
                                    response_pending = false,
                                    response_timer = undefined,
                                    request_pending = undefined}
                                });

websocket_info({Type, Name, Pattern, RequestInfo, Request,
                Timeout, Priority, TransId, Source}, Req,
               #cowboy_state{output_type = OutputType,
                             websocket_output_type = WebSocketOutputType,
                             websocket_protocol = undefined,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 response_pending = false} = WebSocketState
                             } = State)
    when ((((OutputType =:= external) orelse
            (OutputType =:= internal)) andalso
           (is_binary(Request) orelse is_list(Request))) orelse
          ((OutputType =:= binary) andalso
           is_binary(Request)) orelse
          ((OutputType =:= list) andalso
           is_list(Request))),
         (Type =:= 'cloudi_service_send_async' orelse
          Type =:= 'cloudi_service_send_sync') ->
    ResponseTimer = erlang:send_after(Timeout, self(), response_timeout),
    T = {Type, Name, Pattern, undefined, undefined,
         Timeout, Priority, TransId, Source},
    NewState = State#cowboy_state{
        websocket_state = WebSocketState#websocket_state{
            response_pending = true,
            response_timer = ResponseTimer,
            request_pending = T}},
    case websocket_terminate_check(RequestInfo) of
        true when Request == <<>> ->
            {reply, close, Req, NewState};
        true ->
            {reply, [{WebSocketOutputType, Request}, close], Req, NewState};
        false ->
            {reply, {WebSocketOutputType, Request}, Req, NewState}
    end;

websocket_info({Type, Name, Pattern, RequestInfo, RequestProtocol,
                Timeout, Priority, TransId, Source}, Req,
               #cowboy_state{websocket_output_type = WebSocketOutputType,
                             websocket_protocol = WebSocketProtocol,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 response_pending = false,
                                 response_lookup = ResponseLookup
                                 } = WebSocketState
                             } = State)
    when (Type =:= 'cloudi_service_send_async' orelse
          Type =:= 'cloudi_service_send_sync') ->
    {ID, Request} = WebSocketProtocol(outgoing, RequestProtocol),
    T = {Type, Name, Pattern, undefined, undefined,
         Timeout, Priority, TransId, Source},
    ResponseTimer = erlang:send_after(Timeout, self(),
                                      {response_timeout, ID}),
    NewResponseLookup = dict:store(ID, {T, ResponseTimer}, ResponseLookup),
    NewState = State#cowboy_state{
        websocket_state = WebSocketState#websocket_state{
            response_lookup = NewResponseLookup}},
    case websocket_terminate_check(RequestInfo) of
        true when Request == <<>> ->
            {reply, close, Req, NewState};
        true ->
            {reply, [{WebSocketOutputType, Request}, close], Req, NewState};
        false ->
            {reply, {WebSocketOutputType, Request}, Req, NewState}
    end;

websocket_info({Type, _, _, _, Request,
                Timeout, Priority, TransId, _} = T, Req,
               #cowboy_state{output_type = OutputType,
                             websocket_protocol = undefined,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 response_pending = true,
                                 recv_timeouts = RecvTimeouts,
                                 queued = Queue
                                 } = WebSocketState
                             } = State)
    when ((((OutputType =:= external) orelse
            (OutputType =:= internal)) andalso
           (is_binary(Request) orelse is_list(Request))) orelse
          ((OutputType =:= binary) andalso
           is_binary(Request)) orelse
          ((OutputType =:= list) andalso
           is_list(Request))),
         (Type =:= 'cloudi_service_send_async' orelse
          Type =:= 'cloudi_service_send_sync'),
         (Timeout > 0) ->
    {ok, Req,
     State#cowboy_state{
         websocket_state = WebSocketState#websocket_state{
             recv_timeouts = dict:store(TransId,
                 erlang:send_after(Timeout, self(),
                     {'cloudi_service_recv_timeout', Priority, TransId}),
                 RecvTimeouts),
             queued = cloudi_x_pqueue4:in(T, Priority, Queue)}
         }};

websocket_info({Type, Name, _, _, _,
                Timeout, _, TransId, _}, Req,
               #cowboy_state{output_type = OutputType,
                             websocket_protocol = undefined,
                             use_websockets = true} = State)
    when Type =:= 'cloudi_service_send_async';
         Type =:= 'cloudi_service_send_sync' ->
    if
        Timeout > 0 ->
            ?LOG_ERROR("output ~p config ignoring service request to ~s (~s)",
                       [OutputType, Name,
                        cloudi_x_uuid:uuid_to_string(TransId)]);
        true ->
            ok
    end,
    {ok, Req, State};

websocket_info({'cloudi_service_recv_timeout', Priority, TransId}, Req,
               #cowboy_state{websocket_protocol = undefined,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 recv_timeouts = RecvTimeouts,
                                 queued = Queue
                                 } = WebSocketState
                             } = State) ->
    F = fun({_, {_, _, _, _, _, _, _, Id, _}}) -> Id == TransId end,
    {_, NewQueue} = cloudi_x_pqueue4:remove_unique(F, Priority, Queue),
    {ok, Req,
     State#cowboy_state{
         websocket_state = WebSocketState#websocket_state{
             recv_timeouts = dict:erase(TransId, RecvTimeouts),
             queued = NewQueue}
         }};

websocket_info({'cloudi_service_return_async',
                _, _, <<>>, <<>>, _, TransId, _}, Req,
               #cowboy_state{use_websockets = true,
                             websocket_state = #websocket_state{
                                 websocket_connect_trans_id = TransId
                                 } = WebSocketState
                             } = State) ->
    {ok, Req,
     State#cowboy_state{
         websocket_state = WebSocketState#websocket_state{
             websocket_connect_trans_id = undefined}
         }};

websocket_info({'cloudi_service_return_async',
                _, _, ResponseInfo, Response, _, TransId, _}, Req,
               #cowboy_state{output_type = OutputType,
                             websocket_output_type = WebSocketOutputType,
                             websocket_protocol = WebSocketProtocol,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 websocket_connect_trans_id = TransId
                                 } = WebSocketState
                             } = State) ->
    WebSocketResponse = if
        Response == <<>> ->
            % websocket_connect is special because a
            % <<>> response will not be sent back to the websocket
            % since this is the response to an event rather than a
            % request/response pair
            undefined;
        WebSocketProtocol =:= undefined ->
            true = ((((OutputType =:= external) orelse
                      (OutputType =:= internal)) andalso
                     (is_binary(Response) orelse is_list(Response))) orelse
                     ((OutputType =:= binary) andalso
                      is_binary(Response)) orelse
                     ((OutputType =:= list) andalso
                      is_list(Response))),
            {WebSocketOutputType, Response};
        is_function(WebSocketProtocol) ->
            {_, ResponseBinary} = WebSocketProtocol(outgoing, Response),
            {WebSocketOutputType, ResponseBinary}
    end,
    NewState = State#cowboy_state{
                   websocket_state = WebSocketState#websocket_state{
                       websocket_connect_trans_id = undefined}},
    case websocket_terminate_check(ResponseInfo) of
        true ->
            if
                WebSocketResponse =:= undefined ->
                    {reply, close, Req, NewState};
                true ->
                    {reply, [WebSocketResponse, close], Req, NewState}
            end;
        false ->
            if
                WebSocketResponse =:= undefined ->
                    {ok, Req, NewState};
                true ->
                    {reply, WebSocketResponse, Req, NewState}
            end
    end;

websocket_info({websocket_ping, WebSocketPing}, Req,
               #cowboy_state{websocket_ping = WebSocketPingStatus} = State) ->
    if
        WebSocketPingStatus =:= undefined ->
            {shutdown, Req, State};
        WebSocketPingStatus =:= received ->
            erlang:send_after(WebSocketPing, self(),
                              {websocket_ping, WebSocketPing}),
            {reply, {ping, <<>>}, Req,
             State#cowboy_state{websocket_ping = undefined}}
    end;

websocket_info({cowboy_error, shutdown}, Req, State) ->
    % from cloudi_service_http_cowboy:close/1
    {shutdown, Req, State};

websocket_info(Info, Req,
               #cowboy_state{use_websockets = true} = State) ->
    ?LOG_ERROR("Invalid websocket request state: \"~p\"", [Info]),
    {ok, Req, State}.

websocket_terminate(Reason, _Req,
                    #cowboy_state{
                        websocket_disconnect = WebSocketDisconnect} = State) ->
    websocket_disconnect_check(WebSocketDisconnect, Reason, State),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

upgrade_request(Req0) ->
    case cloudi_x_cowboy_req:parse_header(<<"connection">>, Req0) of
        {undefined, _, Req1} ->
            {undefined, Req1};
        {ok, C, Req1} ->
            case lists:member(<<"upgrade">>, C) of
                true ->
                    {ok, [U0 | _],
                     Req2} = cloudi_x_cowboy_req:parse_header(<<"upgrade">>,
                                                              Req1),
                    try erlang:binary_to_existing_atom(U0, utf8) of
                        U1 ->
                            {U1, Req2}
                    catch
                        error:badarg ->
                            % non-atom is ignored and logged
                            {U0, Req2}
                    end;
                false ->
                    {undefined, Req1}
            end
    end.

header_accept_check(Headers, ContentTypesAccepted) ->
    case lists:keyfind(<<"accept">>, 1, Headers) of
        false ->
            true;
        {<<"accept">>, Value} ->
            case binary:match(Value, ContentTypesAccepted) of
                nomatch ->
                    false;
                _ ->
                    true
            end
    end.

header_content_type(Headers) ->
    case lists:keyfind(<<"content-type">>, 1, Headers) of
        false ->
            <<>>;
        {<<"content-type">>, Value} ->
            hd(binary:split(Value, <<";">>))
    end.

% format for external services, http headers passed as key-value pairs
headers_external_incoming(L) ->
    erlang:iolist_to_binary(headers_external_incoming_text(L)).

headers_external_incoming_text([] = L) ->
    L;
headers_external_incoming_text([{K, V} | L]) when is_binary(K) ->
    [[K, 0, V, 0] | headers_external_incoming_text(L)].

headers_external_outgoing(<<>>) ->
    [];
headers_external_outgoing([] = ResponseInfo) ->
    ResponseInfo;
headers_external_outgoing([{_, _} | _] = ResponseInfo) ->
    % assumes key/value within tuple are iodata()
    % (cowboy can error if this is not true)
    ResponseInfo;
headers_external_outgoing(ResponseInfo)
    when is_binary(ResponseInfo) ->
    headers_external_outgoing_text(binary:split(ResponseInfo, <<0>>, [global])).

headers_external_outgoing_text([<<>>]) ->
    [];
headers_external_outgoing_text([K, V | L]) ->
    [{K, V} | headers_external_outgoing_text(L)].

get_query_string_external([]) ->
    <<>>;
get_query_string_external(QsVals) ->
    erlang:iolist_to_binary(get_query_string_external_text(QsVals)).

get_query_string_external_text([] = L) ->
    L;
get_query_string_external_text([{K, V} | L]) ->
    if
        V =:= true ->
            [[K, 0, <<"true">>, 0] | get_query_string_external_text(L)];
        is_binary(V) ->
            [[K, 0, V, 0] | get_query_string_external_text(L)]
    end.

request_time_start() ->
    cloudi_x_uuid:get_v1_time(os).

request_time_end_success(HttpCode, Method, NameIncoming, NameOutgoing,
                         RequestStartMicroSec) ->
    ?LOG_TRACE("~w ~s ~s (to ~s) ~p ms",
               [HttpCode, Method, NameIncoming, NameOutgoing,
                (cloudi_x_uuid:get_v1_time(os) -
                 RequestStartMicroSec) / 1000.0]).

request_time_end_error(HttpCode, Method, NameIncoming,
                       RequestStartMicroSec, Reason) ->
    ?LOG_WARN("~w ~s ~s ~p ms: ~p",
              [HttpCode, Method, NameIncoming,
               (cloudi_x_uuid:get_v1_time(os) -
                RequestStartMicroSec) / 1000.0, Reason]).

websocket_time_start() ->
    cloudi_x_uuid:get_v1_time(os).

websocket_time_end_success(NameIncoming, NameOutgoing,
                           RequestStartMicroSec) ->
    ?LOG_TRACE("~s (to ~s) ~p ms",
               [NameIncoming, NameOutgoing,
                (cloudi_x_uuid:get_v1_time(os) -
                 RequestStartMicroSec) / 1000.0]).

websocket_time_end_error(NameIncoming,
                         RequestStartMicroSec, Reason) ->
    ?LOG_WARN("~s ~p ms: ~p",
              [NameIncoming,
               (cloudi_x_uuid:get_v1_time(os) -
                RequestStartMicroSec) / 1000.0, Reason]).

websocket_request_end(Name, NewTimeout, OldTimeout) ->
    ?LOG_TRACE("~s ~p ms", [Name, OldTimeout - NewTimeout]).

handle_request(Name, Headers, 'normal', Req,
               #cowboy_state{
                   timeout_body = TimeoutBody,
                   length_body_read = LengthBodyRead,
                   length_body_chunk = LengthBodyChunk} = State) ->
    BodyOpts = [{length, LengthBodyChunk},
                {read_length, LengthBodyRead},
                {read_timeout, TimeoutBody}],
    {ok, Body, NextReq} = handle_request_body(Req, BodyOpts),
    handle_request(Name, Headers, Body, NextReq, State);
handle_request(Name, Headers, 'application_zip', Req,
               #cowboy_state{
                   timeout_body = TimeoutBody,
                   length_body_read = LengthBodyRead,
                   length_body_chunk = LengthBodyChunk} = State) ->
    BodyOpts = [{length, LengthBodyChunk},
                {read_length, LengthBodyRead},
                {read_timeout, TimeoutBody}],
    {ok, Body, NextReq} = handle_request_body(Req, BodyOpts),
    handle_request(Name, Headers, zlib:unzip(Body), NextReq, State);
handle_request(Name, Headers, 'multipart', Req,
               #cowboy_state{
                   dispatcher = Dispatcher,
                   context = Context,
                   timeout_part_header = TimeoutPartHeader,
                   length_part_header_read = LengthPartHeaderRead,
                   length_part_header_chunk = LengthPartHeaderChunk,
                   timeout_part_body = TimeoutPartBody,
                   length_part_body_read = LengthPartBodyRead,
                   length_part_body_chunk = LengthPartBodyChunk,
                   parts_destination_lock = PartsDestinationLock} = State) ->
    DestinationLock = if
        PartsDestinationLock =:= true ->
            cloudi_service:get_pid(Dispatcher, Name,
                                   cloudi:timeout_async(Context));
        PartsDestinationLock =:= false ->
            {ok, undefined}
    end,
    case DestinationLock of
        {ok, Destination} ->
            Self = self(),
            PartHeaderOpts = [{length, LengthPartHeaderChunk},
                              {read_length, LengthPartHeaderRead},
                              {read_timeout, TimeoutPartHeader}],
            PartBodyOpts = [{length, LengthPartBodyChunk},
                            {read_length, LengthPartBodyRead},
                            {read_timeout, TimeoutPartBody}],
            MultipartId = erlang:list_to_binary(erlang:pid_to_list(Self)),
            handle_request_multipart(Name, lists:keysort(1, Headers),
                                     Destination, Self,
                                     PartHeaderOpts, PartBodyOpts,
                                     MultipartId, Req, State);
        {error, timeout} ->
            {{cowboy_error, timeout}, Req, State}
    end;
handle_request(Name, Headers, Body, Req,
               #cowboy_state{
                   dispatcher = Dispatcher,
                   context = Context,
                   output_type = OutputType} = State) ->
    RequestInfo = if
        (OutputType =:= external) orelse (OutputType =:= binary) ->
            headers_external_incoming(Headers);
        (OutputType =:= internal) orelse (OutputType =:= list) ->
            Headers
    end,
    Request = if
        (OutputType =:= external) orelse (OutputType =:= internal) orelse
        (OutputType =:= binary) ->
            Body;
        (OutputType =:= list) ->
            erlang:binary_to_list(Body)
    end,
    case send_sync_minimal(Dispatcher, Context,
                           Name, RequestInfo, Request, self()) of
        {{ok, ResponseInfo, Response}, NewContext} ->
            HeadersOutgoing = headers_external_outgoing(ResponseInfo),
            {{cowboy_response, HeadersOutgoing, Response},
             Req, State#cowboy_state{context = NewContext}};
        {{error, timeout}, NewContext} ->
            {{cowboy_error, timeout},
             Req, State#cowboy_state{context = NewContext}}
    end.

handle_request_body(Req, BodyOpts) ->
    case cloudi_x_cowboy_req:body(Req, BodyOpts) of
        {ok, _, _} = Success ->
            Success;
        {error, Reason} ->
            erlang:exit({cowboy_error, Reason})
    end.

handle_request_multipart(Name, Headers,
                         Destination, Self, PartHeaderOpts, PartBodyOpts,
                         MultipartId, Req0, State) ->
    case cloudi_x_cowboy_req:part(Req0, PartHeaderOpts) of
        {ok, HeadersPart, ReqN} ->
            handle_request_multipart([], 0, Name, Headers, HeadersPart,
                                     Destination, Self,
                                     PartHeaderOpts, PartBodyOpts,
                                     MultipartId, ReqN, State);
        {done, ReqN} ->
            {{cowboy_error, multipart_empty}, ReqN, State}
    end.

handle_request_multipart(TransIdList, I, Name, Headers, HeadersPart,
                         Destination, Self, PartHeaderOpts, PartBodyOpts,
                         MultipartId, Req0, State) ->
    case handle_request_multipart_send([], I, Name, Headers, HeadersPart,
                                       Destination, Self,
                                       PartHeaderOpts, PartBodyOpts,
                                       MultipartId, Req0, State) of
        {{ok, TransId}, undefined,
         ReqN, NewState} ->
            handle_request_multipart_receive(lists:reverse([TransId |
                                                            TransIdList]),
                                             ReqN, NewState);
        {{ok, TransId}, HeadersPartNext,
         ReqN, NewState} ->
            handle_request_multipart([TransId | TransIdList], I + 1,
                                     Name, Headers, HeadersPartNext,
                                     Destination, Self,
                                     PartHeaderOpts, PartBodyOpts,
                                     MultipartId, ReqN, NewState)
    end.

headers_merge(HeadersPart, Headers) ->
    lists:keymerge(1, lists:keysort(1, HeadersPart), Headers).

handle_request_multipart_send(PartBodyList, I, Name, Headers, HeadersPart0,
                              Destination, Self, PartHeaderOpts, PartBodyOpts,
                              MultipartId, Req0,
                              #cowboy_state{
                                  dispatcher = Dispatcher,
                                  context = Context,
                                  output_type = OutputType} = State) ->
    case cloudi_x_cowboy_req:part_body(Req0, PartBodyOpts) of
        {ok, PartBodyChunkLast, Req1} ->
            PartBody = if
                PartBodyList == [] ->
                    PartBodyChunkLast;
                true ->
                    erlang:iolist_to_binary(lists:reverse([PartBodyChunkLast |
                                                           PartBodyList]))
            end,
            {HeadersPartNextN,
             ReqN} = case cloudi_x_cowboy_req:part(Req1, PartHeaderOpts) of
                {ok, HeadersPartNext0, Req2} ->
                    {HeadersPartNext0, Req2};
                {done, Req2} ->
                    {undefined, Req2}
            end,
            % each multipart part becomes a separate service request
            % however, the non-standard HTTP header request data provides
            % information to handle the sequence concurrently
            % (use multipart_destination_lock (defaults to true) if you need
            %  the same destination used for each part)
            HeadersPart1 = headers_merge(HeadersPart0, Headers),
            HeadersPartN = if
                HeadersPartNextN =:= undefined ->
                    [% socket pid as a string
                     {<<"x-multipart-id">>, MultipartId},
                     % 0-based index
                     {<<"x-multipart-index">>, erlang:integer_to_binary(I)},
                     % yes, this is the last part
                     {<<"x-multipart-last">>, <<"true">>} |
                     HeadersPart1];
                true ->
                    [% socket pid as a string
                     {<<"x-multipart-id">>, MultipartId},
                     % 0-based index
                     {<<"x-multipart-index">>, erlang:integer_to_binary(I)} |
                     HeadersPart1]
            end,
            RequestInfo = if
                (OutputType =:= external) orelse (OutputType =:= binary) ->
                    headers_external_incoming(HeadersPartN);
                (OutputType =:= internal) orelse (OutputType =:= list) ->
                    HeadersPartN
            end,
            Request = if
                (OutputType =:= external) orelse
                (OutputType =:= internal) orelse
                (OutputType =:= binary) ->
                    PartBody;
                (OutputType =:= list) ->
                    erlang:binary_to_list(PartBody)
            end,
            {SendResult,
             NewContext} = send_async_minimal(Dispatcher, Context, Name,
                                              RequestInfo, Request,
                                              Destination, Self),
            {SendResult, HeadersPartNextN,
             ReqN, State#cowboy_state{context = NewContext}};
        {more, PartBodyChunk, Req1} ->
            handle_request_multipart_send([PartBodyChunk | PartBodyList],
                                          I, Name, Headers, HeadersPart0,
                                          Destination, Self,
                                          PartHeaderOpts, PartBodyOpts,
                                          MultipartId, Req1, State)
    end.
    
handle_request_multipart_receive_results([], _, [Error | _],
                                         Req, State) ->
    {Error, Req, State};
handle_request_multipart_receive_results([], [Success | _], _,
                                         Req, State) ->
    {Success, Req, State};
handle_request_multipart_receive_results([{ResponseInfo, Response, _} |
                                          ResponseList],
                                         SuccessList, ErrorList,
                                         Req, State) ->
    HeadersOutgoing = headers_external_outgoing(ResponseInfo),
    Status = case lists:keyfind(<<"status">>, 1, HeadersOutgoing) of
        {_, V} ->
            erlang:binary_to_integer(hd(binary:split(V, <<" ">>)));
        false ->
            200
    end,
    if
        (Status >= 200) andalso (Status =< 299) ->
            handle_request_multipart_receive_results(ResponseList,
                                                     [{cowboy_response,
                                                       HeadersOutgoing,
                                                       Response} |
                                                      SuccessList],
                                                     ErrorList,
                                                     Req, State);
        true ->
            handle_request_multipart_receive_results(ResponseList,
                                                     SuccessList,
                                                     [{cowboy_response,
                                                       HeadersOutgoing,
                                                       Response} |
                                                      ErrorList],
                                                     Req, State)
    end.

handle_request_multipart_receive([_ | _] = TransIdList, Req,
                                 #cowboy_state{context = Context} = State) ->
    case recv_asyncs_minimal(Context, TransIdList) of
        {{ok, ResponseList}, NewContext} ->
            handle_request_multipart_receive_results(ResponseList,
                                                     [], [], Req,
                                                     State#cowboy_state{
                                                         context = NewContext});
        {{error, timeout}, NewContext} ->
            {{cowboy_error, timeout}, Req,
             State#cowboy_state{context = NewContext}}
    end.

handle_response(NameIncoming, HeadersOutgoing0, Response,
                ReqN, OutputType, ContentTypeForced) ->
    ResponseBinary = if
        (((OutputType =:= external) orelse
          (OutputType =:= internal)) andalso
         (is_binary(Response) orelse is_list(Response))) orelse
        ((OutputType =:= binary) andalso
         is_binary(Response)) ->
            Response;
        ((OutputType =:= list) andalso
         is_list(Response)) ->
            erlang:iolist_to_binary(Response)
    end,
    {HttpCode, HeadersOutgoingN} = case lists:keytake(<<"status">>, 1,
                                                      HeadersOutgoing0) of
        false ->
            {200, HeadersOutgoing0};
        {value, {_, Status}, HeadersOutgoing1}
            when is_binary(Status) ->
            {erlang:binary_to_integer(hd(binary:split(Status, <<" ">>))),
             HeadersOutgoing1}
    end,
    ResponseHeadersOutgoing = if
        HeadersOutgoingN =/= [] ->
            HeadersOutgoingN;
        ContentTypeForced =/= undefined ->
            [{<<"content-type">>, ContentTypeForced}];
        true ->
            Extension = filename:extension(NameIncoming),
            if
                Extension == [] ->
                    [{<<"content-type">>, <<"text/html">>}];
                true ->
                    case cloudi_response_info:
                         lookup_content_type(binary, Extension) of
                        error ->
                            [{<<"content-disposition">>,
                              ["attachment; filename=\"",
                               filename:basename(NameIncoming), "\""]},
                             {<<"content-type">>,
                              <<"application/octet-stream">>}];
                        {ok, {request, ContentType}} ->
                            [{<<"content-type">>, ContentType}];
                        {ok, {attachment, ContentType}} ->
                            [{<<"content-disposition">>,
                              ["attachment; filename=\"",
                               filename:basename(NameIncoming), "\""]},
                             {<<"content-type">>, ContentType}]
                    end
            end
    end,
    {ok, Req} = cloudi_x_cowboy_req:reply(HttpCode,
                                          ResponseHeadersOutgoing,
                                          ResponseBinary,
                                          ReqN),
    {HttpCode, Req}.

service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, Client, Req0)
    when UseClientIpPrefix =:= true, UseHostPrefix =:= true ->
    {HostRaw, Req1} = cloudi_x_cowboy_req:host(Req0),
    {service_name_incoming_merge(Client, HostRaw, PathRaw), Req1};
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, Client, Req0)
    when UseClientIpPrefix =:= true, UseHostPrefix =:= false ->
    {service_name_incoming_merge(Client, undefined, PathRaw), Req0};
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, _Client, Req0)
    when UseClientIpPrefix =:= false, UseHostPrefix =:= true ->
    {HostRaw, Req1} = cloudi_x_cowboy_req:host(Req0),
    {service_name_incoming_merge(undefined, HostRaw, PathRaw), Req1};
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, _Client, Req0)
    when UseClientIpPrefix =:= false, UseHostPrefix =:= false ->
    {service_name_incoming_merge(undefined, undefined, PathRaw), Req0}.

service_name_incoming_merge(undefined, undefined, PathRaw) ->
    erlang:binary_to_list(PathRaw);
service_name_incoming_merge(undefined, HostRaw, PathRaw) ->
    erlang:binary_to_list(<<HostRaw/binary, PathRaw/binary>>);
service_name_incoming_merge({ClientIpAddr, _ClientPort}, undefined, PathRaw) ->
    cloudi_ip_address:to_string(ClientIpAddr) ++
    erlang:binary_to_list(PathRaw);
service_name_incoming_merge({ClientIpAddr, _ClientPort}, HostRaw, PathRaw) ->
    cloudi_ip_address:to_string(ClientIpAddr) ++
    erlang:binary_to_list(<<$/, HostRaw/binary, PathRaw/binary>>).

websocket_terminate_check(<<>>) ->
    false;
websocket_terminate_check([]) ->
    false;
websocket_terminate_check(ResponseInfo) ->
    HeadersOutgoing = headers_external_outgoing(ResponseInfo),
    case lists:keyfind(<<"connection">>, 1, HeadersOutgoing) of
        {<<"connection">>, <<"close">>} ->
            true;
        {<<"connection">>, _} ->
            false;
        false ->
            false
    end.

websocket_connect_request(OutputType)
    when OutputType =:= external; OutputType =:= internal;
         OutputType =:= binary ->
    <<"CONNECT">>;
websocket_connect_request(OutputType)
    when OutputType =:= list ->
    "CONNECT".

websocket_connect_check(undefined, State) ->
    State;
websocket_connect_check({async, WebSocketConnectName},
                        #cowboy_state{
                            dispatcher = Dispatcher,
                            context = Context,
                            output_type = OutputType,
                            websocket_state = #websocket_state{
                                request_info = RequestInfo} = WebSocketState
                            } = State) ->
    Request = websocket_connect_request(OutputType),
    {{ok, TransId},
     NewContext} = send_async_minimal(Dispatcher, Context,
                                      WebSocketConnectName,
                                      RequestInfo, Request, self()),
    State#cowboy_state{
        context = NewContext,
        websocket_state = WebSocketState#websocket_state{
            websocket_connect_trans_id = TransId}};
websocket_connect_check({sync, WebSocketConnectName},
                        #cowboy_state{
                            dispatcher = Dispatcher,
                            context = Context,
                            output_type = OutputType,
                            websocket_state = #websocket_state{
                                request_info = RequestInfo} = WebSocketState
                            } = State) ->
    Self = self(),
    Timeout = cloudi:timeout_sync(Context),
    {TransId, NextContext} = cloudi:trans_id(Context),
    case send_sync_minimal(Dispatcher, NextContext,
                           WebSocketConnectName,
                           RequestInfo,
                           websocket_connect_request(OutputType), Self) of
        {{ok, ResponseInfo, Response}, NewContext} ->
            % must provide the response after the websocket_init is done
            Self ! {'cloudi_service_return_async',
                    WebSocketConnectName,
                    WebSocketConnectName,
                    ResponseInfo, Response,
                    Timeout, TransId, Self},
            State#cowboy_state{
                context = NewContext,
                websocket_state = WebSocketState#websocket_state{
                    websocket_connect_trans_id = TransId}};
        {{error, timeout}, NewContext} ->
            State#cowboy_state{
                context = NewContext}
    end.

websocket_disconnect_request(OutputType)
    when OutputType =:= external; OutputType =:= internal;
         OutputType =:= binary ->
    <<"DISCONNECT">>;
websocket_disconnect_request(OutputType)
    when OutputType =:= list ->
    "DISCONNECT".

websocket_disconnect_request_info_reason({remote, CloseCode, CloseBinary})
    when is_integer(CloseCode) ->
    [<<"remote,">>,
     erlang:integer_to_binary(CloseCode), <<",">>,
     CloseBinary];
websocket_disconnect_request_info_reason({ReasonType, ReasonDescription}) ->
    [erlang:atom_to_binary(ReasonType, utf8), <<",">>,
     erlang:atom_to_binary(ReasonDescription, utf8)].

websocket_disconnect_request_info(Reason, RequestInfo, OutputType)
    when OutputType =:= external; OutputType =:= binary ->
    erlang:iolist_to_binary([<<"disconnection">>, 0,
                             websocket_disconnect_request_info_reason(Reason),
                             0, RequestInfo]);
websocket_disconnect_request_info(Reason, RequestInfo, OutputType)
    when OutputType =:= internal; OutputType =:= list ->
    [{<<"disconnection">>,
      websocket_disconnect_request_info_reason(Reason)} | RequestInfo].

websocket_disconnect_check(undefined, _, _) ->
    ok;
websocket_disconnect_check({async, WebSocketDisconnectName},
                           Reason,
                           #cowboy_state{
                               dispatcher = Dispatcher,
                               context = Context,
                               output_type = OutputType,
                               websocket_state = #websocket_state{
                                   request_info = RequestInfo}}) ->
    send_async_minimal(Dispatcher, Context,
                       WebSocketDisconnectName,
                       websocket_disconnect_request_info(Reason,
                                                         RequestInfo,
                                                         OutputType),
                       websocket_disconnect_request(OutputType), self()),
    ok;
websocket_disconnect_check({sync, WebSocketDisconnectName},
                           Reason,
                           #cowboy_state{
                               dispatcher = Dispatcher,
                               context = Context,
                               output_type = OutputType,
                               websocket_state = #websocket_state{
                                   request_info = RequestInfo}}) ->
    send_sync_minimal(Dispatcher, Context,
                      WebSocketDisconnectName,
                      websocket_disconnect_request_info(Reason,
                                                        RequestInfo,
                                                        OutputType),
                      websocket_disconnect_request(OutputType), self()),
    ok.

websocket_subscriptions([], _, _) ->
    ok;
websocket_subscriptions([F | Functions], Parameters, Scope) ->
    case F(Parameters) of
        {ok, NameWebSocket} ->
            ok = cloudi_x_cpg:join(Scope, NameWebSocket, self(), infinity);
        {error, _} ->
            ok
    end,
    websocket_subscriptions(Functions, Parameters, Scope).

websocket_handle_incoming_request(Dispatcher, Context, NameOutgoing,
                                  RequestInfo, Request, ResponseF,
                                  WebSocketRequestType, Req,
                                  NameIncoming, State) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun websocket_time_start/0, []),
    case send_sync_minimal(Dispatcher, Context,
                           NameOutgoing, RequestInfo, Request, self()) of
        {{ok, ResponseInfo, Response}, NewContext} ->
            ?LOG_TRACE_APPLY(fun websocket_time_end_success/3,
                             [NameIncoming, NameOutgoing,
                              RequestStartMicroSec]),
            case websocket_terminate_check(ResponseInfo) of
                true when Response == <<>> ->
                    {reply, close, Req,
                     State#cowboy_state{context = NewContext}};
                true ->
                    {reply, [{WebSocketRequestType,
                              ResponseF(Response)}, close], Req,
                     State#cowboy_state{context = NewContext}};
                false ->
                    {reply, {WebSocketRequestType,
                             ResponseF(Response)}, Req,
                     State#cowboy_state{context = NewContext}}
            end;
        {{error, timeout}, NewContext} ->
            ?LOG_WARN_APPLY(fun websocket_time_end_error/3,
                            [NameIncoming,
                             RequestStartMicroSec, timeout]),
            {reply, {WebSocketRequestType, <<>>}, Req,
             State#cowboy_state{context = NewContext}}
    end.

websocket_handle_outgoing_response({SendType,
                                    Name, Pattern, _, _,
                                    OldTimeout, _, TransId, Source},
                                   ResponseTimer,
                                   ResponseInfo, Response) ->
    Timeout = case erlang:cancel_timer(ResponseTimer) of
        false ->
            0;
        V ->
            V
    end,
    ReturnType = if
        SendType =:= 'cloudi_service_send_async' ->
            'cloudi_service_return_async';
        SendType =:= 'cloudi_service_send_sync' ->
            'cloudi_service_return_sync'
    end,
    Source ! {ReturnType,
              Name, Pattern, ResponseInfo, Response,
              Timeout, TransId, Source},
    ?LOG_TRACE_APPLY(fun websocket_request_end/3,
                     [Name, Timeout, OldTimeout]).

websocket_process_queue(Req,
                        #cowboy_state{websocket_state =
                            #websocket_state{
                                response_pending = false,
                                recv_timeouts = RecvTimeouts,
                                queued = Queue} = WebSocketState} = State) ->
    case cloudi_x_pqueue4:out(Queue) of
        {empty, NewQueue} ->
            {ok, Req,
             State#cowboy_state{websocket_state =
                 WebSocketState#websocket_state{queued = NewQueue}}};
        {{value, {Type, Name, Pattern, RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Timeout = case erlang:cancel_timer(dict:fetch(TransId,
                                                          RecvTimeouts)) of
                false ->
                    0;
                V ->
                    V
            end,
            websocket_info({Type, Name, Pattern, RequestInfo, Request,
                            Timeout, Priority, TransId, Pid}, Req,
                           State#cowboy_state{websocket_state =
                               WebSocketState#websocket_state{
                                   recv_timeouts = dict:erase(TransId,
                                                              RecvTimeouts),
                                   queued = NewQueue}})
    end.

