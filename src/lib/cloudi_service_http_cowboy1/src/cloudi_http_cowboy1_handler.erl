%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cowboy CloudI HTTP Handler==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2021 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2012-2021 Michael Truog
%%% @version 2.0.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_http_cowboy1_handler).
-author('mjtruog at protonmail dot com').

%-behaviour(cloudi_x_cowboy1_http_handler).
%-behaviour(cloudi_x_cowboy1_websocket_handler).

%% external interface

%% cloudi_x_cowboy1_http_handler callbacks
-export([init/3,
         handle/2,
         info/3,
         terminate/3]).

%% cloudi_x_cowboy1_websocket_handler callbacks
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").
-include("cloudi_http_cowboy1_handler.hrl").

-record(websocket_state,
    {
        % for service requests entering CloudI
        websocket_connect_trans_id = undefined
            :: undefined | cloudi_service:trans_id(),
        name_incoming :: string(),
        name_outgoing :: cloudi_service:service_name(),
        request_info :: list({binary(), binary()}) | binary(),
        % for a service request exiting CloudI
        response_pending = false :: boolean(),
        response_timer = undefined :: undefined | reference(),
        request_pending = undefined
            :: undefined | cloudi:message_service_request(),
        response_lookup
            :: undefined |
               #{any() := {cloudi:message_service_request(), reference()}},
        recv_timeouts
            :: undefined | #{cloudi:trans_id() := reference()},
        queued
            :: undefined |
               cloudi_x_pqueue4:cloudi_x_pqueue4(
                   cloudi:message_service_request())
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_x_cowboy1_http_handler
%%%------------------------------------------------------------------------

init(_Transport, Req0,
     #cowboy1_state{use_websockets = UseWebSockets} = State)
    when UseWebSockets =:= true; UseWebSockets =:= exclusively ->
    case upgrade_request(Req0) of
        {websocket, Req1} ->
            {upgrade, protocol, cloudi_x_cowboy1_websocket, Req1,
             State#cowboy1_state{use_websockets = true}};
        {undefined, Req1} ->
            if
                UseWebSockets =:= exclusively ->
                    {shutdown, Req1, State};
                true ->
                    {ok, Req1, State}
            end;
        {Upgrade, Req1} ->
            ?LOG_ERROR("Unknown protocol: ~w", [Upgrade]),
            {shutdown, Req1, State}
    end;
init(_Transport, Req,
     #cowboy1_state{use_websockets = false} = State) ->
    {ok, Req, State}.

handle(Req0,
       #cowboy1_state{
           output_type = OutputType,
           content_type_forced = ContentTypeForced,
           content_types_accepted = ContentTypesAccepted,
           content_security_policy = ContentSecurityPolicy,
           content_security_policy_report = ContentSecurityPolicyReport,
           set_x_forwarded_for = SetXForwardedFor,
           set_x_xss_protection = SetXXSSProtection,
           set_x_content_type_options = SetXContentTypeOptions,
           status_code_timeout = StatusCodeTimeout,
           query_get_format = QueryGetFormat,
           use_host_prefix = UseHostPrefix,
           use_client_ip_prefix = UseClientIpPrefix,
           use_x_method_override = UseXMethodOverride,
           use_method_suffix = UseMethodSuffix} = State) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun request_time_start/0, []),
    {MethodHTTP, Req1} = cloudi_x_cowboy1_req:method(Req0),
    {HeadersIncoming0, Req2} = cloudi_x_cowboy1_req:headers(Req1),
    Method = if
        UseXMethodOverride =:= true ->
            case lists:keyfind(<<"x-http-method-override">>, 1,
                               HeadersIncoming0) of
                {_, MethodOverride} ->
                    MethodOverride;
                false ->
                    MethodHTTP
            end;
        UseXMethodOverride =:= false ->
            MethodHTTP
    end,
    {QS, Req4} = if
        MethodHTTP =:= <<"GET">> ->
            if
                QueryGetFormat =:= text_pairs ->
                    {QSVals, Req3} = cloudi_x_cowboy1_req:qs_vals(Req2),
                    if
                        (OutputType =:= external) orelse
                        (OutputType =:= binary) orelse (OutputType =:= list) ->
                            {get_query_string_external(QSVals), Req3};
                        OutputType =:= internal ->
                            % cloudi_key_value format
                            {QSVals, Req3}
                    end;
                QueryGetFormat =:= raw ->
                    cloudi_x_cowboy1_req:qs(Req2)
            end;
        true ->
            % query strings only handled for GET methods
            {undefined, Req2}
    end,
    {PathRaw, Req5} = cloudi_x_cowboy1_req:path(Req4),
    {{ClientIpAddr, ClientPort} = Client,
     Req6} = cloudi_x_cowboy1_req:peer(Req5),
    {NameIncoming, ReqN} = service_name_incoming(UseClientIpPrefix,
                                                 UseHostPrefix,
                                                 PathRaw,
                                                 Client,
                                                 Req6),
    RequestAccepted = if
        ContentTypesAccepted =:= undefined ->
            true;
        true ->
            header_accept_check(HeadersIncoming0, ContentTypesAccepted)
    end,
    if
        RequestAccepted =:= false ->
            HttpCode = 406,
            {ok, Req} = cloudi_x_cowboy1_req:reply(HttpCode,
                                                  ReqN),
            ?LOG_WARN_APPLY(fun request_time_end_error/6,
                            [HttpCode, MethodHTTP,
                             NameIncoming, undefined,
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
                        cloudi_string:lowercase(erlang:binary_to_list(Method))]
            end,
            SourceAddress = cloudi_ip_address:to_binary(ClientIpAddr),
            SourcePort = erlang:integer_to_binary(ClientPort),
            HeadersIncoming1 = [{<<"source-address">>, SourceAddress},
                                {<<"source-port">>, SourcePort},
                                {<<"url-path">>, PathRaw} |
                                HeadersIncoming0],
            HeadersIncomingN = if
                SetXForwardedFor =:= true ->
                    case lists:keyfind(<<"x-forwarded-for">>, 1,
                                       HeadersIncoming0) of
                        false ->
                            [{<<"x-forwarded-for">>, SourceAddress} |
                             HeadersIncoming1];
                        _ ->
                            HeadersIncoming1
                            
                    end;
                SetXForwardedFor =:= false ->
                    HeadersIncoming1
            end,
            Body = if
                MethodHTTP =:= <<"GET">> ->
                    % only the query string is provided as the
                    % body of a GET request passed within the Request parameter
                    % of a CloudI service request, which prevents misuse of GET
                    QS;
                (MethodHTTP =:= <<"HEAD">>) orelse
                (MethodHTTP =:= <<"OPTIONS">>) orelse
                (MethodHTTP =:= <<"TRACE">>) orelse
                (MethodHTTP =:= <<"CONNECT">>) ->
                    <<>>;
                true ->
                    % POST, PUT, DELETE or anything else
                    case header_content_type(HeadersIncoming0) of
                        <<"application/zip">> ->
                            'application_zip';
                        <<"multipart/", _/binary>> ->
                            'multipart';
                        _ ->
                            'normal'
                    end
            end,
            case handle_request(NameOutgoing, HeadersIncomingN,
                                Body, ReqN, State) of
                {{cowboy1_response, HeadersOutgoing, Response},
                 ReqN0, NewState} ->
                    {HttpCode,
                     Req} = handle_response(NameIncoming, HeadersOutgoing,
                                            Response, ReqN0, OutputType,
                                            ContentTypeForced,
                                            SetXContentTypeOptions,
                                            SetXXSSProtection,
                                            ContentSecurityPolicy,
                                            ContentSecurityPolicyReport),
                    ?LOG_TRACE_APPLY(fun request_time_end_success/5,
                                     [HttpCode, MethodHTTP,
                                      NameIncoming, NameOutgoing,
                                      RequestStartMicroSec]),
                    {ok, Req, NewState};
                {{cowboy1_error, timeout},
                 ReqN0, NewState} ->
                    HttpCode = StatusCodeTimeout,
                    {ok, Req} = if
                        HttpCode =:= 405 ->
                            % currently not providing a list of valid methods
                            % (a different HTTP status code is a better
                            %  choice, since this service name may not exist)
                            HeadersOutgoing = [{<<"allow">>, <<"">>}],
                            cloudi_x_cowboy1_req:reply(HttpCode,
                                                      HeadersOutgoing,
                                                      ReqN0);
                        true ->
                            cloudi_x_cowboy1_req:reply(HttpCode,
                                                      ReqN0)
                    end,
                    ?LOG_WARN_APPLY(fun request_time_end_error/6,
                                    [HttpCode, MethodHTTP,
                                     NameIncoming, NameOutgoing,
                                     RequestStartMicroSec, timeout]),
                    {ok, Req, NewState};
                {{cowboy1_error, Reason},
                 ReqN0, NewState} ->
                    HttpCode = 500,
                    {ok, Req} = cloudi_x_cowboy1_req:reply(HttpCode,
                                                          ReqN0),
                    ?LOG_WARN_APPLY(fun request_time_end_error/6,
                                    [HttpCode, MethodHTTP,
                                     NameIncoming, NameOutgoing,
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
               #cowboy1_state{scope = Scope,
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
    {Method, Req1} = cloudi_x_cowboy1_req:method(Req0),
    {HeadersIncoming0, Req2} = cloudi_x_cowboy1_req:headers(Req1),
    {PathRaw, Req3} = cloudi_x_cowboy1_req:path(Req2),
    {{ClientIpAddr, ClientPort} = Client,
     Req4} = cloudi_x_cowboy1_req:peer(Req3),
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
    SourceAddress = cloudi_ip_address:to_binary(ClientIpAddr),
    SourcePort = erlang:integer_to_binary(ClientPort),
    HeadersIncoming2 = [{<<"source-address">>, SourceAddress},
                        {<<"source-port">>, SourcePort},
                        {<<"url-path">>, PathRaw} | HeadersIncoming1],
    HeadersIncomingN = if
        SetXForwardedFor =:= true ->
            case lists:keyfind(<<"x-forwarded-for">>, 1, HeadersIncoming0) of
                false ->
                    [{<<"x-forwarded-for">>, SourceAddress} | HeadersIncoming2];
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
            #{};
        true ->
            undefined
    end,
    RecvTimeouts = if
        WebSocketProtocol =:= undefined ->
            #{};
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
                                cloudi_service_http_cowboy1:close(OldConnection),
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
                    % match websocket_subscriptions to determine if
                    % more subscriptions should occur, possibly
                    % using parameters in a pattern template
                    % for the subscription
                    case cloudi_x_trie:find_match2(PathRawStr,
                                                   WebSocketSubscriptions) of
                        error ->
                            ok;
                        {ok, Pattern, Functions} ->
                            Parameters = cloudi_service_name:
                                         parse(PathRawStr, Pattern),
                            websocket_subscriptions(Functions, Parameters,
                                                    Scope)
                    end
            end;
        SubscribeWebSocket =:= false ->
            ok
    end,
    {ok, ReqN,
     websocket_connect_check(WebSocketConnect,
                             State#cowboy1_state{
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
    % cowboy1 automatically responds with pong
    {ok, Req, State};

websocket_handle({pong, _Payload}, Req, State) ->
    {ok, Req, State#cowboy1_state{websocket_ping = received}};

websocket_handle({WebSocketResponseType, ResponseBinary}, Req,
                 #cowboy1_state{output_type = OutputType,
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
                            State#cowboy1_state{websocket_state =
                                WebSocketState#websocket_state{
                                    response_pending = false,
                                    response_timer = undefined,
                                    request_pending = undefined}
                                });

websocket_handle({WebSocketRequestType, RequestBinary}, Req,
                 #cowboy1_state{dispatcher = Dispatcher,
                               timeout_sync = TimeoutSync,
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
    websocket_handle_incoming_request(Dispatcher, NameOutgoing,
                                      RequestInfo, Request,
                                      TimeoutSync, ResponseBinaryF,
                                      WebSocketRequestType, Req,
                                      NameIncoming, State);

websocket_handle({WebSocketRequestType, RequestBinary}, Req,
                 #cowboy1_state{dispatcher = Dispatcher,
                               timeout_sync = TimeoutSync,
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
            case maps:find(ID, ResponseLookup) of
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
            websocket_handle_incoming_request(Dispatcher, NameOutgoing,
                                              Info, Value,
                                              TimeoutSync, ResponseF,
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
             State#cowboy1_state{websocket_state =
                 WebSocketState#websocket_state{
                     response_lookup = maps:remove(LookupID, ResponseLookup)}
                 }}
    end.

websocket_info({response_timeout, ID}, Req,
               #cowboy1_state{use_websockets = true,
                             websocket_state = #websocket_state{
                                 response_pending = false,
                                 response_lookup = ResponseLookup
                                 } = WebSocketState
                             } = State) ->
    {ok, Req,
     State#cowboy1_state{websocket_state =
         WebSocketState#websocket_state{
             response_lookup = maps:remove(ID, ResponseLookup)}
         }};

websocket_info(response_timeout, Req,
               #cowboy1_state{websocket_protocol = undefined,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 response_pending = true} = WebSocketState
                             } = State) ->
    websocket_process_queue(Req,
                            State#cowboy1_state{websocket_state =
                                WebSocketState#websocket_state{
                                    response_pending = false,
                                    response_timer = undefined,
                                    request_pending = undefined}
                                });

websocket_info({Type, Name, Pattern, RequestInfo, Request,
                Timeout, Priority, TransId, Source}, Req,
               #cowboy1_state{output_type = OutputType,
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
    NewState = State#cowboy1_state{
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
               #cowboy1_state{websocket_output_type = WebSocketOutputType,
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
    NewResponseLookup = maps:put(ID, {T, ResponseTimer}, ResponseLookup),
    NewState = State#cowboy1_state{
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
               #cowboy1_state{output_type = OutputType,
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
     State#cowboy1_state{
         websocket_state = WebSocketState#websocket_state{
             recv_timeouts = maps:put(TransId,
                 erlang:send_after(Timeout, self(),
                     {'cloudi_service_recv_timeout', Priority, TransId}),
                 RecvTimeouts),
             queued = cloudi_x_pqueue4:in(T, Priority, Queue)}
         }};

websocket_info({Type, Name, _, _, _,
                Timeout, _, TransId, _}, Req,
               #cowboy1_state{output_type = OutputType,
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
               #cowboy1_state{websocket_protocol = undefined,
                             use_websockets = true,
                             websocket_state = #websocket_state{
                                 recv_timeouts = RecvTimeouts,
                                 queued = Queue
                                 } = WebSocketState
                             } = State) ->
    F = fun({_, {_, _, _, _, _, _, _, Id, _}}) -> Id == TransId end,
    {_, NewQueue} = cloudi_x_pqueue4:remove_unique(F, Priority, Queue),
    {ok, Req,
     State#cowboy1_state{
         websocket_state = WebSocketState#websocket_state{
             recv_timeouts = maps:remove(TransId, RecvTimeouts),
             queued = NewQueue}
         }};

websocket_info({'cloudi_service_return_async',
                _, _, <<>>, <<>>, _, TransId, _}, Req,
               #cowboy1_state{use_websockets = true,
                             websocket_state = #websocket_state{
                                 websocket_connect_trans_id = TransId
                                 } = WebSocketState
                             } = State) ->
    {ok, Req,
     State#cowboy1_state{
         websocket_state = WebSocketState#websocket_state{
             websocket_connect_trans_id = undefined}
         }};

websocket_info({'cloudi_service_return_async',
                _, _, ResponseInfo, Response, _, TransId, _}, Req,
               #cowboy1_state{output_type = OutputType,
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
    NewState = State#cowboy1_state{
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
               #cowboy1_state{websocket_ping = WebSocketPingStatus} = State) ->
    if
        WebSocketPingStatus =:= undefined ->
            {shutdown, Req, State};
        WebSocketPingStatus =:= received ->
            erlang:send_after(WebSocketPing, self(),
                              {websocket_ping, WebSocketPing}),
            {reply, {ping, <<>>}, Req,
             State#cowboy1_state{websocket_ping = undefined}}
    end;

websocket_info({cowboy1_error, shutdown}, Req, State) ->
    % from cloudi_service_http_cowboy1:close/1
    {shutdown, Req, State};

websocket_info(Info, Req,
               #cowboy1_state{use_websockets = true} = State) ->
    ?LOG_ERROR("Invalid websocket request state: \"~p\"", [Info]),
    {ok, Req, State}.

websocket_terminate(Reason, _Req,
                    #cowboy1_state{
                        websocket_disconnect = WebSocketDisconnect} = State) ->
    websocket_disconnect_check(WebSocketDisconnect, Reason, State),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

upgrade_request(Req0) ->
    case cloudi_x_cowboy1_req:parse_header(<<"connection">>, Req0) of
        {undefined, _, Req1} ->
            {undefined, Req1};
        {ok, C, Req1} ->
            case lists:member(<<"upgrade">>, C) of
                true ->
                    {ok, [U0 | _],
                     Req2} = cloudi_x_cowboy1_req:parse_header(<<"upgrade">>,
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
    cloudi_request_info:key_value_new(L, text_pairs).

headers_external_outgoing(<<>>) ->
    [];
headers_external_outgoing([] = ResponseInfo) ->
    ResponseInfo;
headers_external_outgoing([{_, _} | _] = ResponseInfo) ->
    % assumes key/value within tuple are iodata()
    % (cowboy1 can error if this is not true)
    ResponseInfo;
headers_external_outgoing(ResponseInfo)
    when is_binary(ResponseInfo) ->
    cloudi_response_info:key_value_parse(ResponseInfo, list).

header_set_if_not(Key, Value, Headers) ->
    case lists:keyfind(Key, 1, Headers) of
        {_, _} ->
            Headers;
        false ->
            [{Key, Value} | Headers]
    end.

get_query_string_external(QsVals) ->
    cloudi_request_info:key_value_new(QsVals, text_pairs).

request_time_start() ->
    cloudi_timestamp:microseconds_monotonic().

request_time_end_success(HttpCode, Method, NameIncoming, NameOutgoing,
                         RequestStartMicroSec) ->
    ?LOG_TRACE("~w ~s ~s (to ~s) ~p ms",
               [HttpCode, Method, NameIncoming, NameOutgoing,
                (cloudi_timestamp:microseconds_monotonic() -
                 RequestStartMicroSec) / 1000.0]).

request_time_end_error(HttpCode, Method, NameIncoming, NameOutgoing,
                       RequestStartMicroSec, Reason) ->
    RequestTime = (cloudi_timestamp:microseconds_monotonic() -
                   RequestStartMicroSec) / 1000.0,
    if
        NameOutgoing =:= undefined ->
            ?LOG_WARN("~w ~s ~s ~p ms: ~p",
                      [HttpCode, Method, NameIncoming,
                       RequestTime, Reason]);
        true ->
            ?LOG_WARN("~w ~s ~s (to ~s) ~p ms: ~p",
                      [HttpCode, Method, NameIncoming, NameOutgoing,
                       RequestTime, Reason])
    end.

websocket_time_start() ->
    cloudi_timestamp:microseconds_monotonic().

websocket_time_end_success(NameIncoming, NameOutgoing,
                           RequestStartMicroSec) ->
    ?LOG_TRACE("~s (to ~s) ~p ms",
               [NameIncoming, NameOutgoing,
                (cloudi_timestamp:microseconds_monotonic() -
                 RequestStartMicroSec) / 1000.0]).

websocket_time_end_error(NameIncoming, NameOutgoing,
                         RequestStartMicroSec, Reason) ->
    ?LOG_WARN("~s (to ~s) ~p ms: ~p",
              [NameIncoming, NameOutgoing,
               (cloudi_timestamp:microseconds_monotonic() -
                RequestStartMicroSec) / 1000.0, Reason]).

websocket_request_end(Name, NewTimeout, OldTimeout) ->
    ?LOG_TRACE("~s ~p ms", [Name, OldTimeout - NewTimeout]).

handle_request(Name, Headers, 'normal', Req,
               #cowboy1_state{
                   timeout_body = TimeoutBody,
                   length_body_read = LengthBodyRead,
                   length_body_chunk = LengthBodyChunk} = State) ->
    BodyOpts = [{length, LengthBodyChunk},
                {read_length, LengthBodyRead},
                {read_timeout, TimeoutBody}],
    {ok, Body, NextReq} = handle_request_body(Req, BodyOpts),
    handle_request(Name, Headers, Body, NextReq, State);
handle_request(Name, Headers, 'application_zip', Req,
               #cowboy1_state{
                   timeout_body = TimeoutBody,
                   length_body_read = LengthBodyRead,
                   length_body_chunk = LengthBodyChunk} = State) ->
    BodyOpts = [{length, LengthBodyChunk},
                {read_length, LengthBodyRead},
                {read_timeout, TimeoutBody}],
    {ok, Body, NextReq} = handle_request_body(Req, BodyOpts),
    handle_request(Name, Headers, zlib:unzip(Body), NextReq, State);
handle_request(Name, Headers, 'multipart', Req,
               #cowboy1_state{
                   dispatcher = Dispatcher,
                   timeout_async = TimeoutAsync,
                   timeout_part_header = TimeoutPartHeader,
                   length_part_header_read = LengthPartHeaderRead,
                   length_part_header_chunk = LengthPartHeaderChunk,
                   timeout_part_body = TimeoutPartBody,
                   length_part_body_read = LengthPartBodyRead,
                   length_part_body_chunk = LengthPartBodyChunk,
                   parts_destination_lock = PartsDestinationLock} = State) ->
    DestinationLock = if
        PartsDestinationLock =:= true ->
            cloudi_service:get_pid(Dispatcher, Name, TimeoutAsync);
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
            {{cowboy1_error, timeout}, Req, State}
    end;
handle_request(Name, Headers, Body, Req,
               #cowboy1_state{
                   dispatcher = Dispatcher,
                   timeout_sync = TimeoutSync,
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
    case send_sync_minimal(Dispatcher, Name, RequestInfo, Request,
                           TimeoutSync, self()) of
        {ok, ResponseInfo, Response} ->
            HeadersOutgoing = headers_external_outgoing(ResponseInfo),
            {{cowboy1_response, HeadersOutgoing, Response}, Req, State};
        {error, timeout} ->
            {{cowboy1_error, timeout}, Req, State}
    end.

handle_request_body(Req, BodyOpts) ->
    case cloudi_x_cowboy1_req:body(Req, BodyOpts) of
        {ok, _, _} = Success ->
            Success;
        {error, Reason} ->
            erlang:exit({cowboy1_error, Reason})
    end.

handle_request_multipart(Name, Headers,
                         Destination, Self, PartHeaderOpts, PartBodyOpts,
                         MultipartId, Req0, State) ->
    case cloudi_x_cowboy1_req:part(Req0, PartHeaderOpts) of
        {ok, HeadersPart, ReqN} ->
            handle_request_multipart([], 0, Name, Headers, HeadersPart,
                                     Destination, Self,
                                     PartHeaderOpts, PartBodyOpts,
                                     MultipartId, ReqN, State);
        {done, ReqN} ->
            {{cowboy1_error, multipart_empty}, ReqN, State}
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
                              #cowboy1_state{
                                  dispatcher = Dispatcher,
                                  timeout_async = TimeoutAsync,
                                  output_type = OutputType} = State) ->
    case cloudi_x_cowboy1_req:part_body(Req0, PartBodyOpts) of
        {ok, PartBodyChunkLast, Req1} ->
            PartBody = if
                PartBodyList == [] ->
                    PartBodyChunkLast;
                true ->
                    erlang:iolist_to_binary(lists:reverse([PartBodyChunkLast |
                                                           PartBodyList]))
            end,
            {HeadersPartNextN,
             ReqN} = case cloudi_x_cowboy1_req:part(Req1, PartHeaderOpts) of
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
            SendResult = send_async_minimal(Dispatcher, Name,
                                            RequestInfo, Request,
                                            TimeoutAsync, Destination, Self),
            {SendResult, HeadersPartNextN, ReqN, State};
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
                                                     [{cowboy1_response,
                                                       HeadersOutgoing,
                                                       Response} |
                                                      SuccessList],
                                                     ErrorList,
                                                     Req, State);
        true ->
            handle_request_multipart_receive_results(ResponseList,
                                                     SuccessList,
                                                     [{cowboy1_response,
                                                       HeadersOutgoing,
                                                       Response} |
                                                      ErrorList],
                                                     Req, State)
    end.

handle_request_multipart_receive([_ | _] = TransIdList, Req,
                                 #cowboy1_state{
                                     timeout_sync = TimeoutSync} = State) ->
    case recv_asyncs_minimal(TimeoutSync, TransIdList) of
        {ok, ResponseList} ->
            handle_request_multipart_receive_results(ResponseList,
                                                     [], [], Req, State);
        {error, timeout} ->
            {{cowboy1_error, timeout}, Req, State}
    end.

handle_response(NameIncoming, HeadersOutgoing0, Response,
                Req0, OutputType, ContentTypeForced,
                SetXContentTypeOptions, SetXXSSProtection,
                ContentSecurityPolicy, ContentSecurityPolicyReport) ->
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
    {HttpCode, HeadersOutgoing2} = case lists:keytake(<<"status">>, 1,
                                                      HeadersOutgoing0) of
        false ->
            {200, HeadersOutgoing0};
        {value, {_, Status}, HeadersOutgoing1}
            when is_binary(Status) ->
            {erlang:binary_to_integer(hd(binary:split(Status, <<" ">>))),
             HeadersOutgoing1}
    end,
    HeadersOutgoing3 = if
        HeadersOutgoing2 =/= [] ->
            HeadersOutgoing2;
        ContentTypeForced =/= undefined ->
            [{<<"content-type">>, ContentTypeForced}];
        true ->
            Extension = filename:extension(NameIncoming),
            if
                Extension == [] ->
                    [{<<"content-type">>, <<"text/html">>}];
                true ->
                    {AttachmentGuess,
                     ContentType} = case cloudi_response_info:
                                         lookup_content_type(binary,
                                                             Extension) of
                        error ->
                            {attachment, <<"application/octet-stream">>};
                        {ok, AttachmentGuessContentTypeTuple} ->
                            AttachmentGuessContentTypeTuple
                    end,
                    if
                        AttachmentGuess =:= attachment,
                        HttpCode >= 200, HttpCode < 300, HttpCode /= 204 ->
                            [{<<"content-disposition">>,
                              ["attachment; filename=\"",
                               filename:basename(NameIncoming), "\""]},
                             {<<"content-type">>, ContentType}];
                        true ->
                            [{<<"content-type">>, ContentType}]
                    end
            end
    end,
    {ContentTypeHTML,
     ContentTypeSet} = case lists:keyfind(<<"content-type">>, 1,
                                          HeadersOutgoing3) of
        {_,  <<"text/html", _/binary>>} ->
            {true, true};
        {_,  <<_/binary>>} ->
            {false, true};
        false ->
            {false, false}
    end,
    HeadersOutgoing4 = if
        ContentTypeSet =:= true ->
            if
                SetXContentTypeOptions =:= true ->
                    header_set_if_not(<<"X-Content-Type-Options">>,
                                      <<"nosniff">>,
                                      HeadersOutgoing3);
                SetXContentTypeOptions =:= false ->
                    HeadersOutgoing3
            end;
        ContentTypeSet =:= false ->
            HeadersOutgoing3
    end,
    HeadersOutgoingN = if
        ContentTypeHTML =:= true ->
            HeadersOutgoing5 = if
                SetXXSSProtection =:= true ->
                    header_set_if_not(<<"X-XSS-Protection">>,
                                      <<"0">>,
                                      HeadersOutgoing4);
                SetXXSSProtection =:= false ->
                    HeadersOutgoing4
            end,
            HeadersOutgoing6 = if
                is_binary(ContentSecurityPolicyReport) ->
                    header_set_if_not(<<"content-security-policy-report-only">>,
                                      ContentSecurityPolicyReport,
                                      HeadersOutgoing5);
                ContentSecurityPolicyReport =:= undefined ->
                    HeadersOutgoing5
            end,
            if
                is_binary(ContentSecurityPolicy) ->
                    header_set_if_not(<<"content-security-policy">>,
                                      ContentSecurityPolicy,
                                      HeadersOutgoing6);
                ContentSecurityPolicy =:= undefined ->
                    HeadersOutgoing6
            end;
        ContentTypeHTML =:= false ->
            HeadersOutgoing4
    end,
    ResponseHeadersOutgoing = HeadersOutgoingN,
    {ok, ReqN} = cloudi_x_cowboy1_req:reply(HttpCode,
                                            ResponseHeadersOutgoing,
                                            ResponseBinary,
                                            Req0),
    {HttpCode, ReqN}.

service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, Client, Req0)
    when UseClientIpPrefix =:= true, UseHostPrefix =:= true ->
    {HostRaw, Req1} = cloudi_x_cowboy1_req:host(Req0),
    {service_name_incoming_merge(Client, HostRaw, PathRaw), Req1};
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, Client, Req0)
    when UseClientIpPrefix =:= true, UseHostPrefix =:= false ->
    {service_name_incoming_merge(Client, undefined, PathRaw), Req0};
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, _Client, Req0)
    when UseClientIpPrefix =:= false, UseHostPrefix =:= true ->
    {HostRaw, Req1} = cloudi_x_cowboy1_req:host(Req0),
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
                        #cowboy1_state{
                            dispatcher = Dispatcher,
                            timeout_async = TimeoutAsync,
                            output_type = OutputType,
                            websocket_state = #websocket_state{
                                request_info = RequestInfo} = WebSocketState
                            } = State) ->
    Request = websocket_connect_request(OutputType),
    case send_async_minimal(Dispatcher, WebSocketConnectName,
                            RequestInfo, Request, TimeoutAsync, self()) of
        {ok, TransId} ->
            State#cowboy1_state{
                websocket_state = WebSocketState#websocket_state{
                    websocket_connect_trans_id = TransId}};
        {error, timeout} ->
            State
    end;
websocket_connect_check({sync, WebSocketConnectName},
                        #cowboy1_state{
                            dispatcher = Dispatcher,
                            timeout_sync = TimeoutSync,
                            output_type = OutputType,
                            websocket_state = #websocket_state{
                                request_info = RequestInfo} = WebSocketState
                            } = State) ->
    Self = self(),
    case send_async_minimal(Dispatcher, WebSocketConnectName,
                            RequestInfo, websocket_connect_request(OutputType),
                            TimeoutSync, Self) of
        {ok, TransId} ->
            case recv_async_minimal(TimeoutSync, TransId) of
                {ok, ResponseInfo, Response} ->
                    % must provide the response after the websocket_init is done
                    Self ! {'cloudi_service_return_async',
                            WebSocketConnectName,
                            WebSocketConnectName,
                            ResponseInfo, Response,
                            TimeoutSync, TransId, Self},
                    State#cowboy1_state{
                        websocket_state = WebSocketState#websocket_state{
                            websocket_connect_trans_id = TransId}};
                {error, timeout} ->
                    State
            end;
        {error, timeout} ->
            State
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
websocket_disconnect_check({async, WebSocketDisconnectName}, Reason,
                           #cowboy1_state{
                               dispatcher = Dispatcher,
                               timeout_async = TimeoutAsync,
                               output_type = OutputType,
                               websocket_state = #websocket_state{
                                   request_info = RequestInfo}}) ->
    _ = send_async_minimal(Dispatcher, WebSocketDisconnectName,
                           websocket_disconnect_request_info(Reason,
                                                             RequestInfo,
                                                             OutputType),
                           websocket_disconnect_request(OutputType),
                           TimeoutAsync, self()),
    ok;
websocket_disconnect_check({sync, WebSocketDisconnectName}, Reason,
                           #cowboy1_state{
                               dispatcher = Dispatcher,
                               timeout_sync = TimeoutSync,
                               output_type = OutputType,
                               websocket_state = #websocket_state{
                                   request_info = RequestInfo}}) ->
    _ = send_sync_minimal(Dispatcher, WebSocketDisconnectName,
                          websocket_disconnect_request_info(Reason,
                                                            RequestInfo,
                                                            OutputType),
                          websocket_disconnect_request(OutputType),
                          TimeoutSync, self()),
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

websocket_handle_incoming_request(Dispatcher, NameOutgoing,
                                  RequestInfo, Request, TimeoutSync, ResponseF,
                                  WebSocketRequestType, Req,
                                  NameIncoming, State) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun websocket_time_start/0, []),
    case send_sync_minimal(Dispatcher, NameOutgoing, RequestInfo, Request,
                           TimeoutSync, self()) of
        {ok, ResponseInfo, Response} ->
            ?LOG_TRACE_APPLY(fun websocket_time_end_success/3,
                             [NameIncoming, NameOutgoing,
                              RequestStartMicroSec]),
            case websocket_terminate_check(ResponseInfo) of
                true when Response == <<>> ->
                    {reply, close, Req, State};
                true ->
                    {reply, [{WebSocketRequestType,
                              ResponseF(Response)}, close], Req, State};
                false ->
                    {reply, {WebSocketRequestType,
                             ResponseF(Response)}, Req, State}
            end;
        {error, timeout} ->
            ?LOG_WARN_APPLY(fun websocket_time_end_error/4,
                            [NameIncoming, NameOutgoing,
                             RequestStartMicroSec, timeout]),
            {reply, {WebSocketRequestType, <<>>}, Req, State}
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
                        #cowboy1_state{websocket_state =
                            #websocket_state{
                                response_pending = false,
                                recv_timeouts = RecvTimeouts,
                                queued = Queue} = WebSocketState} = State) ->
    case cloudi_x_pqueue4:out(Queue) of
        {empty, NewQueue} ->
            {ok, Req,
             State#cowboy1_state{websocket_state =
                 WebSocketState#websocket_state{queued = NewQueue}}};
        {{value, {Type, Name, Pattern, RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Timeout = case erlang:cancel_timer(maps:get(TransId,
                                                        RecvTimeouts)) of
                false ->
                    0;
                V ->
                    V
            end,
            websocket_info({Type, Name, Pattern, RequestInfo, Request,
                            Timeout, Priority, TransId, Pid}, Req,
                           State#cowboy1_state{websocket_state =
                               WebSocketState#websocket_state{
                                   recv_timeouts = maps:remove(TransId,
                                                               RecvTimeouts),
                                   queued = NewQueue}})
    end.

