%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI HTTP Client Service==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_client).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([head/4,
         head/5,
         get/4,
         get/5,
         put/4,
         put/5,
         post/4,
         post/5,
         trace/4,
         trace/5,
         options/4,
         options/5,
         delete/4,
         delete/5]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_CLIENT,                     inets).
-define(DEFAULT_PROFILE,                undefined). % settings/cookies
-define(DEFAULT_INPUT,                   external).
-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).

% supported clients
-define(MODULE_INETS, httpc). % Erlang/OTP inets HTTP client

-record(state,
    {
        module :: ?MODULE_INETS,
        profile,
        input_type :: external | internal,
        debug_level :: off | trace | debug | info | warn | error | fatal,
        content_type_lookup :: cloudi_x_trie:cloudi_x_trie(),
        prefix_length :: pos_integer()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type dispatcher() :: cloudi:context() | cloudi_service:dispatcher().
-type headers() :: list({binary(), binary()}).
-export_type([headers/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP HEAD request.===
%% @end
%%-------------------------------------------------------------------------

-spec head(Dispatcher :: dispatcher(),
           Prefix :: cloudi_service:service_name(),
           RequestInfo :: headers(),
           Request :: binary()) ->
    {ok, headers(), binary()} | {error, any()}.

head(Dispatcher, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/head",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP HEAD request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec head(Dispatcher :: dispatcher(),
           Prefix :: cloudi_service:service_name(),
           RequestInfo :: headers(),
           Request :: binary(),
           Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, headers(), binary()} | {error, any()}.

head(Dispatcher, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/head",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP GET request.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Dispatcher :: dispatcher(),
          Prefix :: cloudi_service:service_name(),
          RequestInfo :: headers(),
          Request :: binary()) ->
    {ok, headers(), binary()} | {error, any()}.

get(Dispatcher, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/get",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP GET request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Dispatcher :: dispatcher(),
          Prefix :: cloudi_service:service_name(),
          RequestInfo :: headers(),
          Request :: binary(),
          Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, headers(), binary()} | {error, any()}.

get(Dispatcher, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/get",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP PUT request.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Dispatcher :: dispatcher(),
          Prefix :: cloudi_service:service_name(),
          RequestInfo :: headers(),
          Request :: binary()) ->
    {ok, headers(), binary()} | {error, any()}.

put(Dispatcher, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/put",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP PUT request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Dispatcher :: dispatcher(),
          Prefix :: cloudi_service:service_name(),
          RequestInfo :: headers(),
          Request :: binary(),
          Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, headers(), binary()} | {error, any()}.

put(Dispatcher, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/put",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP POST request.===
%% @end
%%-------------------------------------------------------------------------

-spec post(Dispatcher :: dispatcher(),
           Prefix :: cloudi_service:service_name(),
           RequestInfo :: headers(),
           Request :: binary()) ->
    {ok, headers(), binary()} | {error, any()}.

post(Dispatcher, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/post",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP POST request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec post(Dispatcher :: dispatcher(),
           Prefix :: cloudi_service:service_name(),
           RequestInfo :: headers(),
           Request :: binary(),
           Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, headers(), binary()} | {error, any()}.

post(Dispatcher, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/post",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP TRACE request.===
%% @end
%%-------------------------------------------------------------------------

-spec trace(Dispatcher :: dispatcher(),
            Prefix :: cloudi_service:service_name(),
            RequestInfo :: headers(),
            Request :: binary()) ->
    {ok, headers(), binary()} | {error, any()}.

trace(Dispatcher, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/trace",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP TRACE request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec trace(Dispatcher :: dispatcher(),
            Prefix :: cloudi_service:service_name(),
            RequestInfo :: headers(),
            Request :: binary(),
            Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, headers(), binary()} | {error, any()}.

trace(Dispatcher, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/trace",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP OPTIONS request.===
%% @end
%%-------------------------------------------------------------------------

-spec options(Dispatcher :: dispatcher(),
              Prefix :: cloudi_service:service_name(),
              RequestInfo :: headers(),
              Request :: binary()) ->
    {ok, headers(), binary()} | {error, any()}.

options(Dispatcher, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/options",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP OPTIONS request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec options(Dispatcher :: dispatcher(),
              Prefix :: cloudi_service:service_name(),
              RequestInfo :: headers(),
              Request :: binary(),
              Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, headers(), binary()} | {error, any()}.

options(Dispatcher, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/options",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP DELETE request.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(Dispatcher :: dispatcher(),
             Prefix :: cloudi_service:service_name(),
             RequestInfo :: headers(),
             Request :: binary()) ->
    {ok, headers(), binary()} | {error, any()}.

delete(Dispatcher, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/delete",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP DELETE request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(Dispatcher :: dispatcher(),
             Prefix :: cloudi_service:service_name(),
             RequestInfo :: headers(),
             Request :: binary(),
             Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, headers(), binary()} | {error, any()}.

delete(Dispatcher, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Dispatcher, Prefix ++ "/delete",
                            RequestInfo, Request, Timeout, undefined)).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {client,                         ?DEFAULT_CLIENT},
        {profile,                        ?DEFAULT_PROFILE},
        {input,                          ?DEFAULT_INPUT},
        {debug,                          ?DEFAULT_DEBUG},
        {debug_level,                    ?DEFAULT_DEBUG_LEVEL}],
    [Client, Profile0, InputType,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
    true = ((Profile0 =:= undefined) orelse
            (is_list(Profile0) andalso is_integer(hd(Profile0)))),
    true = (InputType =:= external) orelse (InputType =:= internal),
    {Module, Methods, ProfileN} = if
        Client =:= inets ->
            Profile1 = if
                Profile0 =:= undefined ->
                    default;
                true ->
                    erlang:list_to_atom(Profile0)
            end,
            case inets:start(?MODULE_INETS, [{profile, Profile1}], inets) of
                {ok, _} ->
                    ok;
                {error, {already_started, _}} ->
                    ok
            end,
            {?MODULE_INETS,
             ["head", "get", "put", "post", "trace", "options", "delete"],
             Profile1}
    end,
    false = lists:member($*, Prefix),
    [cloudi_service:subscribe(Dispatcher, [$/ | Method]) || Method <- Methods],
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    DebugLogLevel = if
        Debug =:= false ->
            off;
        Debug =:= true ->
            DebugLevel
    end,
    ContentTypeLookup = cloudi_response_info:lookup_content_type(list),
    {ok, #state{module = Module,
                profile = ProfileN,
                input_type = InputType,
                debug_level = DebugLogLevel,
                content_type_lookup = ContentTypeLookup,
                prefix_length = erlang:length(Prefix)}}.

cloudi_service_handle_request(_Type, Name, _Pattern, RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{module = Module,
                                     profile = Profile,
                                     input_type = InputType,
                                     debug_level = DebugLevel,
                                     content_type_lookup = ContentTypeLookup,
                                     prefix_length = PrefixLength} = State,
                              _Dispatcher) ->
    RequestStartMicroSec = client_debug_start(DebugLevel),
    [$/ | Method] = string:to_upper(lists:nthtail(PrefixLength, Name)),
    HeadersIncoming = headers_request(RequestInfo, InputType),
    {HttpCode,
     HeadersOutgoing,
     Response} = client_request(Module, Profile, Method,
                                HeadersIncoming, Request, Timeout,
                                ContentTypeLookup),
    client_debug_end(DebugLevel, HttpCode, Method,
                     HeadersIncoming, Request,
                     HeadersOutgoing, Response, RequestStartMicroSec),
    {reply, HeadersOutgoing, Response, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

headers_external_incoming(<<>>) ->
    [];
headers_external_incoming([] = ResponseInfo) ->
    ResponseInfo;
headers_external_incoming([{K, V} | _] = ResponseInfo) ->
    % K/V possibly from cloudi_service_http_cowboy HTTP request headers
    true = is_binary(K),
    true = is_binary(V),
    ResponseInfo;
headers_external_incoming(ResponseInfo)
    when is_binary(ResponseInfo) ->
    headers_external_incoming(binary:split(ResponseInfo, <<0>>, [global]), []).

headers_external_incoming([<<>>], Result) ->
    lists:reverse(Result);
headers_external_incoming([K, V | L], Result) ->
    headers_external_incoming(L, [{K, V} | Result]).

headers_request(RequestInfo, internal) ->
    % internal, enforces internal service communication
    % (i.e., erlang-terms-only)
    case RequestInfo of
        [] ->
            RequestInfo;
        [{K, V} | _] ->
            % K/V possibly from cloudi_service_http_cowboy HTTP request headers
            true = is_binary(K),
            true = is_binary(V),
            RequestInfo
    end;
headers_request(RequestInfo, external) ->
    headers_external_incoming(RequestInfo).

headers_request_filter_host(Host) ->
    case binary:split(Host, <<":">>) of
        [HostName] ->
            {HostName, <<"80">>};
        [HostName, PortStr] ->
            {HostName, strip_whitespace(PortStr)}
    end.

headers_request_filter(Headers0) ->
    Defaults = [
        % required parameters
        {<<"host">>,                     undefined},
        {<<"url-path">>,                 undefined},
        % removed parameters
        % (possible if coming from cloudi_service_http_cowboy)
        {<<"version">>,                  undefined},
        {<<"peer">>,                     undefined},
        {<<"peer-port">>,                undefined},
        {<<"source-address">>,           undefined},
        {<<"source-port">>,              undefined}],
    case cloudi_lists:take_values(Defaults, Headers0) of
        [undefined, _,
         _, _, _, _, _ | _] ->
            {error, {request_info_missing, <<"host">>}};
        [Host, _,
         _, _, _, _, _ | _]
            when not is_binary(Host) ->
            {error, {request_info_invalid, <<"host">>}};
        [_, undefined,
         _, _, _, _, _ | _] ->
            {error, {request_info_missing, <<"url-path">>}};
        [_, URLPath,
         _, _, _, _, _ | _]
            when not is_binary(URLPath) ->
            {error, {request_info_invalid, <<"url-path">>}};
        [Host, URLPath,
         _, _, _, _, _ | Headers1] ->
            {ok, Host, URLPath, Headers1}
    end.

header_content_type(Headers) ->
    case lists:keyfind("content-type", 1, Headers) of
        false ->
            undefined;
        {"content-type", Value} ->
            cloudi_string:beforel($;, Value, input)
    end.

url_string({HostName, <<"80">>}, URL) ->
    "http://" ++ erlang:binary_to_list(HostName) ++
    erlang:binary_to_list(URL);
url_string({HostName, <<"443">>}, URL) ->
    "https://" ++ erlang:binary_to_list(HostName) ++
    erlang:binary_to_list(URL);
url_string({HostName, Port}, URL) ->
    "http://" ++ erlang:binary_to_list(HostName) ++
    ":" ++ erlang:binary_to_list(Port) ++
    erlang:binary_to_list(URL).

client_request(?MODULE_INETS, Profile, Method0,
               HeadersIncoming0, Request, Timeout,
               ContentTypeLookup) ->
    Method1 = if
        Method0 == "HEAD" ->
            head;
        Method0 == "GET" ->
            get;
        Method0 == "PUT" ->
            put;
        Method0 == "POST" ->
            post;
        Method0 == "TRACE" ->
            trace;
        Method0 == "OPTIONS" ->
            options;
        Method0 == "DELETE" ->
            delete
    end,
    case headers_request_filter(HeadersIncoming0) of
        {ok, Host, URLPath, HeadersIncoming1} ->
            URL = url_string(headers_request_filter_host(Host), URLPath),
            RequestHeaders = [{erlang:binary_to_list(Kin),
                               erlang:binary_to_list(Vin)} ||
                              {Kin, Vin} <- HeadersIncoming1],
            ClientRequest = if
                Method1 =:= get ->
                    {URL, RequestHeaders};
                true ->
                    ContentTypeN = case header_content_type(RequestHeaders) of
                        undefined ->
                            case cloudi_x_trie:find(filename:extension(URL),
                                                    ContentTypeLookup) of
                                {ok, {_, ContentType1}} ->
                                    ContentType1;
                                error ->
                                    "text/html"
                            end;
                        ContentType0 ->
                            ContentType0
                    end,
                    {URL, RequestHeaders, ContentTypeN, Request}
            end,
            case ?MODULE_INETS:request(Method1, ClientRequest,
                                       [{autoredirect, false},
                                        {timeout, Timeout}],
                                       [{body_format, binary}], Profile) of
                {ok, {{_HttpVersion, StatusCode, _Reason},
                      ResponseHeaders, Response}} ->
                    HeadersOutgoing0 = [{erlang:list_to_binary(Kout),
                                         erlang:list_to_binary(Vout)} ||
                                        {Kout, Vout} <- ResponseHeaders],
                    HeadersOutgoing1 = [{<<"status">>,
                                         erlang:integer_to_binary(StatusCode)} |
                                        HeadersOutgoing0],
                    {StatusCode, HeadersOutgoing1, Response};
                {error, _} = Error ->
                    {undefined, <<>>, Error}
            end;
        {error, _} = Error ->
            {undefined, <<>>, Error}
    end.

client_debug_log(trace, Message, Args) ->
    ?LOG_TRACE(Message, Args);
client_debug_log(debug, Message, Args) ->
    ?LOG_DEBUG(Message, Args);
client_debug_log(info, Message, Args) ->
    ?LOG_INFO(Message, Args);
client_debug_log(warn, Message, Args) ->
    ?LOG_WARN(Message, Args);
client_debug_log(error, Message, Args) ->
    ?LOG_ERROR(Message, Args);
client_debug_log(fatal, Message, Args) ->
    ?LOG_FATAL(Message, Args).

client_debug_start(off) ->
    undefined;
client_debug_start(_) ->
    cloudi_x_uuid:get_v1_time(os).

client_debug_end(off, _, _, _, _, _, _, _) ->
    undefined;
client_debug_end(Level, HttpCode, Method,
                 HeadersIncoming, Request,
                 HeadersOutgoing, Response, RequestStartMicroSec) ->
    client_debug_log(Level,
                     "~p ~s (~p, ~p) -> (~p, ~p) ~p ms",
                     [HttpCode, Method,
                      HeadersIncoming, Request,
                      HeadersOutgoing, Response,
                      (cloudi_x_uuid:get_v1_time(os) -
                       RequestStartMicroSec) / 1000.0]).

result({ok, {error, _} = Error}) ->
    Error;
result({error, _} = Error) ->
    Error;
result({ok, Response}) ->
    {ok, [], Response};
result({ok, _, _} = Success) ->
    Success.

strip_whitespace(String) when is_binary(String) ->
    erlang:list_to_binary(string:strip(erlang:binary_to_list(String))).

