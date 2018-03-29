%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI HTTP Client Service==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2018 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_client).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([head/4,
         head/5,
         head_async/4,
         head_async/5,
         get/4,
         get/5,
         get_async/4,
         get_async/5,
         put/4,
         put/5,
         put_async/4,
         put_async/5,
         post/4,
         post/5,
         post_async/4,
         post_async/5,
         trace/4,
         trace/5,
         trace_async/4,
         trace_async/5,
         options/4,
         options/5,
         options_async/4,
         options_async/5,
         delete/4,
         delete/5,
         delete_async/4,
         delete_async/5]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

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

-type headers() :: list({binary(), binary()}).
-export_type([headers/0]).

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP HEAD request.===
%% @end
%%-------------------------------------------------------------------------

-spec head(Agent :: agent(),
           Prefix :: service_name(),
           RequestInfo :: headers(),
           Request :: binary()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

head(Agent, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/head",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP HEAD request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec head(Agent :: agent(),
           Prefix :: service_name(),
           RequestInfo :: headers(),
           Request :: binary(),
           Timeout :: timeout_milliseconds()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

head(Agent, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/head",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP HEAD async request.===
%% @end
%%-------------------------------------------------------------------------

-spec head_async(Agent :: agent(),
                 Prefix :: service_name(),
                 RequestInfo :: headers(),
                 Request :: binary()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

head_async(Agent, Prefix, RequestInfo, Request) ->
    cloudi:send_async(Agent, Prefix ++ "/head",
                      RequestInfo, Request, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP HEAD async request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec head_async(Agent :: agent(),
                 Prefix :: service_name(),
                 RequestInfo :: headers(),
                 Request :: binary(),
                 Timeout :: timeout_milliseconds()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

head_async(Agent, Prefix, RequestInfo, Request, Timeout) ->
    cloudi:send_async(Agent, Prefix ++ "/head",
                      RequestInfo, Request, Timeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP GET request.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Agent :: agent(),
          Prefix :: service_name(),
          RequestInfo :: headers(),
          Request :: binary()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

get(Agent, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/get",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP GET request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Agent :: agent(),
          Prefix :: service_name(),
          RequestInfo :: headers(),
          Request :: binary(),
          Timeout :: timeout_milliseconds()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

get(Agent, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/get",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP GET async request.===
%% @end
%%-------------------------------------------------------------------------

-spec get_async(Agent :: agent(),
                Prefix :: service_name(),
                RequestInfo :: headers(),
                Request :: binary()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

get_async(Agent, Prefix, RequestInfo, Request) ->
    cloudi:send_async(Agent, Prefix ++ "/get",
                      RequestInfo, Request, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP GET async request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec get_async(Agent :: agent(),
                Prefix :: service_name(),
                RequestInfo :: headers(),
                Request :: binary(),
                Timeout :: timeout_milliseconds()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

get_async(Agent, Prefix, RequestInfo, Request, Timeout) ->
    cloudi:send_async(Agent, Prefix ++ "/get",
                      RequestInfo, Request, Timeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP PUT request.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Agent :: agent(),
          Prefix :: service_name(),
          RequestInfo :: headers(),
          Request :: binary()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

put(Agent, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/put",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP PUT request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Agent :: agent(),
          Prefix :: service_name(),
          RequestInfo :: headers(),
          Request :: binary(),
          Timeout :: timeout_milliseconds()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

put(Agent, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/put",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP PUT async request.===
%% @end
%%-------------------------------------------------------------------------

-spec put_async(Agent :: agent(),
                Prefix :: service_name(),
                RequestInfo :: headers(),
                Request :: binary()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

put_async(Agent, Prefix, RequestInfo, Request) ->
    cloudi:send_async(Agent, Prefix ++ "/put",
                      RequestInfo, Request, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP PUT async request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec put_async(Agent :: agent(),
                Prefix :: service_name(),
                RequestInfo :: headers(),
                Request :: binary(),
                Timeout :: timeout_milliseconds()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

put_async(Agent, Prefix, RequestInfo, Request, Timeout) ->
    cloudi:send_async(Agent, Prefix ++ "/put",
                      RequestInfo, Request, Timeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP POST request.===
%% @end
%%-------------------------------------------------------------------------

-spec post(Agent :: agent(),
           Prefix :: service_name(),
           RequestInfo :: headers(),
           Request :: binary()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

post(Agent, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/post",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP POST request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec post(Agent :: agent(),
           Prefix :: service_name(),
           RequestInfo :: headers(),
           Request :: binary(),
           Timeout :: timeout_milliseconds()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

post(Agent, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/post",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP POST async request.===
%% @end
%%-------------------------------------------------------------------------

-spec post_async(Agent :: agent(),
                 Prefix :: service_name(),
                 RequestInfo :: headers(),
                 Request :: binary()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

post_async(Agent, Prefix, RequestInfo, Request) ->
    cloudi:send_async(Agent, Prefix ++ "/post",
                      RequestInfo, Request, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP POST async request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec post_async(Agent :: agent(),
                 Prefix :: service_name(),
                 RequestInfo :: headers(),
                 Request :: binary(),
                 Timeout :: timeout_milliseconds()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

post_async(Agent, Prefix, RequestInfo, Request, Timeout) ->
    cloudi:send_async(Agent, Prefix ++ "/post",
                      RequestInfo, Request, Timeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP TRACE request.===
%% @end
%%-------------------------------------------------------------------------

-spec trace(Agent :: agent(),
            Prefix :: service_name(),
            RequestInfo :: headers(),
            Request :: binary()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

trace(Agent, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/trace",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP TRACE request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec trace(Agent :: agent(),
            Prefix :: service_name(),
            RequestInfo :: headers(),
            Request :: binary(),
            Timeout :: timeout_milliseconds()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

trace(Agent, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/trace",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP TRACE async request.===
%% @end
%%-------------------------------------------------------------------------

-spec trace_async(Agent :: agent(),
                  Prefix :: service_name(),
                  RequestInfo :: headers(),
                  Request :: binary()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

trace_async(Agent, Prefix, RequestInfo, Request) ->
    cloudi:send_async(Agent, Prefix ++ "/trace",
                      RequestInfo, Request, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP TRACE async request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec trace_async(Agent :: agent(),
                  Prefix :: service_name(),
                  RequestInfo :: headers(),
                  Request :: binary(),
                  Timeout :: timeout_milliseconds()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

trace_async(Agent, Prefix, RequestInfo, Request, Timeout) ->
    cloudi:send_async(Agent, Prefix ++ "/trace",
                      RequestInfo, Request, Timeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP OPTIONS request.===
%% @end
%%-------------------------------------------------------------------------

-spec options(Agent :: agent(),
              Prefix :: service_name(),
              RequestInfo :: headers(),
              Request :: binary()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

options(Agent, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/options",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP OPTIONS request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec options(Agent :: agent(),
              Prefix :: service_name(),
              RequestInfo :: headers(),
              Request :: binary(),
              Timeout :: timeout_milliseconds()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

options(Agent, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/options",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP OPTIONS async request.===
%% @end
%%-------------------------------------------------------------------------

-spec options_async(Agent :: agent(),
                    Prefix :: service_name(),
                    RequestInfo :: headers(),
                    Request :: binary()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

options_async(Agent, Prefix, RequestInfo, Request) ->
    cloudi:send_async(Agent, Prefix ++ "/options",
                      RequestInfo, Request, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP OPTIONS async request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec options_async(Agent :: agent(),
                    Prefix :: service_name(),
                    RequestInfo :: headers(),
                    Request :: binary(),
                    Timeout :: timeout_milliseconds()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

options_async(Agent, Prefix, RequestInfo, Request, Timeout) ->
    cloudi:send_async(Agent, Prefix ++ "/options",
                      RequestInfo, Request, Timeout, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP DELETE request.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(Agent :: agent(),
             Prefix :: service_name(),
             RequestInfo :: headers(),
             Request :: binary()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

delete(Agent, Prefix, RequestInfo, Request) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/delete",
                            RequestInfo, Request, undefined, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP DELETE request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(Agent :: agent(),
             Prefix :: service_name(),
             RequestInfo :: headers(),
             Request :: binary(),
             Timeout :: timeout_milliseconds()) ->
    {{ok, headers(), binary()} | {error, any()},
     NewAgent :: agent()}.

delete(Agent, Prefix, RequestInfo, Request, Timeout) ->
    result(cloudi:send_sync(Agent, Prefix ++ "/delete",
                            RequestInfo, Request, Timeout, undefined)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP DELETE async request.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_async(Agent :: agent(),
                   Prefix :: service_name(),
                   RequestInfo :: headers(),
                   Request :: binary()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

delete_async(Agent, Prefix, RequestInfo, Request) ->
    cloudi:send_async(Agent, Prefix ++ "/delete",
                      RequestInfo, Request, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===A HTTP DELETE async request with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec delete_async(Agent :: agent(),
                   Prefix :: service_name(),
                   RequestInfo :: headers(),
                   Request :: binary(),
                   Timeout :: timeout_milliseconds()) ->
    {{ok, cloudi:trans_id()} | {error, cloudi:error_reason()},
     NewAgent :: agent()}.

delete_async(Agent, Prefix, RequestInfo, Request, Timeout) ->
    cloudi:send_async(Agent, Prefix ++ "/delete",
                      RequestInfo, Request, Timeout, undefined).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
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
    [$/ | Method] = cloudi_string:uppercase(lists:nthtail(PrefixLength, Name)),
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

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
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
            {HostName, cloudi_string:trim(PortStr)}
    end.

headers_request_filter(Headers0) ->
    Defaults = [
        % required parameters
        {<<"host">>,                     undefined},
        {<<"url-path">>,                 undefined},
        % removed parameters
        % (possible if coming from cloudi_service_http_cowboy)
        {<<"peer">>,                     undefined},
        {<<"peer-port">>,                undefined},
        {<<"source-address">>,           undefined},
        {<<"source-port">>,              undefined}],
    case cloudi_lists:take_values(Defaults, Headers0) of
        [undefined, _,
         _, _, _, _ | _] ->
            {error, {request_info_missing, <<"host">>}};
        [Host, _,
         _, _, _, _ | _]
            when not is_binary(Host) ->
            {error, {request_info_invalid, <<"host">>}};
        [_, undefined,
         _, _, _, _ | _] ->
            {error, {request_info_missing, <<"url-path">>}};
        [_, URLPath,
         _, _, _, _ | _]
            when not is_binary(URLPath) ->
            {error, {request_info_invalid, <<"url-path">>}};
        [Host, URLPath,
         _, _, _, _ | Headers1] ->
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
    cloudi_timestamp:microseconds_monotonic().

client_debug_end(off, _, _, _, _, _, _, _) ->
    undefined;
client_debug_end(Level, HttpCode, Method,
                 HeadersIncoming, Request,
                 HeadersOutgoing, Response, RequestStartMicroSec) ->
    client_debug_log(Level,
                     "~p ~s ~p ms~n"
                     "headers__in(~p)~n"
                     "request__in(~p)~n"
                     "headers_out(~p)~n"
                     "request_out(~p)",
                     [HttpCode, Method,
                      (cloudi_timestamp:microseconds_monotonic() -
                       RequestStartMicroSec) / 1000.0,
                      HeadersIncoming, Request,
                      HeadersOutgoing, Response]).

result({{ok, {error, _} = Error}, NewAgent}) ->
    {Error, NewAgent};
result({{error, _}, _} = Error) ->
    Error;
result({{ok, Response}, NewAgent}) ->
    {{ok, [], Response}, NewAgent};
result({{ok, _, _}, _} = Success) ->
    Success.

