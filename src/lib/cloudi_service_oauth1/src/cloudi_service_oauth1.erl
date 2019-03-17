%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OAuth v1.0 Service==
%%% Provide OAuth v1.0 support as a CloudI service based on RFC 5849
%%% (http://tools.ietf.org/html/rfc5849).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2019 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2019 Michael Truog
%%% @version 1.8.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_oauth1).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_DATABASE_TYPE,          pgsql).
-define(DEFAULT_DATABASE,               undefined).
-define(DEFAULT_URL_HOST,               "http://0.0.0.0:8000"). % see below:
        % - must not end with a "/"
        % from http://tools.ietf.org/html/rfc5849#section-3.4.1.2
        % - must be lowercase
        % - must include port number if not 80
        % - must match Host HTTP request header value
-define(DEFAULT_AUTHENTICATION,   undefined). % see below:
        % either an anonymous function or a {module(), atom()} tuple
        % (to reference a function), i.e.:
        % fun((RequestHeaders :: cloudi_service:key_values(),
        %      Request :: binary(),
        %      Timeout :: cloudi_service:timeout_value_milliseconds()) ->
        %     Valid :: boolean())
        % can be provided as {Module, FunctionName}
        % for checking the user's current authentication status before
        % using the OAuth authorize endpoint
        % (if this value is set to undefined, no function checks the user's
        %  authentication in this service, so that needs to have been done
        %  by the service sending to the OAuth authorize endpoint)
-define(DEFAULT_TOKENS_CLEAN,           300). % seconds (how often to delete)
-define(DEFAULT_TOKEN_REQUEST_LENGTH,   12).      % characters minimum
-define(DEFAULT_TOKEN_REQUEST_SECRET_LENGTH, 12). % characters minimum
-define(DEFAULT_TOKEN_REQUEST_EXPIRATION,    300). % seconds
-define(DEFAULT_TOKEN_ACCESS_LENGTH,    12).      % characters minimum
-define(DEFAULT_TOKEN_ACCESS_SECRET_LENGTH,  12). % characters minimum
-define(DEFAULT_TOKEN_ACCESS_EXPIRATION,     300). % seconds
-define(DEFAULT_VERIFIER_LENGTH,        12).      % characters minimum
-define(DEFAULT_DEBUG_DB,               false). % see below:
        % debug db data with consumer key from
        % http://tools.ietf.org/html/rfc5849#section-1.2
-define(DEFAULT_DEBUG,                  false). % see below:
        % debug with raw data from
        % http://tools.ietf.org/html/rfc5849#section-1.2

-record(state,
    {
        database_module :: module(),
        database :: cloudi_service:service_name(),
        url_host :: string(),
        host :: binary(),
        authentication_f :: fun((cloudi_service:key_values(),
                                 binary(),
                                 cloudi_service:timeout_value_milliseconds()) ->
                                boolean()) | undefined,
        token_request_bytes :: pos_integer(),
        token_request_secret_bytes :: pos_integer(),
        token_request_expiration :: pos_integer(),
        token_access_bytes :: pos_integer(),
        token_access_secret_bytes :: pos_integer(),
        token_access_expiration :: pos_integer(),
        verifier_bytes :: pos_integer(),
        debug_db :: boolean(),
        debug :: boolean(),
        prefix_length :: pos_integer()
    }).

% for features specific to Erlang/OTP version 21.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-ifdef(ERLANG_OTP_VERSION_20).
-else.
-ifdef(OTP_RELEASE).
-define(ERLANG_OTP_VERSION_21_FEATURES, true).
-else.
-error("Erlang/OTP version invalid").
-endif.
-endif.
-endif.

% Get the stacktrace in a way that is backwards compatible
-ifdef(ERLANG_OTP_VERSION_21_FEATURES).
-define(STACKTRACE(ErrorType, Error, ErrorStackTrace),
        ErrorType:Error:ErrorStackTrace ->).
-else.
-define(STACKTRACE(ErrorType, Error, ErrorStackTrace),
        ErrorType:Error ->
            ErrorStackTrace = erlang:get_stacktrace(),).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {database_type,               ?DEFAULT_DATABASE_TYPE},
        {database,                    ?DEFAULT_DATABASE},
        {url_host,                    ?DEFAULT_URL_HOST},
        {authentication,              ?DEFAULT_AUTHENTICATION},
        {tokens_clean,                ?DEFAULT_TOKENS_CLEAN},
        {token_request_length,        ?DEFAULT_TOKEN_REQUEST_LENGTH},
        {token_request_secret_length, ?DEFAULT_TOKEN_REQUEST_SECRET_LENGTH},
        {token_request_expiration,    ?DEFAULT_TOKEN_REQUEST_EXPIRATION},
        {token_access_length,         ?DEFAULT_TOKEN_ACCESS_LENGTH},
        {token_access_secret_length,  ?DEFAULT_TOKEN_ACCESS_SECRET_LENGTH},
        {token_access_expiration,     ?DEFAULT_TOKEN_ACCESS_EXPIRATION},
        {verifier_length,             ?DEFAULT_VERIFIER_LENGTH},
        {debug_db,                    ?DEFAULT_DEBUG_DB},
        {debug,                       ?DEFAULT_DEBUG}],
    [DatabaseType, Database, URLHost, Authentication0, TokensClean,
     TokenRequestLength, TokenRequestSecretLength, TokenRequestExpiration,
     TokenAccessLength, TokenAccessSecretLength, TokenAccessExpiration,
     VerifierLength, DebugDB0, Debug
     ] = cloudi_proplists:take_values(Defaults, Args),
    cloudi_service:self(Dispatcher) ! initialize, % db initialize
    DatabaseModule = if
        DatabaseType =:= pgsql ->
            cloudi_service_oauth1_db_pgsql;
        DatabaseType =:= riak ->
            cloudi_service_oauth1_db_riak
    end,
    true = (is_list(Database) andalso is_integer(hd(Database))),
    true = (is_list(URLHost) andalso is_integer(hd(URLHost))),
    Host = case URLHost of
        "https://" ++ HostValue ->
            HostValue;
        "http://" ++ HostValue ->
            HostValue
    end,
    Authentication1 = case Authentication0 of
        {AuthenticationModule, AuthenticationFunction}
        when is_atom(AuthenticationModule), is_atom(AuthenticationFunction) ->
            {file, _} = code:is_loaded(AuthenticationModule),
            fun(AuthenticationRequestHeaders,
                AuthenticationRequest,
                AuthenticationTimeout) ->
                AuthenticationModule:
                AuthenticationFunction(AuthenticationRequestHeaders,
                                       AuthenticationRequest,
                                       AuthenticationTimeout)
            end;
        _ when is_function(Authentication0, 3) ->
            Authentication0;
        undefined ->
            undefined
    end,
    if
        TokensClean =:= undefined ->
            ok;
        is_integer(TokensClean), TokensClean > 0, TokensClean =< 4294967 ->
            erlang:send_after(TokensClean * 1000,
                              cloudi_service:self(Dispatcher),
                              {tokens_clean, TokensClean})
    end,
    true = (is_integer(TokenRequestLength) andalso
            (TokenRequestLength > 0)),
    true = (is_integer(TokenRequestSecretLength) andalso
            (TokenRequestSecretLength > 0)),
    true = (is_integer(TokenRequestExpiration) andalso
            (TokenRequestExpiration > 0)),
    true = (is_integer(TokenAccessLength) andalso
            (TokenAccessLength > 0)),
    true = (is_integer(TokenAccessSecretLength) andalso
            (TokenAccessSecretLength > 0)),
    true = (is_integer(TokenAccessExpiration) andalso
            (TokenAccessExpiration > 0)),
    true = (is_integer(VerifierLength) andalso
            (VerifierLength > 0)),
    DebugDB1 = if
        Debug =:= true ->
            true;
        Debug =:= false ->
            true = is_boolean(DebugDB0),
            DebugDB0
    end,
    false = cloudi_service_name:pattern(Prefix),
    % endpoints based on http://tools.ietf.org/html/rfc5849#section-1.2
    ok = cloudi_service:subscribe(Dispatcher, "initiate/post"),
    ok = cloudi_service:subscribe(Dispatcher, "authorize/get"),
    ok = cloudi_service:subscribe(Dispatcher, "token/post"),
    % revoke an access token
    ok = cloudi_service:subscribe(Dispatcher, "token/delete"),
    % proxy verify requests through
    ok = cloudi_service:subscribe(Dispatcher, "verify*"),
    {ok, #state{database_module = DatabaseModule,
                database = Database,
                url_host = URLHost,
                host = erlang:list_to_binary(Host),
                authentication_f = Authentication1,
                token_request_bytes =
                    token_length_to_bytes(TokenRequestLength),
                token_request_secret_bytes =
                    token_length_to_bytes(TokenRequestSecretLength),
                token_request_expiration = TokenRequestExpiration,
                token_access_bytes =
                    token_length_to_bytes(TokenAccessLength),
                token_access_secret_bytes =
                    token_length_to_bytes(TokenAccessSecretLength),
                token_access_expiration = TokenAccessExpiration,
                verifier_bytes =
                    token_length_to_bytes(VerifierLength),
                debug_db = DebugDB1,
                debug = Debug,
                prefix_length = erlang:length(Prefix)}}.

cloudi_service_handle_request(_RequestType, Name, Pattern, RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{prefix_length = PrefixLength} = State,
                              Dispatcher) ->
    Suffix = lists:nthtail(PrefixLength, Pattern),
    RequestHeaders = cloudi_request_info:key_value_parse(RequestInfo),
    request(Suffix, Name, Pattern, RequestHeaders, Request,
            Timeout, State, Dispatcher).

cloudi_service_handle_info(initialize,
                           #state{database_module = DatabaseModule,
                                  database = Database,
                                  debug_db = DebugDB} = State, Dispatcher) ->
    ok = DatabaseModule:initialize(Dispatcher, Database, DebugDB),
    {noreply, State};
cloudi_service_handle_info({tokens_clean, TokensClean} = Request,
                           #state{database_module = DatabaseModule,
                                  database = Database} = State, Dispatcher) ->
    ok = DatabaseModule:tokens_clean(Dispatcher, Database),
    erlang:send_after(TokensClean * 1000,
                      cloudi_service:self(Dispatcher),
                      Request),
    {noreply, State};
cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

request("initiate/post", _Name, _Pattern, RequestHeaders, Request, Timeout,
        #state{url_host = URLHost,
               host = Host} = State, Dispatcher) ->
    Method = "POST",
    case url(Method, RequestHeaders, URLHost, Host, Request) of
        {ok, URL} ->
            case oauth_parameters(Method, RequestHeaders, Request) of
                {ok, Params} ->
                    request_initiate(Method, URL, Params, Timeout,
                                     State, Dispatcher);
                {error, _} ->
                    response_error_authorization_type(State)
            end;
        {error, _} ->
            response_error_authorization_type(State)
    end;
request("authorize/get", _Name, _Pattern, RequestHeaders, RequestQS, Timeout,
        #state{url_host = URLHost,
               host = Host,
               authentication_f = AuthenticationF} = State, Dispatcher) ->
    Method = "GET",
    case url(Method, RequestHeaders, URLHost, Host, RequestQS) of
        {ok, _} ->
            Authenticated = if
                AuthenticationF =:= undefined ->
                    true;
                is_function(AuthenticationF) ->
                    AuthenticationF(RequestHeaders, RequestQS, Timeout)
            end,
            if
                Authenticated =:= true ->
                    case lists:keyfind(<<"oauth_token">>, 1, RequestQS) of
                        {_, TokenRequest} ->
                            request_authorize(RequestQS, TokenRequest,
                                              Timeout, State, Dispatcher);
                        false ->
                            response_error_authorization_type(State)
                    end;
                Authenticated =:= false ->
                    response_error_authorization_type(State)
            end;
        {error, _} ->
            response_error_authorization_type(State)
    end;
request("token/post", _Name, _Pattern, RequestHeaders, Request, Timeout,
        #state{url_host = URLHost,
               host = Host} = State, Dispatcher) ->
    Method = "POST",
    case url(Method, RequestHeaders, URLHost, Host, Request) of
        {ok, URL} ->
            case oauth_parameters(Method, RequestHeaders, Request) of
                {ok, Params} ->
                    request_token(Method, URL, Params, Timeout,
                                  State, Dispatcher);
                {error, _} ->
                    response_error_authorization_type(State)
            end;
        {error, _} ->
            response_error_authorization_type(State)
    end;
request("token/delete", _Name, _Pattern, RequestHeaders, Request, Timeout,
        #state{url_host = URLHost,
               host = Host} = State, Dispatcher) ->
    Method = "DELETE",
    case url(Method, RequestHeaders, URLHost, Host, Request) of
        {ok, URL} ->
            case oauth_parameters(Method, RequestHeaders, Request) of
                {ok, Params} ->
                    request_delete(Method, URL, Params, Timeout,
                                   State, Dispatcher);
                {error, _} ->
                    response_error_authorization_type(State)
            end;
        {error, _} ->
            response_error_authorization_type(State)
    end;
request("verify*", Name, Pattern, RequestHeaders, Request, Timeout,
        #state{url_host = URLHost,
               host = Host} = State, Dispatcher) ->
    [NextName] = cloudi_service_name:parse(Name, Pattern),
    Method = cloudi_string:uppercase(cloudi_string:afterr($/, NextName)),
    case url(Method, RequestHeaders, URLHost, Host, Request) of
        {ok, URL} ->
            case oauth_parameters(Method, RequestHeaders, Request) of
                {ok, Params} ->
                    request_access(Method, URL, Params,
                                   NextName, RequestHeaders, Request, Timeout,
                                   State, Dispatcher);
                {error, _} ->
                    response_error_authorization_type(State)
            end;
        {error, _} ->
            response_error_authorization_type(State)
    end.

url(Method, RequestHeaders, URLHost, Host, Request) ->
    {ok, URLPath} = cloudi_key_value:find(<<"url-path">>,
                                                  RequestHeaders),
    case cloudi_key_value:find(<<"host">>, RequestHeaders) of
        {ok, Host} when Method == "GET" ->
            {ok, URLHost ++ erlang:binary_to_list(URLPath) ++ "?" ++
                 erlang:binary_to_list(cloudi_x_cow1_qs:qs(Request))};
        {ok, Host} ->
            {ok, URLHost ++ erlang:binary_to_list(URLPath)};
        {ok, HostInvalid} ->
            % from http://tools.ietf.org/html/rfc5849#section-3.4.1.2
            ?LOG_WARN("host invalid: ~s", [HostInvalid]),
            {error, host_invalid};
        error ->
            {error, host_missing}
    end.

oauth_parameters(Method, RequestHeaders, Request) ->
    QS = if
        Method == "GET" ->
            [{erlang:binary_to_list(K), erlang:binary_to_list(V)} ||
             {K, V} <- Request];
        true ->
            []
    end,
    case cloudi_key_value:find(<<"authorization">>, RequestHeaders) of
        {ok, <<Scheme:48/bits, OAuth/binary>>}
            when Scheme == <<"OAuth ">> ->
            case cloudi_service_oauth1_parse:authorization(OAuth) of
                [_ | _] = Params ->
                    % from http://tools.ietf.org/html/rfc5849#section-3.2
                    case lists:keyfind("oauth_version", 1, Params) of
                        {_, "1.0"} ->
                            {ok, QS ++ Params};
                        {_, _} ->
                            {error, oauth_version};
                        false ->
                            {ok, QS ++ Params}
                    end;
                {error, badarg} = Error ->
                    Error
            end;
        {ok, _} ->
            {error, not_oauth};
        error ->
            {error, not_found}
    end.

-spec oauth_verify(Signature :: string(),
                   Method :: string(),
                   URL :: string(),
                   Params :: list({string(), string()}),
                   Consumer :: {string(), string(),
                                plaintext | hmac_sha1 | rsa_sha1},
                   TokenSecret :: string()) ->
    boolean().

oauth_verify(Signature, Method, URL, Params,
             {_, _, _} = Consumer, TokenSecret) ->
    try cloudi_service_oauth1_data:verify(Signature, Method, URL,
                                         Params, Consumer, TokenSecret) of
        true ->
            true;
        false ->
            % invalid oauth_signature
            false
    catch
        ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
            % implementation error?
            ?LOG_ERROR("oauth failed ~p ~p~n ~p",
                       [ErrorType, Error, ErrorStackTrace]),
            false
    end.

initiate_input_verify(Realm, ConsumerKey, "PLAINTEXT" = SignatureMethod,
                      Signature, Timestamp, NonceRequest, Params, _Timeout,
                      _State, _Dispatcher) ->
    {ok, {Realm, ConsumerKey, SignatureMethod,
          Signature, Timestamp, NonceRequest}, Params};
initiate_input_verify(Realm, ConsumerKey, SignatureMethod,
                      Signature, Timestamp, NonceRequest, Params, Timeout,
                      #state{database_module = DatabaseModule,
                             database = Database}, Dispatcher) ->
    % check to make sure nonce/timestamp/token (if present) have not
    % been used before for "HMAC-SHA1" or "RSA-SHA1" signature methods
    % from http://tools.ietf.org/html/rfc5849#section-3.2
    % (Timestamp is client-side time, so that is ignored.  Just verifying
    %  all the nonces for a single access token are different)
    case DatabaseModule:token_request_check(Dispatcher, Database,
                                            Realm, ConsumerKey,
                                            NonceRequest, Timeout) of
        ok ->
            {ok, {Realm, ConsumerKey, SignatureMethod,
                  Signature, Timestamp, NonceRequest}, Params};
        {error, Reason} = Error ->
            ?LOG_ERROR("request token check error (~p, ~p): ~p",
                       [Realm, ConsumerKey, Reason]),
            Error
    end.

initiate_input_check(Realm, ConsumerKey, "PLAINTEXT" = SignatureMethod,
                     Signature, Params, Timeout, State, Dispatcher) ->
    % from http://tools.ietf.org/html/rfc5849#section-3.1
    Timestamp = null,
    NonceRequest = null,
    initiate_input_verify(Realm, ConsumerKey, SignatureMethod,
                          Signature, Timestamp, NonceRequest, Params, Timeout,
                          State, Dispatcher);
initiate_input_check(Realm, ConsumerKey, SignatureMethod,
                     Signature, Params, Timeout, State, Dispatcher) ->
    case lists:keyfind("oauth_timestamp", 1, Params) of
        {_, Timestamp} ->
            case lists:keyfind("oauth_nonce", 1, Params) of
                {_, NonceRequest} ->
                    initiate_input_verify(Realm, ConsumerKey, SignatureMethod,
                                          Signature, Timestamp, NonceRequest,
                                          Params, Timeout, State, Dispatcher);
                false ->
                    {error, nonce_missing}
            end;
        false ->
            {error, timestamp_missing}
    end.

initiate_input(Params, Timeout, State, Dispatcher) ->
    case lists:keyfind("oauth_consumer_key", 1, Params) of
        {_, ConsumerKey} ->
            case lists:keyfind("oauth_signature_method", 1, Params) of
                {_, SignatureMethod} ->
                    case clean_input(Params) of
                        {ok, Signature, Realm, NewParams} ->
                            initiate_input_check(Realm, ConsumerKey,
                                                 SignatureMethod, Signature,
                                                 NewParams, Timeout,
                                                 State, Dispatcher);
                        {error, _} = Error ->
                            Error
                    end;
                false ->
                    {error, signature_method_missing}
            end;
        false ->
            {error, consumer_key_missing}
    end.

request_initiate_store(_Realm, _ConsumerKey, _SignatureMethod,
                       _ClientSharedSecret,
                       _Timestamp, _NonceRequest, _Callback, _Timeout,
                       #state{debug = true} = State, _Dispatcher) ->
    % example from http://tools.ietf.org/html/rfc5849#section-1.2
    {reply,
     [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
     <<"oauth_token=hh5s93j4hdidpola&oauth_token_secret=hdhd0244k9j7ao03&"
       "oauth_callback_confirmed=true">>,
     State};
request_initiate_store(Realm, ConsumerKey, SignatureMethod,
                       ClientSharedSecret,
                       Timestamp, NonceRequest, Callback, Timeout,
                       #state{database_module = DatabaseModule,
                              database = Database,
                              token_request_expiration =
                                  TokenRequestExpiration} =
                       State, Dispatcher) ->
    % provide request token and secret
    TokenRequest = token_request(State),
    TokenRequestSecret = token_request_secret(State),
    {CallbackURL, CallbackQS} = cloudi_string:splitl($?, Callback, input),
    case DatabaseModule:token_request_store(Dispatcher, Database,
                                            Realm, ConsumerKey,
                                            SignatureMethod, ClientSharedSecret,
                                            Timestamp, NonceRequest,
                                            TokenRequest, TokenRequestSecret,
                                            CallbackURL, CallbackQS,
                                            TokenRequestExpiration,
                                            Timeout) of
        ok ->
            L = ["oauth_token=", TokenRequest,
                 "&oauth_token_secret=", TokenRequestSecret,
                 "&oauth_callback_confirmed=true"],
            {reply,
             [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
             erlang:iolist_to_binary(L),
             State};
        {error, Reason} ->
            ?LOG_ERROR("token request store error (~p, ~p): ~p",
                       [Realm, ConsumerKey, Reason]),
            response_error(400, [], State)
    end.

request_initiate_verify(Signature, Method, URL, Params,
                        {_, ClientSharedSecret, _} = Consumer,
                        Realm, ConsumerKey,
                        SignatureMethod, Timestamp, NonceRequest,
                        CallbackRegex, Timeout,
                        State, Dispatcher) ->
    % from http://tools.ietf.org/html/rfc5849#section-2.1
    % (n.b., http://tools.ietf.org/html/rfc5849#section-3.2)
    case lists:keyfind("oauth_callback", 1, Params) of
        {_, Callback} ->
            case re:run(Callback, CallbackRegex) of
                {match, _} ->
                    case oauth_verify(Signature, Method, URL,
                                      Params, Consumer, "") of
                        true ->
                            request_initiate_store(Realm, ConsumerKey,
                                                   SignatureMethod,
                                                   ClientSharedSecret,
                                                   Timestamp, NonceRequest,
                                                   Callback, Timeout,
                                                   State, Dispatcher);
                        false ->
                            response_error_authorization_type(State)
                    end;
                nomatch ->
                    response_error_authorization_type(State)
            end;
        false ->
            response_error(400, [], State)
    end.

signature_method_select("PLAINTEXT", {PLAINTEXT, _, _}) ->
    if
        PLAINTEXT =:= null ->
            {error, signature_method_plaintext_invalid};
        true ->
            {ok, {erlang:binary_to_list(PLAINTEXT), plaintext}}
    end;
signature_method_select("HMAC-SHA1", {_, HMAC_SHA1, _}) ->
    if
        HMAC_SHA1 =:= null ->
            {error, signature_method_hmac_sha1_invalid};
        true ->
            {ok, {erlang:binary_to_list(HMAC_SHA1), hmac_sha1}}
    end;
signature_method_select("RSA-SHA1", {_, _, RSA_SHA1}) ->
    if
        RSA_SHA1 =:= null ->
            {error, signature_method_rsa_sha1_invalid};
        true ->
            % keep the certificate contents as binary data
            {ok, {RSA_SHA1, rsa_sha1}}
    end;
signature_method_select(_, {_, _, _}) ->
    {error, signature_method_unsupported}.

request_initiate(Method, URL, Params, Timeout,
                 #state{database_module = DatabaseModule,
                        database = Database} = State,
                 Dispatcher) ->
    % similar to: oauth_mochiweb_server:serve_oauth_request_token/1
    % (from the https://github.com/tim/erlang-oauth-examples repository)
    % (n.b., http://tools.ietf.org/html/rfc5849#section-3.2)
    case initiate_input(Params, Timeout, State, Dispatcher) of
        {ok, {Realm, ConsumerKey, SignatureMethod, Signature,
              Timestamp, NonceRequest}, NewParams} ->
            case DatabaseModule:signature_methods(Dispatcher, Database,
                                                  Realm, ConsumerKey,
                                                  Timeout) of
                {ok, {_, _, _} = SignatureMethods, CallbackRegex} ->
                    case signature_method_select(SignatureMethod,
                                                 SignatureMethods) of
                        {ok, {ClientSharedSecret, SignatureMethodType}} ->
                            Consumer = {ConsumerKey,
                                        ClientSharedSecret,
                                        SignatureMethodType},
                            request_initiate_verify(Signature, Method, URL,
                                                    NewParams, Consumer,
                                                    Realm, ConsumerKey,
                                                    SignatureMethod,
                                                    Timestamp, NonceRequest,
                                                    CallbackRegex, Timeout,
                                                    State, Dispatcher);
                        {error, signature_method_unsupported} ->
                            response_error(400, [], State);
                        {error, Reason} ->
                            % signature method invalid
                            ?LOG_ERROR("signature method error (~p, ~p): ~p",
                                       [Realm, ConsumerKey, Reason]),
                            response_error_authorization_type(State)
                    end;
                {error, not_found} ->
                    response_error_authorization_type(State);
                {error, Reason} ->
                    % unable to get signature methods
                    ?LOG_ERROR("signature methods error (~p, ~p): ~p",
                               [Realm, ConsumerKey, Reason]),
                    response_error(400, [], State)
            end;
        {error, nonce_exists} ->
            response_error_authorization_type(State);
        {error, _} ->
            % missing parameters
            response_error(400, [], State)
    end.

callback_merge_qs_filter_callback(CallbackQS) ->
    callback_merge_qs_filter_callback(CallbackQS, []).

callback_merge_qs_filter_callback([], Output) ->
    lists:reverse(Output);
callback_merge_qs_filter_callback([{<<Prefix:48, _/binary>>, _} |
                                   CallbackQS], Output)
    when Prefix == <<"oauth_">> ->
    callback_merge_qs_filter_callback(CallbackQS, Output);
callback_merge_qs_filter_callback([Entry | CallbackQS], Output) ->
    callback_merge_qs_filter_callback(CallbackQS, [Entry | Output]).

callback_merge_qs_filter_request(RequestQS, CallbackQS) ->
    callback_merge_qs_filter_request(RequestQS, [], CallbackQS).

callback_merge_qs_filter_request([], Output, _) ->
    lists:reverse(Output);
callback_merge_qs_filter_request([{<<"oauth_token">>, _} = Entry | 
                                  RequestQS], Output, CallbackQS) ->
    callback_merge_qs_filter_request(RequestQS, [Entry | Output], CallbackQS);
callback_merge_qs_filter_request([{<<Prefix:48, _/binary>>, _} |
                                  RequestQS], Output, CallbackQS)
    when Prefix == <<"oauth_">> ->
    callback_merge_qs_filter_request(RequestQS, Output, CallbackQS);
callback_merge_qs_filter_request([{K, _} = Entry |
                                  RequestQS], Output, CallbackQS) ->
    case lists:keyfind(K, 1, CallbackQS) of
        {_, _} ->
            % should not overwrite a callback qs value
            callback_merge_qs_filter_request(RequestQS, Output, CallbackQS);
        false ->
            callback_merge_qs_filter_request(RequestQS,
                                             [Entry | Output], CallbackQS)
    end.

callback_merge(CallbackURL, CallbackQS, RequestQS, Verifier) ->
    NewCallbackQS = callback_merge_qs_filter_callback(CallbackQS),
    NewRequestQS = callback_merge_qs_filter_request(RequestQS, CallbackQS),
    QS = cloudi_x_cow1_qs:qs(NewCallbackQS ++ NewRequestQS ++
                   [{<<"oauth_verifier">>, Verifier}]),
    <<CallbackURL/binary, $?, QS/binary>>.

request_authorize([{<<"oauth_token">>, <<"hh5s93j4hdidpola">>}] = RequestQS,
                  <<"hh5s93j4hdidpola">>, _Timeout,
                  #state{debug = true} = State, _Dispatcher) ->
    % example from http://tools.ietf.org/html/rfc5849#section-1.2
    Verifier = <<"hfdp7dh39dks9884">>,
    CallbackURL = <<"http://printer.example.com/ready">>,
    Callback = callback_merge(CallbackURL, [], RequestQS, Verifier),
    response_redirect(Callback, State);
request_authorize(RequestQS, TokenRequest, Timeout,
                  #state{database_module = DatabaseModule,
                         database = Database} = State, Dispatcher) ->
    % based on http://tools.ietf.org/html/rfc5849#section-2.2
    Verifier = erlang:list_to_binary(verifier(State)),
    case DatabaseModule:token_request_find(Dispatcher, Database,
                                           TokenRequest, Timeout) of
        {ok, <<"oob">>, <<"">>} ->
            % based on http://tools.ietf.org/html/rfc5849#section-2.1
            case DatabaseModule:token_request_update(Dispatcher, Database,
                                                     TokenRequest, Verifier,
                                                     Timeout) of
                ok ->
                    {reply,
                     [{<<"content-type">>,
                       <<"application/x-www-form-urlencoded">>}],
                     <<(<<"oauth_token=">>)/binary, TokenRequest/binary, $&,
                       (<<"oauth_verifier=">>)/binary, Verifier/binary>>,
                     State};
                {error, Reason} ->
                    ?LOG_ERROR("token request update error (~p): ~p",
                               [TokenRequest, Reason]),
                    response_error(400, [], State)
            end;
        {ok, CallbackURL, CallbackQS} ->
            Callback = callback_merge(CallbackURL,
                                      cloudi_x_cow1_qs:parse_qs(CallbackQS),
                                      RequestQS, Verifier),
            case DatabaseModule:token_request_update(Dispatcher, Database,
                                                     TokenRequest, Verifier,
                                                     Timeout) of
                ok ->
                    response_redirect(Callback, State);
                {error, Reason} ->
                    ?LOG_ERROR("token request update error (~p): ~p",
                               [TokenRequest, Reason]),
                    response_error(400, [], State)
            end;
        {error, Reason} ->
            ?LOG_WARN("token request find error (~p): ~p",
                      [TokenRequest, Reason]),
            response_error_authorization_type(State)
    end.

token_input_verify(Realm, ConsumerKey, SignatureMethod,
                   Signature, Timestamp, NonceAccess,
                   "hh5s93j4hdidpola" = TokenRequest,
                   _Verifier, Params, _Timeout,
                   #state{debug = true}, _Dispatcher) ->
    % example from http://tools.ietf.org/html/rfc5849#section-1.2
    {ok, {Realm, ConsumerKey, SignatureMethod,
          "kd94hf93k423kf44", Signature, Timestamp, "wIjqoS", NonceAccess,
          TokenRequest, "hdhd0244k9j7ao03"}, Params};
token_input_verify(Realm, ConsumerKey, SignatureMethod,
                   Signature, Timestamp, NonceAccess,
                   TokenRequest, Verifier, Params, Timeout,
                   #state{database_module = DatabaseModule,
                          database = Database}, Dispatcher) ->
    case DatabaseModule:token_request_verify(Dispatcher, Database,
                                             Realm, ConsumerKey,
                                             SignatureMethod, Timestamp,
                                             NonceAccess, TokenRequest,
                                             Verifier, Timeout) of
        {ok, ClientSharedSecret, NonceRequest, TokenRequestSecret} ->
            {ok, {Realm, ConsumerKey, SignatureMethod,
                  erlang:binary_to_list(ClientSharedSecret),
                  Signature, Timestamp,
                  erlang:binary_to_list(NonceRequest),
                  NonceAccess, TokenRequest,
                  erlang:binary_to_list(TokenRequestSecret)}, Params};
        {error, Reason} = Error ->
            ?LOG_ERROR("request token check error (~p, ~p): ~p",
                       [Realm, ConsumerKey, Reason]),
            Error
    end.

token_input_check(Realm, ConsumerKey, "PLAINTEXT" = SignatureMethod,
                  Signature, TokenRequest, Verifier,
                  Params, Timeout, State, Dispatcher) ->
    % from http://tools.ietf.org/html/rfc5849#section-3.1
    Timestamp = null,
    NonceAccess = null,
    token_input_verify(Realm, ConsumerKey, SignatureMethod,
                       Signature, Timestamp, NonceAccess,
                       TokenRequest, Verifier, Params, Timeout,
                       State, Dispatcher);
token_input_check(Realm, ConsumerKey, SignatureMethod,
                  Signature, TokenRequest, Verifier,
                  Params, Timeout, State, Dispatcher) ->
    case lists:keyfind("oauth_timestamp", 1, Params) of
        {_, Timestamp} ->
            case lists:keyfind("oauth_nonce", 1, Params) of
                {_, NonceAccess} ->
                    token_input_verify(Realm, ConsumerKey, SignatureMethod,
                                       Signature, Timestamp, NonceAccess,
                                       TokenRequest, Verifier,
                                       Params, Timeout,
                                       State, Dispatcher);
                false ->
                    {error, nonce_missing}
            end;
        false ->
            {error, timestamp_missing}
    end.

token_input(Params, Timeout, State, Dispatcher) ->
    case lists:keyfind("oauth_consumer_key", 1, Params) of
        {_, ConsumerKey} ->
            case lists:keyfind("oauth_signature_method", 1, Params) of
                {_, SignatureMethod} ->
                    case lists:keyfind("oauth_token", 1, Params) of
                        {_, TokenRequest} ->
                            case lists:keyfind("oauth_verifier", 1, Params) of
                                {_, Verifier} ->
                                    case clean_input(Params) of
                                        {ok, Signature, Realm, NewParams} ->
                                            token_input_check(Realm,
                                                              ConsumerKey,
                                                              SignatureMethod,
                                                              Signature,
                                                              TokenRequest,
                                                              Verifier,
                                                              NewParams,
                                                              Timeout,
                                                              State,
                                                              Dispatcher);
                                        {error, _} = Error ->
                                            Error
                                    end;
                                false ->
                                    {error, verifier_missing}
                            end;
                        false ->
                            {error, token_missing}
                    end;
                false ->
                    {error, signature_method_missing}
            end;
        false ->
            {error, consumer_key_missing}
    end.

request_token_access(_Realm, _ConsumerKey, _SignatureMethod,
                     _ClientSharedSecret, _Timestamp,
                     _NonceRequest, _NonceAccess, _TokenRequest, _Timeout,
                     #state{debug = true} = State, _Dispatcher) ->
    % example from http://tools.ietf.org/html/rfc5849#section-1.2
    {reply,
     [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
     <<"oauth_token=nnch734d00sl2jdk&oauth_token_secret=pfkkdhi9sl3r4s00">>,
     State};
request_token_access(Realm, ConsumerKey, SignatureMethod,
                     ClientSharedSecret, Timestamp,
                     NonceRequest, NonceAccess, TokenRequest, Timeout,
                     #state{database_module = DatabaseModule,
                            database = Database,
                            token_access_expiration = TokenAccessExpiration} =
                     State, Dispatcher) ->
    TokenAccess = token_access(State),
    TokenAccessSecret = token_access_secret(State),
    case DatabaseModule:token_access_store(Dispatcher, Database,
                                           Realm, ConsumerKey, SignatureMethod,
                                           ClientSharedSecret, Timestamp,
                                           NonceRequest, NonceAccess,
                                           TokenAccess, TokenAccessSecret,
                                           TokenAccessExpiration, Timeout) of
        ok ->
            % ignore delete failure if it occurs
            % (can not use a transaction due to using an extended query, to
            %  avoid SQL injection problems, if a failure does occur,
            %  the expiration of the request token can still happen based on
            %  time and the access token can still be used instead)
            DatabaseModule:token_request_delete(Dispatcher, Database,
                                                TokenRequest, Timeout),
            L = ["oauth_token=", TokenAccess,
                 "&oauth_token_secret=", TokenAccessSecret],
            {reply,
             [{<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
             erlang:iolist_to_binary(L),
             State};
        {error, Reason} ->
            ?LOG_ERROR("token access store error (~p, ~p): ~p",
                       [Realm, ConsumerKey, Reason]),
            response_error(400, [], State)
    end.

request_token(Method, URL, Params, Timeout,
              #state{} = State, Dispatcher) ->
    % similar to: oauth_mochiweb_server:serve_oauth_access_token/1
    % (from the https://github.com/tim/erlang-oauth-examples repository)
    % (n.b., http://tools.ietf.org/html/rfc5849#section-3.2)
    case token_input(Params, Timeout, State, Dispatcher) of
        {ok, {Realm, ConsumerKey, SignatureMethod, ClientSharedSecret,
              Signature, Timestamp, NonceRequest, NonceAccess,
              TokenRequest, TokenRequestSecret}, NewParams} ->
            SignatureMethodType = if
                SignatureMethod == "PLAINTEXT" ->
                    plaintext;
                SignatureMethod == "HMAC-SHA1" ->
                    hmac_sha1;
                SignatureMethod == "RSA-SHA1" ->
                    rsa_sha1
            end,
            Consumer = {ConsumerKey, ClientSharedSecret, SignatureMethodType},
            case oauth_verify(Signature, Method, URL, NewParams, Consumer,
                              TokenRequestSecret) of
                true ->
                    request_token_access(Realm, ConsumerKey, SignatureMethod,
                                         ClientSharedSecret, Timestamp,
                                         NonceRequest, NonceAccess,
                                         TokenRequest, Timeout,
                                         State, Dispatcher);
                false ->
                    response_error_authorization_type(State)
            end;
        {error, not_found} ->
            response_error_authorization_type(State);
        {error, _} ->
            % missing parameters
            response_error(400, [], State)
    end.

access_input_verify(Realm, ConsumerKey, SignatureMethod,
                    Signature, Timestamp, Nonce,
                    "nnch734d00sl2jdk" = TokenAccess, Params, _Timeout,
                    #state{debug = true}, _Dispatcher) ->
    % example from http://tools.ietf.org/html/rfc5849#section-1.2
    {ok, {Realm, ConsumerKey, SignatureMethod, "kd94hf93k423kf44",
          Signature, Timestamp, Nonce, TokenAccess,
          "pfkkdhi9sl3r4s00"}, Params};
access_input_verify(Realm, ConsumerKey, SignatureMethod,
                    Signature, Timestamp, Nonce,
                    TokenAccess, Params, Timeout,
                    #state{database_module = DatabaseModule,
                           database = Database}, Dispatcher) ->
    case DatabaseModule:token_access_verify(Dispatcher, Database,
                                            Realm, ConsumerKey,
                                            SignatureMethod, Timestamp,
                                            Nonce, TokenAccess, Timeout) of
        {ok, ClientSharedSecret, TokenAccessSecret} ->
            {ok, {Realm, ConsumerKey, SignatureMethod,
                  erlang:binary_to_list(ClientSharedSecret),
                  Signature, Timestamp, Nonce, TokenAccess,
                  erlang:binary_to_list(TokenAccessSecret)}, Params};
        {error, Reason} = Error ->
            ?LOG_ERROR("access token check error (~p, ~p): ~p",
                       [Realm, ConsumerKey, Reason]),
            Error
    end.

access_input_check(Realm, ConsumerKey, "PLAINTEXT" = SignatureMethod,
                   Signature, TokenAccess,
                   Params, Timeout, State, Dispatcher) ->
    % from http://tools.ietf.org/html/rfc5849#section-3.1
    Timestamp = null,
    Nonce = null,
    access_input_verify(Realm, ConsumerKey, SignatureMethod,
                        Signature, Timestamp, Nonce,
                        TokenAccess, Params, Timeout,
                        State, Dispatcher);
access_input_check(Realm, ConsumerKey, SignatureMethod,
                   Signature, TokenAccess,
                   Params, Timeout, State, Dispatcher) ->
    case lists:keyfind("oauth_timestamp", 1, Params) of
        {_, Timestamp} ->
            case lists:keyfind("oauth_nonce", 1, Params) of
                {_, Nonce} ->
                    access_input_verify(Realm, ConsumerKey, SignatureMethod,
                                        Signature, Timestamp, Nonce,
                                        TokenAccess, Params, Timeout,
                                        State, Dispatcher);
                false ->
                    {error, nonce_missing}
            end;
        false ->
            {error, timestamp_missing}
    end.

access_input(Params, Timeout, State, Dispatcher) ->
    case lists:keyfind("oauth_consumer_key", 1, Params) of
        {_, ConsumerKey} ->
            case lists:keyfind("oauth_signature_method", 1, Params) of
                {_, SignatureMethod} ->
                    case lists:keyfind("oauth_token", 1, Params) of
                        {_, TokenAccess} ->
                            case clean_input(Params) of
                                {ok, Signature, Realm, NewParams} ->
                                    access_input_check(Realm,
                                                       ConsumerKey,
                                                       SignatureMethod,
                                                       Signature,
                                                       TokenAccess,
                                                       NewParams,
                                                       Timeout,
                                                       State,
                                                       Dispatcher);
                                {error, _} = Error ->
                                    Error
                            end;
                        false ->
                            {error, token_missing}
                    end;
                false ->
                    {error, signature_method_missing}
            end;
        false ->
            {error, consumer_key_missing}
    end.

request_delete(Method, URL, Params, Timeout,
               #state{database_module = DatabaseModule,
                      database = Database} = State, Dispatcher) ->
    case access_input(Params, Timeout, State, Dispatcher) of
        {ok, {Realm, ConsumerKey, SignatureMethod, ClientSharedSecret,
              Signature, _Timestamp, _Nonce,
              TokenAccess, TokenAccessSecret}, NewParams} ->
            SignatureMethodType = if
                SignatureMethod == "PLAINTEXT" ->
                    plaintext;
                SignatureMethod == "HMAC-SHA1" ->
                    hmac_sha1;
                SignatureMethod == "RSA-SHA1" ->
                    rsa_sha1
            end,
            Consumer = {ConsumerKey, ClientSharedSecret, SignatureMethodType},
            case oauth_verify(Signature, Method, URL, NewParams, Consumer,
                              TokenAccessSecret) of
                true ->
                    % OAuth verification of the access token was successful,
                    % delete the access token
                    case DatabaseModule:token_request_delete(Dispatcher,
                                                             Database,
                                                             TokenAccess,
                                                             Timeout) of
                        ok ->
                            {reply, [], <<>>, State};
                        {error, Reason} ->
                            ?LOG_ERROR("access token delete error (~p, ~p): ~p",
                                       [Realm, ConsumerKey, Reason]),
                            response_error(400, [], State)
                    end;
                false ->
                    response_error_authorization_type(State)
            end;
        {error, not_found} ->
            response_error_authorization_type(State);
        {error, _} ->
            % missing parameters
            response_error(400, [], State)
    end.

request_access(Method, URL, Params, NextName, RequestHeaders, Request, Timeout,
               #state{} = State, Dispatcher) ->
    case access_input(Params, Timeout, State, Dispatcher) of
        {ok, {_Realm, ConsumerKey, SignatureMethod, ClientSharedSecret,
              Signature, _Timestamp, _Nonce,
              _TokenAccess, TokenAccessSecret}, NewParams} ->
            SignatureMethodType = if
                SignatureMethod == "PLAINTEXT" ->
                    plaintext;
                SignatureMethod == "HMAC-SHA1" ->
                    hmac_sha1;
                SignatureMethod == "RSA-SHA1" ->
                    rsa_sha1
            end,
            Consumer = {ConsumerKey, ClientSharedSecret, SignatureMethodType},
            case oauth_verify(Signature, Method, URL, NewParams, Consumer,
                              TokenAccessSecret) of
                true ->
                    % OAuth verification of the access token was successful,
                    % forward the service request to a different service
                    {forward, NextName, RequestHeaders, Request, State};
                false ->
                    response_error_authorization_type(State)
            end;
        {error, not_found} ->
            response_error_authorization_type(State);
        {error, _} ->
            % missing parameters
            response_error(400, [], State)
    end.

response_redirect(Callback, State) ->
    % from http://tools.ietf.org/html/rfc5849#section-2.2
    {reply,
     [{<<"status">>, <<"302">>}, % 302 instead of 303 or 307 for compatibility
      {<<"location">>, Callback}],
     <<>>,
     State}.

response_error_authorization_type(State) ->
    % from http://tools.ietf.org/html/rfc2617#section-1.2
    response_error(401, [{<<"www-authenticate">>, <<"OAuth">>}], State).

response_error(StatusCode, ResponseHeaders, State)
    when is_integer(StatusCode), StatusCode >= 400, StatusCode =< 599 ->
    {reply,
     [{<<"status">>, erlang:integer_to_binary(StatusCode)} | ResponseHeaders],
     <<>>,
     State}.

clean_input(Params) ->
    case lists:keytake("oauth_signature", 1, Params) of
        {value, {_, Signature}, NextParams} ->
            case lists:keytake("realm", 1, NextParams) of
                {value, {_, Realm}, NewParams} ->
                    {ok, Signature, Realm, NewParams};
                false ->
                    {ok, Signature, null, NextParams}
            end;
        false ->
            {error, signature_missing}
    end.

token_i_to_list(I) when is_integer(I), I > 0 ->
    token_i_to_list([], I).

token_i_to_list(L, I)
    when I < 62 ->
    [token_i_to_alphanum(I) | L];
token_i_to_list(L, I) ->
    token_i_to_list([token_i_to_alphanum(I rem 62) | L], I div 62).

-compile({inline, [{token_i_to_alphanum,1}]}).

% modulus 62
token_i_to_alphanum(I) when 0 =< I, I =< 9 ->
    I + $0;
token_i_to_alphanum(I) when 10 =< I, I =< 35 ->
    (I - 10) + $a;
token_i_to_alphanum(I) when 36 =< I, I =< 61 ->
    (I - 36) + $A.

-spec token(Random :: binary()) ->
    string().

token(Random) when is_binary(Random) ->
    % avoids percent encoding problems by using only alphanum characters
    token_i_to_list(binary:decode_unsigned(Random, big)).

token_length_to_bytes(Length) ->
    % will always have at least Length characters assuming typical randomness
    % (Length+1 will be created if an extra byte is necessary for the
    %  last character)
    erlang:round((Length * (math:log(62) / math:log(2))) / 8).

token_request(#state{token_request_bytes = N}) ->
    token(crypto:strong_rand_bytes(N)).

token_request_secret(#state{token_request_secret_bytes = N}) ->
    token(crypto:strong_rand_bytes(N)).

token_access(#state{token_access_bytes = N}) ->
    token(crypto:strong_rand_bytes(N)).

token_access_secret(#state{token_access_secret_bytes = N}) ->
    token(crypto:strong_rand_bytes(N)).

verifier(#state{verifier_bytes = N}) ->
    token(crypto:strong_rand_bytes(N)).

