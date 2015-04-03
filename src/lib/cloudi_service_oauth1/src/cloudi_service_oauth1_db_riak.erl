%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OAuth v1.0 Riak DB Interface==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014-2015 Michael Truog
%%% @version 1.5.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_oauth1_db_riak).
-author('mjtruog [at] gmail (dot) com').
-behaviour(cloudi_service_oauth1_db).

%% external interface
-export([initialize/3,
         signature_methods/5,
         tokens_clean/2,
         token_request_check/6,
         token_request_store/14,
         token_request_find/4,
         token_request_update/5,
         token_request_verify/10,
         token_request_delete/4,
         token_access_store/13,
         token_access_verify/9]).

-define(RIAK_BUCKET_CONFIGURATION, "oauth_configuration").
-define(RIAK_BUCKET_TOKEN_REQUEST, "oauth_token_request").
-define(RIAK_BUCKET_TOKEN_ACCESS, "oauth_token_access").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec initialize(Dispatcher :: cloudi_service:dispatcher(),
                 Database :: cloudi_service:service_name(),
                 Debug :: boolean()) ->
    ok | {error, any()}.

initialize(Dispatcher, Database, Debug) ->
    if
        Debug =:= true ->
            riak_configuration_rfc5849_test_data(Dispatcher, Database);
        Debug =:= false ->
            ok
    end.

-spec signature_methods(Dispatcher :: cloudi_service:dispatcher(),
                        Database :: cloudi_service:service_name(),
                        Realm :: string() | binary() | null,
                        ConsumerKey :: string() | binary(),
                        Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, {PLAINTEXT :: binary() | null,
          HMAC_SHA1 :: binary() | null,
          RSA_SHA1 :: binary() | null}, CallbackRegex :: binary()} |
    {error, any()}.

signature_methods(Dispatcher, Database, Realm, ConsumerKey, Timeout) ->
    riak_signature_methods(Dispatcher, Database, Realm, ConsumerKey, Timeout).

-spec tokens_clean(Dispatcher :: cloudi_service:dispatcher(),
                   Database :: cloudi_service:service_name()) ->
    ok | {error, any()}.

tokens_clean(Dispatcher, Database) ->
    riak_tokens_clean(Dispatcher, Database).

-spec token_request_check(Dispatcher :: cloudi_service:dispatcher(),
                          Database :: cloudi_service:service_name(),
                          Realm :: string() | binary() | null,
                          ConsumerKey :: string() | binary(),
                          NonceRequest :: string() | binary(),
                          Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_request_check(Dispatcher, Database,
                    Realm, ConsumerKey, NonceRequest, Timeout) ->
    riak_token_request_check(Dispatcher, Database,
                              Realm, ConsumerKey, NonceRequest, Timeout).

-spec token_request_store(Dispatcher :: cloudi_service:dispatcher(),
                          Database :: cloudi_service:service_name(),
                          Realm :: string() | binary() | null,
                          ConsumerKey :: string() | binary(),
                          SignatureMethod :: string() | binary(),
                          ClientSharedSecret :: string() | binary(),
                          Timestamp :: string() | binary(),
                          NonceRequest :: string() | binary(),
                          TokenRequest :: string() | binary(),
                          TokenRequestSecret :: string() | binary(),
                          CallbackURL :: string() | binary(),
                          CallbackQS :: string() | binary(),
                          ExpirationSeconds :: pos_integer(),
                          Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_request_store(Dispatcher, Database,
                    Realm, ConsumerKey, SignatureMethod, ClientSharedSecret,
                    Timestamp, NonceRequest, TokenRequest, TokenRequestSecret,
                    CallbackURL, CallbackQS, ExpirationSeconds, Timeout) ->
    riak_token_request_store(Dispatcher, Database,
                              Realm, ConsumerKey, SignatureMethod,
                              ClientSharedSecret, Timestamp,
                              NonceRequest, TokenRequest, TokenRequestSecret,
                              CallbackURL, CallbackQS,
                              ExpirationSeconds, Timeout).

-spec token_request_find(Dispatcher :: cloudi_service:dispatcher(),
                         Database :: cloudi_service:service_name(),
                         TokenRequest :: string() | binary(),
                         Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, CallbackURL :: binary(), CallbackQS :: binary()} |
    {error, any()}.

token_request_find(Dispatcher, Database, TokenRequest, Timeout) ->
    riak_token_request_find(Dispatcher, Database, TokenRequest, Timeout).

-spec token_request_update(Dispatcher :: cloudi_service:dispatcher(),
                           Database :: cloudi_service:service_name(),
                           TokenRequest :: string() | binary(),
                           Verifier :: string() | binary(),
                           Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_request_update(Dispatcher, Database, TokenRequest, Verifier, Timeout) ->
    riak_token_request_update(Dispatcher, Database, TokenRequest,
                               Verifier, Timeout).

-spec token_request_verify(Dispatcher :: cloudi_service:dispatcher(),
                           Database :: cloudi_service:service_name(),
                           Realm :: string() | binary() | null,
                           ConsumerKey :: string() | binary(),
                           SignatureMethod :: string() | binary(),
                           Timestamp :: string() | binary(),
                           NonceAccess :: string() | binary(),
                           TokenRequest :: string() | binary(),
                           Verifier :: string() | binary(),
                           Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, ClientSharedSecret :: binary(), NonceRequest :: binary(),
         TokenRequestSecret :: binary()} |
    {error, any()}.

token_request_verify(Dispatcher, Database,
                     Realm, ConsumerKey, SignatureMethod,
                     Timestamp, NonceRequest,
                     TokenRequest, Verifier, Timeout) ->
    riak_token_request_verify(Dispatcher, Database,
                               Realm, ConsumerKey, SignatureMethod,
                               Timestamp, NonceRequest,
                               TokenRequest, Verifier, Timeout).

-spec token_request_delete(Dispatcher :: cloudi_service:dispatcher(),
                           Database :: cloudi_service:service_name(),
                           TokenRequest :: string() | binary(),
                           Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_request_delete(Dispatcher, Database, TokenRequest, Timeout) ->
    riak_token_request_delete(Dispatcher, Database, TokenRequest, Timeout).

-spec token_access_store(Dispatcher :: cloudi_service:dispatcher(),
                         Database :: cloudi_service:service_name(),
                         Realm :: string() | binary() | null,
                         ConsumerKey :: string() | binary(),
                         SignatureMethod :: string() | binary(),
                         ClientSharedSecret :: string() | binary(),
                         Timestamp :: string() | binary(),
                         NonceRequest :: string() | binary(),
                         NonceAccess :: string() | binary(),
                         TokenAccess :: string() | binary(),
                         TokenAccessSecret :: string() | binary(),
                         ExpirationSeconds :: pos_integer(),
                         Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_access_store(Dispatcher, Database,
                   Realm, ConsumerKey,
                   SignatureMethod, ClientSharedSecret,
                   Timestamp, NonceRequest, NonceAccess,
                   TokenAccess, TokenAccessSecret,
                   ExpirationSeconds, Timeout) ->
    riak_token_access_store(Dispatcher, Database,
                             Realm, ConsumerKey,
                             SignatureMethod, ClientSharedSecret,
                             Timestamp, NonceRequest, NonceAccess,
                             TokenAccess, TokenAccessSecret,
                             ExpirationSeconds, Timeout).

-spec token_access_verify(Dispatcher :: cloudi_service:dispatcher(),
                          Database :: cloudi_service:service_name(),
                          Realm :: string() | binary() | null,
                          ConsumerKey :: string() | binary(),
                          SignatureMethod :: string() | binary(),
                          Timestamp :: string() | binary(),
                          Nonce :: string() | binary(),
                          TokenAccess :: string() | binary(),
                          Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, ClientSharedSecret :: binary(), TokenAccessSecret :: binary()} |
    {error, any()}.

token_access_verify(Dispatcher, Database,
                    Realm, ConsumerKey, SignatureMethod,
                    Timestamp, Nonce, TokenAccess, Timeout) ->
    riak_token_access_verify(Dispatcher, Database,
                              Realm, ConsumerKey, SignatureMethod,
                              Timestamp, Nonce, TokenAccess, Timeout).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

riak_configuration_rfc5849_test_data(Dispatcher, Database) ->
    % from http://tools.ietf.org/html/rfc5849#section-1.2
    % (i.e., printer.example.com client with
    %  photos.example.net server)
    Name = Database ++ ?RIAK_BUCKET_CONFIGURATION,
    Key = consumer_key(<<"dpf43f3p2l4k3l03">>, <<"photos">>),
    Value = configuration_to_json("dpf43f3p2l4k3l03", "photos",
                                  null, "kd94hf93k423kf44", null,
                                  "https?://printer.example.com/ready"),
    case cloudi_service_db_riak:new(Dispatcher, Name,
                                    Key, Value, undefined) of
        {{ok, _, _}, Dispatcher} ->
            ok;
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_expirations(Dispatcher, Name, Now) ->
    case cloudi_service_db_riak:get_index_range(Dispatcher, Name,
                                                {binary_index, "expiration"},
                                                <<"1970">>, Now, undefined) of
        {{ok, Keys, _, _}, Dispatcher} ->
            {ok, Keys};
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_delete(_, _, []) ->
    ok;
riak_delete(Dispatcher, Name, [Key | Keys]) ->
    case cloudi_service_db_riak:delete(Dispatcher, Name, Key, undefined) of
        {ok, Dispatcher} ->
            riak_delete(Dispatcher, Name, Keys);
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_tokens_clean(Dispatcher, Database) ->
    Now = timestamp(),
    NameRequest = Database ++ ?RIAK_BUCKET_TOKEN_REQUEST,
    NameAccess = Database ++ ?RIAK_BUCKET_TOKEN_ACCESS,
    case riak_expirations(Dispatcher, NameRequest, Now) of
        {ok, KeysRequest} ->
            case riak_delete(Dispatcher, NameRequest, KeysRequest) of
                ok ->
                    case riak_expirations(Dispatcher, NameAccess, Now) of
                        {ok, KeysAccess} ->
                            case riak_delete(Dispatcher, NameAccess,
                                             KeysAccess) of
                                ok ->
                                    ok;
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

riak_signature_methods(Dispatcher, Database, Realm, ConsumerKey, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_CONFIGURATION,
    Key = consumer_key(ConsumerKey, match_realm(Realm)),
    case cloudi_service_db_riak:get(Dispatcher, Name, Key, Timeout) of
        {{ok, Key, Value}, Dispatcher} ->
            {_, _,
             SignatureMethodPlaintext,
             SignatureMethodHMACSHA1,
             SignatureMethodRSASHA1,
             CallbackRegex} = json_to_configuration(Value),
            {ok, {SignatureMethodPlaintext,
                  SignatureMethodHMACSHA1,
                  SignatureMethodRSASHA1}, CallbackRegex};
        {{error, notfound}, Dispatcher} ->
            {error, not_found};
        {{siblings, _, _}, Dispatcher} ->
            {error, siblings};
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_nonce_check(Dispatcher, Name, Key, Nonce, Timeout) ->
    case cloudi_service_db_riak:get(Dispatcher, Name, Key, Timeout) of
        {{ok, Key, Value}, Dispatcher} ->
            JSON = cloudi_x_jsx:decode(Value),
            case lists:keyfind(<<"nonce_request">>, 1, JSON) of
                {_, Nonce} ->
                    {error, nonce_exists};
                false ->
                    case lists:keyfind(<<"nonce_access">>, 1, JSON) of
                        {_, Nonce} ->
                            {error, nonce_exists};
                        false ->
                            ok
                    end
            end;
        {{error, notfound}, Dispatcher} ->
            ok;
        {{siblings, _, _}, Dispatcher} ->
            {error, siblings};
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_request_check(Dispatcher, Database,
                         Realm, ConsumerKey, NonceRequest, Timeout) ->
    NameRequest = Database ++ ?RIAK_BUCKET_TOKEN_REQUEST,
    NameAccess = Database ++ ?RIAK_BUCKET_TOKEN_ACCESS,
    Key = consumer_key(ConsumerKey, match_realm(Realm)),
    NonceRequestMatch = match_value(NonceRequest),
    case riak_token_nonce_check(Dispatcher, NameRequest,
                                Key, NonceRequestMatch, Timeout) of
        ok ->
            case riak_token_nonce_check(Dispatcher, NameAccess,
                                        Key, NonceRequestMatch, Timeout) of
                ok ->
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

riak_token_request_store(Dispatcher, Database,
                         Realm, ConsumerKey,
                         SignatureMethod, ClientSharedSecret,
                         Timestamp, NonceRequest,
                         TokenRequest, TokenRequestSecret,
                         CallbackURL, CallbackQS,
                         ExpirationSeconds, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_TOKEN_REQUEST,
    ConsumerKeyMatch = match_value(ConsumerKey),
    RealmMatch = match_realm(Realm),
    Expiration = timestamp(ExpirationSeconds),
    TokenRequestMatch = match_value(TokenRequest),
    Key = TokenRequestMatch,
    Value = token_request_to_json(TokenRequestMatch, TokenRequestSecret,
                                  ConsumerKeyMatch, RealmMatch,
                                  SignatureMethod, ClientSharedSecret,
                                  Timestamp, NonceRequest,
                                  CallbackURL, CallbackQS,
                                  null, Expiration),
    Options = [{indexes,
                token_indexes(ConsumerKeyMatch, RealmMatch, Expiration)}],
    case cloudi_service_db_riak:new(Dispatcher, Name,
                                    Key, Value, Options, Timeout) of
        {{ok, _, _}, Dispatcher} ->
            ok;
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_request_find(Dispatcher, Database, TokenRequest, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_TOKEN_REQUEST,
    Key = match_value(TokenRequest),
    case cloudi_service_db_riak:get(Dispatcher, Name, Key, Timeout) of
        {{ok, Key, Value}, Dispatcher} ->
            JSON = cloudi_x_jsx:decode(Value),
            {_, CallbackURL} = lists:keyfind(<<"callback_url">>, 1, JSON),
            {_, CallbackQS} = lists:keyfind(<<"callback_qs">>, 1, JSON),
            {ok, CallbackURL, CallbackQS};
        {{error, notfound}, Dispatcher} ->
            {error, not_found};
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_request_update(Dispatcher, Database, TokenRequest,
                          Verifier, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_TOKEN_REQUEST,
    Key = match_value(TokenRequest),
    case cloudi_service_db_riak:get(Dispatcher, Name, Key,
                                    [{object, true}], Timeout) of
        {{ok, Key, Object}, Dispatcher} ->
            case cloudi_service_db_riak:object_value(Object) of
                {ok, Key, Value} ->
                    {TokenRequest, TokenRequestSecret,
                     ConsumerKey, Realm, SignatureMethod, ClientSharedSecret,
                     Timestamp, NonceRequest, CallbackURL, CallbackQS,
                     _, Expiration} = json_to_token_request(Value),
                    NewValue = token_request_to_json(TokenRequest,
                        TokenRequestSecret, ConsumerKey, Realm,
                        SignatureMethod, ClientSharedSecret,
                        Timestamp, NonceRequest, CallbackURL, CallbackQS,
                        Verifier, Expiration),
                    NewObject = cloudi_service_db_riak:object_update(Object,
                                                                     NewValue),
                    case cloudi_service_db_riak:put(Dispatcher, Name,
                                                    NewObject, Timeout) of
                        {{ok, _, _}, Dispatcher} ->
                            ok;
                        {{error, _} = Error, Dispatcher} ->
                            Error
                    end;
                {siblings, _, _} ->
                    {error, siblings};
                {error, _} = Error ->
                    Error
            end;
        {{error, notfound}, Dispatcher} ->
            {error, not_found};
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_request_verify(Dispatcher, Database,
                          Realm, ConsumerKey, SignatureMethod,
                          TimestampNow, NonceAccess,
                          TokenRequest, Verifier, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_TOKEN_REQUEST,
    TokenRequestMatch = match_value(TokenRequest),
    Key = TokenRequestMatch,
    case cloudi_service_db_riak:get(Dispatcher, Name, Key, Timeout) of
        {{ok, Key, Value}, Dispatcher} ->
            ConsumerKeyMatch = match_value(ConsumerKey),
            RealmMatch = match_realm(Realm),
            SignatureMethodMatch = match_value(SignatureMethod),
            TimestampNowMatch = match_value(TimestampNow),
            NonceAccessMatch = match_value(NonceAccess),
            VerifierMatch = match_value(Verifier),
            case json_to_token_request(Value) of
                {TokenRequestMatch, TokenRequestSecret,
                 ConsumerKeyMatch, RealmMatch,
                 SignatureMethodMatch, ClientSharedSecret,
                 Timestamp, NonceRequest, _CallbackURL, _CallbackQS,
                 VerifierMatch, _Expiration}
                when NonceRequest /= NonceAccessMatch,
                     Timestamp < TimestampNowMatch ->
                    {ok, ClientSharedSecret, NonceRequest, TokenRequestSecret};
                _ ->
                    {error, not_found}
            end;
        {{siblings, _, _}, Dispatcher} ->
            {error, siblings};
        {{error, notfound}, Dispatcher} ->
            {error, not_found};
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_request_delete(Dispatcher, Database, TokenRequest, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_TOKEN_REQUEST,
    Key = match_value(TokenRequest),
    case cloudi_service_db_riak:delete(Dispatcher, Name,
                                       Key, Timeout) of
        {ok, Dispatcher} ->
            ok;
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_access_store(Dispatcher, Database,
                        Realm, ConsumerKey,
                        SignatureMethod, ClientSharedSecret,
                        Timestamp, NonceRequest, NonceAccess,
                        TokenAccess, TokenAccessSecret,
                        ExpirationSeconds, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_TOKEN_ACCESS,
    ConsumerKeyMatch = match_value(ConsumerKey),
    RealmMatch = match_realm(Realm),
    Expiration = timestamp(ExpirationSeconds),
    TokenAccessMatch = match_value(TokenAccess),
    Key = TokenAccessMatch,
    Value = token_access_to_json(TokenAccessMatch, TokenAccessSecret,
                                 ConsumerKeyMatch, RealmMatch,
                                 SignatureMethod, ClientSharedSecret,
                                 Timestamp, NonceRequest, NonceAccess,
                                 Expiration),
    Options = [{indexes,
                token_indexes(ConsumerKeyMatch, RealmMatch, Expiration)}],
    case cloudi_service_db_riak:new(Dispatcher, Name,
                                    Key, Value, Options, Timeout) of
        {{ok, _, _}, Dispatcher} ->
            ok;
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

riak_token_access_verify(Dispatcher, Database,
                         Realm, ConsumerKey, SignatureMethod,
                         TimestampNow, Nonce, TokenAccess, Timeout) ->
    Name = Database ++ ?RIAK_BUCKET_TOKEN_ACCESS,
    TokenAccessMatch = match_value(TokenAccess),
    Key = TokenAccessMatch,
    case cloudi_service_db_riak:get(Dispatcher, Name, Key, Timeout) of
        {{ok, Key, Value}, Dispatcher} ->
            ConsumerKeyMatch = match_value(ConsumerKey),
            RealmMatch = match_realm(Realm),
            SignatureMethodMatch = match_value(SignatureMethod),
            TimestampNowMatch = match_value(TimestampNow),
            NonceMatch = match_value(Nonce),
            case json_to_token_access(Value) of
                {TokenAccessMatch, TokenAccessSecret,
                 ConsumerKeyMatch, RealmMatch,
                 SignatureMethodMatch, ClientSharedSecret,
                 Timestamp, NonceRequest, NonceAccess, _Expiration}
                when NonceRequest /= NonceMatch,
                     NonceAccess /= NonceMatch,
                     Timestamp < TimestampNowMatch ->
                    {ok, ClientSharedSecret, TokenAccessSecret};
                _ ->
                    {error, not_found}
            end;
        {{siblings, _, _}, Dispatcher} ->
            {error, siblings};
        {{error, notfound}, Dispatcher} ->
            {error, not_found};
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

match_value(Value) ->
    if
        is_list(Value) ->
            erlang:list_to_binary(Value);
        is_binary(Value) ->
            Value
    end.

match_value_nullable(Value) ->
    if
        Value =:= null ->
            null;
        is_list(Value) ->
            erlang:list_to_binary(Value);
        is_binary(Value) ->
            Value
    end.

match_realm(Realm) ->
    if
        Realm =:= null ->
            null;
        is_binary(Realm) ->
            erlang:list_to_binary(string:to_lower(
                erlang:binary_to_list(Realm)));
        is_list(Realm) ->
            erlang:list_to_binary(string:to_lower(Realm))
    end.

consumer_key(ConsumerKey, Realm) ->
    RealmValue = if
        Realm =:= null ->
            <<"'null'">>;
        is_binary(Realm) ->
            Realm
    end,
    erlang:iolist_to_binary([ConsumerKey, $,, RealmValue]).

token_indexes(ConsumerKey, Realm, Expiration)
    when is_binary(Expiration) ->
    [{{binary_index, "consumer_key,realm"},
      [consumer_key(ConsumerKey, Realm)]},
     {{binary_index, "expiration"},
      [Expiration]}].

configuration_to_json(ConsumerKey, Realm,
                      SignatureMethodPlaintext, SignatureMethodHMACSHA1,
                      SignatureMethodRSASHA1, CallbackRegex) ->
    ConsumerKeyValue = match_value(ConsumerKey),
    RealmValue = match_value_nullable(Realm),
    SignatureMethodPlaintextValue =
        match_value_nullable(SignatureMethodPlaintext),
    SignatureMethodHMACSHA1Value =
        match_value_nullable(SignatureMethodHMACSHA1),
    SignatureMethodRSASHA1Value =
        match_value_nullable(SignatureMethodRSASHA1),
    CallbackRegexValue = match_value(CallbackRegex),
    cloudi_x_jsx:encode([{<<"consumer_key">>,
                 ConsumerKeyValue},
                {<<"realm">>,
                 RealmValue},
                {<<"signature_method_plaintext">>,
                 SignatureMethodPlaintextValue},
                {<<"signature_method_hmac_sha1">>,
                 SignatureMethodHMACSHA1Value},
                {<<"signature_method_rsa_sha1">>,
                 SignatureMethodRSASHA1Value},
                {<<"callback_regex">>,
                 CallbackRegexValue}]).

json_to_configuration(Value)
    when is_binary(Value) ->
    JSON0 = cloudi_x_jsx:decode(Value),
    {value, {_, ConsumerKey},
     JSON1} = lists:keytake(<<"consumer_key">>, 1, JSON0),
    {value, {_, Realm},
     JSON2} = lists:keytake(<<"realm">>, 1, JSON1),
    {value, {_, SignatureMethodPlaintext},
     JSON3} = lists:keytake(<<"signature_method_plaintext">>, 1, JSON2),
    {value, {_, SignatureMethodHMACSHA1},
     JSON4} = lists:keytake(<<"signature_method_hmac_sha1">>, 1, JSON3),
    {value, {_, SignatureMethodRSASHA1},
     JSON5} = lists:keytake(<<"signature_method_rsa_sha1">>, 1, JSON4),
    {value, {_, CallbackRegex},
     []} = lists:keytake(<<"callback_regex">>, 1, JSON5),
    {ConsumerKey, Realm, SignatureMethodPlaintext,
     SignatureMethodHMACSHA1, SignatureMethodRSASHA1, CallbackRegex}.

token_request_to_json(TokenRequest, TokenRequestSecret,
                      ConsumerKey, Realm,
                      SignatureMethod, ClientSharedSecret,
                      Timestamp, NonceRequest, CallbackURL, CallbackQS,
                      Verifier, Expiration) ->
    TokenRequestValue = match_value(TokenRequest),
    TokenRequestSecretValue = match_value(TokenRequestSecret),
    ConsumerKeyValue = match_value(ConsumerKey),
    RealmValue = match_value_nullable(Realm),
    SignatureMethodValue = match_value(SignatureMethod),
    ClientSharedSecretValue = match_value(ClientSharedSecret),
    TimestampValue = match_value(Timestamp),
    NonceRequestValue = match_value(NonceRequest),
    CallbackURLValue = match_value(CallbackURL),
    CallbackQSValue = match_value(CallbackQS),
    VerifierValue = match_value_nullable(Verifier),
    ExpirationValue = match_value(Expiration),
    cloudi_x_jsx:encode([{<<"token_request">>,
                 TokenRequestValue},
                {<<"token_request_secret">>,
                 TokenRequestSecretValue},
                {<<"consumer_key">>,
                 ConsumerKeyValue},
                {<<"realm">>,
                 RealmValue},
                {<<"signature_method">>,
                 SignatureMethodValue},
                {<<"client_shared_secret">>,
                 ClientSharedSecretValue},
                {<<"timestamp">>,
                 TimestampValue},
                {<<"nonce_request">>,
                 NonceRequestValue},
                {<<"callback_url">>,
                 CallbackURLValue},
                {<<"callback_qs">>,
                 CallbackQSValue},
                {<<"verifier">>,
                 VerifierValue},
                {<<"expiration">>,
                 ExpirationValue}]).

json_to_token_request(Value)
    when is_binary(Value) ->
    JSON0 = cloudi_x_jsx:decode(Value),
    {value, {_, TokenRequest},
     JSON1} = lists:keytake(<<"token_request">>, 1, JSON0),
    {value, {_, TokenRequestSecret},
     JSON2} = lists:keytake(<<"token_request_secret">>, 1, JSON1),
    {value, {_, ConsumerKey},
     JSON3} = lists:keytake(<<"consumer_key">>, 1, JSON2),
    {value, {_, Realm},
     JSON4} = lists:keytake(<<"realm">>, 1, JSON3),
    {value, {_, SignatureMethod},
     JSON5} = lists:keytake(<<"signature_method">>, 1, JSON4),
    {value, {_, ClientSharedSecret},
     JSON6} = lists:keytake(<<"client_shared_secret">>, 1, JSON5),
    {value, {_, Timestamp},
     JSON7} = lists:keytake(<<"timestamp">>, 1, JSON6),
    {value, {_, NonceRequest},
     JSON8} = lists:keytake(<<"nonce_request">>, 1, JSON7),
    {value, {_, CallbackURL},
     JSON9} = lists:keytake(<<"callback_url">>, 1, JSON8),
    {value, {_, CallbackQS},
     JSON10} = lists:keytake(<<"callback_qs">>, 1, JSON9),
    {value, {_, Verifier},
     JSON11} = lists:keytake(<<"verifier">>, 1, JSON10),
    {value, {_, Expiration},
     []} = lists:keytake(<<"expiration">>, 1, JSON11),
    {TokenRequest, TokenRequestSecret,
     ConsumerKey, Realm, SignatureMethod, ClientSharedSecret,
     Timestamp, NonceRequest, CallbackURL, CallbackQS,
     Verifier, Expiration}.

token_access_to_json(TokenAccess, TokenAccessSecret,
                     ConsumerKey, Realm,
                     SignatureMethod, ClientSharedSecret,
                     Timestamp, NonceRequest, NonceAccess, Expiration) ->
    TokenAccessValue = match_value(TokenAccess),
    TokenAccessSecretValue = match_value(TokenAccessSecret),
    ConsumerKeyValue = match_value(ConsumerKey),
    RealmValue = match_value_nullable(Realm),
    SignatureMethodValue = match_value(SignatureMethod),
    ClientSharedSecretValue = match_value(ClientSharedSecret),
    TimestampValue = match_value(Timestamp),
    NonceRequestValue = match_value(NonceRequest),
    NonceAccessValue = match_value(NonceAccess),
    ExpirationValue = match_value(Expiration),
    cloudi_x_jsx:encode([{<<"token_access">>,
                 TokenAccessValue},
                {<<"token_access_secret">>,
                 TokenAccessSecretValue},
                {<<"consumer_key">>,
                 ConsumerKeyValue},
                {<<"realm">>,
                 RealmValue},
                {<<"signature_method">>,
                 SignatureMethodValue},
                {<<"client_shared_secret">>,
                 ClientSharedSecretValue},
                {<<"timestamp">>,
                 TimestampValue},
                {<<"nonce_request">>,
                 NonceRequestValue},
                {<<"nonce_access">>,
                 NonceAccessValue},
                {<<"expiration">>,
                 ExpirationValue}]).

json_to_token_access(Value)
    when is_binary(Value) ->
    JSON0 = cloudi_x_jsx:decode(Value),
    {value, {_, TokenAccess},
     JSON1} = lists:keytake(<<"token_access">>, 1, JSON0),
    {value, {_, TokenAccessSecret},
     JSON2} = lists:keytake(<<"token_access_secret">>, 1, JSON1),
    {value, {_, ConsumerKey},
     JSON3} = lists:keytake(<<"consumer_key">>, 1, JSON2),
    {value, {_, Realm},
     JSON4} = lists:keytake(<<"realm">>, 1, JSON3),
    {value, {_, SignatureMethod},
     JSON5} = lists:keytake(<<"signature_method">>, 1, JSON4),
    {value, {_, ClientSharedSecret},
     JSON6} = lists:keytake(<<"client_shared_secret">>, 1, JSON5),
    {value, {_, Timestamp},
     JSON7} = lists:keytake(<<"timestamp">>, 1, JSON6),
    {value, {_, NonceRequest},
     JSON8} = lists:keytake(<<"nonce_request">>, 1, JSON7),
    {value, {_, NonceAccess},
     JSON9} = lists:keytake(<<"nonce_access">>, 1, JSON8),
    {value, {_, Expiration},
     []} = lists:keytake(<<"expiration">>, 1, JSON9),
    {TokenAccess, TokenAccessSecret,
     ConsumerKey, Realm, SignatureMethod, ClientSharedSecret,
     Timestamp, NonceRequest, NonceAccess, Expiration}.

timestamp() ->
    timestamp(0).

timestamp(0) ->
    {_, _, Microseconds} = Now = os:timestamp(),
    {{Year, Month, Day},
     {Hour, Minute, Second}} = calendar:now_to_universal_time(Now),
    erlang:iolist_to_binary(io_lib:format("~4..0b-~2..0b-~2..0bT"
                                          "~2..0b:~2..0b:~2..0b."
                                          "~6..0bZ",
                                          [Year, Month, Day,
                                           Hour, Minute, Second,
                                           Microseconds]));
timestamp(SecondsInFuture) ->
    {_, _, Microseconds} = Now = os:timestamp(),
    {{Year, Month, Day},
     {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(
            calendar:datetime_to_gregorian_seconds(
                calendar:now_to_universal_time(Now)) + SecondsInFuture),
    erlang:iolist_to_binary(io_lib:format("~4..0b-~2..0b-~2..0bT"
                                          "~2..0b:~2..0b:~2..0b."
                                          "~6..0bZ",
                                          [Year, Month, Day,
                                           Hour, Minute, Second,
                                           Microseconds])).

