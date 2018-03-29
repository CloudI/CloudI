%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OAuth v1.0 PostgreSQL DB Interface==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_oauth1_db_pgsql).
-author('mjtruog at protonmail dot com').
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

-define(PGSQL_TABLE_CONFIGURATION, "oauth_configuration").
-define(PGSQL_TABLE_TOKEN_REQUEST, "oauth_token_request").
-define(PGSQL_TABLE_TOKEN_ACCESS, "oauth_token_access").
-define(SECONDS_IN_MINUTE, (60)).
-define(SECONDS_IN_HOUR, (60 * ?SECONDS_IN_MINUTE)).
-define(SECONDS_IN_DAY, (24 * ?SECONDS_IN_HOUR)).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec initialize(Dispatcher :: cloudi_service:dispatcher(),
                 Database :: cloudi_service:service_name(),
                 Debug :: boolean()) ->
    ok | {error, any()}.

initialize(Dispatcher, Database, Debug) ->
    case pgsql_create_schema(Dispatcher, Database) of
        ok ->
            if
                Debug =:= true ->
                    pgsql_configuration_rfc5849_test_data(Dispatcher, Database);
                Debug =:= false ->
                    ok
            end;
        {error, _} = Error ->
            Error
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
    pgsql_signature_methods(Dispatcher, Database, Realm, ConsumerKey, Timeout).

-spec tokens_clean(Dispatcher :: cloudi_service:dispatcher(),
                   Database :: cloudi_service:service_name()) ->
    ok | {error, any()}.

tokens_clean(Dispatcher, Database) ->
    pgsql_tokens_clean(Dispatcher, Database).

-spec token_request_check(Dispatcher :: cloudi_service:dispatcher(),
                          Database :: cloudi_service:service_name(),
                          Realm :: string() | binary() | null,
                          ConsumerKey :: string() | binary(),
                          NonceRequest :: string() | binary(),
                          Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_request_check(Dispatcher, Database,
                    Realm, ConsumerKey, NonceRequest, Timeout) ->
    pgsql_token_request_check(Dispatcher, Database,
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
    pgsql_token_request_store(Dispatcher, Database,
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
    pgsql_token_request_find(Dispatcher, Database, TokenRequest, Timeout).

-spec token_request_update(Dispatcher :: cloudi_service:dispatcher(),
                           Database :: cloudi_service:service_name(),
                           TokenRequest :: string() | binary(),
                           Verifier :: string() | binary(),
                           Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_request_update(Dispatcher, Database, TokenRequest, Verifier, Timeout) ->
    pgsql_token_request_update(Dispatcher, Database, TokenRequest,
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
    pgsql_token_request_verify(Dispatcher, Database,
                               Realm, ConsumerKey, SignatureMethod,
                               Timestamp, NonceRequest,
                               TokenRequest, Verifier, Timeout).

-spec token_request_delete(Dispatcher :: cloudi_service:dispatcher(),
                           Database :: cloudi_service:service_name(),
                           TokenRequest :: string() | binary(),
                           Timeout :: cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

token_request_delete(Dispatcher, Database, TokenRequest, Timeout) ->
    pgsql_token_request_delete(Dispatcher, Database, TokenRequest, Timeout).

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
    pgsql_token_access_store(Dispatcher, Database,
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
    pgsql_token_access_verify(Dispatcher, Database,
                              Realm, ConsumerKey, SignatureMethod,
                              Timestamp, Nonce, TokenAccess, Timeout).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

pgsql_create_schema(Dispatcher, Database) ->
    % SQL in a list is done as a single transaction
    Schema = [<<"CREATE TABLE " ?PGSQL_TABLE_CONFIGURATION " ("
                    % from http://tools.ietf.org/html/rfc5849#section-3.1
                    "consumer_key "                "TEXT NOT NULL,"
                    % based on http://tools.ietf.org/html/rfc2617#section-1.2
                    % (null if not being used for the consumer_key)
                    "realm "                       "TEXT NULL,"
                    % from http://tools.ietf.org/html/rfc5849#section-3.4
                    "signature_method_plaintext "  "TEXT NULL,"
                    "signature_method_hmac_sha1 "  "TEXT NULL,"
                    "signature_method_rsa_sha1 "   "TEXT NULL,"
                    % inferred from
                    % http://tools.ietf.org/html/rfc5849#section-2.1
                    "callback_regex "              "TEXT NOT NULL,"
                    "PRIMARY KEY(consumer_key, realm)"
                ");">>,
              <<"CREATE TABLE " ?PGSQL_TABLE_TOKEN_REQUEST " ("
                    "token_request "               "TEXT NOT NULL,"
                    "token_request_secret "        "TEXT NOT NULL,"
                    % from http://tools.ietf.org/html/rfc5849#section-3.1
                    "consumer_key "                "TEXT NOT NULL,"
                    % based on http://tools.ietf.org/html/rfc2617#section-1.2
                    % (null if not being used for the consumer_key)
                    "realm "                       "TEXT NULL,"
                    % based on http://tools.ietf.org/html/rfc5849#section-3.2
                    "signature_method "            "TEXT NOT NULL,"
                    "client_shared_secret "        "TEXT NOT NULL,"
                    "timestamp "                   "TEXT NOT NULL,"
                    "nonce_request "               "TEXT NOT NULL,"
                    "callback_url "                "TEXT NOT NULL,"
                    "callback_qs "                 "TEXT NOT NULL,"
                    % from http://tools.ietf.org/html/rfc5849#section-2.2
                    "verifier "                    "TEXT NULL,"
                    % based on http://tools.ietf.org/html/rfc5849#section-3.2
                    "expiration "                  "TIMESTAMP NOT NULL,"
                    "PRIMARY KEY(token_request)"
                ");">>,
              <<"CREATE UNIQUE INDEX "
                ?PGSQL_TABLE_TOKEN_REQUEST "_token_request_secret_index ON "
                ?PGSQL_TABLE_TOKEN_REQUEST " (token_request_secret);">>,
              <<"CREATE INDEX "
                ?PGSQL_TABLE_TOKEN_REQUEST "_consumer_key_index ON "
                ?PGSQL_TABLE_TOKEN_REQUEST " (consumer_key, realm);">>,
              <<"CREATE TABLE " ?PGSQL_TABLE_TOKEN_ACCESS " ("
                    "token_access "                "TEXT NOT NULL,"
                    "token_access_secret "         "TEXT NOT NULL,"
                    % from http://tools.ietf.org/html/rfc5849#section-3.1
                    "consumer_key "                "TEXT NOT NULL,"
                    % based on http://tools.ietf.org/html/rfc2617#section-1.2
                    % (null if not being used for the consumer_key)
                    "realm "                       "TEXT NULL,"
                    % based on http://tools.ietf.org/html/rfc5849#section-3.2
                    "signature_method "            "TEXT NOT NULL,"
                    "client_shared_secret "        "TEXT NOT NULL,"
                    "timestamp "                   "TEXT NOT NULL,"
                    "nonce_request "               "TEXT NOT NULL,"
                    "nonce_access "                "TEXT NOT NULL,"
                    % based on http://tools.ietf.org/html/rfc5849#section-3.2
                    "expiration "                  "TIMESTAMP NOT NULL,"
                    "PRIMARY KEY(token_access)"
                ");">>,
              <<"CREATE UNIQUE INDEX "
                ?PGSQL_TABLE_TOKEN_ACCESS "_token_access_secret_index ON "
                ?PGSQL_TABLE_TOKEN_ACCESS " (token_access_secret);">>,
              <<"CREATE INDEX "
                ?PGSQL_TABLE_TOKEN_ACCESS "_consumer_key_index ON "
                ?PGSQL_TABLE_TOKEN_ACCESS " (consumer_key, realm);">>
                ],
    case cloudi_service_db_pgsql:transaction(Dispatcher, Database, Schema) of
        {{ok, ok}, Dispatcher}  ->
            ok;
        {{ok, {error, <<"relation \"" ?PGSQL_TABLE_CONFIGURATION "\" "
                        "already exists">>}}, Dispatcher} ->
            ok;
        {{error, _} = Error, Dispatcher} ->
            Error
    end.

pgsql_configuration_rfc5849_test_data(Dispatcher, Database) ->
    Inserts = [% from http://tools.ietf.org/html/rfc5849#section-1.2
               % (i.e., printer.example.com client with
               %  photos.example.net server)
               <<"INSERT INTO " ?PGSQL_TABLE_CONFIGURATION " "
                 "(consumer_key, realm, signature_method_plaintext, "
                  "signature_method_hmac_sha1, signature_method_rsa_sha1, "
                  "callback_regex) "
                 "VALUES ('dpf43f3p2l4k3l03', 'photos', NULL, "
                         "'kd94hf93k423kf44', NULL, "
                         "'https?://printer.example.com/ready');">>],
    case cloudi_service_db_pgsql:transaction(Dispatcher, Database, Inserts) of
        {{ok, ok}, Dispatcher} ->
            ok;
        {{ok, {error, <<Exists:160/bits, _/binary>>}}, _}
            when Exists == <<"duplicate key value ">> ->
            ok;
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_tokens_clean(Dispatcher, Database) ->
    Delete = [<<"DELETE FROM " ?PGSQL_TABLE_TOKEN_REQUEST " "
                "WHERE expiration < CURRENT_TIMESTAMP;">>,
              <<"DELETE FROM " ?PGSQL_TABLE_TOKEN_ACCESS " "
                "WHERE expiration < CURRENT_TIMESTAMP;">>],
    case cloudi_service_db_pgsql:transaction(Dispatcher, Database, Delete) of
        {{ok, ok}, Dispatcher} ->
            ok;
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_signature_methods(Dispatcher, Database, Realm, ConsumerKey, Timeout) ->
    Select = <<"SELECT signature_method_plaintext, "
                      "signature_method_hmac_sha1, "
                      "signature_method_rsa_sha1, "
                      "callback_regex "
               "FROM " ?PGSQL_TABLE_CONFIGURATION " "
               "WHERE realm = lower($1) AND consumer_key = $2">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Select,
                                        [Realm, ConsumerKey], Timeout) of
        {{ok, {selected, []}}, Dispatcher} ->
            {error, not_found};
        {{ok, {selected,
               [{PLAINTEXT, HMAC_SHA1, RSA_SHA1, CallbackRegex}]}}, Dispatcher} ->
            {ok, {PLAINTEXT, HMAC_SHA1, RSA_SHA1}, CallbackRegex};
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_request_check(Dispatcher, Database,
                          Realm, ConsumerKey, NonceRequest, Timeout) ->
    Select = <<"SELECT TRUE "
               "FROM " ?PGSQL_TABLE_TOKEN_REQUEST ", "
                       ?PGSQL_TABLE_TOKEN_ACCESS " "
               "WHERE (" ?PGSQL_TABLE_TOKEN_REQUEST ".realm = lower($1) AND "
                         ?PGSQL_TABLE_TOKEN_REQUEST ".consumer_key = $2 AND "
                      "(" ?PGSQL_TABLE_TOKEN_REQUEST ".nonce_request = $3 )) "
                     "OR "
                     "(" ?PGSQL_TABLE_TOKEN_ACCESS ".realm = lower($1) AND "
                         ?PGSQL_TABLE_TOKEN_ACCESS ".consumer_key = $2 AND "
                      "(" ?PGSQL_TABLE_TOKEN_ACCESS ".nonce_request = $3 OR "
                          ?PGSQL_TABLE_TOKEN_ACCESS ".nonce_access = $3)) "
               "LIMIT 1">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Select,
                                        [Realm, ConsumerKey,
                                         NonceRequest], Timeout) of
        {{ok, {selected, []}}, Dispatcher} ->
            ok;
        {{ok, {selected, [_]}}, Dispatcher} ->
            {error, nonce_exists};
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_request_store(Dispatcher, Database,
                          Realm, ConsumerKey,
                          SignatureMethod, ClientSharedSecret,
                          Timestamp, NonceRequest,
                          TokenRequest, TokenRequestSecret,
                          CallbackURL, CallbackQS,
                          ExpirationSeconds, Timeout) ->
    Insert = <<"INSERT INTO " ?PGSQL_TABLE_TOKEN_REQUEST " "
               "(realm, consumer_key, signature_method, client_shared_secret, "
                "timestamp, nonce_request, "
                "token_request, token_request_secret, "
                "callback_url, callback_qs, verifier, expiration) "
               "VALUES (lower($1), $2, $3, $4, $5, $6, $7, $8, $9, $10, null, "
                       "CURRENT_TIMESTAMP + ($11)::INTERVAL)">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Insert,
                                        [Realm, ConsumerKey,
                                         SignatureMethod, ClientSharedSecret,
                                         Timestamp, NonceRequest,
                                         TokenRequest, TokenRequestSecret,
                                         CallbackURL, CallbackQS,
                                         pgsql_interval(ExpirationSeconds)],
                                        Timeout) of
        {{ok, {updated, 1}}, Dispatcher} ->
            ok;
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_request_find(Dispatcher, Database, TokenRequest, Timeout) ->
    Select = <<"SELECT callback_url, callback_qs "
               "FROM " ?PGSQL_TABLE_TOKEN_REQUEST " "
               "WHERE token_request = $1">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Select,
                                        [TokenRequest], Timeout) of
        {{ok, {selected, []}}, Dispatcher} ->
            {error, not_found};
        {{ok, {selected, [{CallbackURL, CallbackQS}]}}, Dispatcher} ->
            {ok, CallbackURL, CallbackQS};
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_request_update(Dispatcher, Database, TokenRequest,
                           Verifier, Timeout) ->
    Update = <<"UPDATE " ?PGSQL_TABLE_TOKEN_REQUEST " "
               "SET verifier = $2 "
               "WHERE token_request = $1">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Update,
                                        [TokenRequest, Verifier], Timeout) of
        {{ok, {updated, 1}}, Dispatcher} ->
            ok;
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_request_verify(Dispatcher, Database,
                           Realm, ConsumerKey, SignatureMethod,
                           Timestamp, NonceAccess,
                           TokenRequest, Verifier, Timeout) ->
    Select = <<"SELECT client_shared_secret, nonce_request, "
                      "token_request_secret "
               "FROM " ?PGSQL_TABLE_TOKEN_REQUEST " "
               "WHERE realm = lower($1) AND consumer_key = $2 AND "
                     "signature_method = $3 AND "
                     "timestamp < $4 AND nonce_request <> $5 AND "
                     "token_request = $6 AND verifier = $7">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Select,
                                        [Realm, ConsumerKey, SignatureMethod,
                                         Timestamp, NonceAccess,
                                         TokenRequest, Verifier], Timeout) of
        {{ok, {selected, []}}, Dispatcher} ->
            {error, not_found};
        {{ok, {selected, [{ClientSharedSecret, NonceRequest,
                           TokenRequestSecret}]}}, Dispatcher} ->
            {ok, ClientSharedSecret, NonceRequest, TokenRequestSecret};
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_request_delete(Dispatcher, Database, TokenRequest, Timeout) ->
    Delete = <<"DELETE FROM " ?PGSQL_TABLE_TOKEN_REQUEST " "
               "WHERE token_request = $1">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Delete,
                                        [TokenRequest], Timeout) of
        {{ok, {updated, 1}}, Dispatcher} ->
            ok;
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_access_store(Dispatcher, Database,
                         Realm, ConsumerKey,
                         SignatureMethod, ClientSharedSecret,
                         Timestamp, NonceRequest, NonceAccess,
                         TokenAccess, TokenAccessSecret,
                         ExpirationSeconds, Timeout) ->
    Insert = <<"INSERT INTO " ?PGSQL_TABLE_TOKEN_ACCESS " "
               "(realm, consumer_key, signature_method, client_shared_secret, "
                "timestamp, nonce_request, nonce_access, "
                "token_access, token_access_secret, expiration) "
               "VALUES (lower($1), $2, $3, $4, $5, $6, $7, $8, $9, "
                       "CURRENT_TIMESTAMP + ($10)::INTERVAL)">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Insert,
                                        [Realm, ConsumerKey,
                                         SignatureMethod, ClientSharedSecret,
                                         Timestamp, NonceRequest, NonceAccess,
                                         TokenAccess, TokenAccessSecret,
                                         pgsql_interval(ExpirationSeconds)],
                                        Timeout) of
        {{ok, {updated, 1}}, Dispatcher} ->
            ok;
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_token_access_verify(Dispatcher, Database,
                          Realm, ConsumerKey, SignatureMethod,
                          Timestamp, Nonce, TokenAccess, Timeout) ->
    Select = <<"SELECT client_shared_secret, token_access_secret "
               "FROM " ?PGSQL_TABLE_TOKEN_ACCESS " "
               "WHERE realm = lower($1) AND consumer_key = $2 AND "
                     "signature_method = $3 AND timestamp < $4 AND "
                     "nonce_request <> $5 AND nonce_access <> $5 AND "
                     "token_access = $6">>,
    case cloudi_service_db_pgsql:equery(Dispatcher, Database, Select,
                                        [Realm, ConsumerKey, SignatureMethod,
                                         Timestamp, Nonce,
                                         TokenAccess], Timeout) of
        {{ok, {selected, []}}, Dispatcher} ->
            {error, not_found};
        {{ok, {selected, [{ClientSharedSecret, TokenAccessSecret}]}}, Dispatcher} ->
            {ok, ClientSharedSecret, TokenAccessSecret};
        {{ok, {error, _} = Error}, Dispatcher} ->
            Error; % database error
        {{error, _} = Error, Dispatcher} ->
            Error % database driver error
    end.

pgsql_interval(Seconds0) ->
    Days = Seconds0 div ?SECONDS_IN_DAY,
    Seconds1 = Seconds0 - (?SECONDS_IN_DAY * Days),
    Hours = Seconds1 div ?SECONDS_IN_HOUR,
    Seconds2 = Seconds1 - (?SECONDS_IN_HOUR * Hours),
    Minutes = Seconds2 div ?SECONDS_IN_MINUTE,
    SecondsN = Seconds2 - (?SECONDS_IN_MINUTE * Minutes),
    lists:flatten(io_lib:format("~w ~w:~w:~w",
                                [Days, Hours, Minutes, SecondsN])).
