%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OAuth v1.0 DB Interface==
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

-module(cloudi_service_oauth1_db).
-author('mjtruog [at] gmail (dot) com').

%%%------------------------------------------------------------------------
%%% Callback functions for behavior
%%%------------------------------------------------------------------------

-callback initialize(Dispatcher :: cloudi_service:dispatcher(),
                     Database :: cloudi_service:service_name(),
                     Debug :: boolean()) ->
    ok | {error, any()}.

-callback signature_methods(Dispatcher :: cloudi_service:dispatcher(),
                            Database :: cloudi_service:service_name(),
                            Realm :: string() | binary() | null,
                            ConsumerKey :: string() | binary(),
                            Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, {PLAINTEXT :: binary() | null,
          HMAC_SHA1 :: binary() | null,
          RSA_SHA1 :: binary() | null}, CallbackRegex :: binary()} |
    {error, any()}.

-callback tokens_clean(Dispatcher :: cloudi_service:dispatcher(),
                       Database :: cloudi_service:service_name()) ->
    ok | {error, any()}.

-callback token_request_check(Dispatcher :: cloudi_service:dispatcher(),
                              Database :: cloudi_service:service_name(),
                              Realm :: string() | binary() | null,
                              ConsumerKey :: string() | binary(),
                              NonceRequest :: string() | binary(),
                              Timeout ::
                                  cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

-callback token_request_store(Dispatcher :: cloudi_service:dispatcher(),
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
                              Timeout ::
                                  cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

-callback token_request_find(Dispatcher :: cloudi_service:dispatcher(),
                             Database :: cloudi_service:service_name(),
                             TokenRequest :: string() | binary(),
                             Timeout ::
                                 cloudi_service:timeout_milliseconds()) ->
    {ok, CallbackURL :: binary(), CallbackQS :: binary()} |
    {error, any()}.

-callback token_request_update(Dispatcher :: cloudi_service:dispatcher(),
                               Database :: cloudi_service:service_name(),
                               TokenRequest :: string() | binary(),
                               Verifier :: string() | binary(),
                               Timeout ::
                                   cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

-callback token_request_verify(Dispatcher :: cloudi_service:dispatcher(),
                               Database :: cloudi_service:service_name(),
                               Realm :: string() | binary() | null,
                               ConsumerKey :: string() | binary(),
                               SignatureMethod :: string() | binary(),
                               Timestamp :: string() | binary(),
                               NonceAccess :: string() | binary(),
                               TokenRequest :: string() | binary(),
                               Verifier :: string() | binary(),
                               Timeout ::
                                   cloudi_service:timeout_milliseconds()) ->
    {ok, ClientSharedSecret :: binary(), NonceRequest :: binary(),
         TokenRequestSecret :: binary()} |
    {error, any()}.

-callback token_request_delete(Dispatcher :: cloudi_service:dispatcher(),
                               Database :: cloudi_service:service_name(),
                               TokenRequest :: string() | binary(),
                               Timeout ::
                                   cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

-callback token_access_store(Dispatcher :: cloudi_service:dispatcher(),
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
                             Timeout ::
                                 cloudi_service:timeout_milliseconds()) ->
    ok | {error, any()}.

-callback token_access_verify(Dispatcher :: cloudi_service:dispatcher(),
                              Database :: cloudi_service:service_name(),
                              Realm :: string() | binary() | null,
                              ConsumerKey :: string() | binary(),
                              SignatureMethod :: string() | binary(),
                              Timestamp :: string() | binary(),
                              Nonce :: string() | binary(),
                              TokenAccess :: string() | binary(),
                              Timeout ::
                                  cloudi_service:timeout_milliseconds()) ->
    {ok, ClientSharedSecret :: binary(), TokenAccessSecret :: binary()} |
    {error, any()}.

