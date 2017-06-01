%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OAuth v1.0 DB Interface==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
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

