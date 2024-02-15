%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% 
%%% Copyright (C) 2010 Brian Buchanan. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%
%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% HTTP client abstraction for erlcloud. Simplifies changing http clients.
%%
%% @end
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% From erlcloud, in erlcloud_httpc.erl
%%%------------------------------------------------------------------------

-module(nodefinder_ec2_api_httpc).

-include("nodefinder.hrl").
-include("nodefinder_ec2_api.hrl").

-export([request/7]).

-ifdef(ERLANG_OTP_VERSION_25_FEATURES).
-define(SSL_OPTIONS(Host),
        [{server_name_indication, Host},
         {verify, verify_peer},
         {depth, 100},
         {cacerts, public_key:cacerts_get()}]).
-else.
-define(SSL_OPTIONS(_Host), []).
-endif.

request(URL, Method, Host, Hdrs, Body, Timeout,
        #aws_config{http_client = httpc} = Config) ->
    request_httpc(URL, Method, Host, Hdrs, Body, Timeout, Config);
request(URL, Method, Host, Hdrs, Body, Timeout,
        #aws_config{http_client = hackney} = Config) ->
    request_hackney(URL, Method, Host, Hdrs, Body, Timeout, Config).

%% Guard clause protects against empty bodied requests from being
%% unable to find a matching httpc:request call.
request_httpc(URL, Method, Host, Hdrs, <<>>, Timeout, _Config) 
    when (Method =:= options) orelse 
         (Method =:= get) orelse 
         (Method =:= head) orelse 
         (Method =:= delete) orelse 
         (Method =:= trace) ->
    HdrsStr = [{to_list_string(K), to_list_string(V)} || {K, V} <- Hdrs],
    response_httpc(httpc:request(Method, {URL, HdrsStr},
                                 [{ssl, ?SSL_OPTIONS(Host)},
                                  {timeout, Timeout}],
                                 [{body_format, binary}]));
request_httpc(URL, Method, Host, Hdrs, Body, Timeout, _Config) ->
    HdrsStr = [{to_list_string(K), to_list_string(V)} || {K, V} <- Hdrs],
    {"content-type", ContentType} = lists:keyfind("content-type", 1, HdrsStr),
    response_httpc(httpc:request(Method,
                                 {URL, HdrsStr,
                                  ContentType, Body},
                                 [{ssl, ?SSL_OPTIONS(Host)},
                                  {timeout, Timeout}],
                                 [{body_format, binary}])).

request_hackney(URL, Method, Host, Hdrs, Body, Timeout,
                #aws_config{hackney_pool = Pool}) ->
    BinURL = to_binary(URL),
    BinHdrs = [{to_binary(K), to_binary(V)} || {K, V} <- Hdrs],
    PoolOpt = if Pool =:= undefined ->
                      [];
                 true ->
                      [{pool, Pool}]
              end,
    Module = hide_module(hackney),
    response_hackney(Module:
                     request(Method, BinURL, BinHdrs, Body,
                             [{ssl_options, ?SSL_OPTIONS(Host)},
                              {recv_timeout, Timeout}] ++ PoolOpt)).

response_httpc({ok, {{_HTTPVer, Status, StatusLine}, Headers, Body}}) ->
    {ok, {{Status, StatusLine}, Headers, Body}};
response_httpc({error, _} = Error) ->
    Error.

response_hackney({ok, Status, Hdrs}) ->
    HdrsStr = header_str(Hdrs),
    {ok, {{Status, undefined}, HdrsStr, undefined}};
response_hackney({ok, Status, Hdrs, Ref}) ->
    Module = hide_module(hackney),
    case Module:body(Ref) of
        {ok, Body} ->
            HdrsStr = header_str(Hdrs),
            {ok, {{Status, undefined}, HdrsStr, Body}};
        {error, Reason} when Status >= 200, Status =< 299 ->
            {error, {hackney_error, Reason}};
        {error, _} ->
            HdrsStr = header_str(Hdrs),
            {ok, {{Status, undefined}, HdrsStr, undefined}}
    end;
response_hackney({error, _} = Error) ->
    Error.

header_str(Hdrs) ->
    [{nodefinder_string:lowercase(to_list_string(K)), to_list_string(V)}
     || {K, V} <- Hdrs].

to_list_string(Val) when erlang:is_binary(Val) ->
  erlang:binary_to_list(Val);
to_list_string(Val) when erlang:is_list(Val) ->
  Val.

to_binary(Val) when erlang:is_list(Val) ->
  erlang:list_to_binary(Val);
to_binary(Val) when erlang:is_binary(Val) ->
  Val.

hide_module(Module) ->
    % hide module from xref to prevent reltool from pulling in
    % extra applications that are unnecessary
    % (the Erlang compiler never inlines functions by default)
    Module.

