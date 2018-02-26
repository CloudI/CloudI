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
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% From erlcloud, in erlcloud_aws.erl
%%%------------------------------------------------------------------------

-module(nodefinder_ec2_api_aws).

-export([aws_request_xml4/6,
         param_list/2
]).

-include("nodefinder_ec2_api.hrl").

-define(ERLCLOUD_RETRY_TIMEOUT, 10000).

%-record(metadata_credentials,
%        {access_key_id :: string(),
%         secret_access_key :: string(),
%         security_token=undefined :: string(),
%         expiration_gregorian_seconds :: integer()
%        }).
%
%-record(profile_options, {
%          session_name :: string(),
%          session_secs :: 900..3600,
%          external_id :: string()
%}).


aws_request_xml4(Method, Host, Path, Params, Service, #aws_config{} = Config) ->
    aws_request_xml4(Method, undefined, Host, undefined, Path, Params, Service, Config).
aws_request_xml4(Method, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    case aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) of
        {ok, Body} ->
            {ok, element(1, xmerl_scan:string(binary_to_list(Body)))};
        {error, Reason} ->
            {error, Reason}
    end.

aws_request4(Method, Protocol, Host, Port, Path, Params, Service, Config) ->
    case update_config(Config) of
        {ok, Config1} ->
            aws_request4_no_update(Method, Protocol, Host, Port, Path, Params, Service, Config1);
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_config(aws_config()) -> {ok, aws_config()} | {error, term()}.
update_config(#aws_config{access_key_id = KeyId} = Config)
  when is_list(KeyId), KeyId /= [] ->
    %% In order to support caching of the aws_config, we could store the expiration_time
    %% and check it here. If it is about to expire (within 5 minutes is what boto uses)
    %% then we should get the new config.
    {ok, Config};
update_config(#aws_config{}) ->
    {error, invalid_credentials}.

aws_request4_no_update(Method, Protocol, Host, Port, Path, Params, Service, #aws_config{} = Config) ->
    Query = nodefinder_ec2_api_http:make_query_string(Params),
    Region = aws_region_from_host(Host),

    SignedHeaders = case Method of
                        post ->
                            sign_v4(Method, Path, Config,
                                    [{"host", Host}], list_to_binary(Query),
                                    Region, Service, []);
                        get ->
                            sign_v4(Method, Path, Config, [{"host", Host}],
                                    <<>>, Region, Service, Params)
                    end,

    aws_request_form(Method, Protocol, Host, Port, Path, Query, SignedHeaders, Config).

aws_region_from_host(Host) ->
    case nodefinder_string:split(Host, ".") of
        %% the aws endpoint can vary depending on the region
        %% we need to account for that:
        %%  us-west-2: s3.us-west-2.amazonaws.com
        %%  cn-north-1 (AWS China): s3.cn-north-1.amazonaws.com.cn
        %% it's assumed that the first element is the aws service (s3, ec2, etc),
        %% the second is the region identifier, the rest is ignored
        %% the exception (of course) is the dynamodb streams which follows a different
        %% format
        ["streams", "dynamodb", Value | _Rest] ->
            Value;
        [_, Value, _, _ | _Rest] ->
            Value;
        _ ->
            "us-east-1"
    end.

%% http://docs.aws.amazon.com/general/latest/gr/signature-version-4.html
-spec sign_v4(atom(), list(), aws_config(), headers(), binary(), string(), string(), list()) -> headers().
sign_v4(Method, Uri, Config, Headers, Payload, Region, Service, QueryParams) ->
    Date = iso_8601_basic_time(),
    PayloadHash = hash_encode(Payload),
    Headers1 = [{"x-amz-content-sha256", PayloadHash}, {"x-amz-date", Date} | Headers],
    Headers2 = case Config#aws_config.security_token of
                   undefined -> Headers1;
                   Token -> [{"x-amz-security-token", Token} | Headers1]
               end,
    {Request, SignedHeaders} = canonical_request(Method, Uri, QueryParams, Headers2, PayloadHash),
    CredentialScope = credential_scope(Date, Region, Service),
    ToSign = to_sign(Date, CredentialScope, Request),
    SigningKey = signing_key(Config, Date, Region, Service),
    Signature = base16(sha256_mac( SigningKey, ToSign)),
    Authorization = authorization(Config, CredentialScope, SignedHeaders, Signature),
    [{"Authorization", lists:flatten(Authorization)} | Headers2].

iso_8601_basic_time() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
    lists:flatten(io_lib:format(
                    "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).

canonical_request(Method, CanonicalURI, QParams, Headers, PayloadHash) ->
    {CanonicalHeaders, SignedHeaders} = canonical_headers(Headers),
    CanonicalQueryString = canonical_query_string(QParams),
    {[nodefinder_string:uppercase(atom_to_list(Method)), $\n,
      CanonicalURI, $\n,
      CanonicalQueryString, $\n,
      CanonicalHeaders, $\n,
      SignedHeaders, $\n,
      PayloadHash],
     SignedHeaders}.

canonical_headers(Headers) ->
    Normalized = [{nodefinder_string:lowercase(Name), trimall(Value)} || {Name, Value} <- Headers],
    Sorted = lists:keysort(1, Normalized),
    Canonical = [[Name, $:, Value, $\n] || {Name, Value} <- Sorted],
    Signed = lists:join($;, [Name || {Name, _} <- Sorted]),
    {Canonical, Signed}.

%% @doc calculate canonical query string out of query params and according to v4 documentation
canonical_query_string([]) ->
    "";
canonical_query_string(Params) ->
    Normalized = [{nodefinder_ec2_api_http:url_encode(Name), nodefinder_ec2_api_http:url_encode(nodefinder_ec2_api_http:value_to_string(Value))} || {Name, Value} <- Params],
    Sorted = lists:keysort(1, Normalized),
    lists:join($&,
               [case Value of
                    [] -> [Key, "="];
                    _ -> [Key, "=", Value]
                end
                || {Key, Value} <- Sorted, Value =/= none, Value =/= undefined]).

trimall(Value) ->
    %% TODO - remove excess internal whitespace in header values
    re:replace(Value, "(^\\s+)|(\\s+$)", "", [global]).

hash_encode(Data) ->
    Hash = sha256(Data),
    base16(Hash).

base16(Data) ->
    io_lib:format("~64.16.0b", [binary:decode_unsigned(Data)]).

credential_scope(Date, Region, Service) ->
    DateOnly = lists:sublist(Date, 8),
    [DateOnly, $/, Region, $/, Service, "/aws4_request"].

to_sign(Date, CredentialScope, Request) ->
    ["AWS4-HMAC-SHA256\n",
     Date, $\n,
     CredentialScope, $\n,
     hash_encode(Request)].

signing_key(Config, Date, Region, Service) ->
    %% TODO cache the signing key so we don't have to recompute for every request
    DateOnly = lists:sublist(Date, 8),
    KDate = sha256_mac( "AWS4" ++ Config#aws_config.secret_access_key, DateOnly),
    KRegion = sha256_mac( KDate, Region),
    KService = sha256_mac( KRegion, Service),
    sha256_mac( KService, "aws4_request").

authorization(Config, CredentialScope, SignedHeaders, Signature) ->
    ["AWS4-HMAC-SHA256"
     " Credential=", Config#aws_config.access_key_id, $/, CredentialScope, $,,
     " SignedHeaders=", SignedHeaders, $,,
     " Signature=", Signature].

-spec aws_request_form(Method :: atom(), Protocol :: undefined | string(), Host :: string(),
                        Port :: undefined | integer() | string(), Path :: string(), Form :: iodata(),
                        Headers :: list(), Config :: aws_config()) -> {ok, binary()} | {error, tuple()}.
aws_request_form(Method, Protocol, Host, Port, Path, Form, Headers, Config) ->
    UProtocol = case Protocol of
        undefined -> "https://"%;
        %_ -> [Protocol, "://"]
    end,

    URL = case Port of
        undefined -> [UProtocol, Host, Path]%;
        %_ -> [UProtocol, Host, $:, port_to_str(Port), Path]
    end,

    %% Note: httpc MUST be used with {timeout, timeout()} option
    %%       Many timeout related failures is observed at prod env
    %%       when library is used in 24/7 manner
    Response =
        case Method of
            get ->
                Req = lists:flatten([URL, $?, Form]),
                nodefinder_ec2_api_httpc:request(
                  Req, get, Headers, <<>>, get_timeout(Config), Config);
            _ ->
                nodefinder_ec2_api_httpc:request(
                  lists:flatten(URL), Method,
                  [{<<"content-type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>} | Headers],
                  list_to_binary(Form), get_timeout(Config), Config)
        end,

    http_body(Response).

%port_to_str(Port) when is_integer(Port) ->
%    integer_to_list(Port);
%port_to_str(Port) when is_list(Port) ->
%    Port.

get_timeout(#aws_config{timeout = undefined}) ->
    ?ERLCLOUD_RETRY_TIMEOUT;
get_timeout(#aws_config{timeout = Timeout}) ->
    Timeout.

param_list([], _Key) -> [];
param_list(Values, Key) when is_tuple(Key) ->
    Seq = lists:seq(1, size(Key)),
    lists:flatten(
      [[{lists:append([element(J, Key), ".", integer_to_list(I)]),
         element(J, Value)} || J <- Seq] ||
          {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list([[{_, _}|_]|_] = Values, Key) ->
    lists:flatten(
      [[{lists:flatten([Key, $., integer_to_list(I), $., SubKey]),
         value_to_string(Value)} || {SubKey, Value} <- SValues] ||
          {I, SValues} <- lists:zip(lists:seq(1, length(Values)), Values)]
     );
param_list(Values, Key) ->
    [{lists:flatten([Key, $., integer_to_list(I)]), Value} ||
        {I, Value} <- lists:zip(lists:seq(1, length(Values)), Values)].

-spec http_body({ok, tuple()} | {error, term()})
               -> {ok, binary()} | {error, tuple()}.
%% Extract the body and do error handling on the return of a httpc:request call.
http_body(Return) ->
    case http_headers_body(Return) of
        {ok, {_, Body}} ->
            {ok, Body};
        {error, Reason} ->
            {error, Reason}
    end.

-type headers() :: [{string(), string()}].
-spec http_headers_body({ok, tuple()} | {error, term()})
                       -> {ok, {headers(), binary()}} | {error, tuple()}.
%% Extract the headers and body and do error handling on the return of a httpc:request call.
http_headers_body({ok, {{OKStatus, _StatusLine}, Headers, Body}})
  when OKStatus >= 200, OKStatus =< 299 ->
    {ok, {Headers, Body}};
http_headers_body({ok, {{Status, StatusLine}, _Headers, Body}}) ->
    {error, {http_error, Status, StatusLine, Body}};
http_headers_body({error, Reason}) ->
    {error, {socket_error, Reason}}.

value_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value_to_string(Binary) when is_binary(Binary) -> Binary;
value_to_string(String) when is_list(String) -> String;
value_to_string({{_Yr, _Mo, _Da}, {_Hr, _Min, _Sec}} = Timestamp) -> format_timestamp(Timestamp).

format_timestamp({{Yr, Mo, Da}, {H, M, S}}) ->
    lists:flatten(
      io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
                    [Yr, Mo, Da, H, M, S])).

%%%------------------------------------------------------------------------
%%% From erlcloud, in erlcloud_util.erl
%%%------------------------------------------------------------------------

sha256(V) ->
    crypto:hash(sha256, V).

sha256_mac(K, S) ->
    crypto:hmac(sha256, K, S).

