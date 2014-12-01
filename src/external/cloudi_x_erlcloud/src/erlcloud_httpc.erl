%% @author Ransom Richardson <ransom@ransomr.net>
%% @doc
%%
%% HTTP client abstraction for erlcloud. Simplifies changing http clients.
%% API matches lhttpc, except Config is passed instead of options for
%% future cusomizability.
%%
%% @end

-module(erlcloud_httpc).

-export([request/6]).

request(URL, Method, Hdrs, Body, Timeout, Config) ->
    case application:get_env(erlcloud, http_client) of
        {ok, lhttpc} ->
            request_lhttpc(URL, Method, Hdrs, Body, Timeout, Config);
        {ok, httpc} ->
            request_httpc(URL, Method, Hdrs, Body, Timeout, Config)
    end.

request_lhttpc(URL, Method, Hdrs, Body, Timeout, _Config) ->
    lhttpc:request(URL, Method, Hdrs, Body, Timeout, []).

request_httpc(URL, Method, Hdrs, <<>>, Timeout, _Config) ->
    HdrsStr = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- Hdrs],
    httpc:request(Method, {URL, HdrsStr},
                  [{timeout, Timeout}], []);
request_httpc(URL, Method, Hdrs, Body, Timeout, _Config) ->
    {value,
     {_, ContentType}, HdrsRest} = lists:keytake(<<"content-type">>, 1, Hdrs),
    HdrsStr = [{binary_to_list(K), binary_to_list(V)} || {K, V} <- HdrsRest],
    httpc:request(Method, {URL, HdrsStr, binary_to_list(ContentType), Body},
                  [{timeout, Timeout}], []).
