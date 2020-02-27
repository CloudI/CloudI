%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OAuth v1.0 Data Processing==
%%% based on https://github.com/tim/erlang-oauth
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012 Tim Fletcher <mail@tfletcher.com>
%%%
%%% Permission is hereby granted, free of charge, to any person
%%% obtaining a copy of this software and associated documentation
%%% files (the "Software"), to deal in the Software without
%%% restriction, including without limitation the rights to use,
%%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following
%%% conditions:
%%%
%%% The above copyright notice and this permission notice shall be
%%% included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author Tim Fletcher <mail@tfletcher.com>
%%% @copyright 2012 Tim Fletcher
%%% @version 1.8.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_oauth1_data).
-author('mail [at] tfletcher (dot) com').

%% external interface
-export([verify/6,
         signature/5]).

-include_lib("public_key/include/public_key.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-define(ERLANG_OTP_VERSION_23_FEATURES, true).
-endif.
-endif.

-spec verify(Signature :: string(),
             HttpMethod :: string(),
             URL :: string(),
             Params :: list({string(), string()}),
             Consumer :: {string(), string(),
                          plaintext | hmac_sha1 | rsa_sha1},
             TokenSecret :: string()) ->
    boolean().

verify(Signature, _HttpMethod, _URL, _Params,
       {_, _, plaintext} = Consumer, TokenSecret) ->
    plaintext_verify(Signature, Consumer, TokenSecret);
verify(Signature, HttpMethod, URL, Params,
       {_, _, hmac_sha1} = Consumer, TokenSecret) ->
    hmac_sha1_verify(Signature, HttpMethod, URL, Params, Consumer, TokenSecret);
verify(Signature, HttpMethod, URL, Params,
       {_, _, rsa_sha1} = Consumer, _TokenSecret) ->
    rsa_sha1_verify(Signature, HttpMethod, URL, Params, Consumer).

-spec signature(HttpMethod :: string(),
                URL :: string(),
                Params :: list({string(), string()}),
                Consumer :: {string(), string(), hmac_sha1},
                TokenSecret :: string()) ->
    string().

signature(HttpMethod, URL, Params,
          {_, _, hmac_sha1} = Consumer, TokenSecret) ->
    BaseString = signature_base_string(HttpMethod, URL, Params),
    uri_encode(hmac_sha1_signature(BaseString, Consumer, TokenSecret)).

%%%------------------------------------------------------------------------
%%% From erlang-oauth repository, in oauth.erl
%%%------------------------------------------------------------------------

plaintext_signature({_, ConsumerSecret, _}, TokenSecret) ->
    uri_join([ConsumerSecret, TokenSecret]).

plaintext_verify(Signature, Consumer, TokenSecret) ->
    cloudi_string:compare_constant(plaintext_signature(Consumer,
                                                       TokenSecret), Signature).

hmac_sha1_signature(BaseString, {_, ConsumerSecret, _}, TokenSecret) ->
    Key = uri_join([ConsumerSecret, TokenSecret]),
    base64:encode_to_string(hmac_sha(Key, BaseString)).

hmac_sha1_verify(Signature, HttpMethod, URL, Params, Consumer, TokenSecret) ->
    BaseString = signature_base_string(HttpMethod, URL, Params),
    cloudi_string:compare_constant(hmac_sha1_signature(BaseString,
                                                       Consumer,
                                                       TokenSecret), Signature).

-ifdef(ERLANG_OTP_VERSION_23_FEATURES).
hmac_sha(Key, Data) ->
    crypto:mac(hmac, sha, Key, Data).
-else.
hmac_sha(Key, Data) ->
    crypto:hmac(sha, Key, Data).
-endif.

rsa_sha1_verify(Signature, HttpMethod, URL, Params, {_, ConsumerSecret, _}) ->
    BaseString = signature_base_string(HttpMethod, URL, Params),
    Key = read_cert_key(ConsumerSecret),
    public_key:verify(erlang:list_to_binary(BaseString), sha,
                      base64:decode(Signature), Key).

read_cert_key(Contents) when is_binary(Contents) ->
    [{'Certificate', DerCert, not_encrypted}] = public_key:pem_decode(Contents),
    read_cert_key(public_key:pkix_decode_cert(DerCert, otp));
read_cert_key(#'OTPCertificate'{tbsCertificate=Cert}) ->
    read_cert_key(Cert);
read_cert_key(#'OTPTBSCertificate'{subjectPublicKeyInfo=Info}) ->
    read_cert_key(Info);
read_cert_key(#'OTPSubjectPublicKeyInfo'{subjectPublicKey=Key}) ->
    Key.

signature_base_string(HttpMethod, URL, Params) ->
    uri_join([HttpMethod, uri_normalize(URL), params_encode(Params)]).

params_encode(Params) ->
    % cf. http://tools.ietf.org/html/rfc5849#section-3.4.1.3.2
    Encoded = [{uri_encode(K), uri_encode(V)} || {K, V} <- Params],
    Sorted = lists:sort(Encoded),
    Concatenated = [lists:concat([K, "=", V]) || {K, V} <- Sorted],
    cloudi_string:join("&", Concatenated).

-ifdef(ERLANG_OTP_VERSION_23_FEATURES).
uri_normalize(URI) ->
    try uri_string:parse(URI) of
        Values ->
            Scheme = case maps:get(scheme, Values) of
                "http" ->
                    http;
                "https" ->
                    https;
                SchemeStr ->
                    SchemeStr
            end,
            UserInfo = maps:get(userinfo, Values),
            Host = maps:get(host, Values),
            Port = maps:get(port, Values),
            Path = maps:get(path, Values),
            uri_normalize(Scheme, UserInfo,
                          cloudi_string:lowercase(Host), Port, [Path])
    catch
        ErrorType:Error ->
            {error, {ErrorType, Error}}
    end.
-else.
uri_normalize(URI) ->
    case http_uri:parse(URI) of
        {ok, {Scheme, UserInfo, Host, Port, Path, _Query}} ->
            uri_normalize(Scheme, UserInfo,
                          cloudi_string:lowercase(Host), Port, [Path]);
        {error, _} = Error ->
            Error
    end.
-endif.

uri_normalize(Scheme, [], Acc) ->
  lists:concat([Scheme, "://" | Acc]);
uri_normalize(Scheme, UserInfo, Acc) ->
  lists:concat([Scheme, "://", UserInfo, "@" | Acc]).

uri_normalize(http, UserInfo, Host, 80, Acc) ->
    uri_normalize(http, UserInfo, [Host|Acc]);
uri_normalize(https, UserInfo, Host, 443, Acc) ->
    uri_normalize(https, UserInfo, [Host|Acc]);
uri_normalize(Scheme, UserInfo, Host, Port, Acc) ->
    uri_normalize(Scheme, UserInfo, [Host, ":", Port|Acc]).

uri_join(Values) ->
    uri_join(Values, "&").

uri_join(Values, Separator) ->
    cloudi_string:join(Separator, lists:map(fun uri_encode/1, Values)).

uri_encode(Term) when is_integer(Term) ->
    integer_to_list(Term);
uri_encode(Term) when is_atom(Term) ->
    uri_encode(atom_to_list(Term));
uri_encode(Term) when is_list(Term) ->
    uri_encode(lists:reverse(Term, []), []).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

uri_encode([X | T], Acc)
    when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
    uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
    NewAcc = [$%, dec2hex(X bsr 4), dec2hex(X band 16#0f) | Acc],
    uri_encode(T, NewAcc);
uri_encode([], Acc) ->
    Acc.

-compile({inline, [{dec2hex, 1}]}).

dec2hex(N) when N >= 10 andalso N =< 15 ->
    N + $A - 10;
dec2hex(N) when N >= 0 andalso N =< 9 ->
    N + $0.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

hmac_sha1_signature_test_() ->
    %% [BaseString, {_, ConsumerSecret, _}, TokenSecret] = Args
    Tests = [
        {"http://wiki.oauth.net/TestCases HMAC-SHA1 (section 9.2) #1",
         ["bs", {undefined, "cs", undefined}, ""],
         "egQqG5AJep5sJ7anhXju1unge2I="},
        {"http://wiki.oauth.net/TestCases HMAC-SHA1 (section 9.2) #2",
         ["bs", {undefined, "cs", undefined}, "ts"],
         "VZVjXceV7JgPq/dOTnNmEfO0Fv8="},
        {"http://wiki.oauth.net/TestCases HMAC-SHA1 (section 9.2) #3",
         ["GET&http%3A%2F%2Fphotos.example.net%2Fphotos&"
          "file%3Dvacation.jpg%26"
          "oauth_consumer_key%3Ddpf43f3p2l4k3l03%26"
          "oauth_nonce%3Dkllo9940pd9333jh%26"
          "oauth_signature_method%3DHMAC-SHA1%26"
          "oauth_timestamp%3D1191242096%26"
          "oauth_token%3Dnnch734d00sl2jdk%26"
          "oauth_version%3D1.0"
          "%26size%3Doriginal",
          {undefined, "kd94hf93k423kf44", undefined}, "pfkkdhi9sl3r4s00"],
         "tR3+Ty81lMeYAr/Fid0kMTYa/WM="},
        {"http://oauth.googlecode.com/svn/code/javascript/"
         "example/signature.html #1",
         ["POST&http%3A%2F%2Fphotos.example.net%2Finitiate&"
          "oauth_consumer_key%3Ddpf43f3p2l4k3l03%26"
          "oauth_nonce%3DwIjqoS%26"
          "oauth_signature_method%3DHMAC-SHA1%26"
          "oauth_timestamp%3D137131200",
          {undefined, "kd94hf93k423kf44", undefined}, ""],
         "2nxXYRjOKrs4qL666Zzr1felAdY="}
    ],
    [{T, fun() -> R = erlang:apply(fun hmac_sha1_signature/3, A) end} ||
      {T, A, R} <- Tests].

signature_base_string_test_() ->
    %% [HttpMethod, URL, Params] = Args
    Tests = [
        {"http://wiki.oauth.net/TestCases HMAC-SHA1 (section 9.2) #3",
         ["GET",
          "http://photos.example.net/photos",
          [{"file", "vacation.jpg"},
           {"oauth_consumer_key", "dpf43f3p2l4k3l03"},
           {"oauth_nonce", "kllo9940pd9333jh"},
           {"oauth_signature_method", "HMAC-SHA1"},
           {"oauth_timestamp", "1191242096"},
           {"oauth_token", "nnch734d00sl2jdk"},
           {"oauth_version", "1.0"},
           {"size", "original"}]],
         "GET&http%3A%2F%2Fphotos.example.net%2Fphotos&"
         "file%3Dvacation.jpg%26"
         "oauth_consumer_key%3Ddpf43f3p2l4k3l03%26"
         "oauth_nonce%3Dkllo9940pd9333jh%26"
         "oauth_signature_method%3DHMAC-SHA1%26"
         "oauth_timestamp%3D1191242096%26"
         "oauth_token%3Dnnch734d00sl2jdk%26"
         "oauth_version%3D1.0"
         "%26size%3Doriginal"},
        {"http://oauth.googlecode.com/svn/code/javascript/"
         "example/signature.html #1",
         ["POST",
          "http://photos.example.net/initiate",
          [{"oauth_consumer_key", "dpf43f3p2l4k3l03"},
           {"oauth_nonce", "wIjqoS"},
           {"oauth_signature_method", "HMAC-SHA1"},
           {"oauth_timestamp", "137131200"}]],
         "POST&http%3A%2F%2Fphotos.example.net%2Finitiate&"
         "oauth_consumer_key%3Ddpf43f3p2l4k3l03%26"
         "oauth_nonce%3DwIjqoS%26"
         "oauth_signature_method%3DHMAC-SHA1%26"
         "oauth_timestamp%3D137131200"}
    ],
    [{T, fun() -> R = erlang:apply(fun signature_base_string/3, A) end} ||
      {T, A, R} <- Tests].

oauth_test_() ->
    %% [Signature, HttpMethod, URL, Params, Consumer, TokenSecret] = Args
    Tests = [
        {"http://wiki.oauth.net/TestCases RSA-SHA1 (section 9.3) #1",
         ["jvTp/wX1TYtByB1m+Pbyo0lnCOLIsyGCH7wke8AUs3BpnwZJtAuEJkvQL2/9n4s5w"
          "UmUl4aCI4BwpraNx4RtEXMe5qg5T1LVTGliMRpKasKsW//e+RinhejgCuzoH26dyF"
          "8iY2ZZ/5D1ilgeijhV/vBka5twt399mXwaYdCwFYE=",
          "GET",
          "http://photos.example.net/photos",
          [{"file", "vacaction.jpg"}, % <- spelling error in the example
           {"oauth_consumer_key", "dpf43f3p2l4k3l03"},
           {"oauth_nonce", "13917289812797014437"},
           {"oauth_signature_method", "RSA-SHA1"},
           {"oauth_timestamp", "1196666512"},
           {"oauth_version", "1.0"},
           {"size", "original"}],
          {"dpf43f3p2l4k3l03", <<
           "-----BEGIN CERTIFICATE-----\n"
           "MIIBpjCCAQ+gAwIBAgIBATANBgkqhkiG9w0BAQUFADAZMRcwFQYDVQQDDA5UZXN0\n"
           "IFByaW5jaXBhbDAeFw03MDAxMDEwODAwMDBaFw0zODEyMzEwODAwMDBaMBkxFzAV\n"
           "BgNVBAMMDlRlc3QgUHJpbmNpcGFsMIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKB\n"
           "gQC0YjCwIfYoprq/FQO6lb3asXrxLlJFuCvtinTF5p0GxvQGu5O3gYytUvtC2JlY\n"
           "zypSRjVxwxrsuRcP3e641SdASwfrmzyvIgP08N4S0IFzEURkV1wp/IpH7kH41Etb\n"
           "mUmrXSwfNZsnQRE5SYSOhh+LcK2wyQkdgcMv11l4KoBkcwIDAQABMA0GCSqGSIb3\n"
           "DQEBBQUAA4GBAGZLPEuJ5SiJ2ryq+CmEGOXfvlTtEL2nuGtr9PewxkgnOjZpUy+d\n"
           "4TvuXJbNQc8f4AMWL/tO9w0Fk80rWKp9ea8/df4qMq5qlFWlx6yOLQxumNOmECKb\n"
           "WpkUQDIDJEoFUzKMVuJf4KO/FJ345+BNLGgbJ6WujreoM1X/gYfdnJ/J\n"
           "-----END CERTIFICATE-----"
           >>, rsa_sha1},
          "pfkkdhi9sl3r4s00"],
         true},
        {"from http://term.ie/oauth/example/ on 20140109221421",
         ["0djt1IhpCY+dWqJWxEohGDhKRtg=",
          "GET",
          "http://term.ie/oauth/example/request_token.php",
          [{"oauth_version", "1.0"},
           {"oauth_nonce", "aa228aed620b18b900bdb89ace002f26"},
           {"oauth_timestamp", "1389305661"},
           {"oauth_consumer_key", "key"},
           {"oauth_signature_method", "HMAC-SHA1"}],
          {"key", "secret", hmac_sha1},
          ""],
         true},
        {"http://oauth.googlecode.com/svn/code/javascript/"
         "example/signature.html #1",
         ["2nxXYRjOKrs4qL666Zzr1felAdY=",
          "POST",
          "http://photos.example.net/initiate",
          [{"oauth_consumer_key", "dpf43f3p2l4k3l03"},
           {"oauth_nonce", "wIjqoS"},
           {"oauth_signature_method", "HMAC-SHA1"},
           {"oauth_timestamp", "137131200"}],
          {"dpf43f3p2l4k3l03", "kd94hf93k423kf44", hmac_sha1},
          ""],
         true},
        {"from http://tools.ietf.org/html/rfc5849#section-1.2 #1",
         ["74KNZJeDHnMBp0EMJ9ZHt/XKycU=",
          "POST",
          "https://photos.example.net/initiate",
          [{"oauth_consumer_key", "dpf43f3p2l4k3l03"},
           {"oauth_signature_method", "HMAC-SHA1"},
           {"oauth_timestamp", "137131200"},
           {"oauth_nonce", "wIjqoS"},
           {"oauth_callback", "http://printer.example.com/ready"}],
          {"dpf43f3p2l4k3l03", "kd94hf93k423kf44", hmac_sha1},
          ""],
         true},
        {"from http://tools.ietf.org/html/rfc5849#section-1.2 #4",
         ["MdpQcU8iPSUjWoN/UDMsK2sui9I=",
          "GET",
          "http://photos.example.net/photos?file=vacation.jpg&size=original",
          [{"file", "vacation.jpg"},
           {"size", "original"},
           {"oauth_consumer_key", "dpf43f3p2l4k3l03"},
           {"oauth_token", "nnch734d00sl2jdk"},
           {"oauth_signature_method", "HMAC-SHA1"},
           {"oauth_timestamp", "137131202"},
           {"oauth_nonce", "chapoH"}],
          {"dpf43f3p2l4k3l03", "kd94hf93k423kf44", hmac_sha1},
          "pfkkdhi9sl3r4s00"],
         true}
    ],
    [{T, fun() -> R = erlang:apply(fun verify/6, A) end} ||
      {T, A, R} <- Tests].

-endif.

