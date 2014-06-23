`cloudi_service_oauth1`
=======================

OAuth v1.0 support as a CloudI service

Usage
-----

To integrate, a callback URL is used which can be provided with a
query string, if necessary.  The callback URL is used as a GET request, with
"`oauth_token`" and "`oauth_verifier`" added to the query string.  The most
common return format for the callback URL is also provided when the
callback URL is set to "`oob`" (i.e., when a callback URL is not used):
"`oauth_token=TOKEN&oauth_verifier=VERIFIER`".  To understand the integration,
refer to
[RFC 5849: Section 2.1](http://tools.ietf.org/html/rfc5849#section-2.1).

RFC 5849 is [illustrated here](http://hueniverse.com/oauth/guide/workflow/)
(as part of the OAuth v1.0 [guide here](http://hueniverse.com/oauth/)).

To verify the result of the callback URL, provide a function in the `verify`
configuration argument for `cloudi_service_oauth1` that takes a single
binary parameter and returns a boolean result.
The binary data is provided as a suffix on the `oauth_verifier` result
from the callback URL response.  Use a [percent encoded](http://en.wikipedia.org/wiki/Percent_encoding#Percent-encoding_reserved_characters)
`+` character (`%2B`) as a delimiter for the provided `oauth_verifier`
and a custom binary suffix, to create the `oauth_verifier` in the response
of the callback URL.  The binary parameter can then be used to verify that
the authentication took place successfully, before the access token is granted.
The boolean result of the verify function is whether the authentication was
successful.

Build
-----

    rebar get-deps
    rebar compile

Tests
-----

    rebar eunit skip_deps=true
    rebar ct skip_deps=true

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

BSD
