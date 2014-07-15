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

