# CHANGELOG

## v3.3.0

 * Do not use x-forwarded-for for peer #75 
 * Handle arguments with no value in (post|get)_arg_decoded #82
 * Fix compile-time warnings on missing record info. from aleppo #81

## v3.2.0

 * Quell warnings on OTP-21: https://github.com/elli-lib/elli/pull/61

 * Generate HTML docs: https://github.com/elli-lib/elli/pull/58

 * Add OTP-21 to Travis build matrix: https://github.com/elli-lib/elli/pull/62

 * Remove unnecessary `stacktrace_compat` dependency: https://github.com/elli-lib/elli/pull/63

 * Export `elli_request/uri_decode/1` and improve performance: https://github.com/elli-lib/elli/pull/67

 * Update Travis config: https://github.com/elli-lib/elli/pull/69

 * Drop support for OTP-16: https://github.com/elli-lib/elli/pull/71

 * Prefer `OTP_RELEASE` over `rebar_erl_vsn` plugin: https://github.com/elli-lib/elli/pull/73

 * Remove old `maintainers` metadata: https://github.com/elli-lib/elli/pull/74

## v3.1.0

 * Update docs: https://github.com/elli-lib/elli/pull/57

 * Logging and stacktrace OTP-21 support: https://github.com/elli-lib/elli/pull/55

 * Include req_body size in sizes list: https://github.com/elli-lib/elli/pull/52

 * Update CHANGELOG.md for 3.0.0: https://github.com/elli-lib/elli/pull/49

## v3.0.0

 * `scheme`, `host`, and `port` added to the `#req{}` record. Corresponding
   helper functions added to the `elli_request` module.

## v2.1.2

 * Update dependencies and re-enable linting

 * Increase test coverage

 * Declare optional callbacks to elli_handler

 * Add TLS sendfile implementation by James Fish (from Andrew Thompson)

 * Use hackney instead of httpc in tests, due to httpc bug

## v2.0.2

 * Adapt [knutin/elli#108](https://github.com/knutin/elli/pull/108) by Michael Zazaian

 * Incomplete request regression fix by Evan Vigil-McClanahan

 * Handle binary URIs on OTP >=20

 * Bespoke uri_decode/1 to obviate inets dependency by Christoffer Vikström

## v2.0.1

 * Miscellaneous tooling, test, and type spec tweaks

 * Helper functions to reduce redundancy

 * Request start timing fix

## v2.0.0

 * Code and documentation cleanup

 * Instrumentation facilities

 * RFC 2616 section 8.2.3 implementation by Martin Karlsson

 * Send 500 and close connection if file operations fail

## v1.0.5

 * Optimization of SSL accept by Tristan Sloughter

 * Dependency cleanup by Adam Lindberg

## v1.0.4

 * OTP 18.0 compatibility, contributed by Florian Odronitz.

## v1.0.3

 * Various internal cleanup fixes from Andreas Stenius (github.com/kaos)

## v1.0.2

 * Added `elli_request:get_args_decoded/1` which returns the list of
   query args decoded each time it's called.


## v1.0.1

 * Fix bug in SSL acceptor pool where due to failed handshakes, Elli
   runs out of acceptors. Thanks to Stefan Grundmann.

 * In case a handler (or middleware) returns a response Elli does not
   understand, Elli will now respond with a 500 error. Thanks to
   Johannes Huning.

 * Added `elli_request:get_arg_decoded/2,3` which HTTP URI decodes the
   value passed in the request. Thanks to Mariano Valles.

## v1.0

 * SSL using built-in ssl from Erlang/OTP. Thanks to Maas-Maarten Zeeman.

 * "Handover" a socket to user code, making it possible to implement
   WebSockets(https://github.com/mmzeeman/elli_websocket

 * Type fixes from Ingo Struck and Andreas Hasselberg.

## v0.4.1

 * Fix from Christian Lundgren for browsers that include spaces in the
   value of the Content-Length header.

## v0.4

 * Added support for sending ranges of a file with sendfile by
   returning `{Code, Headers, {file, Filename, {Offset, Length}}}`. If
   no offset and length is specified, the entire file is sent. The
   user must provide an appropriate "Content-Length" and
   "Content-Range" header, see the example in
   `elli_example_callback.erl`
   (https://github.com/knutin/elli/blob/master/src/elli_example_callback.erl#L99). Thanks
   Vincent Siliakus (zambal).


## v0.3

 * Breaking change: Timeouts used in the HTTP protocol are now
   configurable. To implement this, changing a record and some
   callbacks was necessary. To upgrade, a restart of Elli is needed.

 * Elli now supports pipelining of any type of request. Some proxies
   or special clients (like ibrowse) will pipeline requests to reduce
   latency.

 * If there are no more file descriptors, Elli will shut down. This
   mimics the behaviour found in Yaws.

 * Chunked transfer responses will now exit the Elli process when the
   client closes the connection. Sending a synchronous chunk will
   return `{error, closed}` if client has closed the connection and
   the `chunk_complete` event is sent to your callback including which
   end closed the connection.

## v0.2.0

 * Breaking change: moved elli_access_log into a separate repository
   at github.com/wooga/elli_access_log. Thanks martinrehfeld.

## v0.1.3

 * Added elli_test which makes it easy to write unit tests for your
   callbacks. Thanks anha0825.

 * Added sendfile support. Thanks chrisavl.

## v0.1.2

 * Added option to specify listen IP address. Thanks hukl.

## v0.1.1

 * Don't look up the peer ip address on every request anymore, do it
   on demand using elli_request:peer/1.

## v0.1

 * Initial release.
