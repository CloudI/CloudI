# CHANGELOG
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
