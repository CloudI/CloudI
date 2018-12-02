

# Module elli_example_callback #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Elli example callback.

__Behaviours:__ [`elli_handler`](elli_handler.md).

<a name="description"></a>

## Description ##
Your callback needs to implement two functions, [`handle/2`](#handle-2) and
[`handle_event/3`](#handle_event-3). For every request, Elli will call your handle
function with the request. When an event happens, like Elli
completed a request, there was a parsing error or your handler
threw an error, [`handle_event/3`](#handle_event-3) is called.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#chunk_loop-1">chunk_loop/1</a></td><td>Send 10 separate chunks to the client.</td></tr><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td>Handle a <code>Req</code>uest.</td></tr><tr><td valign="top"><a href="#handle_event-3">handle_event/3</a></td><td>Handle Elli events, fired throughout processing a request.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="chunk_loop-1"></a>

### chunk_loop/1 ###

`chunk_loop(Ref) -> any()`

Equivalent to [`chunk_loop(Ref, 10)`](#chunk_loop-2).

Send 10 separate chunks to the client.

<a name="handle-2"></a>

### handle/2 ###

<pre><code>
handle(Req, _Args) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li><li><code>_Args = <a href="elli_handler.md#type-callback_args">elli_handler:callback_args()</a></code></li><li><code>Result = <a href="elli_handler.md#type-result">elli_handler:result()</a></code></li></ul>

Handle a `Req`uest.
Delegate to our handler function.

__See also:__ [handle/3](#handle-3).

<a name="handle_event-3"></a>

### handle_event/3 ###

<pre><code>
handle_event(Event, Args, Config) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Event = <a href="elli.md#type-event">elli:event()</a></code></li><li><code>Args = <a href="elli_handler.md#type-callback_args">elli_handler:callback_args()</a></code></li><li><code>Config = [tuple()]</code></li></ul>

Handle Elli events, fired throughout processing a request.

`elli_startup` is sent when Elli is starting up. If you are
implementing a middleware, you can use it to spawn processes,
create ETS tables or start supervised processes in a supervisor
tree.

`request_complete` fires *after* Elli has sent the response to the
client. `Timings` contains timestamps (native units) of events like when the
connection was accepted, when headers/body parsing finished, when the
user callback returns, response sent, etc. `Sizes` contains response sizes
like response headers size, response body or file size.
This allows you to collect performance statistics for monitoring your app.

`request_throw`, `request_error` and `request_exit` events are sent if
the user callback code throws an exception, has an error or
exits. After triggering this event, a generated response is sent to
the user.

`invalid_return` is sent if the user callback code returns a term not
understood by elli, see [`elli_http:execute_callback/1`](elli_http.md#execute_callback-1).
After triggering this event, a generated response is sent to the user.

`chunk_complete` fires when a chunked response is completely
sent. It's identical to the `request_complete` event, except instead
of the response body you get the atom `client` or `server`
depending on who closed the connection. `Sizes` will have the key `chunks`,
which is the total size of all chunks plus encoding overhead.

`request_closed` is sent if the client closes the connection when
Elli is waiting for the next request on a keep alive connection.

`request_timeout` is sent if the client times out when
Elli is waiting for the request.

`request_parse_error` fires if the request is invalid and cannot be parsed by
[`erlang:decode_packet/3`][decode_packet/3] or it contains a path Elli cannot
parse or does not support.

[decode_packet/3]: http://erlang.org/doc/man/erlang.html#decode_packet-3

`client_closed` can be sent from multiple parts of the request
handling. It's sent when the client closes the connection or if for
any reason the socket is closed unexpectedly. The `Where` atom
tells you in which part of the request processing the closed socket
was detected: `receiving_headers`, `receiving_body` or `before_response`.

`client_timeout` can as with `client_closed` be sent from multiple
parts of the request handling. If Elli tries to receive data from
the client socket and does not receive anything within a timeout,
this event fires and the socket is closed.

`bad_request` is sent when Elli detects a request is not well
formatted or does not conform to the configured limits. Currently
the `Reason` variable can be `{too_many_headers, Headers}`
or `{body_size, ContentLength}`.

`file_error` is sent when the user wants to return a file as a
response, but for some reason it cannot be opened.

