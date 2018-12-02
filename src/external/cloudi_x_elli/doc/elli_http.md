

# Module elli_http #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Elli HTTP request implementation.

<a name="description"></a>

## Description ##
An elli_http process blocks in elli_tcp:accept/2 until a client
connects. It then handles requests on that connection until it's
closed either by the client timing out or explicitly by the user.
<a name="types"></a>

## Data Types ##




### <a name="type-version">version()</a> ###


__abstract datatype__: `version()`

HTTP version as a tuple, i.e. `{0, 9} | {1, 0} | {1, 1}`.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept-4">accept/4</a></td><td>Accept on the socket until a client connects.</td></tr><tr><td valign="top"><a href="#chunk_loop-1">chunk_loop/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_request-4">handle_request/4</a></td><td>Handle a HTTP request that will possibly come on the socket.</td></tr><tr><td valign="top"><a href="#keepalive_loop-3">keepalive_loop/3</a></td><td>Handle multiple requests on the same connection, i.e.</td></tr><tr><td valign="top"><a href="#keepalive_loop-5">keepalive_loop/5</a></td><td></td></tr><tr><td valign="top"><a href="#mk_req-10">mk_req/10</a></td><td></td></tr><tr><td valign="top"><a href="#mk_req-7">mk_req/7</a></td><td></td></tr><tr><td valign="top"><a href="#parse_path-1">parse_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#send_response-4">send_response/4</a></td><td>Generate a HTTP response and send it to the client.</td></tr><tr><td valign="top"><a href="#split_args-1">split_args/1</a></td><td>Split the URL arguments into a proplist.</td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept-4"></a>

### accept/4 ###

<pre><code>
accept(Server, ListenSocket, Options, Callback) -&gt; ok
</code></pre>

<ul class="definitions"><li><code>Server = pid()</code></li><li><code>ListenSocket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a></code></li><li><code>Options = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Callback = <a href="elli_handler.md#type-callback">elli_handler:callback()</a></code></li></ul>

Accept on the socket until a client connects.
Handle the request, then loop if we're using keep alive or chunked transfer.
If [`elli_tcp:accept/3`](elli_tcp.md#accept-3) doesn't return a socket within a configurable
timeout, loop to allow code upgrades of this module.

<a name="chunk_loop-1"></a>

### chunk_loop/1 ###

`chunk_loop(Socket) -> any()`

<a name="handle_request-4"></a>

### handle_request/4 ###

<pre><code>
handle_request(Socket, PrevBin, Options, Callback) -&gt; ConnToken
</code></pre>

<ul class="definitions"><li><code>Socket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a></code></li><li><code>PrevBin = binary()</code></li><li><code>Options = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Callback = <a href="elli_handler.md#type-callback">elli_handler:callback()</a></code></li><li><code>ConnToken = {keep_alive | close, binary()}</code></li></ul>

Handle a HTTP request that will possibly come on the socket.
Returns the appropriate connection token and any buffer containing (parts of)
the next request.

<a name="keepalive_loop-3"></a>

### keepalive_loop/3 ###

`keepalive_loop(Socket, Options, Callback) -> any()`

Handle multiple requests on the same connection, i.e. `"keep alive"`.

<a name="keepalive_loop-5"></a>

### keepalive_loop/5 ###

`keepalive_loop(Socket, NumRequests, Buffer, Options, Callback) -> any()`

<a name="mk_req-10"></a>

### mk_req/10 ###

`mk_req(Method, Scheme, Host, Port, PathTuple, Headers, Body, V, Socket, Callback) -> any()`

<a name="mk_req-7"></a>

### mk_req/7 ###

<pre><code>
mk_req(Method, PathTuple, Headers, Body, V, Socket, Callback) -&gt; Req
</code></pre>

<ul class="definitions"><li><code>Method = <a href="elli.md#type-http_method">elli:http_method()</a></code></li><li><code>PathTuple = {PathType::atom(), RawPath::binary()}</code></li><li><code>Headers = <a href="elli.md#type-headers">elli:headers()</a></code></li><li><code>Body = <a href="elli.md#type-body">elli:body()</a></code></li><li><code>V = <a href="#type-version">version()</a></code></li><li><code>Socket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a> | undefined</code></li><li><code>Callback = <a href="elli_handler.md#type-callback">elli_handler:callback()</a></code></li><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li></ul>

<a name="parse_path-1"></a>

### parse_path/1 ###

`parse_path(X1) -> any()`

<a name="send_response-4"></a>

### send_response/4 ###

`send_response(Req, Code, Headers, UserBody) -> any()`

Generate a HTTP response and send it to the client.

<a name="split_args-1"></a>

### split_args/1 ###

<pre><code>
split_args(Qs::binary()) -&gt; [{binary(), binary() | true}]
</code></pre>
<br />

Split the URL arguments into a proplist.
Lifted from `cowboy_http:x_www_form_urlencoded/2`.

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Server, ListenSocket, Options, Callback) -&gt; pid()
</code></pre>

<ul class="definitions"><li><code>Server = pid()</code></li><li><code>ListenSocket = <a href="elli_tcp.md#type-socket">elli_tcp:socket()</a></code></li><li><code>Options = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li><li><code>Callback = <a href="elli_handler.md#type-callback">elli_handler:callback()</a></code></li></ul>

