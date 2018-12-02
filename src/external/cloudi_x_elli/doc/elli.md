

# Module elli #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Elli acceptor manager.

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="description"></a>

## Description ##
This gen_server owns the listen socket and manages the processes
accepting on that socket. When a process waiting for accept gets a
request, it notifies this gen_server so we can start up another
acceptor.

<a name="types"></a>

## Data Types ##




### <a name="type-body">body()</a> ###


__abstract datatype__: `body()`

A binary or iolist.



### <a name="type-header">header()</a> ###


<pre><code>
header() = {Key::binary(), Value::binary() | string()}
</code></pre>




### <a name="type-headers">headers()</a> ###


<pre><code>
headers() = [<a href="#type-header">header()</a>]
</code></pre>




### <a name="type-http_method">http_method()</a> ###


__abstract datatype__: `http_method()`

An uppercase atom representing a known HTTP verb or a
binary for other verbs.



### <a name="type-req">req()</a> ###


__abstract datatype__: `req()`

A record representing an HTTP request.



### <a name="type-response_code">response_code()</a> ###


<pre><code>
response_code() = 100..999
</code></pre>




### <a name="type-state">state()</a> ###


__abstract datatype__: `state()`

Internal state.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_acceptors-1">get_acceptors/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_open_reqs-1">get_open_reqs/1</a></td><td>Equivalent to <a href="#get_open_reqs-2"><tt>get_open_reqs(S, 5000)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_open_reqs-2">get_open_reqs/2</a></td><td></td></tr><tr><td valign="top"><a href="#set_callback-3">set_callback/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td>Create an Elli server process as part of a supervision tree, using the
default configuration.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Stop <code>Server</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_acceptors-1"></a>

### get_acceptors/1 ###

<pre><code>
get_acceptors(S::atom()) -&gt; {reply, {ok, [<a href="ets.md#type-tid">ets:tid()</a>]}, <a href="#type-state">state()</a>}
</code></pre>
<br />

<a name="get_open_reqs-1"></a>

### get_open_reqs/1 ###

<pre><code>
get_open_reqs(S::atom()) -&gt; {reply, {ok, non_neg_integer()}, <a href="#type-state">state()</a>}
</code></pre>
<br />

Equivalent to [`get_open_reqs(S, 5000)`](#get_open_reqs-2).

<a name="get_open_reqs-2"></a>

### get_open_reqs/2 ###

<pre><code>
get_open_reqs(S::atom(), Timeout::non_neg_integer()) -&gt; Reply
</code></pre>

<ul class="definitions"><li><code>Reply = {reply, {ok, non_neg_integer()}, <a href="#type-state">state()</a>}</code></li></ul>

<a name="set_callback-3"></a>

### set_callback/3 ###

<pre><code>
set_callback(S, Callback, CallbackArgs) -&gt; Reply
</code></pre>

<ul class="definitions"><li><code>S = atom()</code></li><li><code>Callback = <a href="elli_handler.md#type-callback_mod">elli_handler:callback_mod()</a></code></li><li><code>CallbackArgs = <a href="elli_handler.md#type-callback_args">elli_handler:callback_args()</a></code></li><li><code>Reply = {reply, ok, <a href="#type-state">state()</a>}</code></li></ul>

<a name="start_link-0"></a>

### start_link/0 ###

<pre><code>
start_link() -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Result = {ok, Pid} | ignore | {error, Error}</code></li><li><code>Pid = pid()</code></li><li><code>Error = {already_started, Pid} | term()</code></li></ul>

Equivalent to [`start_link([{callback, elli_example_callback},{callback_args, []}])`](#start_link-1).

Create an Elli server process as part of a supervision tree, using the
default configuration.

<a name="start_link-1"></a>

### start_link/1 ###

<pre><code>
start_link(Opts) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Opts = [{term(), term()}]</code></li><li><code>Result = {ok, Pid} | ignore | {error, Error}</code></li><li><code>Pid = pid()</code></li><li><code>Error = {already_started, Pid} | term()</code></li></ul>

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Server::atom()) -&gt; {stop, normal, ok, <a href="#type-state">state()</a>}
</code></pre>
<br />

Stop `Server`.

