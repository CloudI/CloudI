

# Module elli_tcp #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Wrapper for plain and SSL sockets.

<a name="description"></a>

## Description ##
Based on `mochiweb_socket.erl`.
<a name="types"></a>

## Data Types ##




### <a name="type-socket">socket()</a> ###


<pre><code>
socket() = {plain, <a href="inet.md#type-socket">inet:socket()</a>} | {ssl, <a href="ssl.md#type-sslsocket">ssl:sslsocket()</a>}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accept-3">accept/3</a></td><td></td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#listen-3">listen/3</a></td><td></td></tr><tr><td valign="top"><a href="#peername-1">peername/1</a></td><td></td></tr><tr><td valign="top"><a href="#recv-3">recv/3</a></td><td></td></tr><tr><td valign="top"><a href="#send-2">send/2</a></td><td></td></tr><tr><td valign="top"><a href="#sendfile-5">sendfile/5</a></td><td></td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accept-3"></a>

### accept/3 ###

`accept(X1, Server, Timeout) -> any()`

<a name="close-1"></a>

### close/1 ###

`close(X1) -> any()`

<a name="listen-3"></a>

### listen/3 ###

`listen(X1, Port, Opts) -> any()`

<a name="peername-1"></a>

### peername/1 ###

`peername(X1) -> any()`

<a name="recv-3"></a>

### recv/3 ###

`recv(X1, Size, Timeout) -> any()`

<a name="send-2"></a>

### send/2 ###

`send(X1, Data) -> any()`

<a name="sendfile-5"></a>

### sendfile/5 ###

`sendfile(Fd, X2, Offset, Length, Opts) -> any()`

<a name="setopts-2"></a>

### setopts/2 ###

`setopts(X1, Opts) -> any()`

