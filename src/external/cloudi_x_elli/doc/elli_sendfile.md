

# Module elli_sendfile #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-sendfile_opts">sendfile_opts()</a> ###


<pre><code>
sendfile_opts() = [{chunk_size, non_neg_integer()}]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#sendfile-5">sendfile/5</a></td><td>Send part of a file on a socket.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="sendfile-5"></a>

### sendfile/5 ###

<pre><code>
sendfile(RawFile::<a href="file.md#type-fd">file:fd()</a>, Socket::<a href="elli_tcp.md#type-socket">elli_tcp:socket()</a>, Offset::non_neg_integer(), Bytes::non_neg_integer(), Opts::<a href="#type-sendfile_opts">sendfile_opts()</a>) -&gt; {ok, non_neg_integer()} | {error, atom()}
</code></pre>
<br />

Send part of a file on a socket.

Basically, @see file:sendfile/5 but for ssl (i.e. not raw OS sockets).
Originally from https://github.com/ninenines/ranch/pull/41/files

