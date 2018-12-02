

# Module elli_middleware_compress #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Response compression as Elli middleware.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#postprocess-3">postprocess/3</a></td><td>Postprocess all requests and compress bodies larger than
<code>compress_byte_size</code> (<code>1024</code> by default).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="postprocess-3"></a>

### postprocess/3 ###

<pre><code>
postprocess(Req, Result, Config) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li><li><code>Result = <a href="elli_handler.md#type-result">elli_handler:result()</a></code></li><li><code>Config = [{compress_byte_size, non_neg_integer()} | tuple()]</code></li></ul>

Postprocess all requests and compress bodies larger than
`compress_byte_size` (`1024` by default).

