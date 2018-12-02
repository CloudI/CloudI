

# Module elli_test #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Helper for calling your Elli callback in unit tests.

__Authors:__ Andreas Hasselberg ([`andreas.hasselberg@gmail.com`](mailto:andreas.hasselberg@gmail.com)).

<a name="description"></a>

## Description ##
Only the callback specified is actually run. Elli's response handling is not
used, so the headers will for example not include a content length and the
return format is not standardized.
The unit tests below test `elli_example_callback`.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#call-5">call/5</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="call-5"></a>

### call/5 ###

<pre><code>
call(Method, Path, Headers, Body, Opts) -&gt; <a href="elli_handler.md#type-result">elli_handler:result()</a>
</code></pre>

<ul class="definitions"><li><code>Method = <a href="elli.md#type-http_method">elli:http_method()</a></code></li><li><code>Path = binary()</code></li><li><code>Headers = <a href="elli.md#type-headers">elli:headers()</a></code></li><li><code>Body = <a href="elli.md#type-body">elli:body()</a></code></li><li><code>Opts = <a href="proplists.md#type-proplist">proplists:proplist()</a></code></li></ul>

