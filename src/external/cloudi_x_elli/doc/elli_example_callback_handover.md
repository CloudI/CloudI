

# Module elli_example_callback_handover #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`elli_handler`](elli_handler.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#handle-2">handle/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td>Return <code>{ok, handover}</code> if <code>Req</code>'s path is <code>/hello/world</code>,
otherwise <code>ignore</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="handle-2"></a>

### handle/2 ###

<pre><code>
handle(Req, Args) -&gt; Result
</code></pre>

<ul class="definitions"><li><code>Req = <a href="elli.md#type-req">elli:req()</a></code></li><li><code>Args = <a href="elli_handler.md#type-callback_args">elli_handler:callback_args()</a></code></li><li><code>Result = <a href="elli_handler.md#type-result">elli_handler:result()</a></code></li></ul>

<a name="init-2"></a>

### init/2 ###

`init(Req, Args) -> any()`

Return `{ok, handover}` if `Req`'s path is `/hello/world`,
otherwise `ignore`.

