

# Module setup_file #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#consult-1">consult/1</a></td><td>Like <a docgen-rel="seemfa" docgen-href="file#consult/1" href="file.md#consult-1"><code>file:consult/1</code></a>, but supports paths into <code>zip</code> and <code>escript</code> archives.</td></tr><tr><td valign="top"><a href="#list_dir-1">list_dir/1</a></td><td>Like <a docgen-rel="seemfa" docgen-href="file#list_dir/1" href="file.md#list_dir-1"><code>file:list_dir/1</code></a>, but supports paths into <code>zip</code> and <code>escript</code> archives.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#read_file-1">read_file/1</a></td><td>Like <a docgen-rel="seemfa" docgen-href="file#read_file/1" href="file.md#read_file-1"><code>file:read_file/1</code></a>, but supports paths into <code>zip</code> and <code>escript</code> archives.</td></tr><tr><td valign="top"><a href="#script-1">script/1</a></td><td>Like <a docgen-rel="seemfa" docgen-href="file#script/1" href="file.md#script-1"><code>file:script/1</code></a>, but supports paths into <code>zip</code> and <code>escript</code> archives.</td></tr><tr><td valign="top"><a href="#script-2">script/2</a></td><td>Like <a docgen-rel="seemfa" docgen-href="file#script/2" href="file.md#script-2"><code>file:script/2</code></a>, but supports paths into <code>zip</code> and <code>escript</code> archives.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

`close(Fd) -> any()`

<a name="consult-1"></a>

### consult/1 ###

<pre><code>
consult(Filename) -&gt; {ok, Terms} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="http://www.erlang.org/doc/man/file.html#type-name_all">file:name_all()</a></code></li><li><code>Terms = [term()]</code></li><li><code>Reason = <a href="http://www.erlang.org/doc/man/file.html#type-posix">file:posix()</a> | badarg | terminated | system_limit | {Line::integer(), Mod::module(), Term::term()}</code></li></ul>

Like [`file:consult/1`](file.md#consult-1), but supports paths into `zip` and `escript` archives

This function works like `file:consult/1` on normal paths, but instead of failing
on paths that lead into archives, it does a fair job of entering the archive and
producing a result.

<a name="list_dir-1"></a>

### list_dir/1 ###

<pre><code>
list_dir(Dir) -&gt; {ok, Filenames} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Dir = <a href="http://www.erlang.org/doc/man/file.html#type-name_all">file:name_all()</a></code></li><li><code>Filenames = [<a href="http://www.erlang.org/doc/man/file.html#type-filename">file:filename()</a>]</code></li><li><code>Reason = <a href="http://www.erlang.org/doc/man/file.html#type-posix">file:posix()</a> | badarg | {no_translation, Filename::<a href="http://www.erlang.org/doc/man/unicode.html#type-latin1_binary">unicode:latin1_binary()</a>}</code></li></ul>

Like [`file:list_dir/1`](file.md#list_dir-1), but supports paths into `zip` and `escript` archives

This function works like `file:list_dir/1` on normal paths, but instead of failing
on paths that lead into archives, it does a fair job of entering the archive and
producing a result.

<a name="open-2"></a>

### open/2 ###

`open(File, Opts) -> any()`

<a name="read_file-1"></a>

### read_file/1 ###

<pre><code>
read_file(Filename) -&gt; {ok, Binary} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="http://www.erlang.org/doc/man/file.html#type-name_all">file:name_all()</a></code></li><li><code>Binary = binary()</code></li><li><code>Reason = <a href="http://www.erlang.org/doc/man/file.html#type-posix">file:posix()</a> | badarg | terminated | system_limit</code></li></ul>

Like [`file:read_file/1`](file.md#read_file-1), but supports paths into `zip` and `escript` archives

This function works like `file:read_file/1` on normal paths, but instead of failing
on paths that lead into archives, it does a fair job of entering the archive and
producing a result.

<a name="script-1"></a>

### script/1 ###

<pre><code>
script(Filename) -&gt; {ok, Value} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="http://www.erlang.org/doc/man/file.html#type-name_all">file:name_all()</a></code></li><li><code>Value = term()</code></li><li><code>Reason = <a href="http://www.erlang.org/doc/man/file.html#type-posix">file:posix()</a> | badarg | terminated | system_limit | {Line::integer(), Mod::module(), Term::term()}</code></li></ul>

Like [`file:script/1`](file.md#script-1), but supports paths into `zip` and `escript` archives

This function works like `file:script/1` on normal paths, but instead of failing
on paths that lead into archives, it does a fair job of entering the archive and
producing a result.

<a name="script-2"></a>

### script/2 ###

<pre><code>
script(Filename, Bindings) -&gt; {ok, Value} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="http://www.erlang.org/doc/man/file.html#type-name_all">file:name_all()</a></code></li><li><code>Bindings = <a href="http://www.erlang.org/doc/man/erl_eval.html#type-binding_struct">erl_eval:binding_struct()</a></code></li><li><code>Value = term()</code></li><li><code>Reason = <a href="http://www.erlang.org/doc/man/file.html#type-posix">file:posix()</a> | badarg | terminated | system_limit | {Line::integer(), Mod::module(), Term::term()}</code></li></ul>

Like [`file:script/2`](file.md#script-2), but supports paths into `zip` and `escript` archives

This function works like `file:script/2` on normal paths, but instead of failing
on paths that lead into archives, it does a fair job of entering the archive and
producing a result.

