

# Module parse_trans_mod #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-compile_options">compile_options()</a> ###



<pre><code>
compile_options() = [term()]
</code></pre>





### <a name="type-erlang_form">erlang_form()</a> ###



<pre><code>
erlang_form() = term()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#abstract_code-1">abstract_code/1</a></td><td></td></tr><tr><td valign="top"><a href="#beam_file-1">beam_file/1</a></td><td></td></tr><tr><td valign="top"><a href="#compile_and_load_forms-1">compile_and_load_forms/1</a></td><td></td></tr><tr><td valign="top"><a href="#compile_and_load_forms-2">compile_and_load_forms/2</a></td><td></td></tr><tr><td valign="top"><a href="#compile_options-1">compile_options/1</a></td><td></td></tr><tr><td valign="top"><a href="#rename_module-2">rename_module/2</a></td><td></td></tr><tr><td valign="top"><a href="#transform_module-3">transform_module/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="abstract_code-1"></a>

### abstract_code/1 ###


<pre><code>
abstract_code(BeamFile::binary()) -&gt; <a href="#type-erlang_form">erlang_form()</a>
</code></pre>
<br />


<a name="beam_file-1"></a>

### beam_file/1 ###


<pre><code>
beam_file(Module::module()) -&gt; binary()
</code></pre>
<br />


<a name="compile_and_load_forms-1"></a>

### compile_and_load_forms/1 ###


<pre><code>
compile_and_load_forms(AbsCode::<a href="#type-erlang_form">erlang_form()</a>) -&gt; ok
</code></pre>
<br />


<a name="compile_and_load_forms-2"></a>

### compile_and_load_forms/2 ###


<pre><code>
compile_and_load_forms(AbsCode::<a href="#type-erlang_form">erlang_form()</a>, Opts::<a href="#type-compile_options">compile_options()</a>) -&gt; ok
</code></pre>
<br />


<a name="compile_options-1"></a>

### compile_options/1 ###


<pre><code>
compile_options(BeamFile::binary() | module()) -&gt; <a href="#type-compile_options">compile_options()</a>
</code></pre>
<br />


<a name="rename_module-2"></a>

### rename_module/2 ###


<pre><code>
rename_module(T::<a href="#type-erlang_form">erlang_form()</a>, NewName::module()) -&gt; <a href="#type-erlang_form">erlang_form()</a>
</code></pre>
<br />


<a name="transform_module-3"></a>

### transform_module/3 ###

`transform_module(Mod, PT, Options) -> any()`


