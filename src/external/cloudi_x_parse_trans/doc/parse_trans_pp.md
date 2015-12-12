

# Module parse_trans_pp #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Generic parse transform library for Erlang.
__Authors:__ : Ulf Wiger ([`ulf@feuerlabs.com`](mailto:ulf@feuerlabs.com)).
<a name="description"></a>

## Description ##



This module contains some useful utility functions for inspecting
the results of parse transforms or code generation.
The function `main/1` is called from escript, and can be used to
pretty-print debug info in a .beam file from a Linux shell.


Using e.g. the following bash alias:

```

   alias pp='escript $PARSE_TRANS_ROOT/ebin/parse_trans_pp.beam'
```


a file could be pretty-printed using the following command:


`$ pp ex_codegen.beam | less`<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#main-1">main/1</a></td><td></td></tr><tr><td valign="top"><a href="#pp_beam-1">pp_beam/1</a></td><td>
Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.</td></tr><tr><td valign="top"><a href="#pp_beam-2">pp_beam/2</a></td><td>
Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.</td></tr><tr><td valign="top"><a href="#pp_src-2">pp_src/2</a></td><td>Pretty-prints the erlang source code corresponding to Forms into Out.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="main-1"></a>

### main/1 ###


<pre><code>
main(X1::[string()]) -&gt; any()
</code></pre>
<br />


<a name="pp_beam-1"></a>

### pp_beam/1 ###


<pre><code>
pp_beam(Beam::<a href="#type-filename">filename()</a>) -&gt; string() | {error, Reason}
</code></pre>
<br />


Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.
<a name="pp_beam-2"></a>

### pp_beam/2 ###


<pre><code>
pp_beam(Beam::<a href="#type-filename">filename()</a>, Out::<a href="#type-filename">filename()</a>) -&gt; ok | {error, Reason}
</code></pre>
<br />


Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.
<a name="pp_src-2"></a>

### pp_src/2 ###


<pre><code>
pp_src(Forms0::Forms, Out::<a href="#type-filename">filename()</a>) -&gt; ok
</code></pre>
<br />

Pretty-prints the erlang source code corresponding to Forms into Out

