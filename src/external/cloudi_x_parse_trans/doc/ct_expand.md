

# Module ct_expand #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Compile-time expansion utility.
__Authors:__ : Ulf Wiger ([`ulf@feuerlabs.com`](mailto:ulf@feuerlabs.com)).
<a name="description"></a>

## Description ##



This module serves as an example of parse_trans-based transforms,
but might also be a useful utility in its own right.
The transform searches for calls to the pseudo-function
`ct_expand:term(Expr)`, and then replaces the call site with the
result of evaluating `Expr` at compile-time.



For example, the line



`ct_expand:term(lists:sort([3,5,2,1,4]))`



would be expanded at compile-time to `[1,2,3,4,5]`.



ct_expand has now been extended to also evaluate calls to local functions.
See examples/ct_expand_test.erl for some examples.



A debugging facility exists: passing the option {ct_expand_trace, Flags} as an option,
or adding a compiler attribute -ct_expand_trace(Flags) will enable a form of call trace.


`Flags` can be `[]` (no trace) or `[F]`, where `F` is `c` (call trace),
`r` (return trace), or `x` (exception trace)'.

<a name="types"></a>

## Data Types ##




### <a name="type-form">form()</a> ###



<pre><code>
form() = any()
</code></pre>





### <a name="type-forms">forms()</a> ###



<pre><code>
forms() = [<a href="#type-form">form()</a>]
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [{atom(), any()}]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#extract_fun-3">extract_fun/3</a></td><td></td></tr><tr><td valign="top"><a href="#lfun_rewrite-2">lfun_rewrite/2</a></td><td></td></tr><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="extract_fun-3"></a>

### extract_fun/3 ###

`extract_fun(Name, Arity, Forms) -> any()`


<a name="lfun_rewrite-2"></a>

### lfun_rewrite/2 ###

`lfun_rewrite(Exprs, Forms) -> any()`


<a name="parse_transform-2"></a>

### parse_transform/2 ###


<pre><code>
parse_transform(Forms::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-forms">forms()</a>
</code></pre>
<br />


