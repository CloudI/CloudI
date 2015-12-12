

# Module parse_trans #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Generic parse transform library for Erlang.
__Authors:__ : Ulf Wiger ([`ulf.wiger@feuerlabs.com`](mailto:ulf.wiger@feuerlabs.com)).
<a name="description"></a>

## Description ##



...


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





### <a name="type-insp_f">insp_f()</a> ###



<pre><code>
insp_f() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, A) -&gt; {boolean(), A})
</code></pre>





### <a name="type-options">options()</a> ###



<pre><code>
options() = [{atom(), any()}]
</code></pre>





### <a name="type-type">type()</a> ###



<pre><code>
type() = atom()
</code></pre>





### <a name="type-xform_f_df">xform_f_df()</a> ###



<pre><code>
xform_f_df() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, Acc) -&gt; {<a href="#type-form">form()</a>, Acc} | {<a href="#type-forms">forms()</a>, <a href="#type-form">form()</a>, <a href="#type-forms">forms()</a>, Acc})
</code></pre>





### <a name="type-xform_f_rec">xform_f_rec()</a> ###



<pre><code>
xform_f_rec() = fun((<a href="#type-type">type()</a>, <a href="#type-form">form()</a>, #context{}, Acc) -&gt; {<a href="#type-form">form()</a>, boolean(), Acc} | {<a href="#type-forms">forms()</a>, <a href="#type-form">form()</a>, <a href="#type-forms">forms()</a>, boolean(), Acc})
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#context-2">context/2</a></td><td>
Accessor function for the Context record.</td></tr><tr><td valign="top"><a href="#depth_first-4">depth_first/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_depth_first-4">do_depth_first/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_insert_forms-4">do_insert_forms/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_inspect-4">do_inspect/4</a></td><td></td></tr><tr><td valign="top"><a href="#do_transform-4">do_transform/4</a></td><td></td></tr><tr><td valign="top"><a href="#error-3">error/3</a></td><td>.</td></tr><tr><td valign="top"><a href="#export_function-3">export_function/3</a></td><td></td></tr><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#format_exception-2">format_exception/2</a></td><td>Equivalent to <a href="#format_exception-3"><tt>format_exception(Class, Reason, 4)</tt></a>.</td></tr><tr><td valign="top"><a href="#format_exception-3">format_exception/3</a></td><td>Produces a few lines of user-friendly formatting of exception info.</td></tr><tr><td valign="top"><a href="#function_exists-3">function_exists/3</a></td><td>
Checks whether the given function is defined in Forms.</td></tr><tr><td valign="top"><a href="#get_attribute-2">get_attribute/2</a></td><td>
Returns the value of the first occurence of attribute A.</td></tr><tr><td valign="top"><a href="#get_attribute-3">get_attribute/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_file-1">get_file/1</a></td><td>
Returns the name of the file being compiled.</td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>
Returns the name of the module being compiled.</td></tr><tr><td valign="top"><a href="#get_orig_syntax_tree-1">get_orig_syntax_tree/1</a></td><td>.</td></tr><tr><td valign="top"><a href="#get_pos-1">get_pos/1</a></td><td>
Tries to retrieve the line number from an erl_syntax form.</td></tr><tr><td valign="top"><a href="#initial_context-2">initial_context/2</a></td><td>
Initializes a context record.</td></tr><tr><td valign="top"><a href="#inspect-4">inspect/4</a></td><td>
Equvalent to do_inspect(Fun,Acc,Forms,initial_context(Forms,Options)).</td></tr><tr><td valign="top"><a href="#optionally_pretty_print-3">optionally_pretty_print/3</a></td><td></td></tr><tr><td valign="top"><a href="#plain_transform-2">plain_transform/2</a></td><td>
Performs a transform of <code>Forms</code> using the fun <code>Fun(Form)</code>.</td></tr><tr><td valign="top"><a href="#pp_beam-1">pp_beam/1</a></td><td>
Reads debug_info from the beam file Beam and returns a string containing
the pretty-printed corresponding erlang source code.</td></tr><tr><td valign="top"><a href="#pp_beam-2">pp_beam/2</a></td><td>
Reads debug_info from the beam file Beam and pretty-prints it as
Erlang source code, storing it in the file Out.</td></tr><tr><td valign="top"><a href="#pp_src-2">pp_src/2</a></td><td>Pretty-prints the erlang source code corresponding to Forms into Out.</td></tr><tr><td valign="top"><a href="#replace_function-4">replace_function/4</a></td><td></td></tr><tr><td valign="top"><a href="#return-2">return/2</a></td><td>Checks the transformed result for errors and warnings.</td></tr><tr><td valign="top"><a href="#revert-1">revert/1</a></td><td>Reverts back from Syntax Tools format to Erlang forms.</td></tr><tr><td valign="top"><a href="#revert_form-1">revert_form/1</a></td><td>Reverts a single form back from Syntax Tools format to Erlang forms.</td></tr><tr><td valign="top"><a href="#top-3">top/3</a></td><td></td></tr><tr><td valign="top"><a href="#transform-4">transform/4</a></td><td>
Makes one pass.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="context-2"></a>

### context/2 ###


<pre><code>
context(X1::Attr, Context) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>Attr = module | function | arity | options</code></li></ul>


Accessor function for the Context record.
<a name="depth_first-4"></a>

### depth_first/4 ###


<pre><code>
depth_first(Fun::<a href="#type-xform_f_df">xform_f_df()</a>, Acc, Forms::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>) -&gt; {<a href="#type-forms">forms()</a>, Acc} | {error, list()}
</code></pre>
<br />


<a name="do_depth_first-4"></a>

### do_depth_first/4 ###


<pre><code>
do_depth_first(F::<a href="#type-xform_f_df">xform_f_df()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -&gt; {<a href="#type-forms">forms()</a>, term()}
</code></pre>
<br />


<a name="do_insert_forms-4"></a>

### do_insert_forms/4 ###


<pre><code>
do_insert_forms(X1::above | below, Insert::<a href="#type-forms">forms()</a>, Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -&gt; <a href="#type-forms">forms()</a>
</code></pre>
<br />


<a name="do_inspect-4"></a>

### do_inspect/4 ###


<pre><code>
do_inspect(F::<a href="#type-insp_f">insp_f()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -&gt; term()
</code></pre>
<br />


<a name="do_transform-4"></a>

### do_transform/4 ###


<pre><code>
do_transform(F::<a href="#type-xform_f_rec">xform_f_rec()</a>, Acc::term(), Forms::<a href="#type-forms">forms()</a>, Context::#context{}) -&gt; {<a href="#type-forms">forms()</a>, term()}
</code></pre>
<br />


<a name="error-3"></a>

### error/3 ###


<pre><code>
error(R::Reason, F::Form, I::Info) -&gt; <a href="#type-throw">throw()</a>
</code></pre>

<ul class="definitions"><li><code>Info = [{Key, Value}]</code></li></ul>



Used to report errors detected during the parse transform.

<a name="export_function-3"></a>

### export_function/3 ###

`export_function(F, Arity, Forms) -> any()`


<a name="format_error-1"></a>

### format_error/1 ###


<pre><code>
format_error(Error::{atom(), term()}) -&gt; iolist()
</code></pre>
<br />


<a name="format_exception-2"></a>

### format_exception/2 ###


<pre><code>
format_exception(Class, Reason) -&gt; String
</code></pre>
<br />

Equivalent to [`format_exception(Class, Reason, 4)`](#format_exception-3).
<a name="format_exception-3"></a>

### format_exception/3 ###


<pre><code>
format_exception(Class, Reason, Lines) -&gt; String
</code></pre>

<ul class="definitions"><li><code>Class = error | throw | exit</code></li><li><code>Reason = term()</code></li><li><code>Lines = integer() | infinity</code></li></ul>


Produces a few lines of user-friendly formatting of exception info



This function is very similar to the exception pretty-printing in the shell,
but returns a string that can be used as error info e.g. by error forms
handled by [`return/2`](#return-2). By default, the first 4 lines of the
pretty-printed exception info are returned, but this can be controlled
with the `Lines` parameter.


Note that a stacktrace is generated inside this function.
<a name="function_exists-3"></a>

### function_exists/3 ###


<pre><code>
function_exists(Fname::atom(), Arity::integer(), Forms) -&gt; boolean()
</code></pre>
<br />


Checks whether the given function is defined in Forms.
<a name="get_attribute-2"></a>

### get_attribute/2 ###


<pre><code>
get_attribute(A, Forms) -&gt; any()
</code></pre>

<ul class="definitions"><li><code>A = atom()</code></li></ul>


Returns the value of the first occurence of attribute A.
<a name="get_attribute-3"></a>

### get_attribute/3 ###

`get_attribute(A, Forms, Undef) -> any()`


<a name="get_file-1"></a>

### get_file/1 ###


<pre><code>
get_file(Forms) -&gt; string()
</code></pre>
<br />


Returns the name of the file being compiled.
<a name="get_module-1"></a>

### get_module/1 ###


<pre><code>
get_module(Forms) -&gt; atom()
</code></pre>
<br />


Returns the name of the module being compiled.
<a name="get_orig_syntax_tree-1"></a>

### get_orig_syntax_tree/1 ###


<pre><code>
get_orig_syntax_tree(File) -&gt; Forms
</code></pre>
<br />



Fetches a Syntax Tree representing the code before pre-processing,
that is, including record and macro definitions. Note that macro
definitions must be syntactically complete forms (this function
uses epp_dodger).

<a name="get_pos-1"></a>

### get_pos/1 ###


<pre><code>
get_pos(I::list()) -&gt; integer()
</code></pre>
<br />


Tries to retrieve the line number from an erl_syntax form. Returns a
(very high) dummy number if not successful.
<a name="initial_context-2"></a>

### initial_context/2 ###


<pre><code>
initial_context(Forms, Options) -&gt; #context{}
</code></pre>
<br />


Initializes a context record. When traversing through the form
list, the context is updated to reflect the current function and
arity. Static elements in the context are the file name, the module
name and the options passed to the transform function.
<a name="inspect-4"></a>

### inspect/4 ###


<pre><code>
inspect(F::Fun, Acc::Forms, Forms::Acc, Options) -&gt; NewAcc
</code></pre>

<ul class="definitions"><li><code>Fun = function()</code></li></ul>


Equvalent to do_inspect(Fun,Acc,Forms,initial_context(Forms,Options)).
<a name="optionally_pretty_print-3"></a>

### optionally_pretty_print/3 ###


<pre><code>
optionally_pretty_print(Result::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>, Context::#context{}) -&gt; ok
</code></pre>
<br />


<a name="plain_transform-2"></a>

### plain_transform/2 ###


<pre><code>
plain_transform(Fun, Forms) -&gt; <a href="#type-forms">forms()</a>
</code></pre>

<ul class="definitions"><li><code>Fun = function()</code></li><li><code>Forms = <a href="#type-forms">forms()</a></code></li></ul>



Performs a transform of `Forms` using the fun `Fun(Form)`. `Form` is always
an Erlang abstract form, i.e. it is not converted to syntax_tools
representation. The intention of this transform is for the fun to have a
catch-all clause returning `continue`. This will ensure that it stays robust
against additions to the language.



`Fun(Form)` must return either of the following:



* `NewForm` - any valid form
* `continue` - dig into the sub-expressions of the form
* `{done, NewForm}` - Replace `Form` with `NewForm`; return all following
forms unchanged
* `{error, Reason}` - Abort transformation with an error message.


Example - This transform fun would convert all instances of `P ! Msg` to
`gproc:send(P, Msg)`:

```

  parse_transform(Forms, _Options) ->
      parse_trans:plain_transform(fun do_transform/1, Forms).
  do_transform({'op', L, '!', Lhs, Rhs}) ->
       [NewLhs] = parse_trans:plain_transform(fun do_transform/1, [Lhs]),
       [NewRhs] = parse_trans:plain_transform(fun do_transform/1, [Rhs]),
      {call, L, {remote, L, {atom, L, gproc}, {atom, L, send}},
       [NewLhs, NewRhs]};
  do_transform(_) ->
      continue.
```

<a name="pp_beam-1"></a>

### pp_beam/1 ###


<pre><code>
pp_beam(Beam::<a href="file.md#type-filename">file:filename()</a>) -&gt; string() | {error, Reason}
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
pp_src(Res::Forms, Out::<a href="#type-filename">filename()</a>) -&gt; ok
</code></pre>
<br />

Pretty-prints the erlang source code corresponding to Forms into Out

<a name="replace_function-4"></a>

### replace_function/4 ###

`replace_function(F, Arity, NewForm, Forms) -> any()`


<a name="return-2"></a>

### return/2 ###


<pre><code>
return(Forms, Context) -&gt; Forms | {error, Es, Ws} | {warnings, Forms, Ws}
</code></pre>
<br />

Checks the transformed result for errors and warnings

Errors and warnings can be produced from inside a parse transform, with
a bit of care. The easiest way is to simply produce an `{error, Err}` or
`{warning, Warn}` form in place. This function finds such forms, and
removes them from the form list (otherwise, the linter will crash), and
produces a return value that the compiler can work with.


The format of the `error` and `warning` "forms" must be
`{Tag, {Pos, Module, Info}}`, where:

* `Tag :: error | warning`

* `Pos :: LineNumber | {LineNumber, ColumnNumber}`

* `Module` is a module that exports a corresponding
`Module:format_error(Info)`

* `Info :: term()`



If the error is in the form of a caught exception, `Info` may be produced
using the function [`format_exception/2`](#format_exception-2).

<a name="revert-1"></a>

### revert/1 ###


<pre><code>
revert(Tree) -&gt; Forms
</code></pre>
<br />

Reverts back from Syntax Tools format to Erlang forms.

Note that the Erlang forms are a subset of the Syntax Tools
syntax tree, so this function is safe to call even on a list of
regular Erlang forms.


Note2: R16B03 introduced a bug, where forms produced by
`erl_syntax:revert/1` (specifically, implicit funs) could crash the linter.
This function works around that limitation, after first verifying that it's
necessary to do so. Use of the workaround can be forced with the help of
the `parse_trans` environment variable {revert_workaround, true}. This
variable will be removed when R16B03 is no longer 'supported'.

<a name="revert_form-1"></a>

### revert_form/1 ###


<pre><code>
revert_form(F::Tree) -&gt; Form
</code></pre>
<br />

Reverts a single form back from Syntax Tools format to Erlang forms.

`erl_syntax:revert/1` has had a long-standing bug where it doesn't
completely revert attribute forms. This function deals properly with those
cases.


Note that the Erlang forms are a subset of the Syntax Tools
syntax tree, so this function is safe to call even on a regular Erlang
form.


Note2: R16B03 introduced a bug, where forms produced by
`erl_syntax:revert/1` (specifically, implicit funs) could crash the linter.
This function works around that limitation, after first verifying that it's
necessary to do so. Use of the workaround can be forced with the help of
the `parse_trans` environment variable {revert_workaround, true}. This
variable will be removed when R16B03 is no longer 'supported'.

<a name="top-3"></a>

### top/3 ###


<pre><code>
top(F::function(), Forms::<a href="#type-forms">forms()</a>, Options::list()) -&gt; <a href="#type-forms">forms()</a> | {error, term()}
</code></pre>
<br />


<a name="transform-4"></a>

### transform/4 ###


<pre><code>
transform(Fun, Acc, Forms, Options) -&gt; {TransformedForms, NewAcc}
</code></pre>

<ul class="definitions"><li><code>Fun = function()</code></li><li><code>Options = [{Key, Value}]</code></li></ul>


Makes one pass
