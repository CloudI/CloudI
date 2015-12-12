

# Module parse_trans_codegen #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Parse transform for code generation pseduo functions.
__Authors:__ : Ulf Wiger ([`ulf@feuerlabs.com`](mailto:ulf@feuerlabs.com)).
<a name="description"></a>

## Description ##



...

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format_error-1">format_error/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td>
Searches for calls to pseudo functions in the module <code>codegen</code>,
and converts the corresponding erlang code to a data structure
representing the abstract form of that code.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="format_error-1"></a>

### format_error/1 ###

`format_error(E) -> any()`


<a name="parse_transform-2"></a>

### parse_transform/2 ###


<pre><code>
parse_transform(Forms, Options) -&gt; NewForms
</code></pre>
<br />



Searches for calls to pseudo functions in the module `codegen`,
and converts the corresponding erlang code to a data structure
representing the abstract form of that code.



The purpose of these functions is to let the programmer write
the actual code that is to be generated, rather than manually
writing abstract forms, which is more error prone and cannot be
checked by the compiler until the generated module is compiled.



Supported functions:




## gen_function/2 ##



Usage: `codegen:gen_function(Name, Fun)`



Substitutes the abstract code for a function with name `Name`
and the same behaviour as `Fun`.


`Fun` can either be a anonymous `fun`, which is then converted to
a named function, or it can be an `implicit fun`, e.g.
`fun is_member/2`. In the latter case, the referenced function is fetched
and converted to an abstract form representation. It is also renamed
so that the generated function has the name `Name`.



Another alternative is to wrap a fun inside a list comprehension, e.g.

```

  f(Name, L) ->
      codegen:gen_function(
          Name,
          [ fun({'$var',X}) ->
               {'$var', Y}
            end || {X, Y} &lt;- L ]).
```




Calling the above with `f(foo, [{1,a},{2,b},{3,c}])` will result in
generated code corresponding to:

```

  foo(1) -> a;
  foo(2) -> b;
  foo(3) -> c.
```




## gen_functions/1 ##



Takes a list of `{Name, Fun}` tuples and produces a list of abstract
data objects, just as if one had written
`[codegen:gen_function(N1,F1),codegen:gen_function(N2,F2),...]`.




## exprs/1 ##



Usage: `codegen:exprs(Fun)`



`Fun` is either an anonymous function, or an implicit fun with only one
function clause. This "function" takes the body of the fun and produces
a data type representing the abstract form of the list of expressions in
the body. The arguments of the function clause are ignored, but can be
used to ensure that all necessary variables are known to the compiler.




## gen_module/3 ##



Generates abstract forms for a complete module definition.



Usage: `codegen:gen_module(ModuleName, Exports, Functions)`



`ModuleName` is either an atom or a `{'$var', V}` reference.



`Exports` is a list of `{Function, Arity}` tuples.



`Functions` is a list of `{Name, Fun}` tuples analogous to that for
`gen_functions/1`.




## Variable substitution ##



It is possible to do some limited expansion (importing a value
bound at compile-time), using the construct `{'$var', V}`, where
`V` is a bound variable in the scope of the call to `gen_function/2`.


Example:

```

  gen(Name, X) ->
     codegen:gen_function(Name, fun(L) -> lists:member({'$var',X}, L) end).
```


After transformation, calling `gen(contains_17, 17)` will yield the
abstract form corresponding to:

```

  contains_17(L) ->
     lists:member(17, L).
```




## Form substitution ##



It is possible to inject abstract forms, using the construct
`{'$form', F}`, where `F` is bound to a parsed form in
the scope of the call to `gen_function/2`.


Example:

```

  gen(Name, F) ->
     codegen:gen_function(Name, fun(X) -> X =:= {'$form',F} end).
```


After transformation, calling `gen(is_foo, {atom,0,foo})` will yield the
abstract form corresponding to:

```

  is_foo(X) ->
     X =:= foo.
```

