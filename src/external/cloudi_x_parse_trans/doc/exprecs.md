

# Module exprecs #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Parse transform for generating record access functions.
__Authors:__ : Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).
<a name="description"></a>

## Description ##

This parse transform can be used to reduce compile-time
dependencies in large systems.


In the old days, before records, Erlang programmers often wrote
access functions for tuple data. This was tedious and error-prone.
The record syntax made this easier, but since records were implemented
fully in the pre-processor, a nasty compile-time dependency was
introduced.


This module automates the generation of access functions for
records. While this method cannot fully replace the utility of
pattern matching, it does allow a fair bit of functionality on
records without the need for compile-time dependencies.


Whenever record definitions need to be exported from a module,
inserting a compiler attribute,
`export_records([RecName|...])` causes this transform
to lay out access functions for the exported records:


As an example, consider the following module:

```

  -module(test_exprecs).
  -export([f/0]).
  -compile({parse_transform, exprecs}).
  -record(r, {a = 0 :: integer(),
              b = 0 :: integer(),
              c = 0 :: integer()}).
  -record(s,{a}).
  -export_records([r,s]).
  f() ->
      {new,'#new-r'([])}.
```



Compiling this (assuming exprecs is in the path) will produce the
following code.



```

  -module(test_exprecs).
  -compile({pt_pp_src,true}).
  -export([f/0]).
  -record(r,{a = 0 :: integer(),b = 0 :: integer(),c = 0 :: integer()}).
  -record(s,{a}).
  -export_records([r,s]).
  -export(['#exported_records-'/0,
           '#new-'/1,
           '#info-'/1,
           '#info-'/2,
           '#pos-'/2,
           '#is_record-'/1,
           '#is_record-'/2,
           '#get-'/2,
           '#set-'/2,
           '#fromlist-'/2,
           '#lens-'/2,
           '#new-r'/0,
           '#new-r'/1,
           '#get-r'/2,
           '#set-r'/2,
           '#pos-r'/1,
           '#fromlist-r'/1,
           '#fromlist-r'/2,
           '#info-r'/1,
           '#lens-r'/1,
           '#new-s'/0,
           '#new-s'/1,
           '#get-s'/2,
           '#set-s'/2,
           '#pos-s'/1,
           '#fromlist-s'/1,
           '#fromlist-s'/2,
           '#info-s'/1,
           '#lens-s'/1]).
  -type '#prop-r'() :: {a, integer()} | {b, integer()} | {c, integer()}.
  -type '#attr-r'() :: a | b | c.
  -type '#prop-s'() :: {a, any()}.
  -type '#attr-s'() :: a.
  -spec '#exported_records-'() -> [r | s].
  '#exported_records-'() ->
      [r,s].
  -spec '#new-'(r) -> #r{};
               (s) -> #s{}.
  '#new-'(r) ->
      '#new-r'();
  '#new-'(s) ->
      '#new-s'().
  -spec '#info-'(r) -> [a | b | c];
                (s) -> [a].
  '#info-'(RecName) ->
      '#info-'(RecName, fields).
  -spec '#info-'(r, size) -> 4;
                (r, fields) -> [a | b | c];
                (s, size) -> 2;
                (s, fields) -> [a].
  '#info-'(r, Info) ->
      '#info-r'(Info);
  '#info-'(s, Info) ->
      '#info-s'(Info).
  -spec '#pos-'(r, a) -> 1;
               (r, b) -> 2;
               (r, c) -> 3;
               (s, a) -> 1.
  '#pos-'(r, Attr) ->
      '#pos-r'(Attr);
  '#pos-'(s, Attr) ->
      '#pos-s'(Attr).
  -spec '#is_record-'(any()) -> boolean().
  '#is_record-'(X) ->
      if
          is_record(X, r) ->
              true;
          is_record(X, s) ->
              true;
          true ->
              false
      end.
  -spec '#is_record-'(any(), any()) -> boolean().
  '#is_record-'(s, Rec) when tuple_size(Rec) == 2, element(1, Rec) == s ->
      true;
  '#is_record-'(r, Rec) when tuple_size(Rec) == 4, element(1, Rec) == r ->
      true;
  '#is_record-'(_, _) ->
      false.
  -spec '#get-'(a, #r{}) -> integer();
               (b, #r{}) -> integer();
               (c, #r{}) -> integer();
               (a, #s{}) -> any();
               (['#attr-r'()], #r{}) -> [integer()];
               (['#attr-s'()], #s{}) -> [any()].
  '#get-'(Attrs, Rec) when is_record(Rec, r) ->
      '#get-r'(Attrs, Rec);
  '#get-'(Attrs, Rec) when is_record(Rec, s) ->
      '#get-s'(Attrs, Rec).
  -spec '#set-'(['#prop-r'()], #r{}) -> #r{};
               (['#prop-s'()], #s{}) -> #s{}.
  '#set-'(Vals, Rec) when is_record(Rec, r) ->
      '#set-r'(Vals, Rec);
  '#set-'(Vals, Rec) when is_record(Rec, s) ->
      '#set-s'(Vals, Rec).
  -spec '#fromlist-'(['#prop-r'()], #r{}) -> #r{};
                    (['#prop-s'()], #s{}) -> #s{}.
  '#fromlist-'(Vals, Rec) when is_record(Rec, r) ->
      '#fromlist-r'(Vals, Rec);
  '#fromlist-'(Vals, Rec) when is_record(Rec, s) ->
      '#fromlist-s'(Vals, Rec).
  -spec '#lens-'('#prop-r'(), r) ->
                   {fun((#r{}) -> any()), fun((any(), #r{}) -> #r{})};
               ('#prop-s'(), s) ->
                   {fun((#s{}) -> any()), fun((any(), #s{}) -> #s{})}.
  '#lens-'(Attr, r) ->
      '#lens-r'(Attr);
  '#lens-'(Attr, s) ->
      '#lens-s'(Attr).
  -spec '#new-r'() -> #r{}.
  '#new-r'() ->
      #r{}.
  -spec '#new-r'(['#prop-r'()]) -> #r{}.
  '#new-r'(Vals) ->
      '#set-r'(Vals, #r{}).
  -spec '#get-r'(a, #r{}) -> integer();
                (b, #r{}) -> integer();
                (c, #r{}) -> integer();
                (['#attr-r'()], #r{}) -> [integer()].
  '#get-r'(Attrs, R) when is_list(Attrs) ->
      [
       '#get-r'(A, R) ||
           A <- Attrs
      ];
  '#get-r'(a, R) ->
      R#r.a;
  '#get-r'(b, R) ->
      R#r.b;
  '#get-r'(c, R) ->
      R#r.c;
  '#get-r'(Attr, R) ->
      error(bad_record_op, ['#get-r',Attr,R]).
  -spec '#set-r'(['#prop-r'()], #r{}) -> #r{}.
  '#set-r'(Vals, Rec) ->
      F = fun([], R, _F1) ->
                 R;
             ([{a,V}|T], R, F1) when is_list(T) ->
                 F1(T, R#r{a = V}, F1);
             ([{b,V}|T], R, F1) when is_list(T) ->
                 F1(T, R#r{b = V}, F1);
             ([{c,V}|T], R, F1) when is_list(T) ->
                 F1(T, R#r{c = V}, F1);
             (Vs, R, _) ->
                 error(bad_record_op, ['#set-r',Vs,R])
          end,
      F(Vals, Rec, F).
  -spec '#fromlist-r'(['#prop-r'()]) -> #r{}.
  '#fromlist-r'(Vals) when is_list(Vals) ->
      '#fromlist-r'(Vals, '#new-r'()).
  -spec '#fromlist-r'(['#prop-r'()], #r{}) -> #r{}.
  '#fromlist-r'(Vals, Rec) ->
      AttrNames = [{a,2},{b,3},{c,4}],
      F = fun([], R, _F1) ->
                 R;
             ([{H,Pos}|T], R, F1) when is_list(T) ->
                 case lists:keyfind(H, 1, Vals) of
                     false ->
                         F1(T, R, F1);
                     {_,Val} ->
                         F1(T, setelement(Pos, R, Val), F1)
                 end
          end,
      F(AttrNames, Rec, F).
  -spec '#pos-r'('#attr-r'() | atom()) -> integer().
  '#pos-r'(a) ->
      2;
  '#pos-r'(b) ->
      3;
  '#pos-r'(c) ->
      4;
  '#pos-r'(A) when is_atom(A) ->
      0.
  -spec '#info-r'(fields) -> [a | b | c];
                 (size) -> 3.
  '#info-r'(fields) ->
      record_info(fields, r);
  '#info-r'(size) ->
      record_info(size, r).
  -spec '#lens-r'('#prop-r'()) ->
                    {fun((#r{}) -> any()), fun((any(), #r{}) -> #r{})}.
  '#lens-r'(a) ->
      {fun(R) ->
              '#get-r'(a, R)
       end,
       fun(X, R) ->
              '#set-r'([{a,X}], R)
       end};
  '#lens-r'(b) ->
      {fun(R) ->
              '#get-r'(b, R)
       end,
       fun(X, R) ->
              '#set-r'([{b,X}], R)
       end};
  '#lens-r'(c) ->
      {fun(R) ->
              '#get-r'(c, R)
       end,
       fun(X, R) ->
              '#set-r'([{c,X}], R)
       end};
  '#lens-r'(Attr) ->
      error(bad_record_op, ['#lens-r',Attr]).
  -spec '#new-s'() -> #s{}.
  '#new-s'() ->
      #s{}.
  -spec '#new-s'(['#prop-s'()]) -> #s{}.
  '#new-s'(Vals) ->
      '#set-s'(Vals, #s{}).
  -spec '#get-s'(a, #s{}) -> any();
                (['#attr-s'()], #s{}) -> [any()].
  '#get-s'(Attrs, R) when is_list(Attrs) ->
      [
       '#get-s'(A, R) ||
           A <- Attrs
      ];
  '#get-s'(a, R) ->
      R#s.a;
  '#get-s'(Attr, R) ->
      error(bad_record_op, ['#get-s',Attr,R]).
  -spec '#set-s'(['#prop-s'()], #s{}) -> #s{}.
  '#set-s'(Vals, Rec) ->
      F = fun([], R, _F1) ->
                 R;
             ([{a,V}|T], R, F1) when is_list(T) ->
                 F1(T, R#s{a = V}, F1);
             (Vs, R, _) ->
                 error(bad_record_op, ['#set-s',Vs,R])
          end,
      F(Vals, Rec, F).
  -spec '#fromlist-s'(['#prop-s'()]) -> #s{}.
  '#fromlist-s'(Vals) when is_list(Vals) ->
      '#fromlist-s'(Vals, '#new-s'()).
  -spec '#fromlist-s'(['#prop-s'()], #s{}) -> #s{}.
  '#fromlist-s'(Vals, Rec) ->
      AttrNames = [{a,2}],
      F = fun([], R, _F1) ->
                 R;
             ([{H,Pos}|T], R, F1) when is_list(T) ->
                 case lists:keyfind(H, 1, Vals) of
                     false ->
                         F1(T, R, F1);
                     {_,Val} ->
                         F1(T, setelement(Pos, R, Val), F1)
                 end
          end,
      F(AttrNames, Rec, F).
  -spec '#pos-s'('#attr-s'() | atom()) -> integer().
  '#pos-s'(a) ->
      2;
  '#pos-s'(A) when is_atom(A) ->
      0.
  -spec '#info-s'(fields) -> [a];
                 (size) -> 1.
  '#info-s'(fields) ->
      record_info(fields, s);
  '#info-s'(size) ->
      record_info(size, s).
  -spec '#lens-s'('#prop-s'()) ->
                    {fun((#s{}) -> any()), fun((any(), #s{}) -> #s{})}.
  '#lens-s'(a) ->
      {fun(R) ->
              '#get-s'(a, R)
       end,
       fun(X, R) ->
              '#set-s'([{a,X}], R)
       end};
  '#lens-s'(Attr) ->
      error(bad_record_op, ['#lens-s', Attr]).
  f() ->
      {new,'#new-r'([])}.
```



It is possible to modify the naming rules of exprecs, through the use
of the following attributes (example reflecting the current rules):



```

  -exprecs_prefix(["#", operation, "-"]).
  -exprecs_fname([prefix, record]).
  -exprecs_vfname([fname, "__", version]).
```


The lists must contain strings or any of the following control atoms:

* in `exprecs_prefix`: `operation`

* in `exprecs_fname`: `operation`, `record`, `prefix`

* in `exprecs_vfname`: `operation`, `record`, `prefix`, `fname`, `version`




Exprecs will substitute the control atoms with the string values of the
corresponding items. The result will then be flattened and converted to an
atom (a valid function or type name).


`operation` is one of:



<dt><code>new</code></dt>

 

<dd>Creates a new record</dd>




<dt><code>get</code></dt>

 

<dd>Retrieves given attribute values from a record</dd>




<dt><code>set</code></dt>

 

<dd>Sets given attribute values in a record</dd>




<dt><code>fromlist</code></dt>

 

<dd>Creates a record from a key-value list</dd>




<dt><code>info</code></dt>

 

<dd>Equivalent to record_info/2</dd>




<dt><code>pos</code></dt>

 

<dd>Returns the position of a given attribute</dd>




<dt><code>is_record</code></dt>

 

<dd>Tests if a value is a specific record</dd>




<dt><code>convert</code></dt>

 

<dd>Converts an old record to the current version</dd>




<dt><code>prop</code></dt>

 

<dd>Used only in type specs</dd>




<dt><code>attr</code></dt>

 

<dd>Used only in type specs</dd>




<dt><code>lens</code></dt>

 

<dd>Returns a 'lens' (an accessor pair) as described in
<a href="http://github.com/jlouis/erl-lenses" target="_top"><tt>http://github.com/jlouis/erl-lenses</tt></a></dd>




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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse_transform-2">parse_transform/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse_transform-2"></a>

### parse_transform/2 ###


<pre><code>
parse_transform(Forms::<a href="#type-forms">forms()</a>, Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-forms">forms()</a>
</code></pre>
<br />


