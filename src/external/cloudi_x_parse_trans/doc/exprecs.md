

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

```erlang

  -module(test_exprecs).
  -record(r,{a = 0 :: integer(),b = 0 :: integer(),c = 0 :: integer()}).
  -record(s,{a}).
  -record(t,{}).
  -export_records([r,s,t]).
  -export_type(['#prop-r'/0,
                '#attr-r'/0,
                '#prop-s'/0,
                '#attr-s'/0,
                '#prop-t'/0,
                '#attr-t'/0]).
  -type '#prop-s'() :: {a, any()}.
  -type '#attr-s'() :: a.
  -type '#prop-r'() :: {a, any()} | {b, any()} | {c, any()}.
  -type '#attr-r'() :: a | b | c.
  -type '#prop-t'() :: any().
  -type '#attr-t'() :: any().
  -spec '#exported_records-'() -> [r | s | t].
  -spec '#new-'(r) -> #r{};
               (s) -> #s{};
               (t) -> #t{}.
  -spec '#info-'(r) -> ['#attr-r'()];
                (s) -> ['#attr-s'()];
                (t) -> ['#attr-t'()].
  -spec '#info-'(r, size) -> 4;
                (r, fields) -> ['#attr-r'()];
                (s, size) -> 2;
                (s, fields) -> ['#attr-s'()];
                (t, size) -> 1;
                (t, fields) -> ['#attr-t'()].
  -spec '#pos-'(r, a) -> 1;
               (r, b) -> 2;
               (r, c) -> 3;
               (s, a) -> 1.
  -spec '#is_record-'(any()) -> boolean().
  -spec '#is_record-'(any(), any()) -> boolean().
  -spec '#get-'(a, #s{}) -> any();
               (a, #r{}) -> any();
               (b, #r{}) -> any();
               (c, #r{}) -> any();
               (['#attr-t'()], #t{}) -> [];
               (['#attr-s'()], #s{}) -> [any()];
               (['#attr-r'()], #r{}) -> [any()].
  -spec '#set-'(['#prop-r'()], #r{}) -> #r{};
               (['#prop-s'()], #s{}) -> #s{};
               (['#prop-t'()], #t{}) -> #t{}.
  -spec '#fromlist-'(['#prop-r'()], #r{}) -> #r{};
                    (['#prop-s'()], #s{}) -> #s{};
                    (['#prop-t'()], #t{}) -> #t{}.
  -spec '#frommap-'(#{a => any(), b => any(), c => any()}, #r{}) -> #r{};
                   (#{a => any()}, #s{}) -> #s{};
                   (#{}, #t{}) -> #t{}.
  -spec '#lens-'('#attr-r'(), r) ->
                    {fun((#r{}) -> any()), fun((any(), #r{}) -> #r{})};
                ('#attr-s'(), s) ->
                    {fun((#s{}) -> any()), fun((any(), #s{}) -> #s{})};
                ('#attr-t'(), t) ->
                    {fun((#t{}) -> any()), fun((any(), #t{}) -> #t{})}.
  -spec '#new-r'() -> #r{}.
  -spec '#new-r'(['#prop-r'()]) -> #r{}.
  -spec '#get-r'(a, #r{}) -> any();
                (b, #r{}) -> any();
                (c, #r{}) -> any();
                (['#attr-r'()], #r{}) -> [any()].
  -spec '#set-r'(['#prop-r'()], #r{}) -> #r{}.
  -spec '#fromlist-r'(['#prop-r'()]) -> #r{}.
  -spec '#fromlist-r'(['#prop-r'()], #r{}) -> #r{}.
  -spec '#frommap-r'(#{a => any(), b => any(), c => any()}) -> #r{}.
  -spec '#frommap-r'(#{a => any(), b => any(), c => any()}, #r{}) -> #r{}.
  -spec '#pos-r'('#attr-r'() | atom()) -> integer().
  -spec '#info-r'(fields) -> [a | b | c];
                 (size) -> 4.
  -spec '#lens-r'('#attr-r'()) ->
                     {fun((#r{}) -> any()), fun((any(), #r{}) -> #r{})}.
  -spec '#new-s'() -> #s{}.
  -spec '#new-s'(['#prop-s'()]) -> #s{}.
  -spec '#get-s'(a, #s{}) -> any();
                (['#attr-s'()], #s{}) -> [any()].
  -spec '#set-s'(['#prop-s'()], #s{}) -> #s{}.
  -spec '#fromlist-s'(['#prop-s'()]) -> #s{}.
  -spec '#fromlist-s'(['#prop-s'()], #s{}) -> #s{}.
  -spec '#frommap-s'(#{a => any()}) -> #s{}.
  -spec '#frommap-s'(#{a => any()}, #s{}) -> #s{}.
  -spec '#pos-s'('#attr-s'() | atom()) -> integer().
  -spec '#info-s'(fields) -> [a];
                 (size) -> 2.
  -spec '#lens-s'('#attr-s'()) ->
                     {fun((#s{}) -> any()), fun((any(), #s{}) -> #s{})}.
  -spec '#new-t'() -> #t{}.
  -spec '#new-t'(['#prop-t'()]) -> #t{}.
  -spec '#get-t'(['#attr-t'()], #t{}) -> [any()].
  -spec '#set-t'(['#prop-t'()], #t{}) -> #t{}.
  -spec '#fromlist-t'(['#prop-t'()]) -> #t{}.
  -spec '#fromlist-t'(['#prop-t'()], #t{}) -> #t{}.
  -spec '#frommap-t'(#{}) -> #t{}.
  -spec '#frommap-t'(#{}, #t{}) -> #t{}.
  -spec '#pos-t'('#attr-t'() | atom()) -> integer().
  -spec '#info-t'(fields) -> [];
                 (size) -> 1.
  -spec '#lens-t'('#attr-t'()) ->
                     {fun((#t{}) -> any()), fun((any(), #t{}) -> #t{})}.
  -file("c:/git/etp/_checkouts/parse_trans/examples/test_exprecs.erl", 1).
  '#exported_records-'() ->
      [r,s,t].
  '#new-'(r) ->
      '#new-r'();
  '#new-'(s) ->
      '#new-s'();
  '#new-'(t) ->
      '#new-t'().
  '#info-'(RecName) ->
      '#info-'(RecName, fields).
  '#info-'(r, Info) ->
      '#info-r'(Info);
  '#info-'(s, Info) ->
      '#info-s'(Info);
  '#info-'(t, Info) ->
      '#info-t'(Info).
  '#pos-'(r, Attr) ->
      '#pos-r'(Attr);
  '#pos-'(s, Attr) ->
      '#pos-s'(Attr);
  '#pos-'(t, Attr) ->
      '#pos-t'(Attr).
  '#is_record-'(X) ->
      if
          is_record(X, r, 4) ->
              true;
          is_record(X, s, 2) ->
              true;
          is_record(X, t, 1) ->
              true;
          true ->
              false
      end.
  '#is_record-'(t, Rec) when tuple_size(Rec) == 1, element(1, Rec) == t ->
      true;
  '#is_record-'(s, Rec) when tuple_size(Rec) == 2, element(1, Rec) == s ->
      true;
  '#is_record-'(r, Rec) when tuple_size(Rec) == 4, element(1, Rec) == r ->
      true;
  '#is_record-'(_, _) ->
      false.
  '#get-'(Attrs, {r,_,_,_} = Rec) when true ->
      '#get-r'(Attrs, Rec);
  '#get-'(Attrs, {s,_} = Rec) when true ->
      '#get-s'(Attrs, Rec);
  '#get-'(Attrs, {t} = Rec) when true ->
      '#get-t'(Attrs, Rec).
  '#set-'(Vals, {r,_,_,_} = Rec) when true ->
      '#set-r'(Vals, Rec);
  '#set-'(Vals, {s,_} = Rec) when true ->
      '#set-s'(Vals, Rec);
  '#set-'(Vals, {t} = Rec) when true ->
      '#set-t'(Vals, Rec).
  '#fromlist-'(Vals, {r,_,_,_} = Rec) when true ->
      '#fromlist-r'(Vals, Rec);
  '#fromlist-'(Vals, {s,_} = Rec) when true ->
      '#fromlist-s'(Vals, Rec);
  '#fromlist-'(Vals, {t} = Rec) when true ->
      '#fromlist-t'(Vals, Rec).
  '#frommap-'(Vals, {r,_,_,_} = Rec) when true ->
      '#frommap-r'(Vals, Rec);
  '#frommap-'(Vals, {s,_} = Rec) when true ->
      '#frommap-s'(Vals, Rec);
  '#frommap-'(Vals, {t} = Rec) when true ->
      '#frommap-t'(Vals, Rec).
  '#lens-'(Attr, r) ->
      '#lens-r'(Attr);
  '#lens-'(Attr, s) ->
      '#lens-s'(Attr);
  '#lens-'(Attr, t) ->
      '#lens-t'(Attr).
  '#new-r'() ->
      {r,0,0,0}.
  '#new-r'(Vals) ->
      '#set-r'(Vals, {r,0,0,0}).
  '#get-r'(Attrs, R) when is_list(Attrs) ->
      [
       '#get-r'(A, R) ||
           A <- Attrs
      ];
  '#get-r'(a, R) ->
      case R of
          {r,rec0,_,_} ->
              rec0;
          _ ->
              error({badrecord,r})
      end;
  '#get-r'(b, R) ->
      case R of
          {r,_,rec1,_} ->
              rec1;
          _ ->
              error({badrecord,r})
      end;
  '#get-r'(c, R) ->
      case R of
          {r,_,_,rec2} ->
              rec2;
          _ ->
              error({badrecord,r})
      end;
  '#get-r'(Attr, R) ->
      error(bad_record_op, ['#get-r',Attr,R]).
  '#set-r'(Vals, Rec) ->
      F = % fun-info: {0,0,'-#set-r/2-fun-0-'}
          fun([], R, _F1) ->
                 R;
             ([{a,V}|T], R, F1) when is_list(T) ->
                 F1(T,
                    begin
                        rec3 = R,
                        case rec3 of
                            {r,_,_,_} ->
                                setelement(2, rec3, V);
                            _ ->
                                error({badrecord,r})
                        end
                    end,
                    F1);
             ([{b,V}|T], R, F1) when is_list(T) ->
                 F1(T,
                    begin
                        rec4 = R,
                        case rec4 of
                            {r,_,_,_} ->
                                setelement(3, rec4, V);
                            _ ->
                                error({badrecord,r})
                        end
                    end,
                    F1);
             ([{c,V}|T], R, F1) when is_list(T) ->
                 F1(T,
                    begin
                        rec5 = R,
                        case rec5 of
                            {r,_,_,_} ->
                                setelement(4, rec5, V);
                            _ ->
                                error({badrecord,r})
                        end
                    end,
                    F1);
             (Vs, R, _) ->
                 error(bad_record_op, ['#set-r',Vs,R])
          end,
      F(Vals, Rec, F).
  '#fromlist-r'(Vals) when is_list(Vals) ->
      '#fromlist-r'(Vals, '#new-r'()).
  '#fromlist-r'(Vals, Rec) ->
      AttrNames = [{a,2},{b,3},{c,4}],
      F = % fun-info: {0,0,'-#fromlist-r/2-fun-0-'}
          fun([], R, _F1) ->
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
  '#frommap-r'(Vals) when is_map(Vals) ->
      '#frommap-r'(Vals, '#new-r'()).
  '#frommap-r'(Vals, Rec) ->
      List = maps:to_list(Vals),
      '#fromlist-r'(List, Rec).
  '#pos-r'(a) ->
      2;
  '#pos-r'(b) ->
      3;
  '#pos-r'(c) ->
      4;
  '#pos-r'(A) when is_atom(A) ->
      0.
  '#info-r'(fields) ->
      [a,b,c];
  '#info-r'(size) ->
      4.
  '#lens-r'(a) ->
      {% fun-info: {0,0,'-#lens-r/1-fun-0-'}
       fun(R) ->
              '#get-r'(a, R)
       end,
       % fun-info: {0,0,'-#lens-r/1-fun-1-'}
       fun(X, R) ->
              '#set-r'([{a,X}], R)
       end};
  '#lens-r'(b) ->
      {% fun-info: {0,0,'-#lens-r/1-fun-2-'}
       fun(R) ->
              '#get-r'(b, R)
       end,
       % fun-info: {0,0,'-#lens-r/1-fun-3-'}
       fun(X, R) ->
              '#set-r'([{b,X}], R)
       end};
  '#lens-r'(c) ->
      {% fun-info: {0,0,'-#lens-r/1-fun-4-'}
       fun(R) ->
              '#get-r'(c, R)
       end,
       % fun-info: {0,0,'-#lens-r/1-fun-5-'}
       fun(X, R) ->
              '#set-r'([{c,X}], R)
       end};
  '#lens-r'(Attr) ->
      error(bad_record_op, ['#lens-r',Attr]).
  '#new-s'() ->
      {s,undefined}.
  '#new-s'(Vals) ->
      '#set-s'(Vals, {s,undefined}).
  '#get-s'(Attrs, R) when is_list(Attrs) ->
      [
       '#get-s'(A, R) ||
           A <- Attrs
      ];
  '#get-s'(a, R) ->
      case R of
          {s,rec6} ->
              rec6;
          _ ->
              error({badrecord,s})
      end;
  '#get-s'(Attr, R) ->
      error(bad_record_op, ['#get-s',Attr,R]).
  '#set-s'(Vals, Rec) ->
      F = % fun-info: {0,0,'-#set-s/2-fun-0-'}
          fun([], R, _F1) ->
                 R;
             ([{a,V}|T], R, F1) when is_list(T) ->
                 F1(T,
                    begin
                        rec7 = R,
                        case rec7 of
                            {s,rec8} ->
                                {s,V};
                            _ ->
                                error({badrecord,s})
                        end
                    end,
                    F1);
             (Vs, R, _) ->
                 error(bad_record_op, ['#set-s',Vs,R])
          end,
      F(Vals, Rec, F).
  '#fromlist-s'(Vals) when is_list(Vals) ->
      '#fromlist-s'(Vals, '#new-s'()).
  '#fromlist-s'(Vals, Rec) ->
      AttrNames = [{a,2}],
      F = % fun-info: {0,0,'-#fromlist-s/2-fun-0-'}
          fun([], R, _F1) ->
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
  '#frommap-s'(Vals) when is_map(Vals) ->
      '#frommap-s'(Vals, '#new-s'()).
  '#frommap-s'(Vals, Rec) ->
      List = maps:to_list(Vals),
      '#fromlist-s'(List, Rec).
  '#pos-s'(a) ->
      2;
  '#pos-s'(A) when is_atom(A) ->
      0.
  '#info-s'(fields) ->
      [a];
  '#info-s'(size) ->
      2.
  '#lens-s'(a) ->
      {% fun-info: {0,0,'-#lens-s/1-fun-0-'}
       fun(R) ->
              '#get-s'(a, R)
       end,
       % fun-info: {0,0,'-#lens-s/1-fun-1-'}
       fun(X, R) ->
              '#set-s'([{a,X}], R)
       end};
  '#lens-s'(Attr) ->
      error(bad_record_op, ['#lens-s',Attr]).
  '#new-t'() ->
      {t}.
  '#new-t'(Vals) ->
      '#set-t'(Vals, {t}).
  '#get-t'(Attrs, R) when is_list(Attrs) ->
      [
       '#get-t'(A, R) ||
           A <- Attrs
      ];
  '#get-t'(Attr, R) ->
      error(bad_record_op, ['#get-t',Attr,R]).
  '#set-t'(Vals, Rec) ->
      F = % fun-info: {0,0,'-#set-t/2-fun-0-'}
          fun([], R, _F1) ->
                 R;
             (Vs, R, _) ->
                 error(bad_record_op, ['#set-t',Vs,R])
          end,
      F(Vals, Rec, F).
  '#fromlist-t'(Vals) when is_list(Vals) ->
      '#fromlist-t'(Vals, '#new-t'()).
  '#fromlist-t'(Vals, Rec) ->
      AttrNames = [],
      F = % fun-info: {0,0,'-#fromlist-t/2-fun-0-'}
          fun([], R, _F1) ->
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
  '#frommap-t'(Vals) when is_map(Vals) ->
      '#frommap-t'(Vals, '#new-t'()).
  '#frommap-t'(Vals, Rec) ->
      List = maps:to_list(Vals),
      '#fromlist-t'(List, Rec).
  '#pos-t'(A) when is_atom(A) ->
      0.
  '#info-t'(fields) ->
      [];
  '#info-t'(size) ->
      1.
  '#lens-t'(Attr) ->
      error(bad_record_op, ['#lens-t',Attr]).
  f() ->
      foo.
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

