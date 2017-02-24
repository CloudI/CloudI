

# Module metrics #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-metric">metric()</a> ###


<pre><code>
metric() = counter | histogram | gauge | meter
</code></pre>




### <a name="type-metrics_engine">metrics_engine()</a> ###


<pre><code>
metrics_engine() = #metrics_ng{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#decrement_counter-2">decrement_counter/2</a></td><td>decrement a counter with 1.</td></tr><tr><td valign="top"><a href="#decrement_counter-3">decrement_counter/3</a></td><td>decrement a counter with value.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td>delete a metric.</td></tr><tr><td valign="top"><a href="#increment_counter-2">increment_counter/2</a></td><td>increment a counter with 1.</td></tr><tr><td valign="top"><a href="#increment_counter-3">increment_counter/3</a></td><td>increment a counter with Value.</td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td>set the module to use for metrics.</td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td>create a new metric.</td></tr><tr><td valign="top"><a href="#update_gauge-3">update_gauge/3</a></td><td>update a gauge with a value.</td></tr><tr><td valign="top"><a href="#update_histogram-3">update_histogram/3</a></td><td>update an histogram with a value or the duration of a function.</td></tr><tr><td valign="top"><a href="#update_meter-3">update_meter/3</a></td><td>update a meter with a valyue.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="decrement_counter-2"></a>

### decrement_counter/2 ###

<pre><code>
decrement_counter(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any()) -&gt; ok | {error, term()}
</code></pre>
<br />

decrement a counter with 1

<a name="decrement_counter-3"></a>

### decrement_counter/3 ###

<pre><code>
decrement_counter(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any(), Value::pos_integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

decrement a counter with value

<a name="delete-2"></a>

### delete/2 ###

<pre><code>
delete(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any()) -&gt; ok
</code></pre>
<br />

delete a metric

<a name="increment_counter-2"></a>

### increment_counter/2 ###

<pre><code>
increment_counter(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any()) -&gt; ok | {error, term()}
</code></pre>
<br />

increment a counter with 1

<a name="increment_counter-3"></a>

### increment_counter/3 ###

<pre><code>
increment_counter(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any(), Value::pos_integer()) -&gt; ok | {error, term()}
</code></pre>
<br />

increment a counter with Value

<a name="init-1"></a>

### init/1 ###

<pre><code>
init(Mod::atom()) -&gt; <a href="#type-metrics_engine">metrics_engine()</a>
</code></pre>
<br />

set the module to use for metrics.
Types are: counter, histograme, gauge, meter

modules supported are:

* `metrics_folsom`: to interface folsom

* `metrics_exometer`: to interface to exometer

* `metrics_dummy`: a dummy module to use by default.


<a name="new-3"></a>

### new/3 ###

<pre><code>
new(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Type::<a href="#type-metric">metric()</a>, Name::any()) -&gt; ok | {error, term()}
</code></pre>
<br />

create a new metric

<a name="update_gauge-3"></a>

### update_gauge/3 ###

<pre><code>
update_gauge(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any(), Value::number()) -&gt; ok | {error, term()}
</code></pre>
<br />

update a gauge with a value

<a name="update_histogram-3"></a>

### update_histogram/3 ###

<pre><code>
update_histogram(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any(), ValueOrFun::number()) -&gt; ok | {error, term()}
</code></pre>
<br />

update an histogram with a value or the duration of a function. When
passing a function the result will be returned once the metric have been
updated with the duration.

<a name="update_meter-3"></a>

### update_meter/3 ###

<pre><code>
update_meter(Metrics_ng::<a href="#type-metrics_engine">metrics_engine()</a>, Name::any(), Value::number()) -&gt; ok | {error, term()}
</code></pre>
<br />

update a meter with a valyue

