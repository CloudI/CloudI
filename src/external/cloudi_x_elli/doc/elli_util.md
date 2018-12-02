

# Module elli_util #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-range">range()</a> ###


<pre><code>
range() = {Offset::non_neg_integer(), Length::non_neg_integer()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#encode_range-2">encode_range/2</a></td><td> Encode Range to a Content-Range value.</td></tr><tr><td valign="top"><a href="#file_size-1">file_size/1</a></td><td> Get the size in bytes of the file.</td></tr><tr><td valign="top"><a href="#normalize_range-2">normalize_range/2</a></td><td> If a valid byte-range, or byte-range-set of size 1
is supplied, returns a normalized range in the format
{Offset, Length}.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="encode_range-2"></a>

### encode_range/2 ###

<pre><code>
encode_range(Range::<a href="#type-range">range()</a> | invalid_range, Size::non_neg_integer()) -&gt; ByteRange::iolist()
</code></pre>
<br />

Encode Range to a Content-Range value.

<a name="file_size-1"></a>

### file_size/1 ###

<pre><code>
file_size(Filename) -&gt; Size | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>Filename = <a href="file.md#type-name_all">file:name_all()</a></code></li><li><code>Size = non_neg_integer()</code></li><li><code>Reason = <a href="file.md#type-posix">file:posix()</a> | badarg | invalid_file</code></li></ul>

Get the size in bytes of the file.

<a name="normalize_range-2"></a>

### normalize_range/2 ###

<pre><code>
normalize_range(RangeOrSet, Size) -&gt; Normalized
</code></pre>

<ul class="definitions"><li><code>RangeOrSet = any()</code></li><li><code>Size = integer()</code></li><li><code>Normalized = <a href="#type-range">range()</a> | undefined | invalid_range</code></li></ul>

If a valid byte-range, or byte-range-set of size 1
is supplied, returns a normalized range in the format
{Offset, Length}. Returns undefined when an empty byte-range-set
is supplied and the atom `invalid_range` in all other cases.

