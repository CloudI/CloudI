

# Module elli_handler #
* [Data Types](#types)

__This module defines the `elli_handler` behaviour.__<br /> Required callback functions: `handle/2`, `handle_event/3`.

<a name="types"></a>

## Data Types ##




### <a name="type-callback">callback()</a> ###


__abstract datatype__: `callback()`

A tuple of a <code><a href="#type-callback_mod">callback_mod()</a></code> and <code><a href="#type-callback_args">callback_args()</a></code>.



### <a name="type-callback_args">callback_args()</a> ###


__abstract datatype__: `callback_args()`

Arguments to pass to a <code><a href="#type-callback_mod">callback_mod()</a></code>.



### <a name="type-callback_mod">callback_mod()</a> ###


__abstract datatype__: `callback_mod()`

A callback module.



### <a name="type-event">event()</a> ###


__abstract datatype__: `event()`

Fired throughout processing a request.
See [`elli_example_callback:handle_event/3`](elli_example_callback.md#handle_event-3) for descriptions.



### <a name="type-result">result()</a> ###


<pre><code>
result() = {<a href="elli.md#type-response_code">elli:response_code()</a> | ok, <a href="elli.md#type-headers">elli:headers()</a>, {file, <a href="file.md#type-name_all">file:name_all()</a>} | {file, <a href="file.md#type-name_all">file:name_all()</a>, <a href="elli_util.md#type-range">elli_util:range()</a>}} | {<a href="elli.md#type-response_code">elli:response_code()</a> | ok, <a href="elli.md#type-headers">elli:headers()</a>, <a href="elli.md#type-body">elli:body()</a>} | {<a href="elli.md#type-response_code">elli:response_code()</a> | ok, <a href="elli.md#type-body">elli:body()</a>} | {chunk, <a href="elli.md#type-headers">elli:headers()</a>} | {chunk, <a href="elli.md#type-headers">elli:headers()</a>, <a href="elli.md#type-body">elli:body()</a>} | ignore
</code></pre>

