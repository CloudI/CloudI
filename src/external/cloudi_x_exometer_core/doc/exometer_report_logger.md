

# Module exometer_report_logger #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Exometer report collector and logger.
__Behaviours:__ [`gen_server`](gen_server.md).

__This module defines the `exometer_report_logger` behaviour.__<br /> Required callback functions: `logger_init_input/1`, `logger_init_output/1`, `logger_handle_data/2`.
<a name="description"></a>

## Description ##



This module implements a behavior for collecting reporting data and
handling it (logging to disk or ets, printing to tty, etc.)



The logger has built-in support for receiving input via UDP, TCP or
internal Erlang messaging, as well as a plugin API for custom input
handling. Correspondingly, it has support for output to TTY or ets, as
well as a plugin API for custom output.



An example of how the logger can be used can be found in
`test/exometer_test_udp_reporter.erl`, which implements a UDP-based
reporter as well as an input plugin and an output plugin. This reporter
is used by `test/exometer_report_SUITE.erl`.



Loggers can be combined, e.g. by creating one logger that receives Erlang
messages, and other loggers that receive from different sources, prefix
their reports and pass them on to the first logger.




## Input plugins ##



An input plugin is initiated by `Module:logger_init_input(State)`, where
`State` is whatever was given as a `state` option (default: `undefined`).
The function must create a process and return `{ok, Pid}`. `Pid` is
responsible for setting up whatever input channel is desired, and passes
on incoming data to the logger via Erlang messages `{plugin, Pid, Data}`.




## Output Chaining ##



Each incoming data item is passed through the list of output operators.
Each output operator is able to modify the data (the `tty` and `ets`
operators leave the data unchanged). Output plugins receive the data
in `Module:logger_handle_data(Data, State)`, which must return
`{NewData, NewState}`. The state is private to the plugin, while `NewData`
will be passed along to the next output operator.




## Flow control ##



The logger will handle flow control automatically for `udp` and `tcp`
inputs. If `{active,once}` or `{active, false}`, the logger will trigger
`{active, once}` each time it has handled an incoming message.
If `{active, N}`, it will "refill" the port each time it receives an
indication that it has become passive.


Input plugins create a process in `Module:logger_init_input/1`. This process
can mimick the behavior of Erlang ports by sending a `{plugin_passive, Pid}`
message to the logger. The logger will reply with a message,
`{plugin_active, N}`, where `N` is the value given by the `active` option.
<a name="types"></a>

## Data Types ##




### <a name="type-logger_info">logger_info()</a> ###



<pre><code>
logger_info() = {id, any()} | {input, <a href="#type-proplist">proplist()</a>} | {output, <a href="#type-proplist">proplist()</a>}
</code></pre>





### <a name="type-proplist">proplist()</a> ###



<pre><code>
proplist() = [{atom(), any()}]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#info-0">info/0</a></td><td>List active logger instances.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Lists the settings of a given logger instance.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create a new logger instance.</td></tr><tr><td valign="top"><a href="#start_link-1">start_link/1</a></td><td>Start function for logger instance.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="info-0"></a>

### info/0 ###


<pre><code>
info() -&gt; [{pid(), [<a href="#type-logger_info">logger_info()</a>]}]
</code></pre>
<br />


List active logger instances.


This function lists the instances started via [`new/1`](#new-1), along with their
respective settings as nested property lists.
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(P::pid()) -&gt; [<a href="#type-logger_info">logger_info()</a>]
</code></pre>
<br />

Lists the settings of a given logger instance.
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Options::[{id, any()} | {input, list()} | {output, list()}]) -&gt; {ok, pid()}
</code></pre>
<br />


Create a new logger instance.



This function creates a logger process with the given input and output
parameters.



* `{id, ID}` is mainly for documentation and simplifying identification
of instances returned by [`info/0`](#info-0).
* `{input, PropList}` specifies what the logger listens to. Only the first
`input` entry is regarded, but the option is mandatory.
* `{output, PropList}` specifies what the logger should to with received
data. Multiple `output` entries are allowed, and they will be processed
in the order given.



Valid input options:



* `{mode, udp | tcp | internal | plugin}` defines the protocol
* `{active, false | true | once | N}` provides flow control. Default: `true`.
* (mode-specific options)



Valid output options:



* `{mode, tty | ets | plugin | internal}` defines output types
* (output-specific options)



Mode-specific options, `udp`:



* `{port, integer()}` - UDP port number
* `{options, list()}` - Options to pass to [`gen_udp:open/2`](gen_udp.md#open-2)



Mode-specific options, `tcp`:



* `{port, integer()}` - TCP port number
* `{options, list()}` - Options to pass to [`gen_tcp:listen/2`](gen_tcp.md#listen-2)



Mode-specific options, `tty`:



* `{prefix, iolist()}` - Prefix string inserted before the data, which is
printed as-is (note that any delimiter would need to be part of the prefix)



Mode-specific options, `ets`:
* `{table, ets:table()}` - Ets table identifier. If not specified, an
ordered-set table will be created by the logger process. The incoming
data will be inserted as `{erlang:now(), Data}`.



Mode-specific options, `internal`:
* `{process, PidOrRegname}` specifies another logger instance, which is to
receive data from this logger (if used in output), or which is allowed
to send to this logger (if used in input). If no process is given for
input, any process can send data (on the form
`{exometer_report_logger, Pid, Data}`) to this logger.



Mode-specific options, `plugin`:


* `{module, Module}` - name of callback module
(behaviour: `exometer_report_logger`)
* `{state, State}` - Passed as initial argument to
`Module:logger_init_input/1` or `Module:logger_init_output/1`, depending
on whether the plugin is specified as input or output.
<a name="start_link-1"></a>

### start_link/1 ###


<pre><code>
start_link(Options::<a href="#type-proplist">proplist()</a>) -&gt; {ok, pid()}
</code></pre>
<br />


Start function for logger instance.


This function is the start function eventually called as a result from
[`new/1`](#new-1), but whereas `new/1` creates a supervised instance, this
function simply creates the process. It would normally not be used directly.
