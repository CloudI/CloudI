

# Module exometer_probe #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Interface library for managing probes.
__Behaviours:__ [`exometer_entry`](exometer_entry.md).

__This module defines the `exometer_probe` behaviour.__<br /> Required callback functions: `behaviour/0`, `probe_init/3`, `probe_terminate/1`, `probe_setopts/3`, `probe_update/2`, `probe_get_value/2`, `probe_get_datapoints/1`, `probe_reset/1`, `probe_sample/1`, `probe_handle_msg/2`, `probe_code_change/3`.
<a name="description"></a>

## Description ##

<br />



This library contains the main API for accessing all probes
executing in exometer.



All exported functions in the `exomter_probe` module are invoked
by the `exometer` module; a developer will not have to call
`exometer_probe` functions directly.



A probe is an implementation of the `exometer_probe` behavior
which runs in its own process in order to collect data to be
handled and reported by exometer. The implementation will be
invoked through the `exomoeter_probe` module, which, as stated
above, in its turn is invoked by the `exometer` module.



A custom exometer probe is invoked by mapping a type to the module
name of the custom exometer probe module. All metrics created with the
given type will trigger the invocation of the new probe module. See
[Configuring type - entry maps](#Configuring_type_-_entry_maps) for details on how to setup
such maps.



If the data can be collected at a high speed, and without
blocking, an `exometer_entry` implementation can be used instead
to do the gathering in-process.



A probe is created throgh the `exomter_probe:new/3` call, which in
its turn is called by `exometer:new/3`. During probe creation, a
new process is spawned to handle the probe and call its
implementation.  While the created process is not a gen_server, it
behaves similarly and provides a state to all implementation
calls.



Once running, the probe collects data from a subsystem, such as
`/proc`, `sysfs`, and `netlink`, through timer-based calls to
`probe_sample/1` or explicit calls to `probe_update/2`.



A probe implementation can support any number of data points,
where each data point is a specifric sample from the probe. For
example, a probe that measures network traffic would have
`rx_packets`, `tx_packets`, `errors`, `dropped`, and other data
points reported by `ifconfig(8)` and `ip(8)`.



Values are retrieved from the probe through the `probe_get_value/2`
call, which specifies the data points to be returned. The probe is
expected to gather the given data points and return them to the
caller.




### <a name="The_probe_callback_interface">The probe callback interface</a> ###



The following functions are to be implemented and exported by a probe
implementation.




#### <a name="behaviour/0">behaviour/0</a> ####



The `behaviour/0` function for an entry implementation should return
the atom `probe`. This function will be involved by the
exometer system in order to determine if a callback is
an entry or a probe.




#### <a name="probe_init/3">probe_init/3</a> ####


The `probe_init/3` function is invoked as follows:



```erlang

       probe_init(Name, Type, Options)
```



The implementation shall initiate the probe, create the
necessary state, and return it for furure access
through `probe_update/2`, `probe_sample/1` and `get_value/2` calls.



The arguments are as follows:



+ `Name`
Specifies the name of the metric to be created as a list of atoms.



+ `Type`
Specifies the type provided to the `exometer:new/3` call (before it
was translated by the type - exometer probe map). It can be used if several
different types are mapped to the same probe module.



+ `Options`
Specifies an option list that contains additional setup directives to
the probe. The actual options to support are a combination of the
standard options, described below, and probe specific options
processed by `probe_init/3`.



Standard options are processed directly by `new/3`, before
`probe_init/3` is calledm and are as follows:



+ `{priority, P}`
Will be forwarded by the probe's process to `erlang:process_flag/2`.



+ `{min_heap_size, S}`
Will be forwarded by the probe's process to `erlang:process_flag/2`.



+ `{min_bin_vheap_size, S}`
Will be forwarded by the probe's process to `erlang:process_flag/2`.



+ `{sensitive, true | false}`
Will be forwarded by the probe's process to `erlang:process_flag/2`.



+ `{sample_interval, t}`
Specifies the interval, in milliseconds, that `exometer_probe:sample/1`.
should be invoked at.



The `probe_init/3` implementation is invoked by `exometer:new/3`,
which calls `exometer_probe:new/3`, which invokes the probe
implementation..



The `probe_init/3` function shall return `{ok, State}` where State
is a tuple that will be provided as a the `State` argument to all
future probe implementation calls for the metric.



If the `sample_interval` option has been specified in `Opts`,
probe_sample/2' will be invoked immediately after `probe_init/2`
returns to retrieve a first sample. After that, `probe_sample/2`
will repeatedly will be called by the probe process at the
millisecond-specified interval.



Should `probe_init/3` return antyhing else but `{ok, State}`,
invoking `new/3` call will fail.


#### <a name="probe_terminate/1">probe_terminate/1</a> ####


The `probe_terminate/1` function is invoked as follows:



```erlang

       probe_terminate(State)
```



The custom probe shall release any resources associated with the
given state and return `ok`.



The arguments are as follows:



+ `State`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.

The `probe_terminate/1` implementation is invoked by `exometer:delete/1`, which
calls `exometer_probe:delete/3`, which invokes the probe
implementation.


#### <a name="probe_setopts/3">probe_setopts/3</a> ####


The `probe_setopts/2` function is invoked as follows:



```erlang

       probe_setopts(Entry, Opts, State)
```



The `probe_setopts/4` implementation is invoked by
`exometer:setopts/3`, which calls `exometer_probe:setopts/3`,
which invokes the probe implementation.



The implementation of this function shall modify the options of a
probe. The `setopts/3` function, which will process standard
options before invoking `probe_setopts/4` with the remaining
options. See the documentation for `probe_init/3` for details.



The arguments are as follows:



+ `Entry`
The (opaque) exometer entry record. See [`exometer_info`](exometer_info.md) for
information on how to inspect the data structure.



+ `Opts`
The probe-specific options to be processed.



+ `Status`
The new status of the entry.



+ `State`
The probe state, originally returned by `probe_init/3` and subsequently
modified by other probe implementation calls.



This function shall return `{ok, NewState}` where `NewState` is
the modified probe state that incorporates the new options.


#### <a name="probe_update/2">probe_update/2</a> ####


The `probe_update/2` function is invoked as follows:



```erlang

       probe_update(Value, State)
```



Incorporate a new value into the metric maintained by the metric.



The arguments are as follows:



+ `Value`
The value to integrate.



+ `State`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.



This function can be called outside the periodic `probe_sample/1/`
call to have the probe process a value given in `Value`.



The `probe_update/2` implementation is invoked by `exometer:update/2`, which
calls `exometer_probe:update/4`, which invokes the probe
implementation.



Once processed, `probe_update/2` shall return `{ok, NewState}`,
where `NewState` contains the new probe state with the processed
value.


#### <a name="probe_get_value/2">probe_get_value/2</a> ####


The `probe_get_value/2` function is invoked as follows:



```erlang

       probe_get_value(DataPoints, State)
```



The `probe_get_value/2` implementation shall retrieve the value of
one or more data points from the probe.



The arguments are as follows:



+ `DataPoints`
List of data point atoms to retrieve values for.



+ `State`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.



The `probe_get_value/2` implementation is invoked by
`exometer:get_value/2`, which calls `exometer_probe:get_value/4`,
which invokes the probe implementation.



If `exometer:get_value/2` is invoked with `default` as a single
data point, the probe's `probe_get_datapoints/1` function will be
called to retrieve all data points supported by the probe
implementation. `probe_get_value/2` will then be called with the
returned set of data points provided as an argument.



This function shall return the value of all data points provided in
`DataPoints`, given that they are supported.



The list in the returned tuple shall have the format:



```erlang

       [{ DP, Val}, ...]
```



Where `DP` one of the data points in the `DataPoints` argument, and
`Val` is the value of that data point.



If one of the argument-provided data points are not supported by the probe,
the tuple returned for that data point shall be `{ DP, {error, undefined}`.



For example, if the provided `DataPoint` argument is set to `[ min,
max, xyzzy ]`, and only `min` and `max` are supported
by the probe, the returned list shall look like below:



```erlang

       [{ min, 0.1265 }, { max, 3338.21 }, { xyzzy, { error, unsupported } ]
```



The `probe_get_value/2` implementation shall return `{ok, List}`,
where `List` is the list of data points and their values described
above. No new state is returned by this function.


#### <a name="probe_get_datapoints/1">probe_get_datapoints/1</a> ####


The `probe_get_datapoints/1` function is invoked as follows:



```erlang

       probe_get_datapoints(State)
```



The `probe_get_datapoints/1` shall return a list with all data points
supported by the probe



The arguments are as follows:



+ `State`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.



The `probe_get_datapoints/1` implementation is invoked by
`exometer:info/2`, which calls `exometer_probe:get_datapoints/3`,
which invokes the probe implementation.



In cases where `exometer:get_value/2` is called with `default` as a
single data point, `probe_get_datapoints/1` is also called to
retrieve a list of all supported data points, which is then
forwarded to `probe_get_value/2`.



The implementation of `probe_get_datapoints/1` shall return `{ok, DpList}`,
where `DpList` is a list of data point atoms supported by the probe.


#### <a name="probe_reset/1">probe_reset/1</a> ####


The `probe_reset/1` function is invoked as follows:



```erlang

       probe_reset(State)
```



The `probe_reset/1` shall reset the state of the probe to its initial state.



The arguments are as follows:



+ `State`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.

The `probe_reset/1` implementation is invoked by
`exometer:reset/1`, which calls `exometer_probe:reset/3`, which
invokes the probe implementation.



The implementation of `probe_reset/1` shall return `{ok,
NewState}`, where `NewState` contains the reset state of the probe.


#### <a name="probe_sample/1">probe_sample/1</a> ####


The `probe_sample/1` function is invoked as follows:



```erlang

       probe_sample(State)
```



The `probe_sample/1` implementation shall sample data from the
subsystem the probe is integrated with.



The arguments are as follows:



+ `State`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.



When invoked, `probe_sample/1` is expected to interface the
sub-system (/proc, /sysfs, etc) monitored by the probe, extract the
relevant data from it, and return an updated probe state that
incorporates the extracted data.



The `probe_sample/1` function is invoked by the probe thread at
intervals specified by the `{sample_interval, Intv}` option
provided to `exometer_probe:new/3`. If this option is missing, or
set to infinity, `probe_sample/1` will never be called.



The implementation of `probe_sample/1` shall return `{ok,
NewState}`, where `NewState` contains the new state of the probe
with the sampled data integrated into it.


#### <a name="probe_handle_msg/2">probe_handle_msg/2</a> ####


The `probe_handle_msg/2` function is invoked as follows:



```erlang

       probe_handle_msg(Msg, State)
```



The `probe_handle_msg/1` is invoked to process messages received
by the probe process.



The arguments are as follows:



+ `State`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.



+ `Msg`
The probe state, originally returned by `probe_init/3` and subsequentially
modified by other probe implementation calls.



The implementation of this function will be called by the probe's
process when it receives a message that is not recognized by the
internal receive loop.



The implementation of `probe_handle_msg/2` shall return `{ok,
NewState}`, where `NewState` contains the new state of the probe
that reflects the processed message.




### <a name="Fault_tolerance">Fault tolerance</a> ###


Probes are supervised by the `exometer_admin` process, and can be restarted
after a crash. Restart parameters are provided via the option
`{restart, Params}`, where `Params` is a list of `{Frequency, Action}`
tuples. `Frequency` is either `{Count, MilliSecs}` or `'_`', and
the corresponding `Action :: restart | disable | delete` will be performed
if the frequency of restarts falls within the given limit.



For example, `[{{3, 1000}, restart}]` will allow 3 restarts within a 1-second
window. The matching is performed from top to bottom, and the first matching
pattern is acted upon. If no matching pattern is found, the default action
is `delete`. A pattern `{'_', Action}` acts as a catch-all, and should
be put last in the list.



It is also possible to specify `{{Count, '_'}, Action}`, which means
that a total of `Count` restarts is permitted, regardless of how far apart
they are. The count is reset after each restart.


Example:

```erlang

    {restart, [{{3,1000}, restart},   % up to 3 restarts within 1 sec
               {{4,'_'} , disable},   % a total of up to 4 restarts
               {'_'     , delete}]}   % anything else
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour-0">behaviour/0</a></td><td></td></tr><tr><td valign="top"><a href="#delete-3">delete/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_datapoints-3">get_datapoints/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-3">get_value/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_value-4">get_value/4</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#reset-3">reset/3</a></td><td></td></tr><tr><td valign="top"><a href="#restart-4">restart/4</a></td><td></td></tr><tr><td valign="top"><a href="#sample-3">sample/3</a></td><td></td></tr><tr><td valign="top"><a href="#setopts-3">setopts/3</a></td><td></td></tr><tr><td valign="top"><a href="#update-4">update/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="behaviour-0"></a>

### behaviour/0 ###

`behaviour() -> any()`


<a name="delete-3"></a>

### delete/3 ###

`delete(Name, Type, Pid) -> any()`


<a name="get_datapoints-3"></a>

### get_datapoints/3 ###

`get_datapoints(Name, Type, Pid) -> any()`


<a name="get_value-3"></a>

### get_value/3 ###

`get_value(Name, Type, Pid) -> any()`


<a name="get_value-4"></a>

### get_value/4 ###

`get_value(Name, Type, Pid, DataPoints) -> any()`


<a name="new-3"></a>

### new/3 ###

`new(Name, Type, Opts) -> any()`


<a name="reset-3"></a>

### reset/3 ###

`reset(Name, Type, Pid) -> any()`


<a name="restart-4"></a>

### restart/4 ###

`restart(Name, Module, Error, SpawnOpts) -> any()`


<a name="sample-3"></a>

### sample/3 ###

`sample(Name, Type, Pid) -> any()`


<a name="setopts-3"></a>

### setopts/3 ###

`setopts(Exometer_entry, Opts, Status) -> any()`


<a name="update-4"></a>

### update/4 ###

`update(Name, Value, Type, Pid) -> any()`


