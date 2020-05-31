# Exometer InfluxDB reporter 

This reporter pushes data to [InfluxDB](https://influxdb.com/index.html).

[![Build Status](https://travis-ci.org/travelping/exometer_influxdb.svg)](https://travis-ci.org/travelping/exometer_influxdb)
[![Hex pm](http://img.shields.io/hexpm/v/exometer_influxdb.svg?style=flat)](https://hex.pm/packages/exometer_influxdb)

## Usage

1. Add exometer_influxdb to your list of dependencies in rebar.config:

    ```erlang
    {deps, [
        {exometer_influxdb, "0.6.0"}
    ]}.
    ```

2. Ensure exometer_influxdb is started before your application:

    ```erlang
    {applications, [exometer_influxdb]}.
    ```

3. Configure it:

    ```erlang
    {exometer_core, [
        {report, [
            {reporters, [
                {exometer_report_influxdb, [{protocol, http},
                                            {host, <<"localhost">>},
                                            {port, 8086},
                                            {db, <<"exometer">>},
                                            {tags, [{region, ru}]}]}
            ]}
        ]}
    ]}.
    ```

Available options:

* __protocol__ - `http`, `https` or `udp` for operating with InfluxDB. `http` by default. If you use `udp`, check __udp_mtu__ below to avoid `{error,emsgsize}`.
* __host__ - InfluxDB host. `127.0.0.1` by default.
* __port__ - InfluxDB port. `8086` by default.
* __db__ - Database on InfluxDB for writing data. `exometer` by default.
* __username__ - Username for authorization on InfluxDB.
* __password__ - Password for authorization on InfluxDB.
* __timestamping__ - Enable timestamping, `false` by default. To enable `timestamping` with the reporter you can use `true` or `{true, Precision}` where `Precision` is a unit taken from `[n,u,ms,s,m,h]`. The default unit is `u`.
* __batch_window_size__ - Set window size in ms for batch sending. This means the reporter will collect measurements within this interval and send all measurements in one packet. `0` by default.
* __udp_mtu__ - (Used only with __protocol__ == `udp`.) MTU of the network interface through which UDP packets flow to the __host__. `65536` by default (Linux loopback interface MTU). Run `ifconfig` on the machine where this app will run to find it out. Metrics will be sent out if their size (in the Line Protocol format) exceeds this value, even if the current __batch_window_size__ is not over yet. (They will also be sent out at the end of __batch_window_size__ as usual, regardless of their size.)

The following options can be set globally in the reporter config or locally in a specific subscription. The latter case overwrites the first.

* __tags__ - List of tags for each time series. The `host` is automatically included here.
* __series_name__ - The name of a time series visible within the `FROM` field. By default this is set to the concatenated elements of the exometer id. Caution: If set in the global reporter config then every time series will have this name.
* __formatting__ - Formatting options to alter the appearance of a series name or tags.

### Subscription examples:

```erlang

{exometer_core, [
    {report, [
        {subscribers, [
            {exometer_report_influxdb, [erlang, memory], total, 5000, [{tags, {tag, value}}]}
         ]}
    ]}
]}.
```

By default the in InfluxDB visible name of the metric is derived from the exometer id: Here `[erlang, memory]` is translated to `erlang_memory`. 
It is possible to remove an item from this list by naming itself or its position with the `from_name` keyword. A removed element is then used as tag value:

```erlang
exometer_report:subscribe(exometer_report_influxdb, [erlang, memory], total, 5000, [{tags, [{tag, {from_name, 2}}]}]).
```

This will result in a name `erlang` with the tag pair `{tag, memory}` (plus the default pair `{host, Host}`). To disable the removal of elements in the series name you can set:

```erlang
{formatting, [{purge, [{all_from_name, false}]}]}
```

Further it might be handy to remove e.g. `undefined` tag keys or values. This can be done via:

```erlang
{formatting, [{purge, [{tag_keys, undefined}, {tag_values, undefined}]}]}
```

### Auto subscriptions:

There is capability for making a subscription automatically for each new entry. By default it is off. If you need to enable it in the reporter options and also provide a callback module which handles newly created entries.

```erlang
{exometer_core, [
    {report, [
        {reporters, [
            {exometer_report_influxdb, [{autosubscribe, true},
                                        {subscriptions_module, exometer_influxdb_subscribe_mod},
                                        {protocol, http},
                                        {host, <<"localhost">>},
                                        {port, 8086},
                                        {db, <<"exometer">>},
                                        {tags, [{region, ru}]}]}
        ]}
    ]}
]}.
```

The callback module may look like:

```erlang
-module(exometer_influxdb_subscribe_mod).
-export([subscribe/2]).

subscribe([metric, test], histogram) ->
    Tags = [{tags, [{type, {from_name, 2}}]}],
    [{[metric, test], min, 1000, Tags},
     {[metric, test], max, 1000, Tags},
     {[metric, test], median, 1000, Tags}];
subscribe([metric, test1], histogram) ->
    Tags = [{tags, [{type, {from_name, 2}}]}],
    [{[metric, test1], max, 1000, Tags},
     {[metric, test1], median, 1000, Tags}];
subscribe(_, _) -> [].
```

`subscribe/2` calls for each new entry and it should return a list or just one subscription. Here a single subscription has the following layout:
```erlang
{exometer_report:metric(), exometer_report:datapoint(), exometer_report:interval(), exometer_report:extra()}
```

# TODO

* Reconfiguration on runtime
* Enhance the formatting options (e.g. concatenation chars, format strings, etc.)
