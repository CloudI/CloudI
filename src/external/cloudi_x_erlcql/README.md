# erlCQL

[![Build Status][travis_ci_image]][travis_ci]
[![Bitdeli Badge][bitdeli_image]][bitdeli]

Cassandra native protocol CQL Erlang client.

## API

### Start

``` erlang
erlcql:start_link(Options :: proplists:proplist()) ->
    {ok, Pid :: pid()} | {error, Reason :: term()}.
```

#### Options

| Option          | Type                            | Default           |
|:--------------- |:------------------------------- |:----------------- |
| host            | string()                        | `"localhost"`     |
| port            | integer()                       | `9042`            |
| username        | bitstring()                     | `<<"cassandra">>` |
| password        | bitstring()                     | `<<"cassandra">>` |
| cql_version     | bitstring()                     | undefined         |
| compression     | erlcql:compression()            | false             |
| use             | bitstring()                     | undefined         |
| event_handler   | pid() &#124; erlcql:event_fun() | self()            |
| auto_reconnect  | boolean()                       | false             |
| reconnect_start | pos_integer()                   | 1000              |
| reconnect_max   | pos_integer()                   | 30000             |
| keepalive       | boolean()                       | false             |

### Query

#### Types

| Cassandra type        | Erlang type                 |
|:--------------------- |:--------------------------- |
| ascii                 | bitstring()                 |
| bigint                | integer()                   |
| blob                  | binary()                    |
| boolean               | boolean()                   |
| counter               | integer()                   |
| decimal               | float()                     |
| double                | float()                     |
| float                 | float()                     |
| inet                  | inet:ip_address()           |
| int                   | integer()                   |
| timestamp             | integer()                   |
| timeuuid              | erlcql:uuid()               |
| uuid                  | erlcql:uuid()               |
| varchar/text          | bitstring()                 |
| varint                | integer()                   |
| list&lt;type&gt;      | list(type())                |
| set&lt;type&gt;       | list(type())                |
| map&lt;key, value&gt; | list(tuple(key(), value())) |
| custom                | binary()                    |

## Notes

### Versions

Supported versions: [`v1`][proto_v1].

[travis_ci]: https://travis-ci.org/rpt/erlcql
[travis_ci_image]: https://travis-ci.org/rpt/erlcql.png
[bitdeli]: https://bitdeli.com/free
[bitdeli_image]: https://d2weczhvl823v0.cloudfront.net/rpt/erlcql/trend.png
[proto_v1]:
https://raw.github.com/apache/cassandra/trunk/doc/native_protocol_v1.spec
