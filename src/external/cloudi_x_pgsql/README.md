Erlang PostgreSQL Driver [![Build Status](https://travis-ci.org/semiocast/pgsql.png)](https://travis-ci.org/semiocast/pgsql)
========================

Introduction
------------

This driver is an OTP-compliant PostgreSQL driver. Connections are OTP-supervised processes.

This implementation was initially inspired and derived from existing database drivers, and especially Will Glozer's and Christian Sunesson's PostgreSQL drivers, yet has eventually little in common with those. API and features are different.

In particular, this driver has the following features:

* OTP-supervision and OTP-upgrades;
* transparently handling many PostgreSQL types, including arrays, numerics and geometric types;
* cancellation of running queries using out-of-band protocol;
* SSL support;
* timeout for queries;
* iteration on results using protocol-level implicit portals and cursors, with fold, map and foreach;
* mapping of types to Erlang using a mapping of known types, handling new types that may arise through the life of the connection (this feature is an improvement of oidmap handling in Christian Sunesson's driver);
* handling both floating point and integer datetimes (this feature is an improvement of timestamp handling in Will Glozer's driver).

Compilation
-----------

Driver can be compiled with [rebar](https://github.com/basho/rebar).

    erlc -o ebin/ src/*.erl

also works.

API and usage
-------------

The application must be started before connections can be made to a PostgreSQL server.

### Opening and closing connections ###

The main module is ```pgsql_connection```. Connections are opened with one of ```pgsql_connection:open/1,2,3,4,5``` functions. Please refer to ```pgsql_connection.erl``` for details.

For convenience with other database APIs, connection objects can be used as parametrized modules as connection objects are ```{pgsql_connection, pid()}``` tuples. API functions taking a connection as the last parameter can be called with the connection as the parametrized module. For example, a connection can be closed with ```pgsql_connection:close(Connection)``` or with ```Connection:close()```.

### Performing queries ###

Two APIs are available. The current, "native" API is composed of the following functions:

#### ```simple_query/2,3,4``` ####

Perform a simple query (in PostgreSQL parlance), i.e. a query with no parameter. Results are cast to Erlang-types. Text is returned as binaries (```unicode:unicode_binary()```). Enums are typed ```{atom() :: TypeName, unicode:unicode_binary()}```. See tests (```pgsql_connection_test.erl```) for details.

#### ```extended_query/3,4,5``` ####

Perform an extended query, i.e. a query with bound parameters. Parameters must be represented in the query with $1, ..., $n placeholders. Most erlang types are supported. Binaries are passed as-is and are interpreted by PostgreSQL as text or binary depending on what is expected. Lists are interpreted as strings, arrays are represented as ```{array, [item()]}```. See tests (```pgsql_connection_test.erl```) for details.

#### ```batch_query/3,4,5``` ####

Perform an extended query several times with different parameters. Saves some network I/O with the server.

#### ```fold/4,5,6,7```, ```map/3,4,5,6```, ```foreach/3,4,5,6``` ####
    
Perform an extended query and fold (resp. map, execute a function on each row). This opens an implicit cursor with the server and iterates on results.

#### ```cancel/1``` ####
    
Cancel the current running query. This opens a new connection to cancel the query (out-of-band cancelation).

### COPY support ###

```COPY``` is a PostgreSQL-specific command for bulk transfers of table contents. It is less structured and more brittle to user errors.

#### copying from a table ####

```Connection:simple_query("COPY mytable TO STDOUT")``` will return ```{{copy, ListSize},[<<Tab-delimited data>>, ...]}```. ```ListSize``` is the number of elements in the associated list, which often corresponds to the number of rows in the table. The actual contents of the returned data depend on the COPY query (for example, specifying a format).

```extended_query```, ```fold```, ```map```, and ```foreach``` may also be used, but note that Postgres does not permit parameter-binding in COPY queries.

#### copying to a table ####

Begin a copy using `simple_query` (not `extended_query`) to copy from "stdin". Then perform any number of `send_copy_data/2` to transmit data to Postgres' stdin. Finally, `send_copy_end/1` to have Postgres process the data.

```erlang
{copy_in,_} = Connection:simple_query("COPY people(fn,ln) FROM STDIN"),
ok = Connection:send_copy_data(<<"Donald\tChamberlin\nRaymond\tBoyce\nMi">>),
ok = Connection:send_copy_data(<<"chael\tStonebraker\n">>),
{copy,3} = Connection:send_copy_end()
```

Using `COPY` is generally the fastest method to bulk-insert data by a large margin.

### ODBC-like API ###

The driver also has an ODBC-like interface, which is deprecated. It is composed of ```sql_query/2,3,4```, ```param_query/3,4,5``` and utility function ```convert_statement/1```

### Asynchronous operations ###

Besides cancelation, the driver supports what PostgreSQL documentation calls [Asynchronous operations](http://www.postgresql.org/docs/current/static/protocol-flow.html#PROTOCOL-ASYNC). Namely, it can receive several types of messages when the connection is idle or at any point in the protocol. This is especially meaningful for notifications and notices. These messages are ignored by default but can be sent to subscribers. To subscribe when opening a connection, simply use ```{async, pid()}``` option. To subscribe later, use ```pgsql_connection:subscribe/2```. Subscribers can presently receive two types of Erlang messages:

* ```{pgsql, Connection, {notice, Fields :: [{atom(), binary()}]}}``` for notices, where ```Connection``` is the connection tuple and Fields describes the notice (typically including ```{severity, <<"NOTICE">>}``` and ```{message, NoticeMessage}```).
* ```{pgsql, Connection, {notification, ProcID :: pos_integer(), Channel :: binary(), Payload :: binary()}}``` for notifications, where ```ProcID``` is the sender backend id.

Implementers of subscribers are encouraged to handle any message of the form ```{pgsql, Connection, _}``` to cope with future features.

Tests
-----

Tests require a user 'test' on postgres, with super privileges, and a test database.
For example:

    psql -h localhost -U postgres
```SQL
create user test;
alter user test with superuser;
create database test with owner=test;
```

OTP upgrades
------------

Application upgrade file ([pgsql.appup](https://github.com/semiocast/pgsql/blob/master/src/pgsql.appup)) is updated for OTP release upgrades, so this application can easily be upgraded without any downtime, even with long running queries. This file is updated for each [release tags](https://github.com/semiocast/pgsql/releases).

License
-------

Copyright (c) 2009-2015, Semiocast.
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
