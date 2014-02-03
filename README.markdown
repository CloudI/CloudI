#[CloudI 1.3.1 (beta)](http://cloudi.org)

[![Build Status](https://secure.travis-ci.org/CloudI/CloudI.png?branch=develop)](http://travis-ci.org/CloudI/CloudI)

## LICENSE

[BSD License](https://github.com/CloudI/CloudI/blob/master/src/LICENSE)

## ABOUT

### What is CloudI?

(short answer) "An application server that efficiently integrates with many
languages, many databases, and many messaging buses in a way that is both
scalable and fault-tolerant."

(shorter answer) "A rock-solid transaction processing system for flexible
software development."

(shortest answer) "A Cloud at the lowest level."

### Who would use CloudI?

Software developers that do not want to get locked into corporate vendors
or frameworks that push for perpetual commercial support or licenses.

### Why should CloudI be used?

CloudI makes software fault-tolerant and scalable, utilizing Erlang,
even if the software is legacy source code.  CloudI mitigates
software development risk (delays or failures) when making
software scale in non-Erlang programming languages, or during a conversion
of a software system (fully or partially) to the Erlang programming language.

### How should CloudI be used?

The CloudI API provides a simple set of functions for
Service-Oriented Architecture (SOA) development in any supported language
(currently C++/C, Java, Python, Ruby, and Erlang):

* `subscribe`, `unsubscribe`
* `send_async`, `send_sync`, `mcast_async` (`mcast_async` == publish)
* `recv_async`
* `return`, `forward`

External communication that needs to scale (beyond 10,000 connections)
can use an internal CloudI service (implemented in Erlang) which may do
processing for one or more external CloudI services
(implemented C++/C, Java, Python, and/or Ruby)

Even if external communication doesn't need to scale, private cloud
computing tasks (number crunching) can gain fault-tolerance and internal
system scalability within CloudI.

### Where should I find more information?

Please see the [FAQ](http://cloudi.org/faq.html) for more details.

## INSTALLATION

### Requirements

* `Erlang >= R16 (erlang/Ubuntu, erlang/macports)`
* `C++ (g++/Ubuntu, libstdcxx/macports)`
* `Java >= 1.5 JDK (default-jdk/Ubuntu, (built-in)/OSX)`
* `Python >= 2.7.0 (python+python-dev/Ubuntu, python27/macports)`
* `Ruby >= 1.9.0 (ruby1.9.1/Ubuntu, ruby19/macports)`
* `GNU MP library (libgmp3-dev/Ubuntu, gmp/macports)`
* `boost >= 1.36.0 (libboost-thread-dev+libboost-dev/Ubuntu, boost/macports)`

Optional (installed/linked statically, automatically):

* `ZeroMQ >= 3.x.x or 2.x.x (use the "--with-zeromq" configure flag)`
  * `uuid-dev (uuid-dev/Ubuntu)`

### Building

For configuration options, see [FAQ: 3.2 - Installation Options](http://cloudi.org/faq.html#3_Options).

    ./configure
    make
    sudo make install

### Running

Within the installation directory the cloudi script controls CloudI.

To start CloudI:

    sudo cloudi start

To stop CloudI:

    sudo cloudi stop

## INTEGRATION

See the [Quick Start Guide](https://github.com/CloudI/CloudI/tree/master/doc#readme) or the [API documentation](http://cloudi.org/api.html#1_Intro)

Integration points:

* CloudI API (See [`src/api/README`](https://github.com/CloudI/CloudI/tree/master/src/api#readme))
* HTTP with [`cloudi_service_http_cowboy`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_cowboy/src/cloudi_service_http_cowboy.erl) and [`cloudi_service_http_elli`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_elli/src/cloudi_service_http_elli.erl)
* ZeroMQ with [`cloudi_service_zeromq`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_zeromq/src/cloudi_service_zeromq.erl)
* Supported databases
  * elasticsearch with [`cloudi_service_db_elasticsearch`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_elasticsearch/src/cloudi_service_db_elasticsearch.erl)
  * Cassandra with [`cloudi_service_db_cassandra`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_cassandra/src/cloudi_service_db_cassandra.erl)
  * CouchDB with [`cloudi_service_db_couchdb`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_couchdb/src/cloudi_service_db_couchdb.erl)
  * memcached with [`cloudi_service_db_memcached`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_memcached/src/cloudi_service_db_memcached.erl)
  * MySQL with [`cloudi_service_db_mysql`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_mysql/src/cloudi_service_db_mysql.erl)
  * PostgreSQL with [`cloudi_service_db_pgsql`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_pgsql/src/cloudi_service_db_pgsql.erl)
  * Tokyo Tyrant with [`cloudi_service_db_tokyotyrant`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_tokyotyrant/src/cloudi_service_db_tokyotyrant.erl)

Dynamic configuration uses the CloudI Service API (See [`src/service_api/README`](https://github.com/CloudI/CloudI/tree/master/src/service_api#readme))

The default CloudI configuration runs many tests that can be used as
examples of CloudI integration
(see [`src/cloudi.conf.in`](https://github.com/CloudI/CloudI/blob/master/src/cloudi.conf.in)).

## CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)

