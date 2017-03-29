# [CloudI 1.7.0](http://cloudi.org)

[![Build Status](https://secure.travis-ci.org/CloudI/CloudI.png?branch=develop)](http://travis-ci.org/CloudI/CloudI) [![CloudI IRC](https://img.shields.io/badge/irc-%23cloudi-orange.svg)](https://webchat.freenode.net?randomnick=1&channels=cloudi) [![Project Stats](https://www.openhub.net/p/CloudI/widgets/project_thin_badge.gif)](https://www.openhub.net/p/CloudI)

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
(currently C/C++, Go, Haskell, Java, JavaScript, OCaml, Perl, PHP, Python,
 Ruby, and Erlang/Elixir):

* `subscribe`, `unsubscribe`, `subscribe_count`
* `send_async`, `send_sync`, `mcast_async` (`mcast_async` == publish)
* `recv_async`
* `return`, `forward`

External communication that needs to scale (beyond 10,000 connections)
can use an existing internal CloudI service (implemented in Erlang or Elixir)
which may do processing for one or more external CloudI services
(implemented in C/C++, Go, Haskell, Java, JavaScript, OCaml, Perl, PHP, Python,
 and/or Ruby)

Even if external communication doesn't need to scale, private cloud
computing tasks (number crunching) can gain fault-tolerance and internal
system scalability within CloudI.

### Where should I find more information?

Please see the [FAQ](http://cloudi.org/faq.html) for more details.

## INSTALLATION

### Requirements

* `Erlang >= 19.0 (erlang/Ubuntu, erlang/macports)`
* `C++ (g++/Ubuntu, libstdcxx/macports)`
* `boost >= 1.36.0 (libboost-system-dev+libboost-thread-dev+libboost-dev/Ubuntu, boost/macports)`

Optional (default="yes"):

* `Java >= 1.5 JDK`
  * `(default-jdk/Ubuntu, (built-in)/OSX)`
  * Use the "--enable-java-support=no" configure flag to disable
* `Javascript >= 0.8.0`
  * `(nodejs/Ubuntu)`
  * Use the "--enable-javascript-support=no" configure flag to disable
* `Perl >= 5.10`
  * `(perl/Ubuntu)`
  * `Compress::Zlib (cpan)`
  * Use the "--enable-perl-support=no" configure flag to disable
* `PHP >= 5.3.6`
  * `(php5/Ubuntu)`
  * Use the "--enable-php-support=no" configure flag to disable
* `Python >= 2.7.0`
  * `(python+python-dev/Ubuntu, python27/macports)`
  * Use the "--enable-python-support=no" and "--enable-python-c-support=no" configure flag to disable
* `Ruby >= 1.9.0`
  * `(ruby1.9.1/Ubuntu, ruby19/macports)`
  * Use the "--enable-ruby-support=no" configure flag to disable
* `GNU MP library`
  * `(libgmp3-dev/Ubuntu, gmp/macports)`
  * Used in the hexpi (C++) integration test only
    ("--with-integration-tests=no" configure flag to disable)

Optional (default="no"):

* `Go >= 1.6`
  * `(golang/Ubuntu, go/macports)`
  * Use the "--enable-go-support" configure flag to enable
* `Haskell (GHC >= 7.10.3 and cabal-install >= 1.22)`
  * Use the "--enable-haskell-support" configure flag to enable
* `OCaml >= 4.03.0`
  * Use the "--enable-ocaml-support" configure flag to enable
* `ZeroMQ >= 3.x.x or 2.x.x`
  * `uuid-dev (uuid-dev/Ubuntu, ossp-uuid/macports)`
  * Use the "--with-zeromq" configure flag to enable

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

* CloudI API (See [`src/api/README.markdown`](https://github.com/CloudI/CloudI/tree/master/src/api#readme))
* HTTP with [`cloudi_service_http_cowboy`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_cowboy/src/cloudi_service_http_cowboy.erl) and [`cloudi_service_http_elli`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_elli/src/cloudi_service_http_elli.erl)
* ZeroMQ with [`cloudi_service_zeromq`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_zeromq/src/cloudi_service_zeromq.erl)
* OAuth v1 with [`cloudi_service_oauth1`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_oauth1/src/cloudi_service_oauth1.erl)
* TCP with [`cloudi_service_tcp`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_tcp/src/cloudi_service_tcp.erl)
* UDP with [`cloudi_service_udp`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_udp/src/cloudi_service_udp.erl)
* Supported databases (included)
  * MySQL with [`cloudi_service_db_mysql`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_mysql/src/cloudi_service_db_mysql.erl)
  * PostgreSQL with [`cloudi_service_db_pgsql`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_pgsql/src/cloudi_service_db_pgsql.erl)
* Supported databases (excluded)
  * elasticsearch with [`cloudi_service_db_elasticsearch`](https://github.com/CloudI/cloudi_service_db_elasticsearch)
  * Cassandra with [`cloudi_service_db_cassandra`](https://github.com/CloudI/cloudi_service_db_cassandra) or [`cloudi_service_db_cassandra_cql`](https://github.com/CloudI/cloudi_service_db_cassandra_cql)
  * CouchDB with [`cloudi_service_db_couchdb`](https://github.com/CloudI/cloudi_service_db_couchdb)
  * memcached with [`cloudi_service_db_memcached`](https://github.com/CloudI/cloudi_service_db_memcached)
  * Riak with [`cloudi_service_db_riak`](https://github.com/CloudI/cloudi_service_db_riak)
  * TokyoTyrant with [`cloudi_service_db_tokyotyrant`](https://github.com/CloudI/cloudi_service_db_tokyotyrant)

Dynamic Configuration and Monitoring:

* CloudI Service API (See [`src/service_api/README.markdown`](https://github.com/CloudI/CloudI/tree/master/src/service_api#readme))
* Monitoring to Graphite, OpenTSDB, SNMP, InfluxDB or StatsD with [`cloudi_service_monitoring`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_monitoring/src/cloudi_service_monitoring.erl)

Routing:

* Caching Static File Data with [`cloudi_service_filesystem`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_filesystem/src/cloudi_service_filesystem.erl)
* HTTP Client Requests with [`cloudi_service_http_client`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_client/src/cloudi_service_http_client.erl)
* HTTP REST Handlers with [`cloudi_service_http_rest`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_rest/src/cloudi_service_http_rest.erl)
* Fault-Tolerant Map-Reduce with [`cloudi_service_map_reduce`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_map_reduce/src/cloudi_service_map_reduce.erl) (See the [`hexpi` integration test controller](https://github.com/CloudI/CloudI/blob/master/src/tests/hexpi/src/cloudi_service_hexpi.erl))
* Durable Service Requests with [`cloudi_service_queue`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_queue/src/cloudi_service_queue.erl)
* Service Redundancy with [`cloudi_service_quorum`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_quorum/src/cloudi_service_quorum.erl)
* Altering Request Destinations with [`cloudi_service_router`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_router/src/cloudi_service_router.erl)
* Validation with [`cloudi_service_validate`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_validate/src/cloudi_service_validate.erl)

The default CloudI configuration can run the included integration tests
if all the supported programming languages are enabled at configure time
(they are by default) and the `--with-integration-tests-ran` configuration
argument is used (to choose the [`src/cloudi_tests.conf.in`](https://github.com/CloudI/CloudI/blob/master/src/cloudi_tests.conf.in) file).

If the `--with-integration-tests-ran` configuration argument is not used,
the more minimal CloudI configuration will be used instead
(in the [`src/cloudi_minimal.conf.in`](https://github.com/CloudI/CloudI/blob/master/src/cloudi_minimal.conf.in) file) to support basic things like the
[Quick Start Guide](https://github.com/CloudI/CloudI/tree/master/doc#readme),
the [Dashboard](https://github.com/CloudI/CloudI/tree/master/src/service_api#readme)
and any tutorials or examples.

## CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)

