#[CloudI 1.0.0 (beta)](http://cloudi.org)

[![Build Status](https://secure.travis-ci.org/okeuday/CloudI.png?branch=develop)](http://travis-ci.org/okeuday/CloudI)

## LICENSE

[BSD License](https://github.com/okeuday/CloudI/blob/master/src/LICENSE)

## ABOUT

### What is CloudI?

(long answer) "An application server that efficiently integrates with many
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

* subscribe, unsubscribe
* send_async, send_sync, mcast_async
* recv_async
* return, forward

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

The default CloudI configuration runs many tests that can be used as
examples of CloudI integration
(see [src/cloudi.conf.in](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in)).

### Requirements

* Erlang >= R14B02
* C++ (g++/Ubuntu, libstdcxx/macports)
* Java (default-jdk/Ubuntu)
* Python >= 2.7.0 (python/Ubuntu)
* Ruby >= 1.9.0 (ruby1.9/Ubuntu, ruby19/macports)
* GNU MP library (libgmp3-dev/Ubuntu, gmp/macports)
* boost >= 1.36.0 (libboost-thread-dev+libboost-dev/Ubuntu, boost/macports)

Optional (installed/linked statically, automatically):

* ZeroMQ >= 2.x.x (or 3.x.x) (use the "--with-zeromq" configure flag)

### Building

For configuration options, see [FAQ: 3.2 - Installation Options](http://cloudi.org/faq.html#3_Options).

    ./configure
    make
    make install

On OSX, to run configure with the OSX ports paths, use:
(macports)

    CXXFLAGS="-I/opt/local/include" LDFLAGS="-L/opt/local/lib" ./configure

(homebrew)

    CXXFLAGS="-I/usr/local/include" LDFLAGS="-L/usr/local/lib" ./configure

(A better solution is forthcoming)

### Running

Within the installation directory the "bin/cloudi" script controls CloudI.

To start CloudI:

    bin/cloudi start

To stop CloudI:

    bin/cloudi stop

## INTEGRATION

Integration points:

* CloudI API (See [src/api/README](https://github.com/okeuday/CloudI/tree/master/src/api#readme))
* HTTP with [cloudi_job_http_misultin](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_internal/src/cloudi_job_http_misultin.erl) or [cloudi_job_http_cowboy](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_internal/src/cloudi_job_http_cowboy.erl)
* ZeroMQ with [cloudi_job_zeromq](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_messaging/src/cloudi_job_zeromq.erl)
* Supported databases
  * CouchDB with [cloudi_job_db_couchdb](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_databases/src/cloudi_job_db_couchdb.erl)
  * memcached with [cloudi_job_db_memcached](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_databases/src/cloudi_job_db_memcached.erl)
  * MySQL with [cloudi_job_db_mysql](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_databases/src/cloudi_job_db_mysql.erl)
  * PostgreSQL with [cloudi_job_db_pgsql](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_databases/src/cloudi_job_db_pgsql.erl)
  * Tokyo Tyrant with [cloudi_job_db_tokyotyrant](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi_services_databases/src/cloudi_job_db_tokyotyrant.erl)

Dynamic configuration uses the CloudI Job API (See [src/job_api/README](https://github.com/okeuday/CloudI/tree/master/src/job_api#readme))

## CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)

