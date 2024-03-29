# [CloudI 2.0.7](https://cloudi.org)

[![Build Status](https://app.travis-ci.com/CloudI/CloudI.svg?branch=develop)](https://app.travis-ci.com/CloudI/CloudI) [![Project Status](https://www.openhub.net/p/CloudI/widgets/project_thin_badge.gif)](https://www.openhub.net/p/CloudI) [![RSS](https://raw.githubusercontent.com/CloudI/CloudI/develop/.badge_rss_releases.png)](https://sourceforge.net/p/cloudi/activity/feed.rss) [![Mastodon](https://img.shields.io/mastodon/follow/111610711899189583?domain=https%3A%2F%2Finfosec.exchange&style=flat&label=mastodon&color=blue)](https://infosec.exchange/@CloudI) [![X (formerly Twitter)](https://img.shields.io/twitter/follow/CloudI_org?style=social)](https://twitter.com/intent/follow?screen_name=CloudI_org) [![IRC](https://raw.githubusercontent.com/CloudI/CloudI/develop/.badge_irc_cloudi.png)](https://webchat.oftc.net/?randomnick=1&channels=cloudi) [![Email](https://raw.githubusercontent.com/CloudI/CloudI/develop/.badge_email_questions.png)](https://sourceforge.net/p/cloudi/mailman/cloudi-questions)

## LICENSE

[MIT License](https://github.com/CloudI/CloudI/blob/master/src/LICENSE)

## ABOUT

### What is CloudI?

(short answer) "A distributed application server that provides efficient
messaging for many programming languages within a single service abstraction
that is both scalable and fault-tolerant."

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
(currently ATS, C/C++, Erlang/Elixir, Go, Haskell, Java, JavaScript,
 OCaml, Perl, PHP, Python, Ruby and Rust):

* `subscribe`, `unsubscribe`, `subscribe_count`
* `send_async`, `send_sync`, `mcast_async` (`mcast_async` == publish)
* `recv_async`
* `return`, `forward`

External communication that needs to scale (beyond 10,000 connections)
can use an existing internal CloudI service (implemented in Erlang or Elixir)
which may do processing for one or more external CloudI services
(implemented in ATS, C/C++, Go, Haskell, Java, JavaScript, OCaml, Perl, PHP,
 Python, Ruby and/or Rust)

Even if external communication doesn't need to scale, private cloud
computing tasks (number crunching) can gain fault-tolerance and internal
system scalability within CloudI.

### Where should I find more information?

Please see the [FAQ](https://cloudi.org/faq.html) for more details.

## INSTALLATION

### Requirements

* `Erlang >= 22.0` (`erlang /Ubuntu, erlang /Alpine`)
* `C/C++ (C++98 compliant, improved error information with C++11 support)`
  * `GCC >= 4.9` (`g++ /Ubuntu, g++ /Alpine`) `or clang >= 3.3` (`clang /Ubuntu, clang /Alpine`)
* `boost >= 1.40.0` (`libboost-system-dev libboost-thread-dev libboost-dev /Ubuntu`, `boost-system boost-thread boost-dev /Alpine`)

Optional (default="yes"):

* `C`
  * Use the "--enable-c-support=no" configure flag to disable
* `C++`
  * `CloudI::API::backtrace() function`
    * `backtrace with backward-cpp (Linux)`
      * `backtrace symbols options:`
        * (`binutils-dev /Ubuntu`, `binutils-dev /Alpine`)
        * (`libdw-dev /Ubuntu`, `elfutils-dev /Alpine`)
        * `libdwarf version < 20210528` (`libdwarf-dev libelf-dev /Ubuntu`, `libdwarf libdwarf-dev elfutils-dev /Alpine`)
      * `backtrace data options:`
        * `compiler unwind.h`
        * (`libexecinfo-dev /Alpine`)
    * `backtrace with boost::stacktrace (boost >= 1.71)`
      * (`libboost-stacktrace-dev /Ubuntu`, `boost-stacktrace_basic /Alpine`)
    * Use the "--with-cxx-backtrace=no" configure flag to disable
  * Use the "--enable-cxx-support=no" configure flag to disable
* `Java >= 1.5 JDK`
  * (`default-jdk /Ubuntu`, `openjdk8 /Alpine`)
  * Use the "--enable-java-support=no" configure flag to disable
  * Java CloudI API is available as a [Maven package](https://github.com/CloudI/cloudi_api_java#readme)
* `JavaScript >= 0.12.18`
  * (`nodejs /Ubuntu`, `nodejs /Alpine`)
  * Use the "--enable-javascript-support=no" configure flag to disable
  * JavaScript CloudI API is available as a [npm package](https://github.com/CloudI/cloudi_api_javascript#readme)
* `Perl >= 5.10 (with Compress::Zlib)`
  * (`perl perl-modules /Ubuntu`, `perl /Alpine`)
  * Use the "--enable-perl-support=no" configure flag to disable
* `PHP >= 7.0`
  * (`php /Ubuntu`, `php7 /Alpine`)
  * Use the "--enable-php-support=no" configure flag to disable
* `Python >= 2.7.0`
  * (`python3 python3-dev /Ubuntu`, `python3 python3-dev /Alpine`)
  * Use the "--enable-python-support=no" and "--enable-python-c-support=no" configure flag to disable
  * Python and Python/C CloudI API is available as a [PyPI package](https://github.com/CloudI/cloudi_api_python#readme)
* `Ruby >= 1.9.0`
  * (`ruby /Ubuntu`, `ruby /Alpine`)
  * Use the "--enable-ruby-support=no" configure flag to disable
  * Ruby CloudI API is available as a [Ruby gem](https://github.com/CloudI/cloudi_api_ruby#readme)
* `syscall_lock service configuration option`
  * `syscall_lock with pledge (OpenBSD)`
    * `libc unistd.h`
  * `syscall_lock with libseccomp (Linux)`
    * (`libseccomp-dev /Ubuntu`, `libseccomp-dev /Alpine`)
* `GNU MP library`
  * (`libgmp-dev /Ubuntu`, `gmp-dev /Alpine`)
  * Used in the hexpi (C++) integration test only
    ("--with-integration-tests=no" configure flag to disable)

Optional (default="no"):

* `ATS2/Postiats >= 0.3.13`
  * (`ats2-lang /Ubuntu`)
  * Use the "--enable-ats2-support" configure flag to enable
* `Go >= 1.11`
  * (`golang /Ubuntu`, `go /Alpine`)
  * Use the "--enable-go-support" configure flag to enable
  * Go CloudI API is available as a [remote import](https://github.com/CloudI/cloudi_api_go#readme)
* `Haskell (GHC >= 8.6.0)`
  * (`ghc zlib1g-dev /Ubuntu`, `ghc zlib-dev /Alpine`)
  * Use the "--enable-haskell-support" configure flag to enable
  * Haskell CloudI API is available as a [Hackage package](https://github.com/CloudI/cloudi_api_haskell#readme) for GHC >= 7.10.3
* `OCaml >= 4.08.0`
  * (`ocaml /Ubuntu`, `ocaml /Alpine`)
  * Use the "--enable-ocaml-support" configure flag to enable
  * OCaml CloudI API is available as an [opam package](https://github.com/CloudI/cloudi_api_ocaml#readme)
* `Rust >= 1.66.1`
  * (`rustc /Ubuntu`, `rust /Alpine`)
  * Use the "--enable-rust-support" configure flag to enable
  * Rust CloudI API is available as a [published crate](https://github.com/CloudI/cloudi_api_rust#readme)

### Building

For configuration options, see [FAQ: 3.2 - Installation Options](https://cloudi.org/faq.html#3_Options).

    cd src
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

See the [Quick Start Guide](https://github.com/CloudI/CloudI/tree/master/doc#readme) or the [API documentation](https://cloudi.org/api.html#1_Intro)

Integration points:

* CloudI API (See [`src/api/README.markdown`](https://github.com/CloudI/CloudI/tree/master/src/api#readme))
* HTTP with [`cloudi_service_http_cowboy`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_cowboy/src/cloudi_service_http_cowboy.erl) and [`cloudi_service_http_elli`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_elli/src/cloudi_service_http_elli.erl)
* OAuth v1 with [`cloudi_service_oauth1`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_oauth1/src/cloudi_service_oauth1.erl)
* TCP with [`cloudi_service_tcp`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_tcp/src/cloudi_service_tcp.erl)
* UDP with [`cloudi_service_udp`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_udp/src/cloudi_service_udp.erl)
* SQL Databases
  * MySQL with [`cloudi_service_db_mysql`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_mysql/src/cloudi_service_db_mysql.erl)
  * PostgreSQL with [`cloudi_service_db_pgsql`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_db_pgsql/src/cloudi_service_db_pgsql.erl)
* shell with [`cloudi_service_shell`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_shell/src/cloudi_service_shell.erl)

Dynamic Configuration and Monitoring:

* CloudI Service API (See [`src/service_api/README.markdown`](https://github.com/CloudI/CloudI/tree/master/src/service_api#readme))
* Batch CloudI Service Execution with [`cloudi_service_api_batch`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_api_batch/src/cloudi_service_api_batch.erl)
* Service Request Sending with [`cloudi_service_send`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_send/src/cloudi_service_send.erl)
* Monitoring to Graphite, OpenTSDB, SNMP, InfluxDB or StatsD with [`cloudi_service_monitoring`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_monitoring/src/cloudi_service_monitoring.erl)
* Remote Host Health Checks with [`cloudi_service_health_check`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_health_check/src/cloudi_service_health_check.erl)
* CloudI Configuration Validation with [`cloudi_service_validate_config`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_validate_config/src/cloudi_service_validate_config.erl)

Routing:

* Caching Static File Data with [`cloudi_service_filesystem`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_filesystem/src/cloudi_service_filesystem.erl)
* HTTP Client Requests with [`cloudi_service_http_client`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_client/src/cloudi_service_http_client.erl)
* HTTP REST Handlers with [`cloudi_service_http_rest`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_http_rest/src/cloudi_service_http_rest.erl)
* Fault-Tolerant Map-Reduce with [`cloudi_service_map_reduce`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_map_reduce/src/cloudi_service_map_reduce.erl) (See the [`hexpi` integration test controller](https://github.com/CloudI/CloudI/blob/master/src/tests/hexpi/erlang/src/cloudi_service_test_hexpi.erl))
* Durable Service Requests with [`cloudi_service_queue`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_queue/src/cloudi_service_queue.erl)
* Service Redundancy with [`cloudi_service_quorum`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_quorum/src/cloudi_service_quorum.erl)
* Service Request Redundancy with [`cloudi_service_funnel`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_funnel/src/cloudi_service_funnel.erl)
* Schedule Service Requests with [`cloudi_service_cron`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_cron/src/cloudi_service_cron.erl)
* Local/Remote(SSH) Request Routing with [`cloudi_service_router`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_router/src/cloudi_service_router.erl)
* Validation with [`cloudi_service_validate`](https://github.com/CloudI/CloudI/blob/master/src/lib/cloudi_service_validate/src/cloudi_service_validate.erl)

Maintained Services Excluded from this Repository:

* HtmlUnit with [`cloudi_service_htmlunit`](https://github.com/CloudI/cloudi_service_htmlunit)

Unmaintained Services Excluded from this Repository:

* elasticsearch with [`cloudi_service_db_elasticsearch`](https://github.com/CloudI/cloudi_service_db_elasticsearch)
* Cassandra with [`cloudi_service_db_cassandra`](https://github.com/CloudI/cloudi_service_db_cassandra) or [`cloudi_service_db_cassandra_cql`](https://github.com/CloudI/cloudi_service_db_cassandra_cql)
* CouchDB with [`cloudi_service_db_couchdb`](https://github.com/CloudI/cloudi_service_db_couchdb)
* memcached with [`cloudi_service_db_memcached`](https://github.com/CloudI/cloudi_service_db_memcached)
* Riak with [`cloudi_service_db_riak`](https://github.com/CloudI/cloudi_service_db_riak)
* TokyoTyrant with [`cloudi_service_db_tokyotyrant`](https://github.com/CloudI/cloudi_service_db_tokyotyrant)
* ZeroMQ with [`cloudi_service_zeromq`](https://github.com/CloudI/cloudi_service_zeromq)

The default CloudI configuration will run the included integration tests
if all the programming languages enabled by default are detected at configure
time and the `--with-integration-tests-ran` configuration argument is used
(to choose the [`src/cloudi_tests.conf.in`](https://github.com/CloudI/CloudI/blob/master/src/cloudi_tests.conf.in) configuration file).

If the `--with-integration-tests=no` configuration argument is used,
the more minimal CloudI configuration will be used instead
(in the [`src/cloudi_minimal.conf.in`](https://github.com/CloudI/CloudI/blob/master/src/cloudi_minimal.conf.in) configuration file)
to support basic things like the
[Quick Start Guide](https://github.com/CloudI/CloudI/tree/master/doc#readme),
the [Dashboard](https://github.com/CloudI/CloudI/tree/master/src/service_api#readme)
and any tutorials or examples.

## CONTACT

Michael Truog (mjtruog at protonmail dot com)

