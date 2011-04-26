#[CloudI 0.1.5 (alpha)](http://cloudi.org)

## ABOUT

CloudI is an open-source private cloud computing framework for secure,
internal data processing.  CloudI manages a dynamic cloud of services with
an internal messaging bus.  Services can be created in Erlang, C, C++, Java,
Python, or Ruby, by using the CloudI interface (API).  The interface provides
a small set of functions that are commonly used for creating 
Service-Oriented Architecture (SOA):

* subscribe, unsubscribe
* send_async, send_sync, mcast_async
* recv_async
* return, forward

CloudI offers a simple way to integrate diverse services into a
fault-tolerant framework.  Messages are easily load balanced based on a
service's destination refresh method.  Access Control Lists (ACL) can be
defined for services that must explicitly allow and/or deny messages from
being sent to other services.  All CloudI functionality supports the creation
of RESTful (Representational State Transfer) services.  Prefixes are given
to service code in their configuration and act like a directory path,
into which a service name is created by calling the "subscribe" interface
function (e.g., "/db/pgsql/" is a prefix for the Erlang service code
cloudi_job_db_pgsql which creates the service name "cloudi_tests"
(also, the database name) so that all messages can reference the service with
"/db/pgsql/cloudi_tests" and ACLs can allow or deny prefixes like "/db/pgsql/").
More features are currently explained in the configuration file
(src/cloudi.conf), but proper documentation will be added soon.

The default configuration runs the hexadecimal PI test using the
Bailey-Borwein-Plouffe formula and verifies that the digits are correct
(the test can be found in src/tests/hexpi/).  A HTTP test also is in the
default configuration and uses the command line curl for simple test cases
(the test can be found in src/tests/http/ with the client script run.sh).
Another HTTP test is provided that can be used for load tests
(the test can be found in src/tests/http_req/) and provides a basic
HTTP server for static files (content is cached and not updated dynamically,
since it just demonstrates basic cloudi_job_filesystem functionality) that are
located at src/tests/http_req/public_html/ (access at
[http://127.0.0.1:6464/tests/http_req/](http://127.0.0.1:6464/tests/http_req/)).

CloudI currently supports the following databases:

* CouchDB (>= 0.9.0)
* memcached (>= 1.3)
* MySQL (>= 4.0)
* PostgreSQL (>= 7.4)
* Tokyo Tyrant (>= 1.1.23)

[To support CloudI development donate here.](http://pledgie.com/campaigns/9269)

## CONFIGURATION

Build Requirements:

* Erlang >= R14B01
* Java
* Python >= 2.5.0
* Ruby >= 1.9.0 (ruby1.9 package in Ubuntu, ruby19 package in OSX ports)
* GNU MP library (libgmp3-dev package in Ubuntu, gmp package in OSX ports)
* boost >= 1.36.0 

Optional:

* ZeroMQ > 2.0

On OSX, to run configure with the OSX ports paths, use:
CXXFLAGS="-I/opt/local/include" LDFLAGS="-L/opt/local/lib" ./configure

To build with ZeroMQ integration, use the "--with-zeromq" configure flag.
If ZeroMQ is not installed, it will be installed locally and linked into a
NIF statically, so it is not a runtime dependency.
The ZeroMQ NIF requires Erlang >= R14B02.

See [src/README](https://github.com/okeuday/CloudI/tree/master/src#readme) for basic build information

See [src/cloudi.conf.in](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in) for system configuration information

## RUNNING

See [src/README](https://github.com/okeuday/CloudI/tree/master/src#readme)

## USING

Integration points:

* CloudI API (See [src/api/README](https://github.com/okeuday/CloudI/tree/master/src/api#readme))
* ZeroMQ with [cloudi_job_zeromq](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_zeromq.erl) [configuration](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in)
* HTTP with [cloudi_job_misultin](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_misultin.erl) [configuration](https://github.com/okeuday/CloudI/blob/master/src/cloudi.conf.in)
* Supported databases
  * CouchDB with [cloudi_job_db_couchdb](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_db_couchdb.erl)
  * memcached with [cloudi_job_db_memcached](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_db_memcached.erl)
  * MySQL with [cloudi_job_db_mysql](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_db_mysql.erl)
  * PostgreSQL with [cloudi_job_db_pgsql](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_db_pgsql.erl)
  * Tokyo Tyrant with [cloudi_job_db_tokyotyrant](https://github.com/okeuday/CloudI/blob/master/src/lib/cloudi/src/cloudi_job_db_tokyotyrant.erl)

Dynamic configuration uses the job API (See [src/job_api/README](https://github.com/okeuday/CloudI/tree/master/src/job_api#readme))

## LICENSE

[BSD License](https://github.com/okeuday/CloudI/blob/master/src/LICENSE)

## CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)

