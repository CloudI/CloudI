#[CloudI 0.1.0 (alpha)](http://cloudi.org)

## ABOUT

CloudI is an open-source private cloud computing framework for secure,
internal data processing.  CloudI manages a dynamic cloud of services with
an internal messaging bus.  Services can be created in Erlang, C, C++, Java,
or Python, by using the CloudI interface (API).  The interface provides
a small set of functions that are commonly used for creating 
Service-Oriented Architecture (SOA):

* subscribe, unsubscribe
* send_async, send_sync
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

Many changes occurred between the CloudI version 0.0.10 and version 0.1.0.
The changes are summarized below:

* CloudI is now a naturally master-less distributed system
  (i.e., requires no special configuration or redundancy)
* messaging can occur between services (i.e., jobs) which
  may exist on other CloudI nodes
* all service (i.e., job) names rely on strings rather than dynamic atoms,
  along with the trie data structure for efficient lookups
* cnodes are no longer used for external service (i.e., job) communication,
  instead sockets are used for each thread (either UDP or TCP can be used)
* external services (i.e., jobs) are no longer implemented in dynamic libraries
  but are instead implemented in executables.  this change makes running various
  VMs or interpreters easy and avoids dynamic library link dependency problems
  with various version dependencies (i.e., helps support Java, Python, etc.)
* HTTP can be used to call services, but JSON-RPC is not supported like it was
  in 0.0.10 (support for JSON-RPC will be added again soon)
* a normal autotools/rebar build system is used that doesn't attempt to
  install critical dependencies locally (it just requires that they exist)
* restarting stopped (e.g., SIGSTOP) OS processes is not implemented in 
  0.1.0 (but may be added back, as necessary)

The default configuration runs the hexadecimal PI test using the
Bailey-Borwein-Plouffe formula and verifies that the digits are correct
(the test can be found in src/tests/hexpi/).  A HTTP test also is in the
default configuration and uses the command line curl for simple test cases
(the test can be found in src/tests/http/ with the client script run.sh).

CloudI currently supports the following databases:

* CouchDB (>= 0.9.0)
* memcached (>= 1.3)
* MySQL (>= 4.0)
* PostgreSQL (>= 7.4)
* Tokyo Tyrant (>= 1.1.23)

[To support CloudI development donate here.](http://pledgie.com/campaigns/9269)

## CONFIGURATION

CloudI requires Erlang R14B01 because of dependencies on new standard modules.
The ETS bug in R14B01 does not affect CloudI (CloudI does not use ETS, so that
referential transparency is maintained and the system is scalable without
global data locks).

## RUNNING

See src/README

## LICENSE

BSD License

## CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)

