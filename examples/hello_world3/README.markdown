# Hello World 3 Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world3.erl`), using rebar.
The example uses the same OTP release for both CloudI and the internal CloudI
service.

## DETAILS

The approach with the Hello World 3 Example is for CloudI to be
compiled and installated locally as a rebar dependency
(so during the build process).  Using rebar in this way is necessary
due to rebar's lack of support for non-Erlang programming languages.
However, if you only want internal service support
(i.e., Erlang-only CloudI usage), you can use the
[`cloudi_core`](https://github.com/CloudI/cloudi_core) repository as a
rebar dependency.

The `hello_world3` internal service configuration can use the
`automatic_loading` service configuration option set to `false` to depend
fully on the generated release loading the `hello_world3` application
and its `hello_world3` module implementation of the `cloudi_service`
interface.  The reltool.config file includes the applications
`cloudi_service_api_requests` and `cloudi_service_http_cowboy` since
they are used in the cloudi.conf but are not listed as `hello_world3`
application dependencies.  This means that both
`cloudi_service_api_requests` and `cloudi_service_http_cowboy` utilize
`automatic_loading` to make sure both the application and the internal
service module is loaded before each service instance is added.
So, this is a method of embedding CloudI into an Erlang application
with CloudI as a rebar dependency.

PLEASE NOTE (as of 10/18/2013, CloudI version 1.3.0): rebar was recently
broken, when fetching the HEAD of https://github.com/rebar/rebar due to
attempts at improving update-deps functionality.  To avoid problems with
update-deps you should use the most recent rebar release tag, which is
`2.1.0-pre`.  This version of rebar is provided within the CloudI repo to
satisfy build-time dependencies (if the configure script is not
given --without-rebar).  This is only a concern when trying to use
rebar with the `update-deps` command.

## USAGE

To use an Erlang/OTP application file for an internal service with the same
module name:

    $ rebar get-deps
    $ rebar compile
    $ rebar generate
    $ cd release
    $ bin/cloudi_hello_world3 start
    $ curl http://localhost:6467/examples/hello_world3
    Hello World!
    $ bin/cloudi_hello_world3 stop

If you want to alter the CloudI configuration
(e.g., to use the ZeroMQ/CloudI integration)
just define the environmental variable `CLOUDI_CONFIGURE_ARGS` for the
rebar get-deps command:

    $ export CLOUDI_CONFIGURE_ARGS="--with-zeromq"
