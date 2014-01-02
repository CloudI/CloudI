# Hello World 2 Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world2.erl`).  The example
uses the same OTP release for both CloudI and the internal CloudI service.

## DETAILS

The approach with the Hello World 2 Example is for reltool to build
a release using the CloudI installation on the system at the default
installation location (e.g., ./configure --prefix="/usr/local/").
The `hello_world2` internal service configuration can use the
`automatic_loading` service configuration option set to `false` to depend
fully on the generated release loading the `hello_world2` application
and its `hello_world2` module implementation of the `cloudi_service`
interface.  The reltool.config file includes the applications
`cloudi_service_api_requests` and `cloudi_service_http_cowboy` since
they are used in the cloudi.conf but are not listed as `hello_world2`
application dependencies.  This means that both
`cloudi_service_api_requests` and `cloudi_service_http_cowboy` utilize
`automatic_loading` to make sure both the application and the internal
service module is loaded before each service instance is added.
So, this is a method of embedding CloudI into an Erlang application by
using a system installation of CloudI with reltool to package a new release.

`hello_world4` provides the equivalent for relx.

## USAGE

To use an Erlang/OTP application file for an internal service with the same
module name:

    $ make release
    $ cd release
    $ bin/cloudi_hello_world2 start
    $ curl http://localhost:6467/examples/hello_world2
    Hello World!
    $ bin/cloudi_hello_world2 stop

