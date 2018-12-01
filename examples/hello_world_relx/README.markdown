# Hello World Relx Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world_relx.erl`), using relx.
The example uses the same OTP release for both CloudI and the internal CloudI
service.

## DETAILS

The approach with the Hello World Relx Example is for relx to build
a release using the CloudI installation on the system at the default
installation location (e.g., ./configure --prefix="/usr/local/").
The `hello_world_relx` internal service configuration can use the
`automatic_loading` service configuration option set to `false` to depend
fully on the generated release loading the `hello_world_relx` application
and its `hello_world_relx` module implementation of the `cloudi_service`
interface.  The relx.config file includes the applications
`cloudi_service_api_requests` and `cloudi_service_http_cowboy1` since
they are used in the cloudi.conf but are not listed as `hello_world_relx`
application dependencies.  This means that both
`cloudi_service_api_requests` and `cloudi_service_http_cowboy1` utilize
`automatic_loading` to make sure both the application and the internal
service module is loaded before each service instance is added.
So, this is a method of embedding CloudI into an Erlang application by
using a system installation of CloudI with relx to package a new release.

[hello_world_reltool](https://github.com/CloudI/CloudI/tree/master/examples/hello_world_reltool#readme) provides the equivalent for reltool.

## USAGE

To use an Erlang/OTP application file for an internal service with the same
module name (the directions assume you don't have relx installed):

    $ git clone https://github.com/erlware/relx
    $ cd relx
    $ make
    $ cd ..
    $ make
    $ relx/relx
    $ cd release
    $ bin/cloudi start
    $ curl http://localhost:6464/examples/hello_world_relx
    Hello World!
    $ bin/cloudi stop

