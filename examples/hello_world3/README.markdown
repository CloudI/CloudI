# Hello World 3 Example

**WARNING: This approach will be removed soon.  The approach causes too
much compilation time during development and is problematic with rebar3.
Please use a solution based on
[hello_world2](https://github.com/CloudI/CloudI/tree/develop/examples/hello_world2#readme) or
[hello_world4](https://github.com/CloudI/CloudI/tree/develop/examples/hello_world4#readme)
instead**

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world3.erl`), using rebar.
The example uses the same OTP release for both CloudI and the internal CloudI
service.

## DETAILS

The approach with the Hello World 3 Example is for CloudI to be
compiled and installated locally as a rebar (version 2.x) dependency
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

If you use the `update-deps` command with rebar you will want to use
either an old release of rebar (e.g., the `2.1.0-pre` tag) or a new
release of rebar (`2.6.0` should work), due to broken behaviour in the
rebar source code (added around 10/18/2013).  To find a new release,
go to the rebar [repository](https://github.com/rebar/rebar).

## USAGE

To use an Erlang/OTP application file for an internal service with the same
module name:

    $ rebar get-deps compile generate
    $ cd release
    $ bin/cloudi_hello_world3 start
    $ curl http://localhost:6464/examples/hello_world3
    Hello World!
    $ bin/cloudi_hello_world3 stop

If you want to alter the CloudI configuration
(e.g., to use the ZeroMQ/CloudI integration)
just define the environmental variable `CLOUDI_CONFIGURE_ARGS` for the
rebar get-deps command:

    $ export CLOUDI_CONFIGURE_ARGS="--with-zeromq"
