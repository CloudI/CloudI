# Hello World 3 Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world3.erl`), using rebar.
The example uses the same OTP release for both CloudI and the internal CloudI
service.

## DETAILS

The approach with the Hello World 3 Example is for CloudI to load the
`hello_world3` application after CloudI has started.  The `hello_world3`
application can be specified within the cloudi.conf or provided
dynamically to the CloudI Service API, to start the CloudI service
(separate from the application's supervision hierarchy, if one is present).
Using this method of deployment (within the same OTP release) the
`hello_world3` application file can specify CloudI as a dependency by
listing the `cloudi_core` application as a dependency, unlike the
approach within the `hello_world1` example.

## USAGE

To use an Erlang/OTP application file for an internal service with the same
module name:

    $ rebar get-deps
    $ export ERL_LIBS=deps/cloudi_core/install_local/lib/cloudi-1.2.4/lib/
    $ rebar compile (or) erlc -o ebin/ src/hello_world3.erl
    $ rebar generate (or) ../../src/lib/reltool_util/release
    $ cd release
    $ bin/cloudi_hello_world3 start
    $ curl http://localhost:6467/examples/hello_world3
    Hello World!
    $ bin/cloudi_hello_world3 stop

