# Hello World 2 Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world2.erl`).  The example
uses the same OTP release for both CloudI and the internal CloudI service.

## DETAILS

The approach with the Hello World 2 Example is for CloudI to load the
`hello_world2` application after CloudI has started.  The `hello_world2`
application can be specified within the cloudi.conf or provided
dynamically to the CloudI Service API.  Using this method of deployment
(within the same OTP release) the `hello_world2` application file can specify
CloudI as a dependency by listing the `cloudi_core` application as a
dependency, unlike the approach within the `hello_world1` example.

## USAGE

To use an Erlang/OTP application file for an internal service with the same
module name:

    $ make release
    (run the release)
    $ cat << EOF > hello_world2_app.conf
    [{internal,
      "/examples/",
      "$PWD/ebin/hello_world2.app",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @hello_world2_app.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world2

