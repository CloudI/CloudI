# Hello World 1 Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world1.erl`).  The example
uses separate OTP release generation for both CloudI and the internal
CloudI service release, application, or module.

## DETAILS

The approach with the Hello World 1 Example is for CloudI to load the
`hello_world1` application after CloudI has started.  The `hello_world1`
application can be specified within the cloudi.conf or provided
dynamically to the CloudI Service API.  Using this method of deployment
requires that the `hello_world1` application file not specify
CloudI as a dependency, because if a reltool generated release is used
to start the `hello_world1` service (with the release script file), reltool
will attempt to pull CloudI dependencies into the release.  Since the CloudI
dependencies are already loaded and running within the Erlang VM,
the dependencies are unnecessary for the `hello_world1` release.

The other usage examples show the other file types that are supported
when providing a string for the internal service module name.

## USAGE

To use an Erlang/OTP application file for an internal service with the same
module name:

    $ make
    $ cat << EOF > hello_world1_app.conf
    [{internal,
      "/examples/",
      "$PWD/ebin/hello_world1.app",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @hello_world1_app.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world1

To use an Erlang source file path with an internal service implementation:

    $ make
    $ cat << EOF > hello_world1_erl.conf
    [{internal,
      "/examples/",
      "$PWD/src/hello_world1.erl",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @hello_world1_erl.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world1

To use a compiled Erlang BEAM file path with an internal service implementation:

    $ make
    $ cat << EOF > hello_world1_beam.conf
    [{internal,
      "/examples/",
      "$PWD/ebin/hello_world1.beam",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @hello_world1_beam.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world1

To use an Erlang/OTP script release file for an internal service with the
same module name as the top-level application:

    $ make release
    $ cat << EOF > hello_world1_script.conf
    [{internal,
      "/examples/",
      "$PWD/release/releases/1/hello_world1.script",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @hello_world1_script.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world1

