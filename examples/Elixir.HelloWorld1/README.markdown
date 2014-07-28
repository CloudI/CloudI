# Elixir Hello World 1 Example

## PURPOSE

Provide the simplest example of an Elixir application running within the same
Erlang VM as CloudI, with a CloudI service (`HelloWorld1.ex`).
The example uses separate OTP release generation for both CloudI and the
internal CloudI service module, application, or release.

## DETAILS

The approach with the Elixir Hello World 1 Example is for CloudI to load the
`Elixir.HelloWorld1` application after CloudI has started.
The `Elixir.HelloWorld1` application can be specified within the
cloudi.conf or provided dynamically to the CloudI Service API.

The other usage examples show the other file types that are supported
when providing a string for the internal service module name.

## USAGE (Different ways of adding the same internal service)

Make sure elixir is in the Erlang VM's code path:

    $ curl -X POST -d '"/usr/local/lib/elixir/lib/elixir/ebin"' http://localhost:6467/cloudi/api/erlang/code_path_add

To use an Elixir/OTP application or module for an internal service in the code path (if the application name is different from the `cloudi_service` module, use the `application_name` service configuration option):

    $ make
    $ curl -X POST -d '"'$PWD'/_build/dev/lib/Elixir.HelloWorld1/ebin"' http://localhost:6467/cloudi/api/erlang/code_path_add
    $ cat << EOF > Elixir.HelloWorld1_module.conf
    [{internal,
      "/examples/",
      'Elixir.HelloWorld1',
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_module.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl -X POST -d '"'$PWD'/_build/dev/lib/Elixir.HelloWorld1/ebin"' http://localhost:6467/cloudi/api/erlang/code_path_remove
    $ curl http://localhost:6467/examples/hello_world1
    Hello World!

To use an Elixir/OTP application file for an internal service with the same
module name:

    $ make
    $ cat << EOF > Elixir.HelloWorld1_app.conf
    [{internal,
      "/examples/",
      "$PWD/_build/dev/lib/Elixir.HelloWorld1/ebin/Elixir.HelloWorld1.app",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_app.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world1
    Hello World!

To use a compiled Elixir BEAM file path with an internal service implementation:

    $ make
    $ cat << EOF > Elixir.HelloWorld1_beam.conf
    [{internal,
      "/examples/",
      "$PWD/_build/dev/lib/Elixir.HelloWorld1/ebin/Elixir.HelloWorld1.beam",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_beam.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world1
    Hello World!

To use an Elixir/OTP script release file for an internal service with the
same module name as the top-level application:

    $ make release
    $ cat << EOF > Elixir.HelloWorld1_script.conf
    [{internal,
      "/examples/",
      "$PWD/release/releases/1/Elixir.HelloWorld1.script",
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_script.conf http://localhost:6467/cloudi/api/erlang/services_add
    $ curl http://localhost:6467/examples/hello_world1
    Hello World!

