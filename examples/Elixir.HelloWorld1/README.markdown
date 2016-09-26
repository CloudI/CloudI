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

**n.b.: SERVICE_UUID will be unique with each new response**

Use these commands first:

    $ make release
    $ export PWD=`pwd`

Make sure elixir is in the Erlang VM's code path:

    $ curl -X POST -d '"/usr/local/lib/elixir/lib/elixir/ebin"' http://localhost:6464/cloudi/api/rpc/code_path_add.erl
    ok

To use an Elixir/OTP application or module for an internal service in the code path (if the application name is different from the `cloudi_service` module, use the `application_name` service configuration option):

    $ curl -X POST -d '"'$PWD'/_build/dev/lib/Elixir.HelloWorld1/ebin"' http://localhost:6464/cloudi/api/rpc/code_path_add.erl
    ok
    $ cat << EOF > Elixir.HelloWorld1_module.conf
    [[{prefix, "/examples/"},
      {module, 'Elixir.HelloWorld1'}]]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_module.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl -X POST -d '"'$PWD'/_build/dev/lib/Elixir.HelloWorld1/ebin"' http://localhost:6464/cloudi/api/rpc/code_path_remove.erl
    ok
    $ curl http://localhost:6464/examples/hello_world1
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use an Elixir/OTP application file for an internal service with the same
module name:

    $ cat << EOF > Elixir.HelloWorld1_app.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/_build/dev/lib/Elixir.HelloWorld1/ebin/Elixir.HelloWorld1.app"}]]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_app.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world1
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use a compiled Elixir BEAM file path with an internal service implementation:

    $ cat << EOF > Elixir.HelloWorld1_beam.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/_build/dev/lib/Elixir.HelloWorld1/ebin/Elixir.HelloWorld1.beam"}]]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_beam.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world1
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use an Elixir/OTP script release file for an internal service with the
same module name as the top-level application:

    $ cat << EOF > Elixir.HelloWorld1_script.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/release/releases/1/Elixir.HelloWorld1.script"}]]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_script.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world1
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use an Elixir/OTP boot release file for an internal service with the
same module name as the top-level application:

    $ cat << EOF > Elixir.HelloWorld1_boot.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/release/releases/1/Elixir.HelloWorld1.boot"}]]
    EOF
    $ curl -X POST -d @Elixir.HelloWorld1_boot.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world1
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

