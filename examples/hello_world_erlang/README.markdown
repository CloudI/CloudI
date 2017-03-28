# Erlang Hello World Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world_erlang.erl`).  The example
uses separate OTP release generation for both CloudI and the internal
CloudI service module, application, or release.

## DETAILS

The approach with the Erlang Hello World Example is for CloudI to load the
`hello_world_erlang` application after CloudI has started
(check that CloudI is already running).  The `hello_world_erlang`
application can be specified within the cloudi.conf or provided
dynamically to the CloudI Service API.

The other usage examples show the other file types that are supported
when providing a string for the internal service module name.

## USAGE (Different ways of adding the same internal service)

**n.b.: SERVICE_UUID will be unique with each new response**

Use these commands first:

    $ make release
    $ export PWD=`pwd`

To use an Erlang/OTP application or module for an internal service in the code path (if the application name is different from the CloudI service module name implementing the `cloudi_service` behaviour, use the `application_name` service configuration option):

    $ curl -X POST -d '"'$PWD'/ebin"' http://localhost:6464/cloudi/api/rpc/code_path_add.erl
    ok
    $ cat << EOF > hello_world_erlang_module.conf
    [[{prefix, "/examples/"},
      {module, hello_world_erlang}]]
    EOF
    $ curl -X POST -d @hello_world_erlang_module.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl -X POST -d '"'$PWD'/ebin"' http://localhost:6464/cloudi/api/rpc/code_path_remove.erl
    ok
    $ curl http://localhost:6464/examples/hello_world_erlang
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use an Erlang/OTP application file for an internal service with the same
module name:

    $ cat << EOF > hello_world_erlang_app.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/ebin/hello_world_erlang.app"}]]
    EOF
    $ curl -X POST -d @hello_world_erlang_app.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world_erlang
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use a compiled Erlang BEAM file path with an internal service implementation:

    $ cat << EOF > hello_world_erlang_beam.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/ebin/hello_world_erlang.beam"}]]
    EOF
    $ curl -X POST -d @hello_world_erlang_beam.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world_erlang
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use an Erlang/OTP script release file for an internal service with the
same module name as the top-level application:

    $ cat << EOF > hello_world_erlang_script.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/release/releases/1/hello_world_erlang.script"}]]
    EOF
    $ curl -X POST -d @hello_world_erlang_script.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world_erlang
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use an Erlang/OTP boot release file for an internal service with the
same module name as the top-level application:

    $ cat << EOF > hello_world_erlang_boot.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/release/releases/1/hello_world_erlang.boot"}]]
    EOF
    $ curl -X POST -d @hello_world_erlang_boot.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world_erlang
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

To use an Erlang source file path with an internal service implementation
(it is better to handle the compilation of Erlang modules separately, but this
 usage may aid quick development of Erlang CloudI services):

    $ cat << EOF > hello_world_erlang_erl.conf
    [[{prefix, "/examples/"},
      {module, "$PWD/src/hello_world_erlang.erl"}]]
    EOF
    $ curl -X POST -d @hello_world_erlang_erl.conf http://localhost:6464/cloudi/api/rpc/services_add.erl
    [SERVICE_UUID]
    $ curl http://localhost:6464/examples/hello_world_erlang
    Hello World!
    $ curl -X POST -d '[SERVICE_UUID]' http://localhost:6464/cloudi/api/rpc/services_remove.erl
    ok

