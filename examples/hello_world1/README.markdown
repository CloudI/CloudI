# Hello World 1 Example

## PURPOSE

Provide the simplest example of an Erlang application running within the same
Erlang VM as CloudI, with a CloudI service (`hello_world1.erl`).

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

