[![Travis][travis badge]][travis]
[![Hex.pm Version][hex version badge]][hex]
[![Hex.pm License][hex license badge]][hex]
[![Build Tool][build tool]][hex]

## Overview

`hut` is a minimal library for Erlang libraries and small applications to
stay agnostic to the logging framework in use. Its purpose is to allow the
developers of umbrella applications to use their logging framework of choice
and ensure that dependency stick to that choice as well.

## Origin

The idea for `hut` came out of
[a discussion](https://github.com/Feuerlabs/exometer_core/issues/57) about
[exometer_core](https://github.com/Feuerlabs/exometer_core)'s logging facilities.

## Usage

1. Include `hut` as a dependency in your toolchain.
2. Include `hut` in your `.erl` files via `-include_lib(hut/include/hut.hrl)`
3. Log with
  - `?log(Level, Msg)` to log the given message
  - `?log(Level, Fmt, Args)` to format the given message with the arguments using `io_lib:format/2`
  - `?log(Level, Fmt, Args, Opts)` to pass additional options to the logging backend in use
  - `?slog(Level, #{key1 => Value1, ...})` structured log message
  - `?slog(Level, Data :: map(), Metadata :: map())` structured log message with custom metadata
  - `?set_process_metadata(Metadata :: map())` add custom metadata to the process
4. Compile and pass the appropriate macro to the compiler to enable a certain backend as described next.
5. Ensure that the application `hut` is started as part of your system.

## Supported Logging Backends

- SASL `error_logger` as the default (on OTP20 and older)
- `logger` as the default on OTP21+
- `io:format/2` via `-DHUT_IOFORMAT`
- no-op logging via `-DHUT_NOOP`
- [Lager](https://github.com/basho/lager) via `-DHUT_LAGER`
  (you may optionally specify a different log message sink by additionally defining
  `-DHUT_LAGER_SINK mysinkname`; the default message sink is `lager`)
- custom callback module via `-DHUT_CUSTOM -DHUT_CUSTOM_CB mycbmod`
  (the module must provide the function `log(Level, Fmt, Args, Opts)`

## Log Level Gates

`hut` provides a simple mechanisms to drop log messages depending on the enabled
log level. The functionality is similar to how Lager works with log levels.

The current log level (default is `info`) can be set through the application variable `level`:

```shell
erl -hut level debug
```

All log messages with levels below the current log level will be dropped.

The gating mechanism can be disabled through the application variable `use_log_level_gate`:

```shell
erl -hut use_log_level_gate false
```

**NOTE:** The gating mechanism is not supported for the Lager backend, since Lager
          itself provides a similar functionality which you should rely on when using
          Lager.

## Examples

Refer to the respective `Makefile` in each example for details.

- `examples/basic` shows the use of all backends from an Erlang library
- `examples/elixir` shows how to use a `hut`-enabled Erlang library within an Elixir application
- `examples/rebar3` shows how to use `hut` in a `rebar3` project

[travis]: https://travis-ci.org/tolbrino/hut
[travis badge]: https://img.shields.io/travis/tolbrino/hut/master.svg?style=flat-square
[hex]: https://hex.pm/packages/hut
[hex version badge]: https://img.shields.io/hexpm/v/hut.svg?style=flat-square
[hex license badge]: https://img.shields.io/hexpm/l/hut.svg?style=flat-square
[build tool]: https://img.shields.io/badge/build%20tool-erlang.mk-orange.svg?style=flat-square
