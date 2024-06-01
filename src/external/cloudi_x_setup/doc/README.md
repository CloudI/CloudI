

# The setup application #

__Authors:__ Ulf Wiger ([`ulf@wiger.net`](mailto:ulf@wiger.net)).

Generic setup utility for Erlang-based systems

[![Build Status](https://github.com/uwiger/setup/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/uwiger/setup/actions/workflows/ci.yml)


## Introduction ##

While Erlang/OTP comes with many wonderful applications, including the
Mnesia DBMS, there is no standard or convention for installing a
system. Erlang/OTP provides tools for building a boot script, and rules
for setting environment variables, etc., and Mnesia offers an API for
creating and modifying the database schema.

However, with no convention for when these tools and API functions
are called - and by whom - application developers are left having to
invent a lot of code and scripts, not to mention meditate over chapters
of Erlang/OTP documentation in order to figure out how things fit
together.

This utility offers a framework for initializing and configuring a
system, with a set of conventions allowing each component to provide
callbacks for different steps in the installation procedure.

The callbacks are defined through OTP application environment variables,
which can easily be overridden at install time.


## The setup_gen utility ##

The `setup_gen` utility is a simple tool to generate .rel file and
boot script for an Erlang-based system. It fetches configuration options
from a .conf file (read using `file:script/2`). As an example of a very
simple build, see examples/gproc.conf':

```

[{apps, [kernel,
	 stdlib,
	 sasl,
	 gproc]}].

```

This configuration file simply lists the applications to start from the
boot script. The `setup_gen` script can either be called from within
Erlang as:

```

Eshell V5.8.1  (abort with ^G)
1> setup_gen:run([{conf,"gproc.conf"},{outdir,"."},{name,"gproc"}]).
Options = [{conf,"gproc.conf"},{outdir,"."},{name,"gproc"}]
Paths = []
add path Res = ok
app_vsn(kernel) -> "2.14.1"
app_vsn(stdlib) -> "1.17.1"
app_vsn(sasl) -> "2.1.9.2"
app_vsn(gproc) -> "0.01"
Rel: {release,{"gproc","tmp"},
              {erts,"5.8.1"},
              [{kernel,"2.14.1"},
               {stdlib,"1.17.1"},
               {sasl,"2.1.9.2"},
               {gproc,"0.01"}]}
entering directory .
Path = []
make_script() -> ok
ok

```

...or as an escript:

```

escript ~/git/setup/ebin/setup_gen.beam gproc gproc.conf .

```

If the option `-install true` is given, the `setup_gen` utility will
generate an installation boot script, and `install.config` file, which
can be used to install the system, using a command like:

```

erl -sys install -boot install

```

This boot script will run kernel, stdlib and sasl, then load all other
applications, and finally run the `setup` application, which will find
and execute any setup hooks.

If the option `-setup stop_when_done true` is added to the command line,
the setup application will automatically shut down all running nodes after
running the setup hooks. Otherwise (default), it will hand over control to
the shell rather than terminate the Erlang VM.

See [`setup_gen:run/1`](setup_gen.md#run-1) for documentation on all supported options.


## Variable expansion ##
`setup` extends the functionality of `application:get_env/[2,3]` by also
supporting variable expansion, as well as a function for searching all
applications for instances of a given variable. This functionality is described
in [`setup`](setup.md).


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="setup.md" class="module">setup</a></td></tr>
<tr><td><a href="setup_file.md" class="module">setup_file</a></td></tr>
<tr><td><a href="setup_gen.md" class="module">setup_gen</a></td></tr>
<tr><td><a href="setup_lib.md" class="module">setup_lib</a></td></tr></table>

