rebar (CloudI fork)
===================

rebar is an Erlang build tool that makes it easy to compile and test Erlang/OTP
applications

rebar is now a single human-readable escript (Erlang script),
so it's easy to distribute or even embed directly in a project.
rebar uses standard Erlang/OTP conventions for project structures,
thus minimizing the amount of build configuration work.
rebar also provides dependency management, enabling application writers to
easily reuse common libraries from a variety of locations (git, hg, etc).

History
-------

This repository is a fork of the historical rebar2 repository
(https://github.com/rebar/rebar using the tag `2.1.0-pre`).
This repository exists to provide a more dependable rebar2 release
that can work with any Erlang/OTP release while avoiding complexity.
The reasons rebar2 is still being used instead of
[rebar3](https://github.com/erlang/rebar3) are:

* rebar3 has many dependencies that are not static, so it represents a very large amount of source code with security risks (due to forcing the use of remote dependencies)
* rebar3 is focused on usage with dependencies as remote packages, not filesystem dependencies
* rebar3 was not created with autoconf/automake use considered (including the concept of keeping srcdir separate from builddir)
* rebar3 was never meant to remain compatible with rebar2 use

This fork of rebar2 has removed historical functionality that was known to be
unused, unreliable or misleading to focus on the compilation and testing of
Erlang/OTP source code.  The differences with the historical rebar2 are
listed below:

* `rebar_port_compiler.erl` source code was removed (use a separate build tool for C/C++, OCaml, Rust, etc.)
* `rebar_escripter.erl` source code was removed (to avoid the creation of opaque binary blobs bound to a small range of Erlang/OTP releases and the related execution failures)
* `rebar_qc.erl` source code was removed (using PropEr in CT use is simpler)
* `rebar_xref.erl` source code was removed (dialyzer use catches more problems)
* `rebar_shell.erl` source code was removed (use a shell separate from rebar execution)
* `rebar_reltool.erl` source code was removed (use the `release` escript in [reltool_util](https://github.com/okeuday/reltool_util) for this functionality)
* `rebar_appups.erl` and `rebar_upgrade.erl` source code was removed
* `rebar_asn1_compiler.erl` source code was removed
* `rebar_abnfc_compiler.erl` source code was removed
* `rebar_lfe_compiler.erl` source code was removed (use `lfec` instead)
* `rebar_neotoma_compiler.erl` source code was removed
* `rebar_protobuffs_compiler.erl` source code was removed
* `rebar_templater.erl` and `rebar_erlydtl_compiler.erl` source code was removed
* `rebar.config` `plugins` module functionality was removed
* `$HOME/.rebar/config` is not loaded
* `--long` command-line arguments are not accepted to avoid `--long=value` or `--long value` ambiguity
* `-f (--force)`, `-D`, `-p (--profile)`, `-k (--keep-going)` command-line arguments were removed

### Dependencies

You will need a working installation of Erlang/OTP R13B03 (or later).
Information on building and installing [Erlang/OTP](https://www.erlang.org) can
be found [here](https://www.erlang.org/doc/installation_guide/install).

Code style
----------

Do not introduce trailing whitespace.

Do not mix spaces and tabs.

Do not introduce lines longer than 80 characters.

Writing Commit Messages
-----------------------

Structure your commit message like this:

<pre>
One line summary (less than 50 characters)

Longer description (wrap at 72 characters)
</pre>

### Commit Message Example

Less Than 50 Characters Subject
* What was changed
* Imperative present tense (fix, add, change)
  * `Fix bug 123`
  * `Add 'foobar' command`
  * `Change default timeout to 123`
* Why, explain intention and implementation approach
* No period

### Atomicity

* Break up logical changes that are unrelated
* Make whitespace changes separately

