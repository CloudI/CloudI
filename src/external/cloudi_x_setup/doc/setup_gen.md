

# Module setup_gen #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#help-0">help/0</a></td><td></td></tr><tr><td valign="top"><a href="#main-1">main/1</a></td><td></td></tr><tr><td valign="top"><a href="#run-1">run/1</a></td><td>Generates .rel file(s) and boot scripts for a given configuration.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="help-0"></a>

### help/0 ###

`help() -> any()`

<a name="main-1"></a>

### main/1 ###

`main(Args) -> any()`

<a name="run-1"></a>

### run/1 ###

<pre><code>
run(Options) -&gt; ok
</code></pre>
<br />

Generates .rel file(s) and boot scripts for a given configuration.

This function reads a configuration specification and generates the
files needed to start a node from an OTP boot script. Optionally, it can
also generate a 'setup' script, which contains the same applications, but
only loaded (except the `setup` application, if present, which is started).
This way, a node can be started with all paths set, and all environment
variables defined, such that a database can be created, and other setup
tasks be performed.

Mandatory options:
* `{name, Name}`  - Name of the release (and of the .rel and .script files)
* `{outdir, Dir}` - Where to put the generated files. Dir is created if not
already present.
* `{conf, Conf}`  - Config file listing apps and perhaps other options
* `{relconf, File}` - can be used instead of `conf`, and identifies a
reltool.config file (see [`reltool`](http://www.erlang.org/doc/man/index.html)) to be used as
system description. If a `conf` option is present, it will be used;
otherwise, a `relconf` option must be present.

Additional options:

* `{apps, [App]}` - List of applications to include in the release. Only the
first instance of this option is considered.
* `{add_apps, [App]}` - Adds applications to the ones given in the `apps`
option.
* `{remove_apps, Apps}` - Remove `Apps` from the list of applications.
* `{sort_app, App, Before}` - Change the sort order so that `App` comes
before `Before`. `Before` can be either an application
name or a list of names. In the latter case, `App`
is inserted before either of the applications in
the list, whichever comes first.
* `{include, ConfigFile}` - include options from the given file. The file
is processed using `file:script/2`.
* `{include, ConfigFile, Vars}` - as above, but passing along a list of
variable bindings to ConfigFile.
* `{include_lib, ConfigFile}` - As above, but ConfigFile is named as with
the `-include_lib(...)` directive in Erlang source code.
* `{include_lib, ConfigFile, Vars}` - as above, but passing along a list of
variable bindings to ConfigFile.
* `{sys, SysConfigFile}` - Read an existing sys.config file. The environment
found in this file may be redefined by `env` and `set_env` entries
(see below).
* `{env, [{App, [{K,V}]}]}` - Environment variables for the `sys.config`
file. `setup_gen` will merge all `env` entries, where later entries
replace earlier entries (based on the environment variable name).
* `{set_env, [{App, [{KeyPath, V}]}]}` - Modifies existing environment
structures, where `KeyPath` is a list of names (top name must be
an atom) describing a path in a tree structure, where each node
is either a `{Key, SubTree}` or a `{Key, Any, SubTree}` tuple. The
`set_env` function will continue into `SubTree` and either replace
the value representing the full `KeyPath` or create the remaining
subtree.
* `{target, Dir}` - Where to produce the generated files. The files will
end up in `Dir/releases/Vsn/`. If a `reltool.config` file is used,
the `{target_dir, D}` option will be translated into `{target,D}`.
* `{vsn, Vsn}` - System version, used to determine where to generate the
files (see `target` above).
* `{root, RootDir}` - Where to look for applications. Normally, `RootDir`
should represent either `RootDir/lib/*/ebin`, or `RootDir/*/ebin`,
but if the option `{wild_roots,true}` is given, it can be either
an "ebin" directory, or any parent directory to "ebin" directories.
Multiple `root` options can be given. If `target` is not given
"boot variables" will be generated for each root directory in turn,
named `V1 ... Vn`, then generating a relocatable boot script.
* `{pa, Path}` - Prepends `Path` to the code path. Multiple `pa` options
can be given.
* `{pz, Path}` - Appends `Path` to the code path. Multiple `pz` options
can be given.
* `{install, true|false}` - Tells setup whether to also build "install"
scripts and config files. An install script contains the same
applications as the normal script, but only loads them, starting
only the `setup` application. This allows a system to be installed
using "setup hooks", while having all the target system code
and environment available. An "install.config" file is also created,
which, if a `{nodes, Ns}` option is given, also configures Erlang
to wait for all given nodes, and then start the `setup` application
on the first node.
* `{start_setup, true|false}` - Tells whether setup should be started
automatically. The default is `true` (as it should be). The best way
to include setup, but not start it, would be to add `{setup, load}` to
the `apps` list.
* `{verbose, true|false}` - (Default: `false`) Turns on verbose printouts.


### <a name="Application_entries">Application entries</a> ###

Applications can be represented in a number of different ways:
* `AppName::atom()` - `setup` will search for the latest version
along the current code path.
* `{App::atom(), Vsn::latest|list()}` - where `Vsn` is an explicit version
identifying the application. `latest` instructs `setup` to pick the
latest version, if several versions can be found along the path.
* `{App::atom(), Type::atom()}` - where
`Type::permanent|temporary|transient|load` is the application start
type (or, in the case of 'load', no start at all).
* `{App, Vsn, Type}` - see `App`, `Vsn` and `Type` above
* `{App, Vsn, Incl}` - where `Incl` is a list of included applications.
* `{App, Vsn, Type, Incl}`


### <a name="Command-line_options">Command-line options</a> ###

The following options can be given on the command line of `setup_gen`:
* `-target Dir` - Equivalent to `{target, Dir}`
* `-name Name`  - Equivalent to `{name, Name}`
* `-root Dir`   - Equivalent to `{root, Dir}`
* `-out Dir`    - Equivalent to `{outdir, Dir}`
* `-relconf F`  - Equivalent to `{relconf, F}`
* `-conf F`     - Equivalent to `{conf, F}`
* `-install`    - Equivalent to `{install, true}`
* `-sys F`      - Equivalent to `{sys, F}`
* `-vsn V`      - Equivalent to `{vsn, V}`
* `-pa Dir`     - Equivalent to `{pa, Dir}`
* `-pz Dir`     - Equivalent to `{pa, Dir}`
* `-v`          - Equivalent to `{verbose, true}`


### <a name="Config_File_evaluation">Config File evaluation</a> ###

`setup` uses a customized version of `file:script()`. The return value
from the script will be treated as a list of instructions to `setup`.
Currently, a pseudo-local function, `b()` allows the script to inspect
the current variable bindings, and using instructions like
`{include, ConfigFile, Vars}`, variables can be passed along to helper
scripts. Using the pattern `{include, ConfigFile, [{Key, Value}|b()]}`,
all current variables can be passed to the helper script. Note that
`setup` may override the values of variables `Name`, `SCRIPT`, `CWD`
and `OPTIONS`. Specifically, these variables are bound to:

* `Name`: the name of the system being installed
* `SCRIPT`: the (absolute) name of the script currently being evaluated
* `CWD`: the current working directory when setup_gen was invoked
* `OPTIONS`: the options passed to the setup_gen script

The following local functions are handled by the script evaluator:

* `b() -> Bindings`
* `eval(File) -> {ok, Result} | {error, Reason}`
* `eval(File, Vars) -> {ok, Result} | {error, Reason}`
* `eval_lib(File) -> {ok, Result} | {error, Reason}`
* `eval_lib(File, Vars) -> {ok, Result} | {error, Reason}`

The `eval/[1,2]` and `eval_lib/[1,2]` functions work like the
`include` and `include_lib` instructions above, except the result is
returned as a normal function return value rather than being embedded
into the `setup` data. Essentially, they work like `file:script()`, but
with the variable bindings expected by a `setup` script and these local
functions supported.

