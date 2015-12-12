

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


Additional options:
* `{apps, [App]}` - List of applications to include in the release. Only the
first instance of this option is considered.
* `{add_apps, [App]}` - Adds applications to the ones given in the `apps`
option.
* `{include, ConfigFile}` - include options from the given file. The file
is processed using `file:script/2`.
* `{include_lib, ConfigFile}` - As above, but ConfigFile is named as with
the `-include_lib(...)` directive in Erlang
source code.
* ...
