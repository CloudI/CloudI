

# Module setup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

Setup utility for erlang applications.

<a name="description"></a>

## Description ##

This API contains:
* Support functions for system install ([`find_hooks/0`](#find_hooks-0),
[`run_hooks/0`](#run_hooks-0), [`lib_dirs/0`](#lib_dirs-0)).
* Functions for managing and inspecting the system environment
([`home/0`](#home-0), [`log_dir/0`](#log_dir-0), [`data_dir/0`](#data_dir-0),
[`verify_directories/0`](#verify_directories-0), [`verify_dir/0`](#verify_dir-0)).
* Support functions for application environments ([`get_env/2`](#get_env-2),
[`get_all_env/1`](#get_all_env-1), [`find_env_vars/1`](#find_env_vars-1), [`expand_value/2`](#expand_value-2)).
* Functions for controlling dynamic load/upgrade of applications
([`find_app/1`](#find_app-1), [`pick_vsn/3`](#pick_vsn-3), [`reload_app/1`](#reload_app-1),
[`patch_app/1`](#patch_app-1)).


### <a name="Variable_expansion">Variable expansion</a> ###

Setup supports variable substitution in application environments. It provides
some global variables, `"$HOME", "$DATA_DIR", "$LOG_DIR"`, corresponding to
the API functions [`home/0`](#home-0), [`data_dir/0`](#data_dir-0) and [`log_dir`](log_dir.md),
as well as some application-specific variables, `"$APP", "$PRIV_DIR",
"$LIB_DIR".

The normal way to use these variables is by embedding them in file names,
e.g. `{my_logs, "$LOG_DIR/$APP"}`, but a variable can also be referenced as:
* `{'$value',Var}` - The variable's value is used as-is (which means that
`{'$value', "$APP"}` expands to an atom corresponding to the current
app name.)
* `{'$string', Var}` - The value is represented as a string (list). If the
value isn't a "string type", `io_lib:format("~w",[Value])` is used.
* `{'$binary', Var}` - Like `'$string`', but using binary representation.

Custom variables can be defined by using either:
* *global scope* - The `setup` environment variable `vars`, containing a
list of `{VarName, Definition}` tuples
* *application-local scope* - Defining an application-local environment
variable `'$setup_vars`', on the same format as above.

The `VarName` shall be a string, e.g. `"MYVAR"` (no `$` prefix).
`Definition` can be one of:
* `{value, Val}` - the value of the variable is exactly `Val`
* `{expand, Val}` - `Val` is expanded in its turn
* `{apply, M, F, A}` - Use the return value of `apply(M, F, A)`.

When using a variable expansion, either insert the variable reference in
a string (or binary), or use one of the following formats:
* `'{'$value', Var}`' - Use value as-is
* `'{'$string', Var}`' - Use the string representation of the value
* `'{'$binary', Var}`' - Use the binary representation of the value.

Example:

```erlang

  2> application:set_env(setup, vars, [{"PLUS", {apply,erlang,'+',[1,2]}},
  2>                                   {"FOO", {value, {foo,1}}}]).
  ok
  3> application:set_env(stdlib, '$setup_vars',
  3>                     [{"MINUS", {apply,erlang,'-',[4,3]}},
  3>                      {"BAR", {value, "bar"}}]).
  ok
  4> application:set_env(setup, v1, "/$BAR/$PLUS/$MINUS/$FOO").
  ok
  5> setup:get_env(setup,v1).
  {ok,"/$BAR/3/$MINUS/{foo,1}"}
  6> application:set_env(stdlib, v1, "/$BAR/$PLUS/$MINUS/$FOO").
  ok
  7> setup:get_env(stdlib,v1).
  {ok,"/bar/3/1/{foo,1}"}
```

In the above example, the first expansion (command no. 5), leaves `$BAR`
and `$MINUS` unexpanded, since they are defined in the `stdlib` application,
and thus not known to `setup`. In command no. 6, however, they _are_
in context, and are expanded. The variables `$PLUS` and `$FOO` have global
context and are expanded in both cases.

It is also possible to refer to environment variables in the same
application. These are referenced as `"$env(VarName)"`. The corresponding
values are expanded in turn - take care not to create expansion loops!
The same rules for expansion as above apply.

Example:

```erlang

  2> application:set_env(setup,foo,"foo").
  ok
  3> application:set_env(setup,foo_dir,"$HOME/$env(foo)").
  ok
  4> setup:get_env(setup,foo_dir).
  {ok,"/Users/uwiger/git/setup/foo"}
```


### <a name="Customizing_setup">Customizing setup</a> ###

The following environment variables can be used to customize `setup`:
* `{home, Dir}` - The topmost directory of the running system. This should
be a writeable area.
* `{data_dir, Dir}` - A directory where applications are allowed to create
their own subdirectories and save data. Default is `Home/data.Node`.
* `{log_dir, Dir}` - A directory for logging. Default is `Home/log.Node`.
* `{stop_when_done, true|false}` - When invoking `setup` for an install,
`setup` normally remains running, allowing for other operations to be
* `{stop_delay, Millisecs}` - If `stop_when_done` is true, and the node
is going to shut down, setup will first wait for a specified number of
milliseconds (default: 5000). This can be useful in order to allow
asynchronous operations to complete before shutting down.
performed from the shell or otherwise. If `{stop_when_done, true}`, the
node is shut down once `setup` is finished.
* `{abort_on_error, true|false}` - When running install or upgrade hooks,
`setup` will normally keep going even if some hooks fail. A more strict
semantics can be had by setting `{abort_on_error, true}`, in which case
`setup` will raise an exception if an error occurs.
* `{mode, atom()}` - Specifies the context for running 'setup'. Default is
`normal`. The `setup` mode has special significance, since it's the default
mode for setup hooks, if no other mode is specified and the node has been
started with the setup-generated `install.boot` script. In theory, one may
specify any atom value, but it's probably wise to stick to the values
'normal', 'setup' and 'upgrade' as global contexts, and instead trigger
other mode hooks by explicitly calling [`run_hooks/1`](#run_hooks-1).
* `{verify_directories, boolean()}` - At startup, setup will normally ensure that
the directories used by setup actually exist. This behavior can be disabled through
the environment variable `{verify_directories, false}`. This can be desirable
if setup is used mainly e.g. for environment variable expansion, but not for
disk storage.
* `{run_timeout, Millisecs}` - Set a time limit for how long it may take for
setup to process the setup hooks. Default is `infinity`. If the timeout
is exceeded, the application start sequence will be aborted, which will
cause a (rather inelegant) boot sequence failure.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#applications-0">applications/0</a></td><td>Find all applications - either from the boot script or all loaded apps.</td></tr><tr><td valign="top"><a href="#data_dir-0">data_dir/0</a></td><td>Returns the configured data dir, or a best guess (<code>home()/data.Node</code>).</td></tr><tr><td valign="top"><a href="#expand_value-2">expand_value/2</a></td><td>Expand <code>Value</code> using global variables and the variables of <code>App</code></td></tr><tr><td valign="top"><a href="#expand_value-3">expand_value/3</a></td><td>Expand <code>Value</code> using global variables and the variables of <code>App</code></td></tr><tr><td valign="top"><a href="#find_app-1">find_app/1</a></td><td>Equivalent to <a href="#find_app-2"><tt>find_app(A, lib_dirs())</tt></a>.</td></tr><tr><td valign="top"><a href="#find_app-2">find_app/2</a></td><td>Locates application <code>A</code> along LibDirs (see <a docgen-rel="seemfa" docgen-href="#lib_dirs/0" href="#lib_dirs-0"><code>lib_dirs/0</code></a> and
<a docgen-rel="seemfa" docgen-href="#lib_dirs/1" href="#lib_dirs-1"><code>lib_dirs/1</code></a>) or under the OTP root, returning all found candidates.</td></tr><tr><td valign="top"><a href="#find_env_vars-1">find_env_vars/1</a></td><td>Searches all loaded apps for instances of the <code>Env</code> environment variable.</td></tr><tr><td valign="top"><a href="#find_hooks-0">find_hooks/0</a></td><td>Finds all custom setup hooks in all applications.</td></tr><tr><td valign="top"><a href="#find_hooks-1">find_hooks/1</a></td><td>Find all setup hooks for <code>Mode</code> in all applications.</td></tr><tr><td valign="top"><a href="#find_hooks-2">find_hooks/2</a></td><td>Find all setup hooks for <code>Mode</code> in <code>Applications</code>.</td></tr><tr><td valign="top"><a href="#get_all_env-1">get_all_env/1</a></td><td>Like <code>application:get_all_env/1</code>, but with variable expansion.</td></tr><tr><td valign="top"><a href="#get_env-2">get_env/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_env-3">get_env/3</a></td><td></td></tr><tr><td valign="top"><a href="#home-0">home/0</a></td><td>Returns the configured <code>home</code> directory, or a best guess (<code>$CWD</code>).</td></tr><tr><td valign="top"><a href="#keep_release-1">keep_release/1</a></td><td>Generates a release based on what's running in the current node.</td></tr><tr><td valign="top"><a href="#lib_dirs-0">lib_dirs/0</a></td><td>Equivalent to <a href="#union-2"><tt>union(lib_dirs("ERL_SETUP_LIBS"), lib_dirs("ERL_LIBS"))</tt></a>.</td></tr><tr><td valign="top"><a href="#lib_dirs-1">lib_dirs/1</a></td><td>Returns an expanded list of application directories under a lib path.</td></tr><tr><td valign="top"><a href="#lib_dirs_under_path-1">lib_dirs_under_path/1</a></td><td></td></tr><tr><td valign="top"><a href="#log_dir-0">log_dir/0</a></td><td>Returns the configured log dir, or a best guess (<code>home()/log.Node</code>).</td></tr><tr><td valign="top"><a href="#mode-0">mode/0</a></td><td>Returns the current "setup mode".</td></tr><tr><td valign="top"><a href="#ok-1">ok/1</a></td><td></td></tr><tr><td valign="top"><a href="#patch_app-1">patch_app/1</a></td><td>Adds an application's "development" path to a target system.</td></tr><tr><td valign="top"><a href="#patch_app-3">patch_app/3</a></td><td></td></tr><tr><td valign="top"><a href="#pick_vsn-3">pick_vsn/3</a></td><td>Picks the specified version out of a list returned by <a docgen-rel="seemfa" docgen-href="#find_app/1" href="#find_app-1"><code>find_app/1</code></a></td></tr><tr><td valign="top"><a href="#read_config_script-3">read_config_script/3</a></td><td></td></tr><tr><td valign="top"><a href="#read_config_script-4">read_config_script/4</a></td><td></td></tr><tr><td valign="top"><a href="#reload_app-1">reload_app/1</a></td><td>Equivalent to <a href="#reload_app-2"><tt>reload_app(AppName, latest)</tt></a>.</td></tr><tr><td valign="top"><a href="#reload_app-2">reload_app/2</a></td><td>Equivalent to <a href="#reload_app-3"><tt>reload_app(AppName, latest, lib_dirs())</tt></a>.</td></tr><tr><td valign="top"><a href="#reload_app-3">reload_app/3</a></td><td>Loads or upgrades an application to the specified version.</td></tr><tr><td valign="top"><a href="#run_hooks-0">run_hooks/0</a></td><td>Execute all setup hooks for current mode in order.</td></tr><tr><td valign="top"><a href="#run_hooks-1">run_hooks/1</a></td><td>Execute setup hooks for current mode in <code>Applications</code> in order.</td></tr><tr><td valign="top"><a href="#run_hooks-2">run_hooks/2</a></td><td>Execute setup hooks for <code>Mode</code> in <code>Applications</code> in order.</td></tr><tr><td valign="top"><a href="#run_selected_hooks-2">run_selected_hooks/2</a></td><td>Execute specified setup hooks in order.</td></tr><tr><td valign="top"><a href="#verify_dir-1">verify_dir/1</a></td><td>Ensures that the directory Dir exists and is writable.</td></tr><tr><td valign="top"><a href="#verify_directories-0">verify_directories/0</a></td><td>Ensures that essential directories exist and are writable.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="applications-0"></a>

### applications/0 ###

<pre><code>
applications() -&gt; [atom()]
</code></pre>
<br />

Find all applications - either from the boot script or all loaded apps.
The applications list is sorted in top application order, where included
applications follow directly after the top application they are included in.

<a name="data_dir-0"></a>

### data_dir/0 ###

<pre><code>
data_dir() -&gt; Directory
</code></pre>
<br />

Returns the configured data dir, or a best guess (`home()/data.Node`).

<a name="expand_value-2"></a>

### expand_value/2 ###

<pre><code>
expand_value(App::atom(), Value::any()) -&gt; any()
</code></pre>
<br />

Expand `Value` using global variables and the variables of `App`

The variable expansion is performed according to the rules outlined in
[Variable expansion](#Variable_expansion). If a loop is detected (a variable ends
up referencing itself), an exception is raised.
Use of [`expand_value/3`](#expand_value-3) (also providing the initial key name) is
recommended; this function is primarily here for backward compatibility
purposes.

<a name="expand_value-3"></a>

### expand_value/3 ###

<pre><code>
expand_value(App::atom(), Key::atom(), Value::any()) -&gt; any()
</code></pre>
<br />

Expand `Value` using global variables and the variables of `App`

The variable expansion is performed according to the rules outlined in
[Variable expansion](#Variable_expansion). The `Key` name as second argument is used
for loop detection, in which case an exception will be raised..

<a name="find_app-1"></a>

### find_app/1 ###

<pre><code>
find_app(A::atom()) -&gt; [{Vsn, Dir}]
</code></pre>
<br />

Equivalent to [`find_app(A, lib_dirs())`](#find_app-2).

<a name="find_app-2"></a>

### find_app/2 ###

<pre><code>
find_app(A::atom(), LibDirs::[string()]) -&gt; [{Vsn, Dir}]
</code></pre>
<br />

Locates application `A` along LibDirs (see [`lib_dirs/0`](#lib_dirs-0) and
[`lib_dirs/1`](#lib_dirs-1)) or under the OTP root, returning all found candidates.
The version is extracted from the `.app` file; thus, no version suffix
in the path name is required.

<a name="find_env_vars-1"></a>

### find_env_vars/1 ###

<pre><code>
find_env_vars(Env) -&gt; [{AppName, Value}]
</code></pre>
<br />

Searches all loaded apps for instances of the `Env` environment variable.

The environment variables are expanded according to the rules outlined in
[Variable expansion](#Variable_expansion)

<a name="find_hooks-0"></a>

### find_hooks/0 ###

<pre><code>
find_hooks() -&gt; [{PhaseNo, [{M, F, A}]}]
</code></pre>
<br />

Finds all custom setup hooks in all applications.
The setup hooks must be of the form
`{'$setup_hooks', [{PhaseNo, {M, F, A}} | {Mode, [{PhaseNo, {M,F,A}}]}]}`,
where PhaseNo should be (but doesn't have to be) an integer.
If `Mode` is not specified, the hook will pertain to the `setup` mode.

The hooks will be called in order:
- The phase numbers will be sorted.
- All hooks for a specific PhaseNo will be called in sequence,
in the same order as the applications appear in the boot script
(and, if included applications exist, in preorder traversal order).

A suggested convention is:
- Create the database at phase 100
- Create tables (or configure schema) at 200
- Populate the database at 300

Using the `setup` environment variable `modes`, it is possible to
define a mode that includes all hooks from different modes.
The format is `[{M1, [M2,...]}]`. The expansion is done recursively,
so a mode entry in the right-hand side of a pair can expand into other
modes. In order to be included in the final list of modes, an expanding
mode needs to include itself in the right-hand side. For example:

- Applying `a` to `[{a, [b]}]` returns `[b]`
- Applying `a` to `[{a, [a,b]}]` returns `[a,b]`
- Applying `a` to `[{a, [a,b]},{b,[c,d]}]` returns `[a,c,d]`

A typical application of this would be `[{test, [normal, test]}]`, where
starting in the `test` mode would cause all `normal` and all `test` hooks
to be executed.

<a name="find_hooks-1"></a>

### find_hooks/1 ###

<pre><code>
find_hooks(Mode) -&gt; [{PhaseNo, [{M, F, A}]}]
</code></pre>
<br />

Find all setup hooks for `Mode` in all applications

<a name="find_hooks-2"></a>

### find_hooks/2 ###

<pre><code>
find_hooks(Mode, Applications) -&gt; [{PhaseNo, [{M, F, A}]}]
</code></pre>
<br />

Find all setup hooks for `Mode` in `Applications`.

<a name="get_all_env-1"></a>

### get_all_env/1 ###

<pre><code>
get_all_env(A::atom()) -&gt; [{atom(), any()}]
</code></pre>
<br />

Like `application:get_all_env/1`, but with variable expansion.

The variable expansion is performed according to the rules outlined in
[Variable expansion](#Variable_expansion).

<a name="get_env-2"></a>

### get_env/2 ###

`get_env(A, Key) -> any()`

<a name="get_env-3"></a>

### get_env/3 ###

`get_env(A, Key, Default) -> any()`

<a name="home-0"></a>

### home/0 ###

<pre><code>
home() -&gt; Directory
</code></pre>
<br />

Returns the configured `home` directory, or a best guess (`$CWD`)

<a name="keep_release-1"></a>

### keep_release/1 ###

<pre><code>
keep_release(RelVsn) -&gt; ok
</code></pre>
<br />

Generates a release based on what's running in the current node.

<a name="lib_dirs-0"></a>

### lib_dirs/0 ###

<pre><code>
lib_dirs() -&gt; [string()]
</code></pre>
<br />

Equivalent to [`union(lib_dirs("ERL_SETUP_LIBS"), lib_dirs("ERL_LIBS"))`](#union-2).

<a name="lib_dirs-1"></a>

### lib_dirs/1 ###

<pre><code>
lib_dirs(Env::string()) -&gt; [string()]
</code></pre>
<br />

Returns an expanded list of application directories under a lib path

This function expands the (ebin/) directories under e.g. `$ERL_SETUP_LIBS` or
`$ERL_LIBS`. `$ERL_SETUP_LIB` has the same syntax and semantics as
`$ERL_LIBS`, but is (hopefully) only recognized by the `setup` application.
This can be useful e.g. when keeping a special 'extensions' or 'plugin'
root that is handled via `setup`, but not treated as part of the normal
'automatic code loading path'.

<a name="lib_dirs_under_path-1"></a>

### lib_dirs_under_path/1 ###

`lib_dirs_under_path(L) -> any()`

<a name="log_dir-0"></a>

### log_dir/0 ###

<pre><code>
log_dir() -&gt; Directory
</code></pre>
<br />

Returns the configured log dir, or a best guess (`home()/log.Node`)

<a name="mode-0"></a>

### mode/0 ###

<pre><code>
mode() -&gt; normal | atom()
</code></pre>
<br />

Returns the current "setup mode".

The mode can be defined using the `setup` environment variable `mode`.
The default value is `normal`. The mode is used to select which setup
hooks to execute when starting the `setup` application.

<a name="ok-1"></a>

### ok/1 ###

`ok(Other) -> any()`

<a name="patch_app-1"></a>

### patch_app/1 ###

<pre><code>
patch_app(AppName::atom()) -&gt; true | {error, Reason}
</code></pre>
<br />

Adds an application's "development" path to a target system

This function locates the given application (`AppName`) along the `$ERL_LIBS`
path, and prepends it to the code path of the existing system. This is useful
not least when one wants to add e.g. a debugging or trace application to a
target system.

The function will not add the same path again, if the new path is already
the 'first' path entry for the application `A`.

<a name="patch_app-3"></a>

### patch_app/3 ###

`patch_app(A, Vsn, LibDirs) -> any()`

<a name="pick_vsn-3"></a>

### pick_vsn/3 ###

<pre><code>
pick_vsn(App::atom(), Dirs::[{Vsn::string(), Dir::string()}], Vsn::Which) -&gt; {Vsn, Dir}
</code></pre>

<ul class="definitions"><li><code>Which = latest | next | Regexp</code></li></ul>

Picks the specified version out of a list returned by [`find_app/1`](#find_app-1)

* If `Which` is a string, it will be used as a `re` regexp pattern, and the
first matching version will be returned.

* If `Which = latest`, the last entry in the list will be returned (assumes
that the list is sorted in ascending version order).

* If `Which = next`, the next version following the current version of the
application `A` is returned, assuming `A` is loaded; if `A` is not loaded,
the first entry in the list is returned.

If no matching version is found, the function raises an exception.

<a name="read_config_script-3"></a>

### read_config_script/3 ###

`read_config_script(F, Name, Opts) -> any()`

<a name="read_config_script-4"></a>

### read_config_script/4 ###

`read_config_script(F, Name, Vars, Opts) -> any()`

<a name="reload_app-1"></a>

### reload_app/1 ###

<pre><code>
reload_app(AppName::atom()) -&gt; {ok, NotPurged} | {error, Reason}
</code></pre>
<br />

Equivalent to [`reload_app(AppName, latest)`](#reload_app-2).

<a name="reload_app-2"></a>

### reload_app/2 ###

<pre><code>
reload_app(AppName::atom(), ToVsn) -&gt; {ok, UnPurged} | {error, Reason}
</code></pre>
<br />

Equivalent to [`reload_app(AppName, latest, lib_dirs())`](#reload_app-3).

<a name="reload_app-3"></a>

### reload_app/3 ###

<pre><code>
reload_app(AppName::atom(), ToVsn0::ToVsn, LibDirs) -&gt; {ok, Unpurged} | {error, Reason}
</code></pre>

<ul class="definitions"><li><code>ToVsn = latest | next | Vsn</code></li><li><code>LibDirs = [string()]</code></li><li><code>Vsn = string()</code></li></ul>

Loads or upgrades an application to the specified version

This function is a convenient function for 'upgrading' an application.
It locates the given version (using [`find_app/1`](#find_app-1) and [`pick_vsn/3`](#pick_vsn-3))
and loads it in the most appropriate way:

* If the application isn't already loaded, it loads the application and
all its modules.

* If the application is loaded, it generates an appup script and performs
a soft upgrade. If the new version of the application has an `.appup` script
on-disk, that script is used instead.

The application is searched for along the existing path (that is, under
the roots of the existing code path, allowing for e.g. $ROOT/lib/app-1.0
and $ROOT/lib/app-1.2 to be found and tested against the version condition),
and also along `LibDirs` (see [`lib_dirs/0`](#lib_dirs-0) an [`lib_dirs/1`](#lib_dirs-1)).

The generated appup script is of the form:

* add modules not present in the previous version of the application

* do a soft upgrade on pre-existing modules, using suspend-code_change-resume

* delete modules that existed in the old version, but not in the new.

The purge method used is `brutal_purge` - see [`//sasl/appup`](http://www.erlang.org/doc/man/appup.html).

For details on how the new version is chosen, see [`find_app/1`](#find_app-1) and
[`pick_vsn/3`](#pick_vsn-3).

<a name="run_hooks-0"></a>

### run_hooks/0 ###

<pre><code>
run_hooks() -&gt; ok
</code></pre>
<br />

Execute all setup hooks for current mode in order.

See [`find_hooks/0`](#find_hooks-0) for details on the order of execution.

<a name="run_hooks-1"></a>

### run_hooks/1 ###

<pre><code>
run_hooks(Apps::Applications) -&gt; ok
</code></pre>
<br />

Execute setup hooks for current mode in `Applications` in order.

See [`find_hooks/0`](#find_hooks-0) for details on the order of execution.

<a name="run_hooks-2"></a>

### run_hooks/2 ###

<pre><code>
run_hooks(Mode, Apps::Applications) -&gt; ok
</code></pre>
<br />

Execute setup hooks for `Mode` in `Applications` in order

Note that no assumptions can be made about which process each setup hook
runs in, nor whether it runs in the same process as the previous hook.
See [`find_hooks/0`](#find_hooks-0) for details on the order of execution.

<a name="run_selected_hooks-2"></a>

### run_selected_hooks/2 ###

<pre><code>
run_selected_hooks(Mode, Hooks) -&gt; ok
</code></pre>
<br />

Execute specified setup hooks in order

Exceptions are caught and printed. This might/should be improved, but the
general idea is to complete as much as possible of the setup, and perhaps
repair afterwards. However, the fact that something went wrong should be
remembered and reflected at the end.

<a name="verify_dir-1"></a>

### verify_dir/1 ###

<pre><code>
verify_dir(Directory::Dir) -&gt; Dir
</code></pre>
<br />

Ensures that the directory Dir exists and is writable.

<a name="verify_directories-0"></a>

### verify_directories/0 ###

<pre><code>
verify_directories() -&gt; ok
</code></pre>
<br />

Ensures that essential directories exist and are writable.
Currently, the directories corresponding to [`home/0`](#home-0),
[`log_dir/0`](#log_dir-0) and [`data_dir/0`](#data_dir-0) are verified.

