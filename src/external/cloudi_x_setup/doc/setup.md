

# Module setup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Setup utility for erlang applications.
__Behaviours:__ [`application`](application.md).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#data_dir-0">data_dir/0</a></td><td>Returns the configured data dir, or a best guess (<code>home()/data.Node</code>).</td></tr><tr><td valign="top"><a href="#expand_value-2">expand_value/2</a></td><td></td></tr><tr><td valign="top"><a href="#find_app-1">find_app/1</a></td><td>Equivalent to <a href="#find_app-2"><tt>find_app(A, lib_dirs())</tt></a>.</td></tr><tr><td valign="top"><a href="#find_app-2">find_app/2</a></td><td>Locates application <code>A</code> along LibDirs (see <a href="#lib_dirs-0"><code>lib_dirs/0</code></a> and
<a href="#lib_dirs-1"><code>lib_dirs/1</code></a>) or under the OTP root, returning all found candidates.</td></tr><tr><td valign="top"><a href="#find_env_vars-1">find_env_vars/1</a></td><td>Searches all loaded apps for instances of the <code>Env</code> environment variable.</td></tr><tr><td valign="top"><a href="#find_hooks-0">find_hooks/0</a></td><td>Finds all custom setup hooks in all applications.</td></tr><tr><td valign="top"><a href="#find_hooks-1">find_hooks/1</a></td><td>Find all setup hooks for <code>Mode</code> in all applications.</td></tr><tr><td valign="top"><a href="#find_hooks-2">find_hooks/2</a></td><td>Find all setup hooks for <code>Mode</code> in <code>Applications</code>.</td></tr><tr><td valign="top"><a href="#get_env-2">get_env/2</a></td><td></td></tr><tr><td valign="top"><a href="#home-0">home/0</a></td><td>Returns the configured <code>home</code> directory, or a best guess (<code>$CWD</code>).</td></tr><tr><td valign="top"><a href="#lib_dirs-0">lib_dirs/0</a></td><td>Equivalent to <a href="#lib_dirs-1"><tt>lib_dirs(concat("ERL_SETUP_LIBS", "ERL_LIBS"))</tt></a>.</td></tr><tr><td valign="top"><a href="#lib_dirs-1">lib_dirs/1</a></td><td>Returns an expanded list of application directories under a lib path.</td></tr><tr><td valign="top"><a href="#log_dir-0">log_dir/0</a></td><td>Returns the configured log dir, or a best guess (<code>home()/log.Node</code>).</td></tr><tr><td valign="top"><a href="#ok-1">ok/1</a></td><td></td></tr><tr><td valign="top"><a href="#patch_app-1">patch_app/1</a></td><td>Adds an application's "development" path to a target system.</td></tr><tr><td valign="top"><a href="#pick_vsn-3">pick_vsn/3</a></td><td>Picks the specified version out of a list returned by <a href="#find_app-1"><code>find_app/1</code></a></td></tr><tr><td valign="top"><a href="#read_config_script-3">read_config_script/3</a></td><td></td></tr><tr><td valign="top"><a href="#reload_app-1">reload_app/1</a></td><td>Equivalent to <a href="#reload_app-2"><tt>reload_app(AppName, latest)</tt></a>.</td></tr><tr><td valign="top"><a href="#reload_app-2">reload_app/2</a></td><td>Equivalent to <a href="#reload_app-3"><tt>reload_app(AppName, latest, lib_dirs())</tt></a>.</td></tr><tr><td valign="top"><a href="#reload_app-3">reload_app/3</a></td><td>Loads or upgrades an application to the specified version.</td></tr><tr><td valign="top"><a href="#run_hooks-0">run_hooks/0</a></td><td>Execute all setup hooks for current mode in order.</td></tr><tr><td valign="top"><a href="#run_hooks-1">run_hooks/1</a></td><td>Execute setup hooks for current mode in <code>Applications</code> in order.</td></tr><tr><td valign="top"><a href="#run_hooks-2">run_hooks/2</a></td><td>Execute setup hooks for <code>Mode</code> in <code>Applications</code> in order.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td>Application start function.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>Application stop function
end.</td></tr><tr><td valign="top"><a href="#verify_dir-1">verify_dir/1</a></td><td>Ensures that the directory Dir exists and is writable.</td></tr><tr><td valign="top"><a href="#verify_directories-0">verify_directories/0</a></td><td>Ensures that essential directories exist and are writable.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="data_dir-0"></a>

### data_dir/0 ###


<pre><code>
data_dir() -&gt; Directory
</code></pre>
<br />

Returns the configured data dir, or a best guess (`home()/data.Node`).

<a name="expand_value-2"></a>

### expand_value/2 ###

`expand_value(App, Value) -> any()`


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


The environment variables may contain instances of
`$APP`, `$PRIV_DIR`, `$LIB_DIR`, `$DATA_DIR`, `$LOG_DIR`, `$HOME`,
inside strings or binaries, and these will be replaced with actual values
for the current system (`$APP` simply expands to the name of the current
application).
<a name="find_hooks-0"></a>

### find_hooks/0 ###


<pre><code>
find_hooks() -&gt; [{PhaseNo, [{M, F, A}]}]
</code></pre>
<br />

Finds all custom setup hooks in all applications.
The setup hooks must be of the form

```
{'$setup_hooks', [{PhaseNo, {M, F, A}}]}
```

,
where PhaseNo should be (but doesn't have to be) an integer.



The hooks will be called in order:
- The phase numbers will be sorted.
- All hooks for a specific PhaseNo will be called in sequence,
in the same order as the applications appear in the boot script
(and, if included applications exist, in preorder traversal order).


A suggested convention is:
- Create the database at phase 100
- Create tables (or configure schema) at 200
- Populate the database at 300
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
<a name="get_env-2"></a>

### get_env/2 ###

`get_env(A, Key) -> any()`


<a name="home-0"></a>

### home/0 ###


<pre><code>
home() -&gt; Directory
</code></pre>
<br />

Returns the configured `home` directory, or a best guess (`$CWD`)
<a name="lib_dirs-0"></a>

### lib_dirs/0 ###


<pre><code>
lib_dirs() -&gt; [string()]
</code></pre>
<br />

Equivalent to [`lib_dirs(concat("ERL_SETUP_LIBS", "ERL_LIBS"))`](#lib_dirs-1).
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
<a name="log_dir-0"></a>

### log_dir/0 ###


<pre><code>
log_dir() -&gt; Directory
</code></pre>
<br />

Returns the configured log dir, or a best guess (`home()/log.Node`)
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



The purge method used is `brutal_purge` - see [`//sasl/appup`](/Users/uwiger/uw/me/sasl/doc/appup.md).


For details on how the new version is chosen, see [`find_app/1`](#find_app-1) and
[`pick_vsn/3`](#pick_vsn-3).
<a name="run_hooks-0"></a>

### run_hooks/0 ###


<pre><code>
run_hooks() -&gt; ok
</code></pre>
<br />

Execute all setup hooks for current mode in order.
<a name="run_hooks-1"></a>

### run_hooks/1 ###


<pre><code>
run_hooks(Apps::Applications) -&gt; ok
</code></pre>
<br />

Execute setup hooks for current mode in `Applications` in order.
<a name="run_hooks-2"></a>

### run_hooks/2 ###


<pre><code>
run_hooks(Mode, Apps::Applications) -&gt; ok
</code></pre>
<br />

Execute setup hooks for `Mode` in `Applications` in order
<a name="start-2"></a>

### start/2 ###


<pre><code>
start(X1::Type, Args) -&gt; {ok, pid()}
</code></pre>
<br />

Application start function.
<a name="stop-1"></a>

### stop/1 ###


<pre><code>
stop(X1::State) -&gt; ok
</code></pre>
<br />

Application stop function
end

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
Currently, only the log directory is verified.
