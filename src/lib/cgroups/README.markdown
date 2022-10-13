Erlang cgroups interface
========================

An interface for cgroups manipulation that handles cgroup version details
(i.e., differences between [v1](https://www.kernel.org/doc/Documentation/cgroup-v1/cgroups.txt) and [v2](https://github.com/torvalds/linux/blob/master/Documentation/admin-guide/cgroup-v2.rst))
and provides safe usage of the cgroups filesystem mount.

Build
-----

    rebar compile

Example
-------

Update or create the cgroup "group1/nested1" with the OS pid 19368,
then delete the cgroup path after moving the OS pid back to the root cgroup.

    $ erl -pz ebin
    1> application:start(cgroups).
    2> OSPid0 = 19368.
    3> CGroupPath = "group1/nested1".
    4> {ok, CGroups} = cgroups:new().
    5> MemoryLimit = case cgroups:version(CGroups) of 1 -> "memory.limit_in_bytes"; 2 -> "memory.high" end.
    6> cgroups:update_or_create(CGroupPath,
                                [OSPid0],
                                [{MemoryLimit, "10000000"}],
                                CGroups).
    7> cgroups:update("", [OSPid0], [], CGroups).
    8> cgroups:delete_recursive(CGroupPath, CGroups).
    9> cgroups:destroy(CGroups).


Troubleshooting
---------------

**`cgroups:update/4` and `cgroups:update_or_create/4` Errors:**
* Either root execution of beam.smp or
  `setcap cap_sys_admin=+ep /path/to/beam.smp` before execution is required
  for most usage of cgroups
* A Linux/systemd setup may have the control group setup mode set to `hybrid`
  due to it being the [recommended default](https://github.com/systemd/systemd/blob/v239/NEWS#L1365)
  for distributions.  However, that mode blocks the use of cgroup controllers
  (the file `/sys/fs/cgroup/unified/cgroup.controllers` is empty)
  because the cgroup controllers can only be [mounted in one hierarchy](https://www.kernel.org/doc/html/latest/admin-guide/cgroup-v2.html#mounting)
  (v1 or v2).  If cgroup v1 should be used, the Linux kernel argument
  `systemd.legacy_systemd_cgroup_controller=1` can be used.
  If cgroup v2 should be used, the Linux kernel argument
  `systemd.unified_cgroup_hierarchy=1` can be used
  (with systemd >= `v226` and kernel >= `4.2`) or
  `cgroup_no_v1=all` can be used (with kernel >= `4.6`).

Author
------

Michael Truog (mjtruog at protonmail dot com)

License
-------

MIT License

