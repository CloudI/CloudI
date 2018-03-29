Erlang cgroups interface
========================

An interface for cgroups manipulation that handles cgroup version details
(i.e., differences between v1 and v2) and provides safe usage of the
cgroups filesystem mount.

**Needs testing with [v2 cgroups](https://github.com/okeuday/cgroups/issues/1)**

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
    5> cgroups:update_or_create(CGroupPath,
                                [OSPid0],
                                [{"memory.limit_in_bytes", "10000000"},
                                 {"memory.memsw.limit_in_bytes", "12000000"}],
                                CGroups).
    6> cgroups:update("", [OSPid0], [], CGroups).
    7> cgroups:delete_recursive(CGroupPath, CGroups).
    8> cgroups:destroy(CGroups).

Author
------

Michael Truog (mjtruog at protonmail dot com)

License
-------

MIT License

