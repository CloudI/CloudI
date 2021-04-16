This examples shows how to use and configure `hut` with `rebar3` as the main
build system.

Specifically the naive approach taken in `rebar.config.script` ensures that
`hut` configuration is passed down to all dependencies as well. This ensures
that dependencies which use `hut` too, are configured properly.
