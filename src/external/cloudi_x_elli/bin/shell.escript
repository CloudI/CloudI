#!/usr/bin/env escript

main(_) ->
    application:load(elli),
    {ok, Mods} = application:get_key(elli, modules),
    ok = code:ensure_modules_loaded(Mods).
