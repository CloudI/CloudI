#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(2),
    etap_can:loaded_ok(ecouchdb, "Module 'ecouchdb' loaded"),
    etap_can:can_ok(ecouchdb, server_info),
    etap:end_tests(),
    ok.
