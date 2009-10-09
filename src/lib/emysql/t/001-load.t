#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl sasl_error_logger false

main(_) ->
    etap:plan(4),
    etap_can:loaded_ok(mysql, "Module 'mysql' loaded."),
    etap_can:loaded_ok(mysql_auth, "Module 'mysql_auth' loaded."),
    etap_can:loaded_ok(mysql_conn, "Module 'mysql_conn' loaded."),
    etap_can:loaded_ok(mysql_recv, "Module 'mysql_recv' loaded."),
    etap:end_tests().
