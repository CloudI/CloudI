%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

{application, hello_world1,
  [{description, "Hello World Example Application"},
   {vsn, "1.2.2"},
   {modules, [
        hello_world1]},
   {applications, [
        stdlib,
        kernel]}]}.
