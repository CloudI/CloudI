%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

{application, hello_world2,
  [{description, "Hello World Example Application"},
   {vsn, "1.2.5"},
   {modules, [
        hello_world2]},
   {registered, []},
   {applications, [
        cloudi_core,
        stdlib,
        kernel]}]}.
