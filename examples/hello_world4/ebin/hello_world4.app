%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

{application, hello_world4,
  [{description, "Hello World Example Application"},
   {vsn, "1.6.0"},
   {modules, [
        hello_world4]},
   {registered, []},
   {applications, [
        cloudi_core,
        stdlib,
        kernel]}]}.
