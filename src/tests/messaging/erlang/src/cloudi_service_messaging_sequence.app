%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

{application, cloudi_service_messaging_sequence,
  [{description, "CloudI Test Messaging Erlang Application"},
   {vsn, "1.7.1"},
   {modules,
    [cloudi_service_messaging_sequence1,
     cloudi_service_messaging_sequence2,
     cloudi_service_messaging_sequence3,
     cloudi_service_messaging_sequence4]},
   {applications,
    [cloudi_core,
     stdlib,
     kernel]}]}.
