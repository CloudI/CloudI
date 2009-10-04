%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

{application, cloud_interface,
  [{description, "Cloud Interface Application"},
   {vsn, "0.0.4"},
   {modules, [
        cloud_work_interface,
        cloud_data_interface
        ]},
   {registered, []},
   {applications, []},
   {start_phases, []}]}.
