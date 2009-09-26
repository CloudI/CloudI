%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

{application, cloud_data_pgsql, 
  [{description, "Cloud Postgres Client Interface"},
   {vsn, "0.1.0"},
   {modules, [
        cloud_data_pgsql,
        cloud_data_interface]},
   {registered,[]},
   {applications, [epgsql]},
   %{applications, [pgsql]},
   {start_phases, []}]}.
