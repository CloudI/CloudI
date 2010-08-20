%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

{application, cloud_stdlib, 
  [{description, "Cloud Standard Library Extensions"},
   {vsn, "0.0.6"},
   {modules, [
        abstract_code,
        dynamic_compile,
        immediate_gc,
        lists_extensions,
        math_extensions,
        monitor_link,
        nd_index,
        proplists_extensions,
        time_extensions,
        trie,
        tuple_extensions,
        string_extensions
        ]},
   {registered,[]},
   {applications, [
        stdlib,
        kernel
        ]},
   {start_phases, []}]}.

