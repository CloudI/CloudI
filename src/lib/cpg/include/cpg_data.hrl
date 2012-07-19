%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-record(cpg_data,
    {
        local_count = 0,
        local = [],
        remote_count = 0,
        remote = []
    }).

-record(cpg_data_pid,
    {
        pid,
        monitor
    }).
