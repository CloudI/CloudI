%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-record(cpg_data,
    {
        local_count = 0,
        local = [],
        remote_count = 0,
        remote = [],
        history = []
    }).

-record(cpg_data_pid,
    {
        pid,
        monitor
    }).
