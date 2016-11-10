%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

-record(cpg_data_pid,
    {
        pid :: pid(),
        monitor :: reference()
    }).

-record(cpg_data,
    {
        local_count = 0 :: non_neg_integer(),
        local = [] :: list(#cpg_data_pid{}),
        remote_count = 0 :: non_neg_integer(),
        remote = [] :: list(#cpg_data_pid{}),
        history = [] :: list(pid()) % every pid join (pid is duplicated)
                                    % ordered from newest(head) to oldest(tail)
    }).

