%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

-record(cpg_data,
    {
        % The same pid can be duplicated for each join.
        % All pid lists are ordered from newest(head) to oldest(tail), i.e.,
        % when the pid was last seen, not the actual age of the pid.
        local_count = 0 :: non_neg_integer(),
        local = [] :: list(pid()),
        remote_count = 0 :: non_neg_integer(),
        remote = [] :: list(pid()),
        history = [] :: list(pid())
    }).

