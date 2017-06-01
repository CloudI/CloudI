%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

-record(process_info,
    {
        memory = undefined :: non_neg_integer() | undefined,
        message_queue_len = undefined :: non_neg_integer() | undefined,
        reductions = undefined :: non_neg_integer() | undefined,
        reductions_now = undefined :: non_neg_integer() | undefined
    }).

