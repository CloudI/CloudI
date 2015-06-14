%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

% CPG constants for changing process group functionality

% only used if the scope application env is an empty list
-define(DEFAULT_SCOPE, cpg_default_scope).

% group names will only be joined/left with local pids
% so that global locking is unnecessary
-define(GROUP_NAME_WITH_LOCAL_PIDS_ONLY, true).

