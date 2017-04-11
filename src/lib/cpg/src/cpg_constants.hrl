%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

% CPG constants for changing process group functionality

% only used if the scope application env is an empty list
-define(DEFAULT_SCOPE, cpg_default_scope).

% how long to wait for remote pid monitor deaths before sending a list of them
% (within cpg_node_monitor)
-define(MONITORS_ACCUMULATE_DELAY, 0). % milliseconds

