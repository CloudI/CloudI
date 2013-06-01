%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

% CPG constants for changing process group functionality

% only used if the scope application env is an empty list
-define(DEFAULT_SCOPE, cpg_default_scope).

% use dict if you don't want to be limited to string (list of integer) keys
% (if not using trie, don't define GROUP_NAME_PATTERN_MATCHING)
-define(GROUP_STORAGE, trie). % dict or any dict-interface module

% group name pattern matching, i.e.,
% should the strings supplied to "get_" functions be matched
% with group names stored as patterns with the "*" wildcard character
% when finding the proper process group?
% (trie must be the GROUP_STORAGE when this is defined)
-define(GROUP_NAME_PATTERN_MATCHING, undefined).

% group names will only be joined/left with local pids
% so that global locking is unnecessary
-define(GROUP_NAME_WITH_LOCAL_PIDS_ONLY, undefined).

% define if you want to use ETS for less contention on a CPG scope process
%-define(CPG_ETS_CACHE, cpg_state).

