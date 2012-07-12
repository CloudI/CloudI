% list_pg constants for changing process group functionality

% service name pattern matching, i.e.,
% should the strings supplied to subscribe/unsubscribe be matched as patterns
% with the "*" wildcard character when routing service name messages?
% (service name message sends do not use the wildcard character)
-define(GROUP_NAME_PATTERN_MATCHING, undefined).

% service names will only be subscribed/unsubscribed with local pids
% so global locking can be avoided
-define(GROUP_NAME_WITH_LOCAL_PIDS_ONLY, undefined).

