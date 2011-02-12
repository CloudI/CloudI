
% interval at which asynchronous messages are checked
-define(RECV_ASYNC_INTERVAL, 500). % milliseconds

% interval at which asynchronous messages are sent
-define(SEND_ASYNC_INTERVAL, 500). % milliseconds

% interval at which synchronous messages are sent
-define(SEND_SYNC_INTERVAL, 500). % milliseconds

% interval at which synchronous forwarded messages are sent
-define(FORWARD_SYNC_INTERVAL, 500). % milliseconds

% interval at which asynchronous forwarded messages are sent
-define(FORWARD_ASYNC_INTERVAL, 500). % milliseconds

% maximum possible time for a process death to remove process group membership
% when using a slow refresh (fast refresh is immediate).  slow refresh is
% used when a process is mainly communicating with long-lived processes
% (and fast refresh is used when mainly communicating with
%  short-lived processes).
-define(DEST_REFRESH_SLOW, 300000). % milliseconds

% blocking operations must decrement the timeout to make sure timeouts
% have time to unravel all synchronous calls
-define(TIMEOUT_DELTA, 100). % milliseconds


%unused
-define(DATABASE_PREFIX, "/db/").

-define(LOCALHOST_MTU, 16436).

% maximum time an external cloudi process can take to connect
-define(CONNECT_TIMEOUT, 5000).

