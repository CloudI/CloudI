-define('ZMQ_PAIR',         0).
-define('ZMQ_PUB',          1).
-define('ZMQ_SUB',          2).
-define('ZMQ_REQ',          3).
-define('ZMQ_REP',          4).
-define('ZMQ_XREQ',         5).
-define('ZMQ_XREP',         6).
-define('ZMQ_PULL',         7).
-define('ZMQ_PUSH',         8).
-define('ZMQ_XPUB',         9).
-define('ZMQ_XSUB',        10).

% ZMQ socket options.
-define('ZMQ_HWM',                1).
-define('ZMQ_SWAP',               3).
-define('ZMQ_AFFINITY',           4).
-define('ZMQ_IDENTITY',           5).
-define('ZMQ_SUBSCRIBE',          6).
-define('ZMQ_UNSUBSCRIBE',        7).
-define('ZMQ_RATE',               8).
-define('ZMQ_RECOVERY_IVL',       9).
-define('ZMQ_MCAST_LOOP',        10).
-define('ZMQ_SNDBUF',            11).
-define('ZMQ_RCVBUF',            12).
-define('ZMQ_RCVMORE',           13).
-define('ZMQ_FD',                14).
-define('ZMQ_EVENTS',            15).
-define('ZMQ_TYPE',              16).
-define('ZMQ_LINGER',            17).
-define('ZMQ_RECONNECT_IVL',     18).
-define('ZMQ_BACKLOG',           19).
-define('ZMQ_RECOVERY_IVL_MSEC', 20).
-define('ZMQ_RECONNECT_IVL_MAX', 21).

% ZMQ send/recv flags
-define('ZMQ_NOBLOCK',    1).
-define('ZMQ_SNDMORE',    2).

%% Types

%% @type erlzmq_socket_type() = pair | pub | sub | req | rep | xreq | xrep |
%% pull | push | xpub | xsub.
%% Possible types for an erlzmq socket.<br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_socket">zmq_socket</a></i>
-type erlzmq_socket_type() :: pair | pub | sub | req | rep | xreq | xrep |
                            pull | push | xpub | xsub.

%% @type erlzmq_endpoint() = string().
%% The endpoint argument is a string consisting of two parts:
%% <b>transport://address</b><br />
%% The following transports are defined:
%% <b>inproc</b>, <b>ipc</b>, <b>tcp</b>, <b>pgm</b>, <b>epgm</b>.<br />
%% The meaning of address is transport specific.<br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_bind">zmq_bind</a> or
%% <a href="http://api.zeromq.org/master:zmq_connect">zmq_connect</a></i>
-type erlzmq_endpoint() :: string().

%% @type errno() = eperm | enoent | srch | eintr | eio | enxio | ebad |
%% echild | edeadlk | enomem | eacces | efault | enotblk | ebusy | eexist |
%% exdev | enodev | enotdir | eisdir | einval | enfile | emfile | enotty |
%% etxtbsy | efbig | enospc | espipe | erofs | emlink | epipe | eagain |
%% einprogress | ealready | enotsock | edestaddrreq | emsgsize |
%% eprototype | enoprotoopt | eprotonosupport | esocktnosupport |
%% enotsup | epfnosupport | eafnosupport | eaddrinuse | eaddrnotavail |
%% enetdown | enetunreach | enetreset | econnaborted | econnreset |
%% enobufs | eisconn | enotconn | eshutdown | etoomanyrefs | etimedout |
%% econnrefused | eloop | enametoolong.
%% Standard error atoms.
-type errno() :: eperm | enoent | srch | eintr | eio | enxio | ebad |
                 echild | edeadlk | enomem | eacces | efault | enotblk |
                 ebusy | eexist | exdev | enodev | enotdir | eisdir |
                 einval | enfile | emfile | enotty | etxtbsy | efbig |
                 enospc | espipe | erofs | emlink | epipe | eagain |
                 einprogress | ealready | enotsock | edestaddrreq |
                 emsgsize | eprototype | enoprotoopt | eprotonosupport |
                 esocktnosupport | enotsup | epfnosupport | eafnosupport |
                 eaddrinuse | eaddrnotavail | enetdown | enetunreach |
                 enetreset | econnaborted | econnreset | enobufs | eisconn |
                 enotconn | eshutdown | etoomanyrefs |
                 etimedout | econnrefused | eloop | enametoolong.

%% @type erlzmq_error_type() = enotsup | eprotonosupport | enobufs |
%% enetdown | eaddrinuse | eaddnotavail | econnrefused | einprogress |
%% efsm | enocompatproto | eterm | emthread | errno() |
%% {unknown, integer()}.
%% Possible error types.
-type erlzmq_error_type() :: enotsup | eprotonosupport | enobufs | enetdown |
                           eaddrinuse | eaddnotavail | econnrefused | 
                           einprogress | efsm | enocompatproto | eterm |
                           emthread | errno() | {unknown, integer()}.

%% @type erlzmq_error() = {error, erlzmq_error_type()}.
%% Error tuples returned by most API functions.
-type erlzmq_error() :: {error, erlzmq_error_type()}.

%% @type erlzmq_data() = iolist().
%% Data to be sent with {@link erlzmq:send/3. send/3} or received with
%% {@link erlzmq:recv/2. recv/2}
-type erlzmq_data() :: iolist().

%% @type erlzmq_context() = binary().
%% An opaque handle to an erlzmq context.
-opaque erlzmq_context() :: binary().

%% @type erlzmq_socket() = binary().
%% An opaque handle to an erlzmq socket.
-opaque erlzmq_socket() :: binary().

%% @type erlzmq_send_recv_flag() = noblock | sndmore | recvmore | {timeout, timeout()}.
%% The individual flags to use with {@link erlzmq:send/3. send/3}
%% and {@link erlzmq:recv/2. recv/2}.<br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_send">zmq_send</a> or
%% <a href="http://api.zeromq.org/master:zmq_recv">zmq_recv</a></i>
-type erlzmq_send_recv_flag() :: noblock | sndmore | recvmore | {timeout, timeout()}.

%% @type erlzmq_send_recv_flags() = list(erlzmq_send_recv_flag()).
%% A list of flags to use with {@link ezqm:send/3. send/3} and
%% {@link erlzmq:recv/2. recv/2}
-type erlzmq_send_recv_flags() :: list(erlzmq_send_recv_flag()).

%% @type erlzmq_sockopt() = hwm | swap | affinity | identity | subscribe |
%% unsubscribe | rate | recovery_ivl | mcast_loop | sndbuf | rcvbuf |
%% rcvmore | fd | events | linger | reconnect_ivl | backlog |
%% recovery_ivl_msec | reconnect_ivl_max.
%% Available options for {@link erlzmq:setsockopt/3. setsockopt/3}
%% and {@link erlzmq:getsockopt/2. getsockopt/2}.<br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_setsockopt">zmq_setsockopt</a>
%% and <a href="http://api.zeromq.org/master:zmq_getsockopt">zmq_getsockopt</a></i>
-type erlzmq_sockopt() :: hwm | swap | affinity | identity | subscribe |
                        unsubscribe | rate | recovery_ivl | mcast_loop |
                        sndbuf | rcvbuf | rcvmore | fd | events | linger |
                        reconnect_ivl | backlog | recovery_ivl_msec |
                        reconnect_ivl_max.

%% @type erlzmq_sockopt_value() = integer() | iolist().
%% Possible option values for {@link erlzmq:setsockopt/3. setsockopt/3}.
-type erlzmq_sockopt_value() :: integer() | iolist().

