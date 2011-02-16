/*
 * ------------------------------------------------------------------
 * Erlang bindings for ZeroMQ.
 * ------------------------------------------------------------------
 * Copyright (c) 2010 Dhammika Pathirana and Serge Aleynikov
 * <dhammika@gmail.com> wrote original C code, copyright disclaimed.
 * <saleyn@gmail.com> C++ rewrite, bug fixes and many enhancements 
 * to the driver to support non-blocking I/O.
 * ------------------------------------------------------------------
 * See ../LICENSE for license details
 * ------------------------------------------------------------------
 */

#include <erl_driver.h>
#include <ei.h>
#include <map>
#include <set>

/* Erlang driver commands. */
enum driver_commands {
      ZMQ_INIT = 1
    , ZMQ_TERM
    , ZMQ_SOCKET
    , ZMQ_CLOSE
    , ZMQ_SETSOCKOPT
    , ZMQ_GETSOCKOPT
    , ZMQ_BIND
    , ZMQ_CONNECT
    , ZMQ_SEND
    , ZMQ_RECV
    , ZMQ_FLUSH = 255
};

// Erlang driver socket options
enum driver_sockopts {
      ZMQ_ACTIVE = 255
};

// Provides auto-cleanup
struct msg_t: public zmq_msg_t {
    msg_t()  { zmq_msg_init(this);  }
    ~msg_t() { zmq_msg_close(this); }
};

typedef void* zmq_socket_t;
typedef void* zmq_app_thread_t;

// Structure encapsulating information about a single 0MQ socket
// managed by the driver.  The driver maintains a doubly linked 
// list of these structures where each 0MQ socket is onwed by 
// one Erlang process ("owner" member).
struct zmq_sock_info {
    zmq_socket_t   socket;      // 0MQ socket handle
    uint32_t       idx;         // index of socket passed to Erlang process
    ErlDrvTermData owner;       // Erlang owner pid of this socket
    int            fd;          // Signaling fd for this socket
    ErlDrvTermData in_caller;   // Caller's pid of the last recv() command in passive mode
    zmq_msg_t      out_msg;     // Pending message to be written to 0MQ socket
    int            out_flags;   // Send flags for the pending message
    ErlDrvTermData out_caller;  // Caller's pid of the last send() command
                                // if it resulted in EAGAIN error.
    bool           active_mode; // true  - messages are delivered to owner
                                // false - owner must explicitely call recv()
    zmq_sock_info* prev;        // Pointer to prev socket info structure
    zmq_sock_info* next;        // Pointer to next socket info structure

    zmq_sock_info(zmq_socket_t _s, uint32_t _idx, ErlDrvTermData _owner, int _sig_fd)
        : socket(_s), idx(_idx), owner(_owner), fd(_sig_fd), in_caller(0)
        , out_flags(0), out_caller(0), active_mode(true), prev(NULL), next(NULL)
    {
        zmq_msg_init(&out_msg);
    }

    ~zmq_sock_info() {
        if (out_caller != 0) zmq_msg_close(&out_msg);
        if (socket) zmq_close(socket);
    }

    // Delete current element from the linked list
    void unlink() {
        if (prev) prev->next = next;
        if (next) next->prev = prev;
    }

    static void* operator new    (size_t sz) { return driver_alloc(sz); }
    static void  operator delete (void* p)   { driver_free(p); }
};

typedef std::set<zmq_sock_info*> zmq_sock_set_t;

// Maintains a set of sockets managed by a monitored Erlang pid
struct monitor_sockets_t {
    ErlDrvMonitor  monitor;
    zmq_sock_set_t sockets;
};

typedef std::map<uint32_t, zmq_sock_info*>          zmq_idx_socket_map_t;
typedef std::map<zmq_socket_t, zmq_sock_info*>      zmq_socket_idx_map_t;
typedef std::map<ErlDrvTermData, monitor_sockets_t> zmq_pid_sockets_map_t;
typedef std::map<int, zmq_sock_set_t>               zmq_fd_sockets_map_t;

// Driver state structure
struct zmq_drv_t {
    ErlDrvPort                  port;
    //ErlDrvTermData              owner;
    void*                       zmq_context;
    // Linked list of all 0MQ socket structures managed by driver
    zmq_sock_info*              zmq_sock_infos;
    // Maps <socket index> -> <0MQ socket structure>
    zmq_idx_socket_map_t        zmq_sockets;
    // Maps <0MQ socket handle> -> <0MQ socket structure>
    zmq_socket_idx_map_t        zmq_idxs;
    // Maps <Erlang pid> -> list of 0MQ socket structs owned by Erlang pid
    zmq_pid_sockets_map_t       zmq_pid_sockets;
    // Maps <thread's signaling fd> -> list of 0MQ socket structs managed by signaler
    zmq_fd_sockets_map_t        zmq_fd_sockets;
    // Current socket struct index
    uint32_t                    zmq_socket_count;

    zmq_drv_t(ErlDrvPort _port) 
        : port(_port), zmq_context(NULL)
        , zmq_sock_infos(NULL), zmq_socket_count(0)
    {
        //owner = driver_connected(_port);
    }

    ~zmq_drv_t();

    void            add_socket(zmq_sock_info* sock);
    int             del_socket(uint32_t idx);
    uint32_t        get_socket_idx(zmq_socket_t sock) const;
    zmq_sock_info*  get_socket_info(uint32_t idx);
    zmq_socket_t    get_zmq_socket(uint32_t idx) const;

    static void* operator new    (size_t sz) { return driver_alloc(sz); }
    static void  operator delete (void* p)   { driver_free(p); }

};

/* Forward declarations */
static int  zmqdrv_driver_init(void);
static ErlDrvData zmqdrv_start(ErlDrvPort port, char* cmd);
static void zmqdrv_stop(ErlDrvData handle);
static void zmqdrv_ready_input(ErlDrvData handle, ErlDrvEvent event);
static void zmqdrv_outputv(ErlDrvData handle, ErlIOVec *ev);
static void zmqdrv_process_exit(ErlDrvData handle, ErlDrvMonitor* monitor);
static void zmqdrv_socket_error(zmq_drv_t *drv, ErlDrvTermData pid, uint32_t idx, int err);
static void zmqdrv_error(zmq_drv_t *zmq_drv, const char *errstr);
static void zmqdrv_error_code(zmq_drv_t *zmq_drv, int err);
static void zmqdrv_ok(zmq_drv_t *zmq_drv);
static void zmqdrv_binary_ok(zmq_drv_t *zmq_drv, void *data, size_t size);
static void zmqdrv_init(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_term(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_socket(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_close(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_bind(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_connect(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_send(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_recv(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_setsockopt(zmq_drv_t *zmq_drv, ErlIOVec *ev);
static void zmqdrv_getsockopt(zmq_drv_t *zmq_drv, ErlIOVec *ev);

static ErlDrvTermData am_zok;
static ErlDrvTermData am_error;
static ErlDrvTermData am_eagain;
static ErlDrvTermData am_zmq;
static ErlDrvTermData am_msg;
static ErlDrvTermData am_true;
static ErlDrvTermData am_false;

