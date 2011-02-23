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
 */

#include <stdio.h>
#include <string.h>
#include <zmq.h>
#include <ctype.h>
#include <sstream>
#include <assert.h>
#include "zmq_drv.h"

#ifdef ZMQDRV_DEBUG
#define zmqdrv_fprintf(...)   fprintf(stderr, __VA_ARGS__)
#else
#define zmqdrv_fprintf(...)
#endif
#define INIT_ATOM(NAME) am_ ## NAME = (ErlDrvTermData)driver_mk_atom((char*)#NAME)

/* Callbacks */
static ErlDrvEntry zmq_driver_entry = {
    zmqdrv_driver_init,                 /* init */
    zmqdrv_start,                       /* startup (defined below) */
    zmqdrv_stop,                        /* shutdown (defined below) */
    NULL,                               /* output */
    zmqdrv_ready_input,                 /* ready_input */
    NULL,                               /* ready_output */
    (char*)"zmq_drv",                   /* driver name */
    NULL,                               /* finish */
    NULL,                               /* handle */
    NULL,                               /* control */
    NULL,                               /* timeout */
    zmqdrv_outputv,                     /* outputv, binary output */
    NULL,                               /* ready_async */
    NULL,                               /* flush */
    NULL,                               /* call */
    NULL,                               /* event */
    ERL_DRV_EXTENDED_MARKER,            /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,     /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,     /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING,      /* ERL_DRV_FLAGs */
    NULL,                               /* handle2 (reserved */
    zmqdrv_process_exit,                /* process_exit */
    NULL                                /* stop_select */
};

/* Driver internal, C hook to driver API. */
extern "C" DRIVER_INIT(zmq_drv)
{
    return &zmq_driver_entry;
}

zmq_drv_t::~zmq_drv_t()
{
    for (zmq_pid_sockets_map_t::iterator it = zmq_pid_sockets.begin();
            it != zmq_pid_sockets.end(); ++it)
        driver_demonitor_process(port, &it->second.monitor);

    for (zmq_fd_sockets_map_t::iterator it = zmq_fd_sockets.begin();
            it != zmq_fd_sockets.end(); ++it)
        driver_select(port, (ErlDrvEvent)it->first, ERL_DRV_READ, 0);

    for (zmq_sock_info *it=zmq_sock_infos, *next=(it ? it->next : NULL); it; it = next) {
        next = it->next;
        delete (&*it);
    }
    zmq_sockets.clear();
    zmq_idxs.clear();
    zmq_pid_sockets.clear();
    zmq_fd_sockets.clear();

    if (zmq_context) {
        zmqdrv_fprintf("calling zmq_term(context) ...\r\n");
        zmq_term(zmq_context);
        zmqdrv_fprintf("terminated zmq context\r\n");
    }
}

void zmq_drv_t::add_socket(zmq_sock_info* s)
{
    // Insert the new socket info to the head of the list
    if (zmq_sock_infos) zmq_sock_infos->prev = s;
    s->next        = zmq_sock_infos;
    zmq_sock_infos = s;

    // Update map: idx -> socket
    zmq_sockets[s->idx] = s;
    // Update map: socket -> idx
    zmq_idxs[s->socket] = s;
    {
        // Update map: pid -> sockets
        zmq_pid_sockets_map_t::iterator it = zmq_pid_sockets.find(s->owner);
        if (it != zmq_pid_sockets.end())
            it->second.sockets.insert(s);
        else {
            monitor_sockets_t ms;
            driver_monitor_process(port, s->owner, &ms.monitor);
            ms.sockets.insert(s);
            zmq_pid_sockets[s->owner] = ms;
        }
    }
    {
        // Update map: fd -> sockets
        zmq_fd_sockets_map_t::iterator it = zmq_fd_sockets.find(s->fd);
        if (it != zmq_fd_sockets.end())
            it->second.insert(s);
        else {
            zmq_sock_set_t set;
            set.insert(s);
            zmq_fd_sockets[s->fd] = set;
            driver_select(port, (ErlDrvEvent)s->fd, ERL_DRV_READ, 1);
            zmqdrv_fprintf("registered sig_fd(%d) with VM\r\n", s->fd);
        }
    }
}

int zmq_drv_t::del_socket(uint32_t idx)
{
    zmq_sock_info* s;
    int ret = -1;

    zmq_idx_socket_map_t::iterator it = zmq_sockets.find(idx);
    if (it == zmq_sockets.end()) {
        zmqdrv_fprintf("warning: socket info not found for idx %d\r\n", idx);
        return ret;
    }

    s = it->second;
    s->unlink();
    if (s == zmq_sock_infos)
        zmq_sock_infos = s->next;

    zmq_sockets.erase(idx);
    zmq_idxs.erase(s->socket);

    {
        // Remove the socket from a list of sockets owned by pid.
        // If this was the last socket, demonitor pid.
        zmq_pid_sockets_map_t::iterator it = zmq_pid_sockets.find(s->owner);
        if (it != zmq_pid_sockets.end()) {
            it->second.sockets.erase(s);
            if (it->second.sockets.empty()) {
                driver_demonitor_process(port, &it->second.monitor);
                zmq_pid_sockets.erase(it);
            }
        }
    }
    {
        zmq_fd_sockets_map_t::iterator it = zmq_fd_sockets.find(s->fd);
        if (it != zmq_fd_sockets.end()) {
            it->second.erase(s);
            if (it->second.empty()) {
                zmq_fd_sockets.erase(it->first);
                driver_select(port, (ErlDrvEvent)it->first, ERL_DRV_READ, 0);
                zmqdrv_fprintf("unregistered sig_fd(%d) with VM\r\n", it->first);
            }
        }
    }

    delete s;
    return 0;
}

uint32_t zmq_drv_t::get_socket_idx(zmq_socket_t sock) const
{
    zmq_socket_idx_map_t::const_iterator it = zmq_idxs.find(sock);
    return it == zmq_idxs.end() ? 0 : it->second->idx;
}

zmq_sock_info* zmq_drv_t::get_socket_info(uint32_t idx)
{
    zmq_idx_socket_map_t::const_iterator it = zmq_sockets.find(idx);
    return it == zmq_sockets.end() ? NULL : it->second;
}

zmq_socket_t zmq_drv_t::get_zmq_socket(uint32_t idx) const
{
    zmq_idx_socket_map_t::const_iterator it = zmq_sockets.find(idx);
    return it == zmq_sockets.end() ? NULL : it->second->socket;
}

static ErlDrvTermData error_atom(int err)
{
    char errstr[128];
    char* s;
    char* t;

    switch (err) {
        case ENOTSUP:           strcpy(errstr, "enotsup");          break;
        case EPROTONOSUPPORT:   strcpy(errstr, "eprotonosupport");  break;
        case ENOBUFS:           strcpy(errstr, "enobufs");          break;
        case ENETDOWN:          strcpy(errstr, "enetdown");         break;
        case EADDRINUSE:        strcpy(errstr, "eaddrinuse");       break;
        case EADDRNOTAVAIL:     strcpy(errstr, "eaddrnotavail");    break;
        case ECONNREFUSED:      strcpy(errstr, "econnrefused");     break;
        case EINPROGRESS:       strcpy(errstr, "einprogress");      break;
        case EFSM:              strcpy(errstr, "efsm");             break;
        case ENOCOMPATPROTO:    strcpy(errstr, "enocompatproto");   break;
        default:
            for (s = erl_errno_id(err), t = errstr; *s; s++, t++)
                *t = tolower(*s);
            *t = '\0';
    }
    return driver_mk_atom(errstr);
}

static void
zmq_free_binary(void* /*data*/, void* hint)
{
    ErlDrvBinary* bin = (ErlDrvBinary*)hint;
    driver_free_binary(bin);
}

static void
zmqdrv_socket_error(zmq_drv_t *drv, ErlDrvTermData pid, uint32_t idx, int err) {
    // Return {zmq, Socket::integer(), {error, Reason::atom()}}
    ErlDrvTermData spec[] =
        {ERL_DRV_ATOM,  am_zmq,
            ERL_DRV_UINT,  idx,
            ERL_DRV_ATOM,  am_error,
            ERL_DRV_ATOM,  error_atom(err),
            ERL_DRV_TUPLE, 2,
            ERL_DRV_TUPLE, 3};
    driver_send_term(drv->port, pid, spec, sizeof(spec)/sizeof(spec[0]));
}

static void
zmqdrv_error(zmq_drv_t *drv, const char *errstr)
{
    ErlDrvTermData spec[] =
        {ERL_DRV_ATOM,   am_error,
         ERL_DRV_STRING, (ErlDrvTermData)errstr, strlen(errstr),
         ERL_DRV_TUPLE,  2};
    driver_send_term(drv->port, driver_caller(drv->port), spec, sizeof(spec)/sizeof(spec[0]));
}

static void
zmqdrv_error_code(zmq_drv_t *drv, int err)
{
    ErlDrvTermData spec[] =
        {ERL_DRV_ATOM, am_error,
         ERL_DRV_ATOM, error_atom(err),
         ERL_DRV_TUPLE,  2};
    driver_send_term(drv->port, driver_caller(drv->port), spec, sizeof(spec)/sizeof(spec[0]));
}

static void
zmqdrv_ok(zmq_drv_t *drv, ErlDrvTermData pid)
{
  ErlDrvTermData spec[] = {ERL_DRV_ATOM, am_zok};
    driver_send_term(drv->port, pid, spec, sizeof(spec)/sizeof(spec[0]));
}

static void
zmqdrv_ok(zmq_drv_t *drv)
{
    zmqdrv_ok(drv, driver_caller(drv->port));
}

static void
zmqdrv_binary_ok(zmq_drv_t *drv, ErlDrvTermData pid, void *data, size_t size)
{
    /* Copy payload. */
    ErlDrvTermData spec[] =
      {ERL_DRV_ATOM,   am_zok,
       ERL_DRV_BUF2BINARY, (ErlDrvTermData)data, size,
       ERL_DRV_TUPLE, 2};

    driver_send_term(drv->port, pid, spec, sizeof(spec)/sizeof(spec[0]));
}

static void
zmqdrv_binary_ok(zmq_drv_t *drv, void *data, size_t size) {
    zmqdrv_binary_ok(drv, driver_caller(drv->port), data, size);
}

//-------------------------------------------------------------------
// Driver callbacks
//-------------------------------------------------------------------

int zmqdrv_driver_init(void)
{
    INIT_ATOM(zok);
    INIT_ATOM(error);
    INIT_ATOM(eagain);
    INIT_ATOM(zmq);
    INIT_ATOM(msg);
    INIT_ATOM(true);
    INIT_ATOM(false);
    return 0;
}

/* Driver Start, called on port open. */
static ErlDrvData
zmqdrv_start(ErlDrvPort port, char* cmd)
{
    zmqdrv_fprintf("driver started by pid %ld\r\n", driver_connected(port));
    return reinterpret_cast<ErlDrvData>(new zmq_drv_t(port));
}

/* Driver Stop, called on port close. */
static void
zmqdrv_stop(ErlDrvData handle)
{
    delete reinterpret_cast<zmq_drv_t*>(handle);
    zmqdrv_fprintf("driver stopped by pid\r\n");
}

static void
zmqdrv_ready_input(ErlDrvData handle, ErlDrvEvent event)
{
    zmq_drv_t *drv = (zmq_drv_t *)handle;

    // Get 0MQ sockets managed by application thread's signaler
    // identified by "event" fd.
    zmq_fd_sockets_map_t::iterator it = drv->zmq_fd_sockets.find((long)event);

    zmqdrv_fprintf("input ready on [idx=%ld]\r\n", (long)event);

    assert(it != drv->zmq_fd_sockets.end());

    zmq_sock_set_t::iterator si = it->second.begin();

    assert(si != it->second.end());

    for (; si != it->second.end(); ++si) {
        zmq_socket_t   s     = (*si)->socket;
        uint32_t       idx   = (*si)->idx;
        ErlDrvTermData owner = (*si)->owner;
        int            rc    = 0;
        uint32_t       events;
        size_t         events_size = sizeof(events);

        zmq_getsockopt(s, ZMQ_EVENTS, &events, &events_size);

        while (((*si)->active_mode || (*si)->in_caller) && (events & ZMQ_POLLIN)) {
            msg_t msg;

            rc = zmq_recv(s, &msg, ZMQ_NOBLOCK);

            ErlDrvTermData pid = (*si)->active_mode ? owner : (*si)->in_caller;

            if (rc == -1) {
                if (zmq_errno() != EAGAIN) {
                    ErlDrvTermData spec[] =
                        {ERL_DRV_ATOM,  am_zmq,
                         ERL_DRV_UINT,  idx,
                         ERL_DRV_ATOM,  error_atom(zmq_errno()),
                         ERL_DRV_TUPLE, 2,
                         ERL_DRV_TUPLE, 3};
                    driver_send_term(drv->port, owner, spec, sizeof(spec)/sizeof(spec[0]));
                    (*si)->in_caller = 0;
                }
                break;
            }

            if ((*si)->active_mode) {
                // Send message {zmq, Socket, binary()} to the owner pid
                ErlDrvTermData spec[] =
                    {ERL_DRV_ATOM,  am_zmq,
                     ERL_DRV_UINT,  idx,
                     ERL_DRV_BUF2BINARY, (ErlDrvTermData)zmq_msg_data(&msg), zmq_msg_size(&msg),
                     ERL_DRV_TUPLE, 3};
                driver_send_term(drv->port, owner, spec, sizeof(spec)/sizeof(spec[0]));
            } else {
                // Return result {ok, binary()} to the waiting caller's pid
                ErlDrvTermData spec[] = 
                    {ERL_DRV_ATOM,   am_zok,
                     ERL_DRV_BUF2BINARY, (ErlDrvTermData)zmq_msg_data(&msg), zmq_msg_size(&msg),
                     ERL_DRV_TUPLE, 2};
                driver_send_term(drv->port, pid, spec, sizeof(spec)/sizeof(spec[0]));
                (*si)->in_caller = 0;
            }

            // FIXME: add error handling
            zmqdrv_fprintf("received %ld byte message relayed to pid %ld\r\n", zmq_msg_size(&msg), pid);
            zmq_getsockopt(s, ZMQ_EVENTS, &events, &events_size);
        }
    
        zmq_getsockopt(s, ZMQ_EVENTS, &events, &events_size);

        if ((*si)->out_caller != 0 && (events & ZMQ_POLLOUT)) {
            // There was a pending unwritten message on this socket.
            // Try to write it.  If the write succeeds/fails clear the ZMQ_POLLOUT
            // flag and notify the waiting caller of completion of operation.
            rc = zmq_send(s, &(*si)->out_msg, (*si)->out_flags | ZMQ_NOBLOCK);

            zmqdrv_fprintf("resending message %p (size=%ld) on socket %p (ret=%d)\r\n", 
                zmq_msg_data(&(*si)->out_msg), zmq_msg_size(&(*si)->out_msg), s, rc);

            if (rc == 0) {
                zmq_msg_close(&(*si)->out_msg);
                // Unblock the waiting caller's pid by returning result
                zmqdrv_ok(drv, (*si)->out_caller);
                (*si)->out_caller = 0;
            } else if (zmq_errno() != EAGAIN) {
                // Unblock the waiting caller's pid by returning result
                zmq_msg_close(&(*si)->out_msg);
                zmqdrv_socket_error(drv, (*si)->out_caller, idx, zmq_errno());
                (*si)->out_caller = 0;
            }
        }

        zmqdrv_fprintf("--> socket %p events=%d\r\n", s, events);
    }
}

// Called when an Erlang process owning sockets died.
// Perform cleanup of orphan sockets owned by pid.
static void 
zmqdrv_process_exit(ErlDrvData handle, ErlDrvMonitor* monitor)
{
    zmq_drv_t*     drv = (zmq_drv_t *)handle;
    ErlDrvTermData pid = driver_get_monitored_process(drv->port, monitor);

    zmqdrv_fprintf("detected death of %lu process\r\n", pid);

    driver_demonitor_process(drv->port, monitor);

    // Walk through the list of sockets and close the ones
    // owned by pid.
    zmq_pid_sockets_map_t::iterator it=drv->zmq_pid_sockets.find(pid);

    if (it != drv->zmq_pid_sockets.end()) {
        zmqdrv_fprintf("pid %lu has %lu sockets to be closed\r\n", pid, it->second.sockets.size());
        for(zmq_sock_set_t::iterator sit = it->second.sockets.begin();
            sit != it->second.sockets.end(); ++sit)
            drv->del_socket((*sit)->idx);
    }
}

/* Erlang command, called on binary input from VM. */
static void
zmqdrv_outputv(ErlDrvData handle, ErlIOVec *ev)
{
    zmq_drv_t*    drv  = (zmq_drv_t *)handle;
    ErlDrvBinary* data = ev->binv[1];
    unsigned char cmd  = data->orig_bytes[0]; // First byte is the command

    zmqdrv_fprintf("driver got command %d on thread %p\r\n", (int)cmd, erl_drv_thread_self());

    switch (cmd) {
        case ZMQ_INIT :
            zmqdrv_init(drv, ev);
            break;
        case ZMQ_TERM :
            zmqdrv_term(drv, ev);
            break;
        case ZMQ_SOCKET :
            zmqdrv_socket(drv, ev);
            break;
        case ZMQ_CLOSE :
            zmqdrv_close(drv, ev);
            break;
        case ZMQ_SETSOCKOPT :
            zmqdrv_setsockopt(drv, ev);
            break;
        case ZMQ_GETSOCKOPT :
            zmqdrv_getsockopt(drv, ev);
            break;
        case ZMQ_BIND :
            zmqdrv_bind(drv, ev);
            break;
        case ZMQ_CONNECT :
            zmqdrv_connect(drv, ev);
            break;
        case ZMQ_SEND :
            zmqdrv_send(drv, ev);
            break;
        case ZMQ_RECV :
            zmqdrv_recv(drv, ev);
            break;
        default :
            zmqdrv_error(drv, "Invalid driver command");
    }
}

static void
zmqdrv_init(zmq_drv_t *drv, ErlIOVec *ev)
{
    /* 
     * FIXME 
     * Use ei_decode_* to decode input from erlang VM.
     * This stuff is not documented anywhere, for now 
     * binary ErlIOVec is decoded by poking in iov struct.
     * 
     * Serge: Dhammika, ei_decode can only be used to decode
     * external binary format in the "output" callback function.
     * It's not suitable for using inside "outputv" body that
     * operates on I/O vectors unless you use term_to_binary/1
     * call to explicitely convert a term to external binary format.
     */

    uint32_t io_threads; 

    ErlDrvBinary* input = ev->binv[1];
    char* bytes = input->orig_bytes;
    io_threads  = ntohl(*(uint32_t *)(bytes + 1));

    zmqdrv_fprintf("iothreads = %u\r\n", io_threads);

    if (drv->zmq_context) {
        zmqdrv_error_code(drv, EBUSY);
        return;
    }
    
    drv->zmq_context = (void *)zmq_init(io_threads);

    if (!drv->zmq_context) {
        zmqdrv_error_code(drv, zmq_errno());
        return;
    }

    zmqdrv_ok(drv);
}

static void
zmqdrv_term(zmq_drv_t *drv, ErlIOVec *ev)
{
    if (!drv->zmq_context) {
        zmqdrv_error_code(drv, ENODEV);
        return;
    }

    zmqdrv_fprintf("calling zmq_term(context) ...\r\n");
    int rc = zmq_term(drv->zmq_context);
    zmqdrv_fprintf("terminated zmq context\r\n");

    if (rc < 0) {
        zmqdrv_error_code(drv, zmq_errno());
        return;
    }

    zmqdrv_ok(drv);
    drv->zmq_context = NULL;
}

static void
zmqdrv_socket(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary* bin   = ev->binv[1];
    char*         bytes = bin->orig_bytes;
    int           type  = *(bytes + 1);

    void* s = zmq_socket(drv->zmq_context, type);
    if (!s) {
        zmqdrv_error_code(drv, zmq_errno());
        return;
    }

    int sig_fd;
    size_t sig_size = sizeof(sig_fd);
    zmq_getsockopt(s, ZMQ_FD, &sig_fd, &sig_size);

    if (sig_fd < 0) {
        zmqdrv_error(drv, "Invalid signaler");
        return;
    }

    // Register a new socket handle in order to avoid
    // passing actual address of socket to Erlang.  This
    // way it's more safe and also portable between 32 and
    // 64 bit OS's.
    uint32_t n = ++drv->zmq_socket_count;

    zmq_sock_info* zsi = new zmq_sock_info(s, n, driver_caller(drv->port), sig_fd);
    if (!zsi) {
        driver_failure_posix(drv->port, ENOMEM);
        return;
    }

    drv->add_socket(zsi);

    zmqdrv_fprintf("socket %p [idx=%d] owner=%ld\r\n", s, n, zsi->owner);

    ErlDrvTermData spec[] = {ERL_DRV_ATOM,  am_zok,
                             ERL_DRV_UINT,  n,
                             ERL_DRV_TUPLE, 2};
    driver_send_term(drv->port, zsi->owner, spec, sizeof(spec)/sizeof(spec[0]));
}

static void
zmqdrv_close(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary* bin   = ev->binv[1];
    char*         bytes = bin->orig_bytes;
    uint32_t      idx   = ntohl(*(uint32_t*)(bytes+1));

    if (idx > drv->zmq_socket_count) {
        zmqdrv_error_code(drv, ENODEV);
        return;
    }

    int ret = drv->del_socket(idx);

    zmqdrv_fprintf("close [idx=%d] -> %d\r\n", idx, ret);

    if (ret < 0) {
        zmqdrv_error_code(drv, zmq_errno());
        return;
    }
    
    zmqdrv_ok(drv);
}

static void 
zmqdrv_setsockopt(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary*  bin   = ev->binv[1];
    char*          bytes = bin->orig_bytes;
    uint32_t       idx   = ntohl(*(uint32_t*)(bytes+1));
    zmq_sock_info* si    = drv->get_socket_info(idx);
    uint8_t        n     = *(uint8_t*)(bytes+sizeof(idx)+1);
    char*          p     = bytes + 1 + sizeof(idx) + 1;

    if (idx > drv->zmq_socket_count || !si) {
        zmqdrv_error_code(drv, ENODEV);
        return;
    }

    zmqdrv_fprintf("setsockopt %p (setting %d options)\r\n", si->socket, (int)n);

    for (uint8_t j=0; j < n; ++j) {
        unsigned char option = *p++;
        uint64_t optvallen   = *p++;
        void*    optval      = p;

        switch (option) {
            case ZMQ_HWM:           assert(optvallen == 8);  break;
            case ZMQ_SWAP:          assert(optvallen == 8);  break;
            case ZMQ_AFFINITY:      assert(optvallen == 8);  break;
            case ZMQ_IDENTITY:      assert(optvallen < 256); break;
            case ZMQ_SUBSCRIBE:     assert(optvallen < 256); break;
            case ZMQ_UNSUBSCRIBE:   assert(optvallen < 256); break;
            case ZMQ_RATE:          assert(optvallen == 8);  break;
            case ZMQ_RECOVERY_IVL:  assert(optvallen == 8);  break;
            case ZMQ_MCAST_LOOP:    assert(optvallen == 8);  break;
            case ZMQ_SNDBUF:        assert(optvallen == 8);  break;
            case ZMQ_RCVBUF:        assert(optvallen == 8);  break;
            case ZMQ_ACTIVE:        assert(optvallen == 1);  break;
        }

        zmqdrv_fprintf("setsockopt %p (%d)\r\n", si->socket, option);

        if (option == ZMQ_ACTIVE)
            si->active_mode = *(char*)optval;
        else if (zmq_setsockopt(si->socket, option, optval, optvallen) < 0) {
            zmqdrv_error_code(drv, zmq_errno());
            return;
        }

        p += optvallen;
    }

    zmqdrv_ok(drv);
}

static void 
zmqdrv_getsockopt(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary*  bin   = ev->binv[1];
    char*          bytes = bin->orig_bytes;
    uint32_t       idx   = ntohl(*(uint32_t*)(bytes+1));
    void*          s     = drv->get_zmq_socket(idx);
    uint32_t       opt   = ntohl (*(uint32_t*)(bytes+sizeof(idx)+1));

    if (opt == ZMQ_RCVMORE) {
        int64_t val;
        size_t valsz = sizeof (val);
        if (zmq_getsockopt (s, opt, &val, &valsz) < 0) {
            zmqdrv_error_code(drv, zmq_errno());
            return;
        }

        ErlDrvTermData spec[] = {
            ERL_DRV_ATOM,  am_zok,
            ERL_DRV_ATOM, (val ? am_true : am_false),
            ERL_DRV_TUPLE, 2};
        driver_send_term(drv->port, driver_caller(drv->port), spec, sizeof(spec)/sizeof(spec[0]));
        return;
    }

    zmqdrv_error(drv, "Not implemented");
}

static void
zmqdrv_bind(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary* bin   = ev->binv[1];
    char*         bytes = bin->orig_bytes;
    uint16_t      size  = bin->orig_size - 5;
    uint32_t      idx   = ntohl(*(uint32_t*)(bytes+1));
    void*         s     = drv->get_zmq_socket(idx);
    char          addr[512];

    if (size > sizeof(addr) - 1) {
        zmqdrv_error_code(drv, E2BIG);
        return;
    }

    memcpy(addr, bytes + 5, size);
    addr[size] = '\0';

    if (idx > drv->zmq_socket_count || !s) {
        zmqdrv_error_code(drv, ENODEV);
        return;
    } else if (addr[0] == '\0') {
        zmqdrv_error_code(drv, EINVAL);
        return;
    }

    if (zmq_bind(s, addr) < 0) {
        zmqdrv_error_code(drv, zmq_errno());
        return;
    }

    zmqdrv_ok(drv);
}

static void
zmqdrv_connect(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary* bin   = ev->binv[1];
    char*         bytes = bin->orig_bytes;
    uint32_t      idx   = ntohl(*(uint32_t*)(bytes+1));
    void*         s     = drv->get_zmq_socket(idx);
    uint16_t      size  = bin->orig_size - 5;
    char          addr[512];

    if (idx > drv->zmq_socket_count || !s) {
        zmqdrv_error_code(drv, ENODEV);
        return;
    }

    if (size > sizeof(addr) - 1) {
        zmqdrv_error_code(drv, E2BIG);
        return;
    }

    memcpy(addr, bytes + 5, size);
    addr[size] = '\0';

    zmqdrv_fprintf("connect %s\r\n", addr);

    if (!addr[0]) {
        zmqdrv_error_code(drv, EINVAL);
        return;
    }

    if (zmq_connect(s, addr) < 0) {
        zmqdrv_error_code(drv, zmq_errno());
        return;
    }

    zmqdrv_ok(drv);
}

static void
zmqdrv_send(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary*  bin   = ev->binv[1];
    char*          bytes = bin->orig_bytes;
    uint32_t       idx   = ntohl(*(uint32_t*)(bytes+1));
    zmq_sock_info* si    = drv->get_socket_info(idx);
    uint32_t       flags = ntohl(*(uint32_t*)(bytes+5));
    void*          data  = (void *)(bytes + 9);
    size_t         size  = bin->orig_size - 9;

    if (idx > drv->zmq_socket_count || !si) {
        zmqdrv_error_code(drv, ENODEV);
        return;
    }

#ifdef ZMQDRV_DEBUG
    uint32_t events;
    size_t events_size = sizeof(events);
    zmq_getsockopt(si->socket, ZMQ_EVENTS, &events, &events_size);
    zmqdrv_fprintf("sending %p [idx=%d] %lu bytes (events=%d)\r\n", si->socket, idx, size, events);
#endif

    if (si->out_caller != 0) {
        // There's still an unwritten message pending
        zmqdrv_error_code(drv, EBUSY);
        return;
    }

    // Increment the reference count on binary so that zmq can
    // take ownership of it.
    driver_binary_inc_refc(bin);

    if (zmq_msg_init_data(&si->out_msg, data, size, &zmq_free_binary, bin)) {
        zmqdrv_error_code(drv, zmq_errno());
        driver_binary_dec_refc(bin);
        return;
    }

    if (zmq_send(si->socket, &si->out_msg, flags | ZMQ_NOBLOCK) == 0) {
        zmqdrv_ok(drv);
        zmqdrv_ready_input((ErlDrvData)drv, (ErlDrvEvent)si->fd);
    } else {
        int e = zmq_errno();
        if (e == EAGAIN) {
            // No msg returned to caller - make him wait until async
            // send succeeds
            si->out_caller = driver_caller(drv->port);
            return;
        }
        zmqdrv_error_code(drv, e);
    }
    zmq_msg_close(&si->out_msg);
}

static void
zmqdrv_recv(zmq_drv_t *drv, ErlIOVec *ev)
{
    ErlDrvBinary*  bin   = ev->binv[1];
    char*          bytes = bin->orig_bytes;
    uint32_t       idx   = ntohl(*(uint32_t*)(bytes+1));
    zmq_sock_info* si    = drv->get_socket_info(idx);

    if (idx > drv->zmq_socket_count || !si) {
        zmqdrv_error_code(drv, ENODEV);
        return;
    }

    if (si->active_mode) {
        zmqdrv_error_code(drv, EINVAL);
        return;
    }

    if (si->in_caller != 0) {
        // Previous recv() call in passive mode didn't complete.
        // The owner must be blocked waiting for result.
        zmqdrv_error_code(drv, EBUSY);
        return;
    }

    uint32_t events;
    size_t events_size = sizeof(events);
    zmq_getsockopt(si->socket, ZMQ_EVENTS, &events, &events_size);

    if (events == 0)
        si->in_caller = driver_caller(drv->port);
    else {
        msg_t msg;

        if (zmq_recv(si->socket, &msg, ZMQ_NOBLOCK) == 0)
            zmqdrv_binary_ok(drv, zmq_msg_data(&msg), zmq_msg_size(&msg));
        else if (zmq_errno() == EAGAIN) {
            // No input available. Make the caller wait by not returning result
            si->in_caller = driver_caller(drv->port);
        } else
            zmqdrv_error_code(drv, zmq_errno());
    }
}
