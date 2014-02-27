//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//     * All advertising materials mentioning features or use of this
//       software must display the following acknowledgment:
//         This product includes software developed by Michael Truog
//     * The name of the author may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.
//

#include <erl_nif.h>
#include <erl_driver.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <unistd.h>
#include <poll.h>
#include <errno.h>
#include <fcntl.h>
#include <cstring>
#include "assert.hpp"

#define MAX_PENDING_SOCKETS 4096
#define PREFIX libcloudi_socket_drv

#define NIF_NAME_EXPAND(prefix, name) NIF_NAME_EXPAND_I(prefix, name)
#define NIF_NAME_EXPAND_I(prefix, name) NIF_NAME_EXPAND_II(prefix ## _ ## name)
#define NIF_NAME_EXPAND_II(res) res
#define NIF_NAME(name) NIF_NAME_EXPAND(PREFIX, name)
#define NIF_FUNC(name) \
    ERL_NIF_TERM NIF_NAME(name)(ErlNifEnv * env,\
                                int argc,\
                                const ERL_NIF_TERM * argv)

static bool local_thread_running = true;
static ErlDrvTid local_thread_id;
static ErlNifMutex * local_mutex = 0;
static int local_queue_event[2];
class local_t
{
    public:
        local_t() : fd_listener(-1), env(0) {}
        void clear() { *this = local_t(); }
        struct sockaddr_un local;
        int fd_listener;
        ErlNifEnv * env;
        ErlNifPid pid;
};

static local_t local_queue[MAX_PENDING_SOCKETS];
static int local_queue_size;

#if defined __cplusplus
extern "C"
{
#endif

NIF_FUNC(local)
{
    if (argc != 1)
    {
        return ::enif_make_badarg(env);
    }
    ::enif_mutex_lock(local_mutex);
    if (local_queue_size == sizeof(local_queue))
    {
        ::enif_mutex_unlock(local_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env, "enomem"));
    }
    local_t parameters;
    parameters.local.sun_family = PF_LOCAL;
    if (! ::enif_get_string(env, argv[0],
                            parameters.local.sun_path, 104, ERL_NIF_LATIN1))
    {
        ::enif_mutex_unlock(local_mutex);
        return ::enif_make_badarg(env);
    }
    parameters.fd_listener = ::socket(PF_LOCAL, SOCK_STREAM, 0);
    if (parameters.fd_listener == -1)
    {
        ::enif_mutex_unlock(local_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::unlink(parameters.local.sun_path) == -1 && errno != ENOENT)
    {
        ::close(parameters.fd_listener);
        ::enif_mutex_unlock(local_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::bind(parameters.fd_listener,
               reinterpret_cast<struct sockaddr *>(&(parameters.local)),
               sizeof(struct sockaddr_un)) == -1)
    {
        ::close(parameters.fd_listener);
        ::enif_mutex_unlock(local_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::listen(parameters.fd_listener, 0) == -1)
    {
        ::close(parameters.fd_listener);
        ::enif_mutex_unlock(local_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    parameters.env = ::enif_alloc_env();
    ::enif_self(env, &(parameters.pid));
    local_queue[local_queue_size++] = parameters;
    ::enif_mutex_unlock(local_mutex);
    char const c = 0;
    if (::write(local_queue_event[1], &c, 1) != 1)
    {
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    return ::enif_make_atom(env, "ok");
}

NIF_FUNC(set)
{
    if (argc != 2)
    {
        return ::enif_make_badarg(env);
    }
    int fd_old;
    if (! ::enif_get_int(env, argv[0], &fd_old))
    {
        return ::enif_make_badarg(env);
    }
    int fd_new;
    if (! ::enif_get_int(env, argv[1], &fd_new))
    {
        return ::enif_make_badarg(env);
    }
    // assign fd_new to fd_old, so that fd_new can be referred to as fd_old
    if (::dup2(fd_new, fd_old) == -1)
    {
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::close(fd_new) == -1)
    {
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    return ::enif_make_atom(env, "ok");
}

NIF_FUNC(setsockopts)
{
    if (argc != 3)
    {
        return ::enif_make_badarg(env);
    }
    int fd;
    if (! ::enif_get_int(env, argv[0], &fd))
    {
        return ::enif_make_badarg(env);
    }
    int recbuf;
    if (! ::enif_get_int(env, argv[1], &recbuf))
    {
        return ::enif_make_badarg(env);
    }
    int sndbuf;
    if (! ::enif_get_int(env, argv[2], &sndbuf))
    {
        return ::enif_make_badarg(env);
    }
    if (::fcntl(fd, F_SETFL, O_NONBLOCK) == -1)
    {
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::setsockopt(fd, SOL_SOCKET, SO_RCVBUF,
                     &recbuf, sizeof(recbuf)) == -1)
    {
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::setsockopt(fd, SOL_SOCKET, SO_SNDBUF,
                     &sndbuf, sizeof(sndbuf)) == -1)
    {
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }

    return ::enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {         "local", 1, NIF_NAME(local)},
    {           "set", 2, NIF_NAME(set)},
    {   "setsockopts", 3, NIF_NAME(setsockopts)}
};

static void * local_thread(void * /*data*/);

static int on_load(ErlNifEnv * /*env*/,
                   void ** /*priv_data*/,
                   ERL_NIF_TERM /*load_info*/)
{
    local_thread_running = true;
    local_mutex = ::enif_mutex_create(const_cast<char *>("local_mutex"));
    if (::pipe(local_queue_event) == -1)
    {
        return -1;
    }
    int value = ::enif_thread_create(const_cast<char *>("local_thread"),
                                     &local_thread_id, local_thread, 0, 0);
    return value;
}

static void on_unload(ErlNifEnv * /*env*/,
                      void * /*priv_data*/)
{
    local_thread_running = false;
    char const c = 0;
    if (::write(local_queue_event[1], &c, 1) == 1)
    {
        ::erl_drv_thread_join(local_thread_id, 0);
    }
    for (int i = 1; i < local_queue_size; ++i)
    {
        local_t & parameters = local_queue[i];
        ::close(parameters.fd_listener);
        ::enif_free_env(parameters.env);
        parameters.clear();
    }
    ::enif_mutex_destroy(local_mutex);
    local_mutex = 0;
    ::close(local_queue_event[0]);
    ::close(local_queue_event[1]);
}

static void * local_thread(void * /*data*/)
{
    struct pollfd poll_fds[MAX_PENDING_SOCKETS];
    struct pollfd const event = {local_queue_event[0], POLLIN, 0};
    poll_fds[0] = event;
    int poll_nfds = 1;
    local_queue_size = 1;
    while (local_thread_running)
    {
        ::enif_mutex_lock(local_mutex);
        if (static_cast<size_t>(poll_nfds) < local_queue_size)
        {
            for (int i = poll_nfds; i < local_queue_size; ++i)
            {
                struct pollfd & entry = poll_fds[i];
                entry.fd = local_queue[i].fd_listener;
                entry.events = POLLIN;
            }
            poll_nfds = local_queue_size;
        }
        ::enif_mutex_unlock(local_mutex);

        int event_count = ::poll(poll_fds, poll_nfds, 5000);
        if (event_count == -1)
        {
            ::erl_drv_thread_exit(0);
            return 0;
        }
        else if (event_count == 0)
        {
            // all entries timed out
            if (poll_nfds > 1)
            {
                // clear all the existing entries
                ::enif_mutex_lock(local_mutex);
                for (int i = 1; i < local_queue_size; ++i)
                {
                    local_t & parameters = local_queue[i];
                    ::close(parameters.fd_listener);
                    ::enif_free_env(parameters.env);
                }
                poll_nfds = 1;
                local_queue_size = 1;
                ::enif_mutex_unlock(local_mutex);
            }
        }
        else
        {
            ::enif_mutex_lock(local_mutex);
            for (int i = 0; event_count > 0; ++i)
            {
                struct pollfd & entry = poll_fds[i];
                if (i == 0)
                {
                    if (entry.revents & POLLIN)
                    {
                        poll_fds[0] = event;
                        --event_count;
                    }
                    else if (entry.revents & (~POLLIN))
                    {
                        ::erl_drv_thread_exit(0);
                        return 0;
                    }
                    continue;
                }
                else if (entry.revents & POLLIN)
                {
                    local_t parameters = local_queue[i];
                    if ((i + 1) < poll_nfds)
                    {
                        ::memmove(&poll_fds[i], &poll_fds[i + 1],
                                  (poll_nfds - (i + 1)) *
                                  sizeof(struct pollfd));
                    }
                    if ((i + 1) < local_queue_size)
                    {
                        ::memmove(&local_queue[i], &local_queue[i + 1],
                                  (local_queue_size - (i + 1)) *
                                  sizeof(local_t));
                    }
                    --i;
                    --poll_nfds;
                    --local_queue_size;
                    --event_count;
                    socklen_t local_size = sizeof(struct sockaddr_un);
                    int const fd_new =
                        ::accept(parameters.fd_listener,
                                 reinterpret_cast<struct sockaddr *>(
                                     &(parameters.local)), &local_size);
                    if (fd_new == -1)
                    {
                        ::enif_send(0, &(parameters.pid), parameters.env,
                            ::enif_make_tuple4(parameters.env,
                                ::enif_make_atom(parameters.env, "inet_async"),
                                ::enif_make_atom(parameters.env, "undefined"),
                                ::enif_make_atom(parameters.env, "undefined"),
                                ::enif_make_tuple2(parameters.env,
                                    ::enif_make_atom(parameters.env, "error"),
                                    ::enif_make_atom(parameters.env,
                                        ::erl_errno_id(errno)))));
                    }
                    else
                    {
                        ::enif_send(0, &(parameters.pid), parameters.env,
                            ::enif_make_tuple4(parameters.env,
                                ::enif_make_atom(parameters.env, "inet_async"),
                                ::enif_make_atom(parameters.env, "undefined"),
                                ::enif_make_atom(parameters.env, "undefined"),
                                ::enif_make_tuple2(parameters.env,
                                    ::enif_make_atom(parameters.env, "ok"),
                                    ::enif_make_int(parameters.env, fd_new))));
                    }
                    ::close(parameters.fd_listener);
                    ::enif_free_env(parameters.env);
                }
                else if (entry.revents & (~POLLIN))
                {
                    local_t parameters = local_queue[i];
                    if ((i + 1) < poll_nfds)
                    {
                        ::memmove(&poll_fds[i], &poll_fds[i + 1],
                                  (poll_nfds - (i + 1)) *
                                  sizeof(struct pollfd));
                    }
                    if ((i + 1) < local_queue_size)
                    {
                        ::memmove(&local_queue[i], &local_queue[i + 1],
                                  (local_queue_size - (i + 1)) *
                                  sizeof(local_t));
                    }
                    --i;
                    --poll_nfds;
                    --local_queue_size;
                    --event_count;
                    ::enif_send(0, &(parameters.pid), parameters.env,
                        ::enif_make_tuple4(parameters.env,
                            ::enif_make_atom(parameters.env, "inet_async"),
                            ::enif_make_atom(parameters.env, "undefined"),
                            ::enif_make_atom(parameters.env, "undefined"),
                            ::enif_make_tuple2(parameters.env,
                                ::enif_make_atom(parameters.env, "error"),
                                ::enif_make_atom(parameters.env, "poll"))));
                    ::close(parameters.fd_listener);
                    ::enif_free_env(parameters.env);
                }
            }
            ::enif_mutex_unlock(local_mutex);
        }
    }
    return 0;
}

#if defined __cplusplus
}
#endif

ERL_NIF_INIT(cloudi_socket, nif_funcs, &on_load, 0, 0, &on_unload);

