// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
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
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <cstring>
#include "assert.hpp"

#define PREFIX libcloudi_socket_drv

#define NIF_NAME_EXPAND(prefix, name) NIF_NAME_EXPAND_I(prefix, name)
#define NIF_NAME_EXPAND_I(prefix, name) NIF_NAME_EXPAND_II(prefix ## _ ## name)
#define NIF_NAME_EXPAND_II(res) res
#define NIF_NAME(name) NIF_NAME_EXPAND(PREFIX, name)
#define NIF_FUNC(name) \
    ERL_NIF_TERM NIF_NAME(name)(ErlNifEnv * env,\
                                int argc,\
                                const ERL_NIF_TERM * argv)

static ErlNifMutex * socket_mutex = 0; // prevent reentrant socket
                                       // function call problems

#if defined __cplusplus
extern "C"
{
#endif

NIF_FUNC(local_listen)
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
    struct sockaddr_un local;
    local.sun_family = PF_LOCAL;
    if (! ::enif_get_string(env, argv[1], local.sun_path, 104, ERL_NIF_LATIN1))
    {
        return ::enif_make_badarg(env);
    }

    ::enif_mutex_lock(socket_mutex);
    int fd_new = ::socket(PF_LOCAL, SOCK_STREAM, 0);
    if (fd_new == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::unlink(local.sun_path) == -1 && errno != ENOENT)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::bind(fd_new,
               reinterpret_cast<struct sockaddr *>(&local),
               sizeof(local)) == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::listen(fd_new, 0) == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::dup2(fd_new, fd_old) == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    ::enif_mutex_unlock(socket_mutex);

    return ::enif_make_atom(env, "ok");
}

NIF_FUNC(local_accept)
{
    if (argc != 5)
    {
        return ::enif_make_badarg(env);
    }
    int fd_listener;
    if (! ::enif_get_int(env, argv[0], &fd_listener))
    {
        return ::enif_make_badarg(env);
    }
    int fd_old;
    if (! ::enif_get_int(env, argv[1], &fd_old))
    {
        return ::enif_make_badarg(env);
    }
    struct sockaddr_un local;
    local.sun_family = PF_LOCAL;
    if (! ::enif_get_string(env, argv[2], local.sun_path, 104, ERL_NIF_LATIN1))
    {
        return ::enif_make_badarg(env);
    }
    int recbuf;
    if (! ::enif_get_int(env, argv[3], &recbuf))
    {
        return ::enif_make_badarg(env);
    }
    int sndbuf;
    if (! ::enif_get_int(env, argv[4], &sndbuf))
    {
        return ::enif_make_badarg(env);
    }
    ::enif_mutex_lock(socket_mutex);
    socklen_t local_size = sizeof(local);
    int fd_new = ::accept(fd_listener,
                          reinterpret_cast<struct sockaddr *>(&local),
                          &local_size);
    if (fd_new == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::fcntl(fd_new, F_SETFL, O_NONBLOCK) == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::setsockopt(fd_new, SOL_SOCKET, SO_RCVBUF,
                     &recbuf, sizeof(recbuf)) == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::setsockopt(fd_new, SOL_SOCKET, SO_SNDBUF,
                     &sndbuf, sizeof(sndbuf)) == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    if (::dup2(fd_new, fd_old) == -1)
    {
        ::enif_mutex_unlock(socket_mutex);
        return ::enif_make_tuple2(env,
                                  ::enif_make_atom(env, "error"),
                                  ::enif_make_atom(env,
                                                   ::erl_errno_id(errno)));
    }
    ::enif_mutex_unlock(socket_mutex);

    return ::enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"local_listen", 2, NIF_NAME(local_listen)},
    {"local_accept", 5, NIF_NAME(local_accept)}
};

static int on_load(ErlNifEnv * /*env*/,
                   void ** /*priv_data*/,
                   ERL_NIF_TERM /*load_info*/)
{
    socket_mutex = ::enif_mutex_create(const_cast<char *>("socket_mutex"));
    return 0;
}

static void on_unload(ErlNifEnv * /*env*/,
                      void * /*priv_data*/)
{
    ::enif_mutex_destroy(socket_mutex);
    socket_mutex = 0;
}

#if defined __cplusplus
}
#endif

ERL_NIF_INIT(cloudi_socket, nif_funcs, &on_load, NULL, NULL, &on_unload);

