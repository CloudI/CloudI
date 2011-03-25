/* -*- coding:utf-8;Mode:C;tab-width:2;c-basic-offset:2;indent-tabs-mode:nil -*-
 * ex: set softtabstop=2 tabstop=2 shiftwidth=2 expandtab fileencoding=utf-8:
 *
 * Copyright (c) 2011 Yurii Rashkovskii, Michael Truog, and Evax Software
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

#include "zmq.h"
#include "erl_nif.h"
#include <string.h>
#include <sys/queue.h>
#include <stdio.h>
#include <assert.h>

static ErlNifResourceType* erlzmq_nif_resource_context;
static ErlNifResourceType* erlzmq_nif_resource_socket;

typedef struct erlzmq_context {
  void * context_zmq;
  void * ipc_socket;
  char * ipc_socket_name;
  int running;
  int64_t socket_index;
  ErlNifCond * cond;
  ErlNifMutex * mutex;
  ErlNifTid polling_tid;
} erlzmq_context_t;

#define ERLZMQ_ACTIVE_OFF      0
#define ERLZMQ_ACTIVE_PENDING  1
#define ERLZMQ_ACTIVE_ON       2

typedef struct erlzmq_socket {
  erlzmq_context_t * context;
  int64_t socket_index;
  void * socket_zmq;
  int active;
} erlzmq_socket_t;

#define ERLZMQ_FLAGS_TERMINATE    0xffffffff /* termination ipc request flag */

typedef struct erlzmq_ipc_request {
  erlzmq_socket_t * socket;
  ErlNifEnv * env;
  ERL_NIF_TERM ref;
  int flags;
  int poll_flag;
  zmq_msg_t msg;
  ErlNifPid pid;
  TAILQ_ENTRY(erlzmq_ipc_request) requests;
} erlzmq_ipc_request_t;
TAILQ_HEAD(requests_head, erlzmq_ipc_request);

// Prototypes
#define NIF(name) \
  ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

NIF(erlzmq_nif_context);
NIF(erlzmq_nif_socket);
NIF(erlzmq_nif_bind);
NIF(erlzmq_nif_connect);
NIF(erlzmq_nif_setsockopt);
NIF(erlzmq_nif_getsockopt);
NIF(erlzmq_nif_send);
NIF(erlzmq_nif_recv);
NIF(erlzmq_nif_close);
NIF(erlzmq_nif_term);

static void * polling_thread(void * handle);
static ERL_NIF_TERM add_active_req(ErlNifEnv* env, erlzmq_socket_t * socket);
static ERL_NIF_TERM return_zmq_errno(ErlNifEnv* env, int const value);

static ErlNifFunc nif_funcs[] =
{
  {"context", 1, erlzmq_nif_context},
  {"socket", 3, erlzmq_nif_socket},
  {"bind", 2, erlzmq_nif_bind},
  {"connect", 2, erlzmq_nif_connect},
  {"setsockopt", 3, erlzmq_nif_setsockopt},
  {"getsockopt", 2, erlzmq_nif_getsockopt},
  {"send", 3, erlzmq_nif_send},
  {"recv", 2, erlzmq_nif_recv},
  {"close", 1, erlzmq_nif_close},
  {"term", 1, erlzmq_nif_term}
};

NIF(erlzmq_nif_context)
{
  int thread_count;

  if (!enif_get_int(env, argv[0], &thread_count)) {
    return enif_make_badarg(env);
  }

  erlzmq_context_t * handle = enif_alloc_resource(erlzmq_nif_resource_context,
                                                  sizeof(erlzmq_context_t));

  handle->context_zmq = zmq_init(thread_count);

  char socket_id[64];
  sprintf(socket_id, "inproc://erlzmq-%ld", (long int) handle);
  handle->ipc_socket_name = strdup(socket_id);

  handle->ipc_socket = zmq_socket(handle->context_zmq, ZMQ_PUSH);
  zmq_bind(handle->ipc_socket,socket_id);

  handle->running = 0;
  handle->socket_index = 1;
  handle->mutex = enif_mutex_create("erlzmq_context_t_mutex");
  handle->cond = enif_cond_create("erlzmq_context_t_cond");

  enif_mutex_lock(handle->mutex);
  int err;
  if ((err = enif_thread_create("erlzmq_polling_thread", &handle->polling_tid,
                                polling_thread, handle, NULL))) {
    enif_mutex_unlock(handle->mutex);
    enif_mutex_destroy(handle->mutex);
    enif_cond_destroy(handle->cond);
    zmq_close(handle->ipc_socket);
    free(handle->ipc_socket_name);
    zmq_term(handle->context_zmq);
    enif_release_resource(handle);
    return return_zmq_errno(env, err);
  }
  while (handle->running == 0) {
    enif_cond_wait(handle->cond, handle->mutex);
  }
  enif_mutex_unlock(handle->mutex);

  ERL_NIF_TERM result = enif_make_resource(env, handle);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

NIF(erlzmq_nif_socket)
{
  erlzmq_context_t * ctx;
  int socket_type;
  int active;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_context,
                         (void **) &ctx)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &socket_type)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[2], &active)) {
    return enif_make_badarg(env);
  }
  
  if (!ctx->running) {
    return enif_make_badarg(env);
  }

  erlzmq_socket_t * handle = enif_alloc_resource(erlzmq_nif_resource_socket,
                                                 sizeof(erlzmq_socket_t));

  handle->context = ctx;
  handle->socket_index = ctx->socket_index++;
  handle->socket_zmq = zmq_socket(ctx->context_zmq, socket_type);
  handle->active = active;

  ERL_NIF_TERM socket =
    enif_make_tuple2(env, enif_make_uint64(env, handle->socket_index),
                     enif_make_resource(env, handle));

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), socket);
}

NIF(erlzmq_nif_bind)
{
  erlzmq_socket_t * socket;
  unsigned endpoint_length;
  char * endpoint;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                         (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_list_length(env, argv[1], &endpoint_length)) {
    return enif_make_badarg(env);
  }

  endpoint = (char *) malloc(endpoint_length + 1);

  if (!enif_get_string(env, argv[1], endpoint, endpoint_length + 1,
                       ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  if (zmq_bind(socket->socket_zmq, endpoint)) {
    free(endpoint);
    return return_zmq_errno(env, zmq_errno());
  } else {
    free(endpoint);
    if (socket->active == ERLZMQ_ACTIVE_PENDING) {
      return add_active_req(env, socket);
    }
    else {
      return enif_make_atom(env, "ok");
    }
  }
}

NIF(erlzmq_nif_connect)
{
  erlzmq_socket_t * socket;
  unsigned endpoint_length;
  char * endpoint;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                         (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_list_length(env, argv[1], &endpoint_length)) {
    return enif_make_badarg(env);
  }

  endpoint = (char *) malloc(endpoint_length + 1);

  if (!enif_get_string(env, argv[1], endpoint, endpoint_length + 1,
                       ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  if (zmq_connect(socket->socket_zmq, endpoint)) {
    free(endpoint);
    return return_zmq_errno(env, zmq_errno());
  } else {
    free(endpoint);
    if (socket->active == ERLZMQ_ACTIVE_PENDING) {
      return add_active_req(env, socket);
    }
    else {
      return enif_make_atom(env, "ok");
    }
  }
}

NIF(erlzmq_nif_setsockopt)
{
  erlzmq_socket_t * socket;
  int option_name;
  ErlNifUInt64 value_uint64;
  ErlNifSInt64 value_int64;
  ErlNifBinary binary;
  int value_int;
  void *option_value;
  size_t option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                         (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &option_name)) {
    return enif_make_badarg(env);
  }

  switch (option_name) {
    case ZMQ_HWM: // uint64_t
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
      if (!enif_get_uint64(env, argv[2], &value_uint64)) {
        return enif_make_badarg(env);
      }
      option_value = &value_uint64;
      break;
    case ZMQ_SWAP: // int64_t
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_MCAST_LOOP:
      if (!enif_get_int64(env, argv[2], &value_int64)) {
        return enif_make_badarg(env);
      }
      option_value = &value_int64;
      break;
    case ZMQ_IDENTITY: // binary
    case ZMQ_SUBSCRIBE:
    case ZMQ_UNSUBSCRIBE:
      if (!enif_inspect_iolist_as_binary(env, argv[2], &binary)) {
        return enif_make_badarg(env);
      }
      option_value = binary.data;
      option_len = binary.size;
      break;
    case ZMQ_LINGER: // int
    case ZMQ_RECONNECT_IVL:
    case ZMQ_BACKLOG:
      if (!enif_get_int(env, argv[1], &value_int)) {
        return enif_make_badarg(env);
      }
      option_value = &value_int;
      option_len = sizeof(int);
      break;
    default:
      return enif_make_badarg(env);
  }

  if (zmq_setsockopt(socket->socket_zmq, option_name,
                     option_value, option_len)) {
    return return_zmq_errno(env, zmq_errno());
  }
  else {
    return enif_make_atom(env, "ok");
  }
}

NIF(erlzmq_nif_getsockopt)
{
  erlzmq_socket_t * socket;
  int option_name;
  ErlNifBinary _bin;
  int64_t option_value_64;
  int64_t option_value_u64;
  char option_value[255];
  int option_value_int;
  size_t option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                         (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &option_name)) {
    return enif_make_badarg(env);
  }

  switch(option_name) {
    case ZMQ_RCVMORE: // int64_t
    case ZMQ_SWAP:
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_RECOVERY_IVL_MSEC:
    case ZMQ_MCAST_LOOP:
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         &option_value_64, &option_len)) {
        return return_zmq_errno(env, zmq_errno());
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_int64(env, option_value_64));
    case ZMQ_HWM: // uint64_t
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         &option_value_u64, &option_len)) {
        return return_zmq_errno(env, zmq_errno());
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_uint64(env, option_value_u64));
    case ZMQ_IDENTITY: // binary
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         option_value, &option_len)) {
        return return_zmq_errno(env, zmq_errno());
      }
      enif_alloc_binary(option_len, &_bin);
      memcpy(_bin.data, option_value, option_len);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_binary(env, &_bin));
    case ZMQ_TYPE: // int
    case ZMQ_LINGER:
    case ZMQ_RECONNECT_IVL:
    case ZMQ_RECONNECT_IVL_MAX:
    case ZMQ_BACKLOG:
    case ZMQ_FD: // FIXME: ZMQ_FD returns SOCKET on Windows
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         &option_value_int, &option_len)) {
        return return_zmq_errno(env, zmq_errno());
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_int(env, option_value_int));
    default:
      return enif_make_badarg(env);
  }
}

NIF(erlzmq_nif_send)
{
  erlzmq_ipc_request_t req;
  erlzmq_socket_t * socket;
  ErlNifBinary binary;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                         (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_iolist_as_binary(env, argv[1], &binary)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[2], &req.flags)) {
    return enif_make_badarg(env);
  }

  if (zmq_msg_init_size(&req.msg, binary.size)) {
    return return_zmq_errno(env, zmq_errno());
  }

  memcpy(zmq_msg_data(&req.msg), binary.data, binary.size);

  if (zmq_send(socket->socket_zmq, &req.msg, req.flags | ZMQ_NOBLOCK)) {
    int const error = zmq_errno();
    if (error != EAGAIN || (error == EAGAIN && (req.flags & ZMQ_NOBLOCK))) {
      zmq_msg_close(&req.msg);
      return return_zmq_errno(env, error);
    }

    req.env = enif_alloc_env();
    req.ref = enif_make_ref(req.env);
    /* req.flags set */
    req.poll_flag = ZMQ_POLLOUT;
    /* req.msg set */
    enif_self(env, &req.pid);
    req.socket = socket;
    enif_keep_resource(socket->context);
    enif_keep_resource(socket);

    zmq_msg_t msg;
    if (zmq_msg_init_size(&msg, sizeof(erlzmq_ipc_request_t))) {
      zmq_msg_close(&req.msg);
      enif_free_env(req.env);
      enif_release_resource(socket->context);
      enif_release_resource(socket);
      return return_zmq_errno(env, zmq_errno());
    }

    memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_ipc_request_t));

    if (zmq_send(socket->context->ipc_socket, &msg, 0)) {
      zmq_msg_close(&msg);
      zmq_msg_close(&req.msg);
      enif_free_env(req.env);
      enif_release_resource(socket->context);
      enif_release_resource(socket);
      return return_zmq_errno(env, zmq_errno());
    }

    zmq_msg_close(&msg);

    return enif_make_copy(env, req.ref);
  }

  zmq_msg_close(&req.msg);

  return enif_make_atom(env, "ok");
}

NIF(erlzmq_nif_recv)
{
  erlzmq_ipc_request_t req;
  erlzmq_socket_t * socket;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                         (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &req.flags)) {
    return enif_make_badarg(env);
  }

  if (socket->active) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                            enif_make_atom(env, "active"));
  }

  zmq_msg_t msg;

  if (zmq_msg_init(&msg)) {
    return return_zmq_errno(env, zmq_errno());
  }

  // try recv with noblock
  if (zmq_recv(socket->socket_zmq, &msg, ZMQ_NOBLOCK)) {
    int const error = zmq_errno();
    if (error != EAGAIN || (error == EAGAIN && (req.flags & ZMQ_NOBLOCK))) {
      zmq_msg_close(&msg);
      return return_zmq_errno(env, error);
    }

    req.env = enif_alloc_env();
    req.ref = enif_make_ref(req.env);
    /* req.flags set */
    req.poll_flag = ZMQ_POLLIN;
    /* req.msg unused */
    enif_self(env, &req.pid);
    req.socket = socket;
    enif_keep_resource(socket->context);
    enif_keep_resource(socket);

    if (zmq_msg_init_size(&msg, sizeof(erlzmq_ipc_request_t))) {
      zmq_msg_close(&msg);
      enif_free_env(req.env);
      enif_release_resource(socket->context);
      enif_release_resource(socket);
      return return_zmq_errno(env, zmq_errno());
    }

    memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_ipc_request_t));

    if (zmq_send(socket->context->ipc_socket, &msg, 0)) {
      zmq_msg_close(&msg);
      enif_free_env(req.env);
      enif_release_resource(socket->context);
      enif_release_resource(socket);
      return return_zmq_errno(env, zmq_errno());
    }

    zmq_msg_close(&msg);

    return enif_make_copy(env, req.ref);
  }
  else {
    ErlNifBinary binary;
    enif_alloc_binary(zmq_msg_size(&msg), &binary);
    memcpy(binary.data, zmq_msg_data(&msg), zmq_msg_size(&msg));
  
    zmq_msg_close(&msg);
  
    return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                            enif_make_binary(env, &binary));
  }
}

NIF(erlzmq_nif_close)
{
  erlzmq_socket_t * socket;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  enif_release_resource(socket);

  if (-1 == zmq_close(socket->socket_zmq)) {
    return return_zmq_errno(env, zmq_errno());
  } else {
    return enif_make_atom(env, "ok");
  }
}

NIF(erlzmq_nif_term)
{
  erlzmq_context_t * ctx;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_context,
                         (void **) &ctx)) {
    return enif_make_badarg(env);
  }

  zmq_msg_t msg;
  erlzmq_ipc_request_t req;

  req.flags = ERLZMQ_FLAGS_TERMINATE;
  req.env = enif_alloc_env();
  req.ref = enif_make_ref(req.env);
  /* req.poll_flag unused */
  /* req.msg unused */
  enif_self(env, &req.pid);
  req.socket = 0;

  zmq_msg_init_size(&msg, sizeof(erlzmq_ipc_request_t));
  memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_ipc_request_t));
  enif_mutex_lock(ctx->mutex);
  zmq_send(ctx->ipc_socket, &msg, 0);
  enif_mutex_unlock(ctx->mutex);
  zmq_msg_close(&msg);

  enif_release_resource(ctx);

  return enif_make_copy(env, req.ref);
}

static void * polling_thread(void * handle)
{
  erlzmq_context_t * ctx = (erlzmq_context_t *) handle;
  ErlNifEnv * final_env = enif_alloc_env();
  ERL_NIF_TERM final_ref = enif_make_ref(final_env);
  ERL_NIF_TERM final_pid = enif_make_ref(final_env);

  enif_keep_resource(ctx);
  struct requests_head * requests_queue;
  requests_queue = malloc(sizeof(struct requests_head));
  TAILQ_INIT(requests_queue);

  void *ipc_socket = zmq_socket(ctx->context_zmq, ZMQ_PULL);
  zmq_connect(ipc_socket,ctx->ipc_socket_name);

  int nwatched = 1;
  enif_mutex_lock(ctx->mutex);
  ctx->running = 1;
  enif_cond_signal(ctx->cond);
  enif_mutex_unlock(ctx->mutex);

  while (ctx->running) {
    int i;
    zmq_msg_t msg;
    erlzmq_ipc_request_t *r, *rtmp;

    zmq_pollitem_t *items = calloc(nwatched, sizeof(zmq_pollitem_t));
    items[0].socket = ipc_socket;
    items[0].events = ZMQ_POLLIN;

    for (i = 1, r = requests_queue->tqh_first;
         r != NULL; r = r->requests.tqe_next, ++i) {
      items[i].socket = r->socket->socket_zmq;
      items[i].events = r->poll_flag;
    }

    int count = zmq_poll(items, nwatched, -1);
    assert(count != -1);
    if (items[0].revents & ZMQ_POLLIN) {
      --count;
    }

    for (i = 1, r = requests_queue->tqh_first; count > 0 &&
         r && ((rtmp = r->requests.tqe_next), 1); r = rtmp, ++i) {
      if (items[i].revents & ZMQ_POLLIN) {
        ErlNifBinary binary;

        zmq_msg_init(&msg);
        zmq_recv(r->socket->socket_zmq, &msg, r->flags);

        enif_alloc_binary(zmq_msg_size(&msg), &binary);
        memcpy(binary.data, zmq_msg_data(&msg), zmq_msg_size(&msg));
        zmq_msg_close(&msg);

        if (r->socket->active == ERLZMQ_ACTIVE_ON) {
          ERL_NIF_TERM socket =
            enif_make_tuple2(r->env,
                             enif_make_uint64(r->env, r->socket->socket_index),
                             enif_make_resource(r->env, r->socket));
          enif_send(NULL, &r->pid, r->env,
                    enif_make_tuple3(r->env,
                                     enif_make_atom(r->env, "zmq"),
                                     socket,
                                     enif_make_binary(r->env, &binary)));
        }
        else {
          enif_send(NULL, &r->pid, r->env,
                    enif_make_tuple2(r->env,
                                     enif_make_copy(r->env, r->ref),
                                     enif_make_binary(r->env, &binary)));
          --nwatched;
          enif_free_env(r->env);
          enif_release_resource(r->socket->context);
          enif_release_resource(r->socket);
          TAILQ_REMOVE(requests_queue, r, requests);
          free(r);
        }
        --count;
      }
      if (items[i].revents & ZMQ_POLLOUT) {
        if (zmq_send(r->socket->socket_zmq, &r->msg, r->flags)) {
          enif_send(NULL, &r->pid, r->env,
                    enif_make_tuple2(r->env,
                                     enif_make_copy(r->env, r->ref),
                                     return_zmq_errno(r->env, zmq_errno())));
        } else {
          enif_send(NULL, &r->pid, r->env,
                    enif_make_tuple2(r->env,
                                     enif_make_copy(r->env, r->ref),
                                     enif_make_atom(r->env, "ok")));
        }
        zmq_msg_close(&r->msg);
        --nwatched;
        enif_free_env(r->env);
        enif_release_resource(r->socket->context);
        enif_release_resource(r->socket);
        TAILQ_REMOVE(requests_queue, r, requests);
        free(r);
        --count;
      }
    }
    if (items[0].revents & ZMQ_POLLIN) {
      zmq_msg_init(&msg);
      if (!zmq_recv(items[0].socket, &msg, 0)) {
        erlzmq_ipc_request_t * req = (erlzmq_ipc_request_t *)zmq_msg_data(&msg);
        if (req->flags == ERLZMQ_FLAGS_TERMINATE) {
          final_ref = enif_make_copy(final_env, req->ref);
          final_pid = enif_make_pid(final_env, &req->pid);
          enif_free_env(req->env);
          ctx->running = 0;
        }
        else {
          ++nwatched;
          erlzmq_ipc_request_t * r = malloc(sizeof(erlzmq_ipc_request_t));
          memcpy(r, req, sizeof(erlzmq_ipc_request_t));
          TAILQ_INSERT_TAIL(requests_queue, r, requests);
        }
      }
      zmq_msg_close(&msg);
    }
    free(items);
  }
  enif_mutex_lock(ctx->mutex);
  enif_mutex_unlock(ctx->mutex);

  // cleanup reader's queue
  erlzmq_ipc_request_t * r;
  while ((r = requests_queue->tqh_first) != NULL) {
    TAILQ_REMOVE(requests_queue, requests_queue->tqh_first, requests);
    enif_free_env(r->env);
    enif_release_resource(r->socket->context);
    enif_release_resource(r->socket);
    free(r);
  }
  free(requests_queue);
  zmq_close(ipc_socket);

  zmq_close(ctx->ipc_socket);
  free(ctx->ipc_socket_name);
  enif_mutex_destroy(ctx->mutex);
  enif_cond_destroy(ctx->cond);

  zmq_term(ctx->context_zmq);
  enif_release_resource(ctx);

  // signal erlzmq:term/2 that the context has been finally terminated
  ErlNifPid pid;
  enif_get_local_pid(final_env, final_pid, &pid);

  enif_send(NULL, &pid, final_env,
            enif_make_tuple2(final_env,
                             enif_make_copy(final_env, final_ref),
                             enif_make_atom(final_env, "ok")));
  enif_free_env(final_env);

  return NULL;
}

static ERL_NIF_TERM add_active_req(ErlNifEnv* env, erlzmq_socket_t * socket)
{
  socket->active = ERLZMQ_ACTIVE_ON;

  zmq_msg_t msg;

  if (zmq_msg_init(&msg)) {
    return return_zmq_errno(env, zmq_errno());
  }

  erlzmq_ipc_request_t req;
  req.env = enif_alloc_env();
  /* req.ref unused */
  req.flags = 0;
  req.poll_flag = ZMQ_POLLIN;
  /* req.msg unused */
  enif_self(env, &req.pid);
  req.socket = socket;
  enif_keep_resource(socket->context);
  enif_keep_resource(socket);

  if (zmq_msg_init_size(&msg, sizeof(erlzmq_ipc_request_t))) {
    zmq_msg_close(&msg);
    enif_free_env(req.env);
    enif_release_resource(socket->context);
    enif_release_resource(socket);
    return return_zmq_errno(env, zmq_errno());
  }

  memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_ipc_request_t));

  if (zmq_send(socket->context->ipc_socket, &msg, 0)) {
    zmq_msg_close(&msg);
    enif_free_env(req.env);
    enif_release_resource(socket->context);
    enif_release_resource(socket);
    return return_zmq_errno(env, zmq_errno());
  }

  zmq_msg_close(&msg);
  return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM return_zmq_errno(ErlNifEnv* env, int const value)
{
  switch (value)
  {
    case EPERM:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eperm"));
    case ENOENT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enoent"));
    case ESRCH:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "esrch"));
    case EINTR:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eintr"));
    case EIO:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eio"));
    case ENXIO:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enxio"));
    case ENOEXEC:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enoexec"));
    case EBADF:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "ebadf"));
    case ECHILD:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "echild"));
    case EDEADLK:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "edeadlk"));
    case ENOMEM:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enomem"));
    case EACCES:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eacces"));
    case EFAULT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "efault"));
    case ENOTBLK:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enotblk"));
    case EBUSY:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "ebusy"));
    case EEXIST:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eexist"));
    case EXDEV:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "exdev"));
    case ENODEV:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enodev"));
    case ENOTDIR:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enotdir"));
    case EISDIR:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eisdir"));
    case EINVAL:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "einval"));
    case ENFILE:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enfile"));
    case EMFILE:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "emfile"));
    case ETXTBSY:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "etxtbsy"));
    case EFBIG:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "efbig"));
    case ENOSPC:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enospc"));
    case ESPIPE:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "espipe"));
    case EROFS:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "erofs"));
    case EMLINK:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "emlink"));
    case EPIPE:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "epipe"));
    case EAGAIN:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eagain"));
    case EINPROGRESS:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "einprogress"));
    case EALREADY:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "ealready"));
    case ENOTSOCK:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enotsock"));
    case EDESTADDRREQ:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "edestaddrreq"));
    case EMSGSIZE:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "emsgsize"));
    case EPROTOTYPE:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eprototype"));
    case ENOPROTOOPT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eprotoopt"));
    case EPROTONOSUPPORT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eprotonosupport"));
    case ESOCKTNOSUPPORT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "esocktnosupport"));
    case ENOTSUP:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enotsup"));
    case EPFNOSUPPORT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "epfnosupport"));
    case EAFNOSUPPORT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eafnosupport"));
    case EADDRINUSE:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eaddrinuse"));
    case EADDRNOTAVAIL:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eaddrnotavail"));
    case ENETDOWN:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enetdown"));
    case ENETUNREACH:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enetunreach"));
    case ENETRESET:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enetreset"));
    case ECONNABORTED:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "econnaborted"));
    case ECONNRESET:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "econnreset"));
    case ENOBUFS:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enobufs"));
    case EISCONN:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eisconn"));
    case ENOTCONN:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enotconn"));
    case ESHUTDOWN:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eshutdown"));
    case ETOOMANYREFS:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "etoomanyrefs"));
    case ETIMEDOUT:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "etimedout"));
    case ECONNREFUSED:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "econnrefused"));
    case ELOOP:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eloop"));
    case ENAMETOOLONG:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enametoolong"));
    case (ZMQ_HAUSNUMERO +  1):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enotsup"));
    case (ZMQ_HAUSNUMERO +  2):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eprotonosupport"));
    case (ZMQ_HAUSNUMERO +  3):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enobufs"));
    case (ZMQ_HAUSNUMERO +  4):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enetdown"));
    case (ZMQ_HAUSNUMERO +  5):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eaddrinuse"));
    case (ZMQ_HAUSNUMERO +  6):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eaddrnotavail"));
    case (ZMQ_HAUSNUMERO +  7):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "econnrefused"));
    case (ZMQ_HAUSNUMERO +  8):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "einprogress"));
    case (ZMQ_HAUSNUMERO + 51):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "efsm"));
    case (ZMQ_HAUSNUMERO + 52):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "enocompatproto"));
    case (ZMQ_HAUSNUMERO + 53):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "eterm"));
    case (ZMQ_HAUSNUMERO + 54):
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_atom(env, "emthread"));
    default:
      return enif_make_tuple2(env, enif_make_atom(env, "error"),
                              enif_make_int(env, value));
  }
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  erlzmq_nif_resource_context =
    enif_open_resource_type(env, "erlzmq_nif",
                            "erlzmq_nif_resource_context",
                            NULL,
                            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                            0);
  erlzmq_nif_resource_socket =
    enif_open_resource_type(env, "erlzmq_nif",
                            "erlzmq_nif_resource_socket",
                            NULL,
                            ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER,
                            0);
  return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data) {
}

ERL_NIF_INIT(erlzmq_nif, nif_funcs, &on_load, NULL, NULL, &on_unload);

