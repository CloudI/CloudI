// -*- coding:utf-8;Mode:C;tab-width:2;c-basic-offset:2;indent-tabs-mode:nil -*-
// ex: set softtabstop=2 tabstop=2 shiftwidth=2 expandtab fileencoding=utf-8:
//
// Copyright (c) 2011 Yurii Rashkovskii, Evax Software and Michael Truog
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#include "zmq.h"
#include "erl_nif.h"
#include "vector.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>

#define ERLZMQ_MAX_CONCURRENT_REQUESTS 16384

static ErlNifResourceType* erlzmq_nif_resource_context;
static ErlNifResourceType* erlzmq_nif_resource_socket;

typedef struct erlzmq_context {
  void * context_zmq;
  void * thread_socket;
  char * thread_socket_name;
  int64_t socket_index;
  ErlNifTid polling_tid;
  ErlNifMutex * mutex;
} erlzmq_context_t;

#define ERLZMQ_SOCKET_ACTIVE_OFF        0
#define ERLZMQ_SOCKET_ACTIVE_PENDING    1
#define ERLZMQ_SOCKET_ACTIVE_ON         2

typedef struct erlzmq_socket {
  erlzmq_context_t * context;
  int64_t socket_index;
  void * socket_zmq;
  int active;
  ErlNifMutex * mutex;
} erlzmq_socket_t;

#define ERLZMQ_THREAD_REQUEST_SEND      1
#define ERLZMQ_THREAD_REQUEST_RECV      2
#define ERLZMQ_THREAD_REQUEST_CLOSE     3
#define ERLZMQ_THREAD_REQUEST_TERM      4

typedef struct {
  int type;
  union {
    struct {
      erlzmq_socket_t * socket;
      ErlNifEnv * env;
      ERL_NIF_TERM ref;
      int flags;
      zmq_msg_t msg;
      ErlNifPid pid;
    } send;
    struct {
      erlzmq_socket_t * socket;
      ErlNifEnv * env;
      ERL_NIF_TERM ref;
      int flags;
      ErlNifPid pid;
    } recv;
    struct {
      erlzmq_socket_t * socket;
      ErlNifEnv * env;
      ERL_NIF_TERM ref;
      ErlNifPid pid;
    } close;
    struct {
      ErlNifEnv * env;
      ERL_NIF_TERM ref;
      ErlNifPid pid;
    } term;
  } data;
} erlzmq_thread_request_t;

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
NIF(erlzmq_nif_version);

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
  {"term", 1, erlzmq_nif_term},
  {"version", 0, erlzmq_nif_version}
};

NIF(erlzmq_nif_context)
{
  int thread_count;

  if (! enif_get_int(env, argv[0], &thread_count)) {
    return enif_make_badarg(env);
  }

  erlzmq_context_t * context = enif_alloc_resource(erlzmq_nif_resource_context,
                                                   sizeof(erlzmq_context_t));
  assert(context);
  context->context_zmq = zmq_init(thread_count);
  if (!context->context_zmq) {
    return return_zmq_errno(env, zmq_errno());
  }

  char thread_socket_id[64];
  sprintf(thread_socket_id, "inproc://erlzmq-%ld", (long int) context);
  context->thread_socket = zmq_socket(context->context_zmq, ZMQ_PUSH);
  assert(context->thread_socket);
  context->mutex = enif_mutex_create("erlzmq_context_t_mutex");
  assert(context->mutex);
  if (zmq_bind(context->thread_socket, thread_socket_id)) {
    zmq_close(context->thread_socket);
    enif_mutex_destroy(context->mutex);
    zmq_term(context->context_zmq);
    enif_release_resource(context);
    return return_zmq_errno(env, zmq_errno());
  }
  context->thread_socket_name = strdup(thread_socket_id);
  assert(context->thread_socket_name);
  context->socket_index = 1;

  int const value_errno = enif_thread_create("erlzmq_polling_thread",
                                             &context->polling_tid,
                                             polling_thread, context, NULL);
  if (value_errno) {
    free(context->thread_socket_name);
    zmq_close(context->thread_socket);
    zmq_term(context->context_zmq);
    enif_release_resource(context);
    return return_zmq_errno(env, value_errno);
  }

  return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                          enif_make_resource(env, context));
}

NIF(erlzmq_nif_socket)
{
  erlzmq_context_t * context;
  int socket_type;
  int active;

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_context,
                          (void **) &context)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_int(env, argv[1], &socket_type)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_int(env, argv[2], &active)) {
    return enif_make_badarg(env);
  }
  
  erlzmq_socket_t * socket = enif_alloc_resource(erlzmq_nif_resource_socket,
                                                 sizeof(erlzmq_socket_t));
  assert(socket);
  socket->context = context;
  socket->socket_index = context->socket_index++;
  socket->socket_zmq = zmq_socket(context->context_zmq, socket_type);
  if (!socket->socket_zmq) {
    return return_zmq_errno(env, zmq_errno());
  }
  socket->active = active;
  socket->mutex = enif_mutex_create("erlzmq_socket_t_mutex");
  assert(socket->mutex);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_tuple2(env,
                          enif_make_uint64(env, socket->socket_index),
                          enif_make_resource(env, socket)));
}

NIF(erlzmq_nif_bind)
{
  erlzmq_socket_t * socket;
  unsigned endpoint_length;

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                          (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_list_length(env, argv[1], &endpoint_length)) {
    return enif_make_badarg(env);
  }

  char * endpoint = (char *) malloc(endpoint_length + 1);
  if (! enif_get_string(env, argv[1], endpoint, endpoint_length + 1,
                        ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  enif_mutex_lock(socket->mutex);
  if (zmq_bind(socket->socket_zmq, endpoint)) {
    enif_mutex_unlock(socket->mutex);
    free(endpoint);
    return return_zmq_errno(env, zmq_errno());
  }
  else {
    enif_mutex_unlock(socket->mutex);
    free(endpoint);
    if (socket->active == ERLZMQ_SOCKET_ACTIVE_PENDING) {
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

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                          (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_list_length(env, argv[1], &endpoint_length)) {
    return enif_make_badarg(env);
  }

  char * endpoint = (char *) malloc(endpoint_length + 1);
  if (! enif_get_string(env, argv[1], endpoint, endpoint_length + 1,
                        ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  enif_mutex_lock(socket->mutex);
  if (zmq_connect(socket->socket_zmq, endpoint)) {
    enif_mutex_unlock(socket->mutex);
    free(endpoint);
    return return_zmq_errno(env, zmq_errno());
  }
  else {
    enif_mutex_unlock(socket->mutex);
    free(endpoint);
    if (socket->active == ERLZMQ_SOCKET_ACTIVE_PENDING) {
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

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                          (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_int(env, argv[1], &option_name)) {
    return enif_make_badarg(env);
  }

  ErlNifUInt64 value_uint64;
  ErlNifSInt64 value_int64;
  ErlNifBinary value_binary;
  int value_int;
  void *option_value;
  size_t option_len;
  switch (option_name) {
    // uint64_t
    case ZMQ_HWM:
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
      if (! enif_get_uint64(env, argv[2], &value_uint64)) {
        return enif_make_badarg(env);
      }
      option_value = &value_uint64;
      option_len = sizeof(int64_t);
      break;
    // int64_t
    case ZMQ_SWAP:
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_MCAST_LOOP:
      if (! enif_get_int64(env, argv[2], &value_int64)) {
        return enif_make_badarg(env);
      }
      option_value = &value_int64;
      option_len = sizeof(int64_t);
      break;
    // binary
    case ZMQ_IDENTITY:
    case ZMQ_SUBSCRIBE:
    case ZMQ_UNSUBSCRIBE:
      if (! enif_inspect_iolist_as_binary(env, argv[2], &value_binary)) {
        return enif_make_badarg(env);
      }
      option_value = value_binary.data;
      option_len = value_binary.size;
      break;
    // int
    case ZMQ_LINGER:
    case ZMQ_RECONNECT_IVL:
    case ZMQ_BACKLOG:
      if (! enif_get_int(env, argv[2], &value_int)) {
        return enif_make_badarg(env);
      }
      option_value = &value_int;
      option_len = sizeof(int);
      break;
    default:
      return enif_make_badarg(env);
  }

  enif_mutex_lock(socket->mutex);
  if (zmq_setsockopt(socket->socket_zmq, option_name,
                     option_value, option_len)) {
    enif_mutex_unlock(socket->mutex);
    return return_zmq_errno(env, zmq_errno());
  }
  else {
    enif_mutex_unlock(socket->mutex);
    return enif_make_atom(env, "ok");
  }
}

NIF(erlzmq_nif_getsockopt)
{
  erlzmq_socket_t * socket;
  int option_name;

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                          (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_int(env, argv[1], &option_name)) {
    return enif_make_badarg(env);
  }

  ErlNifBinary value_binary;
  int64_t value_int64;
  int64_t value_uint64;
  char option_value[256];
  int value_int;
  size_t option_len;
  switch(option_name) {
    // int64_t
    case ZMQ_RCVMORE:
    case ZMQ_SWAP:
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_RECOVERY_IVL_MSEC:
    case ZMQ_MCAST_LOOP:
      option_len = sizeof(value_int64);
      enif_mutex_lock(socket->mutex);
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         &value_int64, &option_len)) {
        enif_mutex_unlock(socket->mutex);
        return return_zmq_errno(env, zmq_errno());
      }
      enif_mutex_unlock(socket->mutex);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_int64(env, value_int64));
    // uint64_t
    case ZMQ_HWM:
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
      option_len = sizeof(value_uint64);
      enif_mutex_lock(socket->mutex);
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         &value_uint64, &option_len)) {
        enif_mutex_unlock(socket->mutex);
        return return_zmq_errno(env, zmq_errno());
      }
      enif_mutex_unlock(socket->mutex);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_uint64(env, value_uint64));
    // binary
    case ZMQ_IDENTITY:
      option_len = sizeof(option_value);
      enif_mutex_lock(socket->mutex);
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         option_value, &option_len)) {
        enif_mutex_unlock(socket->mutex);
        return return_zmq_errno(env, zmq_errno());
      }
      enif_mutex_unlock(socket->mutex);
      enif_alloc_binary(option_len, &value_binary);
      memcpy(value_binary.data, option_value, option_len);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_binary(env, &value_binary));
    // int
    case ZMQ_TYPE:
    case ZMQ_LINGER:
    case ZMQ_RECONNECT_IVL:
    case ZMQ_RECONNECT_IVL_MAX:
    case ZMQ_BACKLOG:
    case ZMQ_FD:   // FIXME: ZMQ_FD returns SOCKET on Windows
      option_len = sizeof(value_int);
      enif_mutex_lock(socket->mutex);
      if (zmq_getsockopt(socket->socket_zmq, option_name,
                         &value_int, &option_len)) {
        enif_mutex_unlock(socket->mutex);
        return return_zmq_errno(env, zmq_errno());
      }
      enif_mutex_unlock(socket->mutex);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                              enif_make_int(env, value_int));
    default:
      return enif_make_badarg(env);
  }
}

NIF(erlzmq_nif_send)
{
  erlzmq_thread_request_t req;
  erlzmq_socket_t * socket;
  ErlNifBinary binary;

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                          (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (! enif_inspect_iolist_as_binary(env, argv[1], &binary)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_int(env, argv[2], &req.data.send.flags)) {
    return enif_make_badarg(env);
  }

  if (zmq_msg_init_size(&req.data.send.msg, binary.size)) {
    return return_zmq_errno(env, zmq_errno());
  }

  memcpy(zmq_msg_data(&req.data.send.msg), binary.data, binary.size);

  int polling_thread_send = 1;
  if (! socket->active) {
    enif_mutex_lock(socket->mutex);
    if (zmq_send(socket->socket_zmq, &req.data.send.msg,
                 req.data.send.flags | ZMQ_NOBLOCK)) {
      enif_mutex_unlock(socket->mutex);
      int const error = zmq_errno();
      if (error != EAGAIN ||
          (error == EAGAIN && (req.data.send.flags & ZMQ_NOBLOCK))) {
        zmq_msg_close(&req.data.send.msg);
        return return_zmq_errno(env, error);
      }
    }
    else {
      enif_mutex_unlock(socket->mutex);
      polling_thread_send = 0;
    }
  }
  
  if (polling_thread_send) {
    req.type = ERLZMQ_THREAD_REQUEST_SEND;
    req.data.send.env = enif_alloc_env();
    req.data.send.ref = enif_make_ref(req.data.send.env);
    enif_self(env, &req.data.send.pid);
    req.data.send.socket = socket;

    zmq_msg_t msg;
    if (zmq_msg_init_size(&msg, sizeof(erlzmq_thread_request_t))) {
      zmq_msg_close(&req.data.send.msg);
      enif_free_env(req.data.send.env);
      return return_zmq_errno(env, zmq_errno());
    }

    memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_thread_request_t));

    enif_mutex_lock(socket->context->mutex);
    if (socket->context->thread_socket_name == NULL) {
      enif_mutex_unlock(socket->context->mutex);
      return return_zmq_errno(env, ETERM);
    }
    if (zmq_send(socket->context->thread_socket, &msg, 0)) {
      enif_mutex_unlock(socket->context->mutex);

      zmq_msg_close(&msg);
      zmq_msg_close(&req.data.send.msg);
      enif_free_env(req.data.send.env);
      return return_zmq_errno(env, zmq_errno());
    }
    else {
      enif_mutex_unlock(socket->context->mutex);

      zmq_msg_close(&msg);
      // each pointer to the socket in a request increments the reference
      enif_keep_resource(socket);
  
      return enif_make_copy(env, req.data.send.ref);
    }
  }
  else {
    zmq_msg_close(&req.data.send.msg);

    return enif_make_atom(env, "ok");
  }
}

NIF(erlzmq_nif_recv)
{
  erlzmq_thread_request_t req;
  erlzmq_socket_t * socket;

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                          (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (! enif_get_int(env, argv[1], &req.data.recv.flags)) {
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
  enif_mutex_lock(socket->mutex);
  if (zmq_recv(socket->socket_zmq, &msg, ZMQ_NOBLOCK)) {
    enif_mutex_unlock(socket->mutex);
    zmq_msg_close(&msg);

    int const error = zmq_errno();
    if (error != EAGAIN ||
        (error == EAGAIN && (req.data.recv.flags & ZMQ_NOBLOCK))) {
      return return_zmq_errno(env, error);
    }
    req.type = ERLZMQ_THREAD_REQUEST_RECV;
    req.data.recv.env = enif_alloc_env();
    req.data.recv.ref = enif_make_ref(req.data.recv.env);
    enif_self(env, &req.data.recv.pid);
    req.data.recv.socket = socket;

    if (zmq_msg_init_size(&msg, sizeof(erlzmq_thread_request_t))) {
      enif_free_env(req.data.recv.env);
      return return_zmq_errno(env, zmq_errno());
    }

    memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_thread_request_t));

    enif_mutex_lock(socket->context->mutex);
    if (socket->context->thread_socket_name == NULL) {
      enif_mutex_unlock(socket->context->mutex);
      return return_zmq_errno(env, ETERM);
    }
    if (zmq_send(socket->context->thread_socket, &msg, 0)) {
      enif_mutex_unlock(socket->context->mutex);

      zmq_msg_close(&msg);
      enif_free_env(req.data.recv.env);
      return return_zmq_errno(env, zmq_errno());
    }
    else {
      enif_mutex_unlock(socket->context->mutex);

      zmq_msg_close(&msg);
      // each pointer to the socket in a request increments the reference
      enif_keep_resource(socket);
  
      return enif_make_copy(env, req.data.recv.ref);
    }
  }
  else {
    enif_mutex_unlock(socket->mutex);
    
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

  if (! enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                          (void **) &socket)) {
    return enif_make_badarg(env);
  }

  erlzmq_thread_request_t req;
  req.type = ERLZMQ_THREAD_REQUEST_CLOSE;
  req.data.close.env = enif_alloc_env();
  req.data.close.ref = enif_make_ref(req.data.close.env);
  enif_self(env, &req.data.close.pid);
  req.data.close.socket = socket;

  zmq_msg_t msg;
  if (zmq_msg_init_size(&msg, sizeof(erlzmq_thread_request_t))) {
    enif_free_env(req.data.close.env);
    return return_zmq_errno(env, zmq_errno());
  }

  memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_thread_request_t));

  enif_mutex_lock(socket->context->mutex);
  if (socket->context->thread_socket_name == NULL) {
    // context is gone
    enif_mutex_lock(socket->mutex);
    zmq_msg_close(&msg);
    zmq_close(socket->socket_zmq);
    enif_mutex_unlock(socket->mutex);
    enif_mutex_destroy(socket->mutex);
    enif_release_resource(socket);
    enif_mutex_unlock(socket->context->mutex);
    return enif_make_atom(env, "ok");
  }
  if (zmq_send(socket->context->thread_socket, &msg, 0)) {
    enif_mutex_unlock(socket->context->mutex);
    zmq_msg_close(&msg);
    enif_free_env(req.data.close.env);
    return return_zmq_errno(env, zmq_errno());
  }
  else {
    enif_mutex_unlock(socket->context->mutex);
    zmq_msg_close(&msg);
    // each pointer to the socket in a request increments the reference
    enif_keep_resource(socket);
    return enif_make_copy(env, req.data.close.ref);
  }
}

NIF(erlzmq_nif_term)
{
  erlzmq_context_t * context;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_context,
                         (void **) &context)) {
    return enif_make_badarg(env);
  }

  erlzmq_thread_request_t req;
  req.type = ERLZMQ_THREAD_REQUEST_TERM;
  req.data.term.env = enif_alloc_env();
  req.data.term.ref = enif_make_ref(req.data.term.env);
  enif_self(env, &req.data.term.pid);

  zmq_msg_t msg;
  if (zmq_msg_init_size(&msg, sizeof(erlzmq_thread_request_t))) {
    enif_free_env(req.data.term.env);
    return return_zmq_errno(env, zmq_errno());
  }

  memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_thread_request_t));

  enif_mutex_lock(context->mutex);
  if (zmq_send(context->thread_socket, &msg, 0)) {
    enif_mutex_unlock(context->mutex);
    zmq_msg_close(&msg);
    enif_free_env(req.data.term.env);
    return return_zmq_errno(env, zmq_errno());
  }
  else {
    enif_mutex_unlock(context->mutex);
    zmq_msg_close(&msg);
    // thread has a reference to the context, decrement here
    enif_release_resource(context);
    return enif_make_copy(env, req.data.term.ref);
  }
}

NIF(erlzmq_nif_version)
{
  int major, minor, patch;
  zmq_version(&major, &minor, &patch);
  return enif_make_tuple3(env, enif_make_int(env, major),
                          enif_make_int(env, minor),
                          enif_make_int(env, patch));
}

static void * polling_thread(void * handle)
{
  erlzmq_context_t * context = (erlzmq_context_t *) handle;
  enif_keep_resource(context);

  void * thread_socket = zmq_socket(context->context_zmq, ZMQ_PULL);
  assert(thread_socket);
  int status = zmq_connect(thread_socket, context->thread_socket_name);
  assert(status == 0);

  vector_t items_zmq;
  status = vector_initialize_pow2(zmq_pollitem_t, &items_zmq, 1,
                                  ERLZMQ_MAX_CONCURRENT_REQUESTS);
  assert(status == 0);
  zmq_pollitem_t thread_socket_poll_zmq = {thread_socket, 0, ZMQ_POLLIN, 0};
  status = vector_append(zmq_pollitem_t, &items_zmq, &thread_socket_poll_zmq);
  assert(status == 0);

  vector_t requests;
  status = vector_initialize_pow2(erlzmq_thread_request_t, &requests, 1,
                                  ERLZMQ_MAX_CONCURRENT_REQUESTS);
  assert(status == 0);
  erlzmq_thread_request_t request_empty;
  memset(&request_empty, 0, sizeof(erlzmq_thread_request_t));
  status = vector_append(erlzmq_thread_request_t, &requests, &request_empty);
  assert(status == 0);

  int i;
  for (;;) {
    int count = zmq_poll(vector_p(zmq_pollitem_t, &items_zmq),
                         vector_count(&items_zmq), -1);
    assert(count != -1);
    if (vector_get(zmq_pollitem_t, &items_zmq, 0)->revents & ZMQ_POLLIN) {
      --count;
    }
    for (i = 1; i < vector_count(&items_zmq); ++i) {
      zmq_pollitem_t * item = vector_get(zmq_pollitem_t, &items_zmq, i);
      erlzmq_thread_request_t * r = vector_get(erlzmq_thread_request_t,
                                               &requests, i);
      if (item->revents & ZMQ_POLLIN) {
        size_t value_len = sizeof(int64_t);
        int64_t flag_value = 0;
        
        assert(r->type == ERLZMQ_THREAD_REQUEST_RECV);
        --count;

        zmq_msg_t msg;
        zmq_msg_init(&msg);
        enif_mutex_lock(r->data.recv.socket->mutex);
        if (zmq_recv(r->data.recv.socket->socket_zmq, &msg,
                     r->data.recv.flags) ||
            (r->data.recv.socket->active == ERLZMQ_SOCKET_ACTIVE_ON && 
            zmq_getsockopt(r->data.recv.socket->socket_zmq, 
                    ZMQ_RCVMORE, &flag_value, &value_len)) )
        {
          enif_mutex_unlock(r->data.recv.socket->mutex);
          if (r->data.recv.socket->active == ERLZMQ_SOCKET_ACTIVE_ON) {
            enif_send(NULL, &r->data.recv.pid, r->data.recv.env,
              enif_make_tuple3(r->data.recv.env,
                enif_make_atom(r->data.recv.env, "zmq"),
                enif_make_tuple2(r->data.recv.env,
                  enif_make_uint64(r->data.recv.env,
                                   r->data.recv.socket->socket_index),
                  enif_make_resource(r->data.recv.env, r->data.recv.socket)),
                return_zmq_errno(r->data.recv.env, zmq_errno())));
            enif_free_env(r->data.recv.env);
            r->data.recv.env = enif_alloc_env();
            item->revents = 0;
          }
          else {
            assert(0);
          }
        }
        else {
          enif_mutex_unlock(r->data.recv.socket->mutex);
        }

        ErlNifBinary binary;
        enif_alloc_binary(zmq_msg_size(&msg), &binary);
        memcpy(binary.data, zmq_msg_data(&msg), zmq_msg_size(&msg));
        zmq_msg_close(&msg);

        if (r->data.recv.socket->active == ERLZMQ_SOCKET_ACTIVE_ON) {
          ERL_NIF_TERM flags_list;
          
          // Should we send the multipart flag
          if(flag_value == 1) {
            flags_list = enif_make_list1(r->data.recv.env, enif_make_atom(r->data.recv.env, "rcvmore"));
          } else {
            flags_list = enif_make_list(r->data.recv.env, 0);
          }
          
          enif_send(NULL, &r->data.recv.pid, r->data.recv.env,
            enif_make_tuple4(r->data.recv.env,
              enif_make_atom(r->data.recv.env, "zmq"),
              enif_make_tuple2(r->data.recv.env,
                enif_make_uint64(r->data.recv.env,
                                 r->data.recv.socket->socket_index),
                enif_make_resource(r->data.recv.env, r->data.recv.socket)),
              enif_make_binary(r->data.recv.env, &binary),
              flags_list));
          enif_free_env(r->data.recv.env);
          r->data.recv.env = enif_alloc_env();
          item->revents = 0;
        }
        else {
          enif_send(NULL, &r->data.recv.pid, r->data.recv.env,
            enif_make_tuple2(r->data.recv.env,
              enif_make_copy(r->data.recv.env, r->data.recv.ref),
              enif_make_binary(r->data.recv.env, &binary)));

          enif_free_env(r->data.recv.env);
          enif_release_resource(r->data.recv.socket);

          status = vector_remove(&items_zmq, i);
          assert(status == 0);
          status = vector_remove(&requests, i);
          assert(status == 0);
          --i;
        }
      }
      else if (item->revents & ZMQ_POLLOUT) {
        assert(r->type == ERLZMQ_THREAD_REQUEST_SEND);
        --count;

        enif_mutex_lock(r->data.send.socket->mutex);
        if (zmq_send(r->data.send.socket->socket_zmq,
                     &r->data.send.msg, r->data.send.flags)) {
          enif_mutex_unlock(r->data.send.socket->mutex);
          enif_send(NULL, &r->data.send.pid, r->data.send.env,
            enif_make_tuple2(r->data.send.env,
              enif_make_copy(r->data.send.env, r->data.send.ref),
              return_zmq_errno(r->data.send.env, zmq_errno())));
        } else {
          enif_mutex_unlock(r->data.send.socket->mutex);
          enif_send(NULL, &r->data.send.pid, r->data.send.env,
            enif_make_tuple2(r->data.send.env,
              enif_make_copy(r->data.send.env, r->data.send.ref),
              enif_make_atom(r->data.send.env, "ok")));
        }
        zmq_msg_close(&r->data.send.msg);
        enif_free_env(r->data.send.env);
        enif_release_resource(r->data.send.socket);

        status = vector_remove(&items_zmq, i);
        assert(status == 0);
        status = vector_remove(&requests, i);
        assert(status == 0);
        --i;
      }
    }

    if (vector_get(zmq_pollitem_t, &items_zmq, 0)->revents & ZMQ_POLLIN) {
      vector_get(zmq_pollitem_t, &items_zmq, 0)->revents = 0;
      zmq_msg_t msg;
      zmq_msg_init(&msg);
      enif_mutex_lock(context->mutex);
      status = zmq_recv(thread_socket, &msg, 0);
      enif_mutex_unlock(context->mutex);
      assert(status == 0);

      assert(zmq_msg_size(&msg) == sizeof(erlzmq_thread_request_t));

      erlzmq_thread_request_t * r =
        (erlzmq_thread_request_t *) zmq_msg_data(&msg);
      if (r->type == ERLZMQ_THREAD_REQUEST_SEND) {
        zmq_pollitem_t item_zmq = {r->data.send.socket->socket_zmq,
                                   0, ZMQ_POLLOUT, 0};
        status = vector_append(zmq_pollitem_t, &items_zmq, &item_zmq);
        assert(status == 0);
        status = vector_append(erlzmq_thread_request_t, &requests, r);
        assert(status == 0);
        zmq_msg_close(&msg);
      }
      else if (r->type == ERLZMQ_THREAD_REQUEST_RECV) {
        zmq_pollitem_t item_zmq = {r->data.recv.socket->socket_zmq,
                                   0, ZMQ_POLLIN, 0};
        status = vector_append(zmq_pollitem_t, &items_zmq, &item_zmq);
        assert(status == 0);
        status = vector_append(erlzmq_thread_request_t, &requests, r);
        assert(status == 0);
        zmq_msg_close(&msg);
      }
      else if (r->type == ERLZMQ_THREAD_REQUEST_CLOSE) {
        // remove all entries with this socket
        for (i = vector_count(&items_zmq) - 1; i > 0; --i) {
          zmq_pollitem_t * item = vector_get(zmq_pollitem_t, &items_zmq, i);
          if (item->socket == r->data.close.socket->socket_zmq) {
            erlzmq_thread_request_t * r_old =
              vector_get(erlzmq_thread_request_t, &requests, i);
            if (r_old->type == ERLZMQ_THREAD_REQUEST_RECV) {
              enif_clear_env(r_old->data.recv.env);
              // FIXME
              // causes crash on R14B01, works fine on R14B02
              // (repeated enif_send with active receive broken on R14B01)
              //enif_free_env(r_old->data.recv.env);
              enif_release_resource(r_old->data.recv.socket);
            }
            else if (r_old->type == ERLZMQ_THREAD_REQUEST_SEND) {
              zmq_msg_close(&(r_old->data.send.msg));
              enif_free_env(r_old->data.send.env);
              enif_release_resource(r_old->data.send.socket);
            }
            else {
              assert(0);
            }
            status = vector_remove(&items_zmq, i);
            assert(status == 0);
            status = vector_remove(&requests, i);
            assert(status == 0);
          }
        }
        // close the socket
        enif_mutex_lock(r->data.close.socket->mutex);
        zmq_close(r->data.close.socket->socket_zmq);
        enif_mutex_unlock(r->data.close.socket->mutex);
        enif_mutex_destroy(r->data.close.socket->mutex);
        enif_release_resource(r->data.close.socket);
        // notify the waiting request
        enif_send(NULL, &r->data.close.pid, r->data.close.env,
          enif_make_tuple2(r->data.close.env,
            enif_make_copy(r->data.close.env, r->data.close.ref),
            enif_make_atom(r->data.close.env, "ok")));
        enif_free_env(r->data.close.env);
        zmq_msg_close(&msg);
      }
      else if (r->type == ERLZMQ_THREAD_REQUEST_TERM) {
        enif_mutex_lock(context->mutex);
        free(context->thread_socket_name);
        // use this to flag context is over
        context->thread_socket_name = NULL;
        enif_mutex_unlock(context->mutex);
        // cleanup pending requests
        for (i = 1; i < vector_count(&requests); ++i) {
          erlzmq_thread_request_t * r_old = vector_get(erlzmq_thread_request_t,
                                                       &requests, i);
          if (r_old->type == ERLZMQ_THREAD_REQUEST_RECV) {
            enif_free_env(r_old->data.recv.env);
            enif_release_resource(r_old->data.recv.socket);
            zmq_close(r_old->data.recv.socket->socket_zmq);
          }
          else if (r_old->type == ERLZMQ_THREAD_REQUEST_SEND) {
            zmq_msg_close(&r_old->data.send.msg);
            enif_free_env(r_old->data.send.env);
            enif_release_resource(r_old->data.send.socket);
            zmq_close(r_old->data.send.socket->socket_zmq);
          }
        }
        // terminate the context
        enif_mutex_lock(context->mutex);
        zmq_close(thread_socket);
        zmq_close(context->thread_socket);
        enif_mutex_unlock(context->mutex);
        zmq_term(context->context_zmq);
        enif_mutex_lock(context->mutex);
        enif_mutex_unlock(context->mutex);
        enif_mutex_destroy(context->mutex);
        enif_release_resource(context);
        // notify the waiting request
        enif_send(NULL, &r->data.term.pid, r->data.term.env,
          enif_make_tuple2(r->data.term.env,
            enif_make_copy(r->data.term.env, r->data.term.ref),
            enif_make_atom(r->data.term.env, "ok")));
        enif_free_env(r->data.term.env);
        zmq_msg_close(&msg);
        vector_destroy(&items_zmq);
        vector_destroy(&requests);
        return NULL;
      }
      else {
        assert(0);
      }
    }
  }
  return NULL;
}

static ERL_NIF_TERM add_active_req(ErlNifEnv* env, erlzmq_socket_t * socket)
{
  socket->active = ERLZMQ_SOCKET_ACTIVE_ON;

  erlzmq_thread_request_t req;
  req.type = ERLZMQ_THREAD_REQUEST_RECV;
  req.data.recv.env = enif_alloc_env();
  req.data.recv.flags = 0;
  enif_self(env, &req.data.recv.pid);
  req.data.recv.socket = socket;

  zmq_msg_t msg;
  if (zmq_msg_init_size(&msg, sizeof(erlzmq_thread_request_t))) {
    enif_free_env(req.data.recv.env);
    return return_zmq_errno(env, zmq_errno());
  }

  memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_thread_request_t));

  if (zmq_send(socket->context->thread_socket, &msg, 0)) {
    zmq_msg_close(&msg);
    enif_free_env(req.data.recv.env);
    return return_zmq_errno(env, zmq_errno());
  }
  else {
    zmq_msg_close(&msg);
    // each pointer to the socket in a request increments the reference
    enif_keep_resource(socket);
    return enif_make_atom(env, "ok");
  }
}

static ERL_NIF_TERM return_zmq_errno(ErlNifEnv* env, int const value)
{
  switch (value) {
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

