#include "zmq.h"
#include "erl_nif.h"
#include <string.h>
#include <sys/queue.h>
#include <stdio.h>

static ErlNifResourceType* erlzmq_nif_resource_context;
static ErlNifResourceType* erlzmq_nif_resource_socket;

typedef struct _erlzmq_context {
  void * context;
  void * ipc_socket;
  char * ipc_socket_name;
  int running;
  ErlNifCond * cond;
  ErlNifMutex * mutex;
  ErlNifTid polling_tid;
} erlzmq_context;

typedef struct _erlzmq_socket {
  void * socket;
  erlzmq_context * context;
} erlzmq_socket;

#define erlzmq_TERM 1211981

typedef struct _erlzmq_ipc_request {
  ErlNifEnv * env;
  ERL_NIF_TERM ref;
  int flags;
  int poll_flag;
  zmq_msg_t msg;
  ErlNifPid pid;
  void * socket;
  TAILQ_ENTRY(_erlzmq_ipc_request) requests;
} erlzmq_ipc_request;
TAILQ_HEAD(requests_head, _erlzmq_ipc_request);

// Prototypes
#define NIF(name) ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

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

static ErlNifFunc nif_funcs[] =
{
  {"context", 1, erlzmq_nif_context},
  {"socket", 2, erlzmq_nif_socket},
  {"bind", 2, erlzmq_nif_bind},
  {"connect", 2, erlzmq_nif_connect},
  {"setsockopt", 3, erlzmq_nif_setsockopt},
  {"getsockopt", 2, erlzmq_nif_getsockopt},
  {"send", 3, erlzmq_nif_send},
  {"recv", 2, erlzmq_nif_recv},
  {"close", 1, erlzmq_nif_close},
  {"term", 1, erlzmq_nif_term}
};

void * polling_thread(void * handle);
NIF(erlzmq_nif_context)
{
  int _threads;

  if (!enif_get_int(env, argv[0], &_threads)) {
    return enif_make_badarg(env);
  }

  erlzmq_context * handle = enif_alloc_resource(erlzmq_nif_resource_context,
                                     sizeof(erlzmq_context));

  handle->context = zmq_init(_threads);

  char socket_id[64];
  sprintf(socket_id, "inproc://erlzmq-%ld", (long int) handle);
  handle->ipc_socket_name = strdup(socket_id);

  handle->ipc_socket = zmq_socket(handle->context, ZMQ_PUSH);
  zmq_bind(handle->ipc_socket,socket_id);

  handle->running = 0;
  handle->mutex = enif_mutex_create("erlzmq_context_mutex");
  handle->cond = enif_cond_create("erlzmq_context_cond");

  enif_mutex_lock(handle->mutex);
  int err;
  if ((err = enif_thread_create("erlzmq_polling_thread", &handle->polling_tid,
                                polling_thread, handle, NULL))) {
    enif_mutex_unlock(handle->mutex);
    enif_mutex_destroy(handle->mutex);
    enif_cond_destroy(handle->cond);
    zmq_close(handle->ipc_socket);
    free(handle->ipc_socket_name);
    zmq_term(handle->context);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                 enif_make_int(env, err));
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
  erlzmq_context * ctx;
  int _type;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_context, (void **) &ctx)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_type)) {
    return enif_make_badarg(env);
  }
  
  if (!ctx->running) {
    return enif_make_badarg(env);
  }

  erlzmq_socket * handle = enif_alloc_resource(erlzmq_nif_resource_socket,
                                             sizeof(erlzmq_socket));

  handle->context = ctx;
  handle->socket = zmq_socket(ctx->context, _type);

  ERL_NIF_TERM result = enif_make_resource(env, handle);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}

NIF(erlzmq_nif_bind)
{
  erlzmq_socket * socket;
  unsigned _endpoint_length;
  char * _endpoint;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_list_length(env, argv[1], &_endpoint_length)) {
    return enif_make_badarg(env);
  }

  _endpoint = (char *) malloc(_endpoint_length + 1);

  if (!enif_get_string(env, argv[1], _endpoint, _endpoint_length + 1, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  int error;

  if (!(error = zmq_bind(socket->socket, _endpoint))) {
    free(_endpoint);
    return enif_make_atom(env, "ok");
  } else {
    free(_endpoint);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }
}

NIF(erlzmq_nif_connect)
{
  erlzmq_socket * socket;
  unsigned _endpoint_length;
  char * _endpoint;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket, (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_list_length(env, argv[1], &_endpoint_length)) {
    return enif_make_badarg(env);
  }

  _endpoint = (char *) malloc(_endpoint_length + 1);

  if (!enif_get_string(env, argv[1], _endpoint, _endpoint_length + 1, ERL_NIF_LATIN1)) {
    return enif_make_badarg(env);
  }

  int error;

  if (!(error = zmq_connect(socket->socket, _endpoint))) {
    free(_endpoint);
    return enif_make_atom(env, "ok");
  } else {
    free(_endpoint);
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_int(env, zmq_errno()));
  }
}

NIF(erlzmq_nif_setsockopt)
{
  erlzmq_socket * socket;
  int _option_name;
  ErlNifUInt64 _uint64;
  ErlNifSInt64 _int64;
  ErlNifBinary _bin;
  int _int;
  void *_option_value;
  size_t _option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_option_name)) {
    return enif_make_badarg(env);
  }

  switch (_option_name) {
    case ZMQ_HWM: // uint64_t
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
        if (!enif_get_uint64(env, argv[2], &_uint64)) {
            goto badarg;
        }
        _option_value = &_uint64;
        break;
    case ZMQ_SWAP: // int64_t
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_MCAST_LOOP:
        if (!enif_get_int64(env, argv[2], &_int64)) {
            goto badarg;
        }
        _option_value = &_int64;
        break;
    case ZMQ_IDENTITY: // binary
    case ZMQ_SUBSCRIBE:
    case ZMQ_UNSUBSCRIBE:
        if (!enif_inspect_iolist_as_binary(env, argv[2], &_bin)) {
            goto badarg;
        }
        _option_value = _bin.data;
        _option_len = _bin.size;
        break;
    case ZMQ_LINGER: // int
    case ZMQ_RECONNECT_IVL:
    case ZMQ_BACKLOG:
        if (!enif_get_int(env, argv[1], &_int)) {
            goto badarg;
        }
        _option_value = &_int;
        _option_len = sizeof(int);
        break;
    default:
        goto badarg;
  }

  if (zmq_setsockopt(socket->socket, _option_name,
                     _option_value, _option_len)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                 enif_make_int(env, zmq_errno()));
  }
  return enif_make_atom(env, "ok");

badarg:
  return enif_make_badarg(env);
}

NIF(erlzmq_nif_getsockopt)
{
  erlzmq_socket * socket;
  int _option_name;
  ErlNifBinary _bin;
  int64_t _option_value_64;
  int64_t _option_value_u64;
  char _option_value[255];
  int _option_value_int;
  size_t _option_len = 8; // 64 bit

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &_option_name)) {
    return enif_make_badarg(env);
  }

  switch(_option_name) {
    case ZMQ_RCVMORE: // int64_t
    case ZMQ_SWAP:
    case ZMQ_RATE:
    case ZMQ_RECOVERY_IVL:
    case ZMQ_RECOVERY_IVL_MSEC:
    case ZMQ_MCAST_LOOP:
      if (zmq_getsockopt(socket->socket, _option_name,
                         &_option_value_64, &_option_len)) {
        goto error;
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_int64(env, _option_value_64));
    case ZMQ_HWM: // uint64_t
    case ZMQ_AFFINITY:
    case ZMQ_SNDBUF:
    case ZMQ_RCVBUF:
      if (zmq_getsockopt(socket->socket, _option_name,
                         &_option_value_u64, &_option_len)) {
        goto error;
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_uint64(env, _option_value_u64));
    case ZMQ_IDENTITY: // binary
      if (zmq_getsockopt(socket->socket, _option_name,
                         _option_value, &_option_len)) {
        goto error;
      }
      enif_alloc_binary(_option_len, &_bin);
      memcpy(_bin.data, _option_value, _option_len);
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_binary(env, &_bin));
    case ZMQ_TYPE: // int
    case ZMQ_LINGER:
    case ZMQ_RECONNECT_IVL:
    case ZMQ_RECONNECT_IVL_MAX:
    case ZMQ_BACKLOG:
    case ZMQ_FD: // FIXME: ZMQ_FD returns SOCKET on Windows
      if (zmq_getsockopt(socket->socket, _option_name,
                         &_option_value_int, &_option_len)) {
        goto error;
      }
      return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                                   enif_make_int(env, _option_value_int));
    default:
      return enif_make_badarg(env);
  }
error:
  return enif_make_tuple2(env, enif_make_atom(env, "error"),
                               enif_make_int(env, zmq_errno()));
}

NIF(erlzmq_nif_send)
{
  erlzmq_ipc_request req;
  erlzmq_socket * socket;
  ErlNifBinary _bin;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_inspect_iolist_as_binary(env, argv[1], &_bin)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[2], &req.flags)) {
    return enif_make_badarg(env);
  }

  enif_self(env, &req.pid);

  int error;

  if (zmq_msg_init_size(&req.msg, _bin.size)) {
    goto errno_out;
  }

  memcpy(zmq_msg_data(&req.msg), _bin.data, _bin.size);

  if (zmq_send(socket->socket, &req.msg, req.flags|ZMQ_NOBLOCK)) {
    error = zmq_errno();
    if (error != EAGAIN || (error == EAGAIN && (req.flags & ZMQ_NOBLOCK))) {
      zmq_msg_close(&req.msg);
      goto out;
    }
    zmq_msg_t msg;
    req.poll_flag = ZMQ_POLLOUT;
    req.env = enif_alloc_env();
    req.ref = enif_make_ref(req.env);
    req.socket = socket->socket;

    if (zmq_msg_init_size(&msg, sizeof(erlzmq_ipc_request))) {
      goto q_err;
    }

    memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_ipc_request));

    if (zmq_send(socket->context->ipc_socket, &msg, 0)) {
      zmq_msg_close(&msg);
      goto q_err;
    }

    zmq_msg_close(&msg);

    return enif_make_copy(env, req.ref);
q_err:
    zmq_msg_close(&req.msg);
    enif_free_env(req.env);
    goto errno_out;
  }

  zmq_msg_close(&req.msg);

  return enif_make_atom(env, "ok");

errno_out:
  error = zmq_errno();
out:
  return enif_make_tuple2(env, enif_make_atom(env, "error"),
                               enif_make_int(env, error));
}

NIF(erlzmq_nif_recv)
{

  erlzmq_ipc_request req;
  erlzmq_socket * socket;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[1], &req.flags)) {
    return enif_make_badarg(env);
  }

  enif_self(env, &req.pid);

  int error;
  zmq_msg_t msg;

  if (zmq_msg_init(&msg)) {
    goto errno_out;
  }

  // try recv with noblock
  if (zmq_recv(socket->socket, &msg, ZMQ_NOBLOCK)) {
    error = zmq_errno();
    if (error != EAGAIN || (error == EAGAIN && (req.flags & ZMQ_NOBLOCK))) {
        goto out;
    }
    req.poll_flag = ZMQ_POLLIN;
    req.env = enif_alloc_env();
    req.ref = enif_make_ref(req.env);
    req.socket = socket->socket;

    if (zmq_msg_init_size(&msg, sizeof(erlzmq_ipc_request))) {
      goto q_err;
    }

    memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_ipc_request));

    if (zmq_send(socket->context->ipc_socket, &msg, 0)) {
      zmq_msg_close(&msg);
      goto q_err;
    }

    zmq_msg_close(&msg);

    return enif_make_copy(env, req.ref);
q_err:
    enif_free_env(req.env);
    goto errno_out;
  }
  ErlNifBinary bin;
  enif_alloc_binary(zmq_msg_size(&msg), &bin);
  memcpy(bin.data, zmq_msg_data(&msg), zmq_msg_size(&msg));

  zmq_msg_close(&msg);

  return enif_make_tuple2(env, enif_make_atom(env, "ok"),
                               enif_make_binary(env, &bin));
errno_out:
  error = zmq_errno();
out:
  return enif_make_tuple2(env, enif_make_atom(env, "error"),
                               enif_make_int(env, error));
}

void * polling_thread(void * handle)
{
  erlzmq_context * ctx = (erlzmq_context *) handle;
  ErlNifEnv * final_env = enif_alloc_env();
  ERL_NIF_TERM final_ref;
  ERL_NIF_TERM final_pid;

  struct requests_head * requests_queue;
  requests_queue = malloc(sizeof(struct requests_head));
  TAILQ_INIT(requests_queue);

  void *ipc_socket = zmq_socket(ctx->context, ZMQ_PULL);
  zmq_connect(ipc_socket,ctx->ipc_socket_name);

  int nwatched = 1;
  enif_mutex_lock(ctx->mutex);
  ctx->running = 1;
  enif_cond_signal(ctx->cond);
  enif_mutex_unlock(ctx->mutex);

  while (ctx->running) {
    int i;
    zmq_msg_t msg;
    erlzmq_ipc_request *r, *rtmp;

    zmq_pollitem_t *items = calloc(nwatched, sizeof(zmq_pollitem_t));
    items[0].socket = ipc_socket;
    items[0].events = ZMQ_POLLIN;

    for (i = 1, r = requests_queue->tqh_first;
         r != NULL; r = r->requests.tqe_next, i++) {
      items[i].socket = r->socket;
      items[i].events = r->poll_flag;
    }

    zmq_poll(items, nwatched, -1);

    for (i = 1, r = requests_queue->tqh_first;
         r && ((rtmp = r->requests.tqe_next), 1); r = rtmp, i++) {
      if (items[i].revents & ZMQ_POLLIN) {
        nwatched--;
        ErlNifBinary bin;

        zmq_msg_init(&msg);
        zmq_recv(r->socket, &msg, r->flags);

        enif_alloc_binary(zmq_msg_size(&msg), &bin);
        memcpy(bin.data, zmq_msg_data(&msg), zmq_msg_size(&msg));

        zmq_msg_close(&msg);

        enif_send(NULL, &r->pid, r->env,
                  enif_make_tuple2(r->env,
                                   enif_make_copy(r->env, r->ref),
                                   enif_make_binary(r->env, &bin)));
        enif_free_env(r->env);
        TAILQ_REMOVE(requests_queue, r, requests);
        free(r);
      }
      if (items[i].revents & ZMQ_POLLOUT) {
        nwatched--;
        if (zmq_send(r->socket, &r->msg, r->flags)) {
            enif_send(NULL, &r->pid, r->env,
                      enif_make_tuple3(r->env,
                                       enif_make_copy(r->env, r->ref),
                                       enif_make_atom(r->env, "error"),
                                       enif_make_int(r->env, zmq_errno())));
        } else {
            enif_send(NULL, &r->pid, r->env,
                      enif_make_tuple2(r->env,
                                       enif_make_copy(r->env, r->ref),
                                       enif_make_atom(r->env, "ok")));
        }
        zmq_msg_close(&r->msg);
        enif_free_env(r->env);
        TAILQ_REMOVE(requests_queue, r, requests);
        free(r);
      }
    }
    if (items[0].revents & ZMQ_POLLIN) {
      zmq_msg_init(&msg);
      if (!zmq_recv(items[0].socket, &msg, 0)) {
        erlzmq_ipc_request * req = (erlzmq_ipc_request *)zmq_msg_data(&msg);
        if (req->flags & erlzmq_TERM) {

          final_ref = enif_make_copy(final_env, req->ref);
          final_pid = enif_make_pid(final_env, &req->pid);

          enif_free_env(req->env);
          ctx->running = 0;
          goto out;
        }
        nwatched++;
        erlzmq_ipc_request * r = malloc(sizeof(erlzmq_ipc_request));
        memcpy(r, req, sizeof(erlzmq_ipc_request));
        TAILQ_INSERT_TAIL(requests_queue, r, requests);
      }
out:
      zmq_msg_close(&msg);
    }
    free(items);
  }
  enif_mutex_lock(ctx->mutex);
  enif_mutex_unlock(ctx->mutex);

  // cleanup reader's queue
  erlzmq_ipc_request * r;
  while ((r = requests_queue->tqh_first) != NULL) {
      TAILQ_REMOVE(requests_queue, requests_queue->tqh_first, requests);
      free(r);
  }
  free(requests_queue);
  zmq_close(ipc_socket);

  zmq_close(ctx->ipc_socket);
  free(ctx->ipc_socket_name);
  enif_mutex_destroy(ctx->mutex);
  enif_cond_destroy(ctx->cond);

  zmq_term(ctx->context);


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

NIF(erlzmq_nif_close)
{

  erlzmq_socket * socket;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_socket,
                                       (void **) &socket)) {
    return enif_make_badarg(env);
  }

  enif_release_resource(socket);


  if (-1 == zmq_close(socket->socket)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"),
                                 enif_make_int(env, zmq_errno()));
  } else {
    return enif_make_atom(env, "ok");
  }
}

NIF(erlzmq_nif_term)
{
  erlzmq_context * ctx;

  if (!enif_get_resource(env, argv[0], erlzmq_nif_resource_context,
                                       (void **) &ctx)) {
    return enif_make_badarg(env);
  }

  zmq_msg_t msg;
  erlzmq_ipc_request req;

  req.flags = erlzmq_TERM;
  req.env = enif_alloc_env();
  req.ref = enif_make_ref(req.env);
  enif_self(env, &req.pid);

  zmq_msg_init_size(&msg, sizeof(erlzmq_ipc_request));
  memcpy(zmq_msg_data(&msg), &req, sizeof(erlzmq_ipc_request));
  enif_mutex_lock(ctx->mutex);
  zmq_send(ctx->ipc_socket, &msg, 0);
  enif_mutex_unlock(ctx->mutex);
  zmq_msg_close(&msg);

  enif_release_resource(ctx);

  return enif_make_copy(env, req.ref);
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

