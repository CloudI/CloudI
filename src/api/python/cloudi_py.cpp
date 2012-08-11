/* -*- coding: utf-8; Mode: C; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
 * ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
 *
 * BSD LICENSE
 * 
 * Copyright (c) 2012, Michael Truog <mjtruog at gmail dot com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *     * All advertising materials mentioning features or use of this
 *       software must display the following acknowledgment:
 *         This product includes software developed by Michael Truog
 *     * The name of the author may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */

#include <Python.h>
#include "cloudi.hpp"
#include <string>

static PyObject *python_cloudi_assert;
static PyObject *python_cloudi_error;
static PyObject *python_cloudi_invalid_input;

#ifdef NDEBUG
#define PY_ASSERT(X)                                                       \
    do {} while(0)
#else
#define PY_ASSERT(X)                                                       \
    if (! (X))                                                             \
    {                                                                      \
        PyErr_Format(python_cloudi_assert, "%s:%d: ASSERT(%s) failed!",    \
                     __FILE__, __LINE__, #X);                              \
        return NULL;                                                       \
    }                                                                      \
    do {} while(0)
#endif

typedef struct {
    PyObject_HEAD;
    CloudI::API * api;
} python_cloudi_instance_object;

static void
python_cloudi_instance_object_dealloc(PyObject * self)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    if (object->api != 0)
    {
        delete object->api;
        object->api = 0;
    }
    PyObject_Del(self);
}

static PyObject *
python_cloudi_instance_object_new(PyTypeObject * type, PyObject *, PyObject *)
{
    python_cloudi_instance_object * self;
    self = (python_cloudi_instance_object *) type->tp_alloc(type, 0);
    if (self != NULL)
        self->api = 0;
    return (PyObject *) self;
}

static int
python_cloudi_instance_object_init(PyObject * self, PyObject * args, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    unsigned int thread_index;
    if (! PyArg_ParseTuple(args, "I:__init__()", &thread_index))
    {
        return -1;
    }
    try
    {
        object->api = new CloudI::API(thread_index);
    }
    catch (CloudI::API::invalid_input_exception const *)
    {
        PyErr_SetString(python_cloudi_invalid_input, "Invalid Input");
        return -1;
    }
    return 0;
}

static PyObject *
python_cloudi_subscribe(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_unsubscribe(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_send_async(PyObject * self, PyObject * args, PyObject * kwargs);
static PyObject *
python_cloudi_send_sync(PyObject * self, PyObject * args, PyObject * kwargs);
static PyObject *
python_cloudi_mcast_async(PyObject * self, PyObject * args, PyObject * kwargs);
static PyObject *
python_cloudi_forward_async(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_forward_sync(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_return_async(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_return_sync(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_recv_async(PyObject * self, PyObject * args, PyObject * kwargs);
static PyObject *
python_cloudi_prefix(PyObject * self, PyObject *);
static PyObject *
python_cloudi_timeout_async(PyObject * self, PyObject *);
static PyObject *
python_cloudi_timeout_sync(PyObject * self, PyObject *);
static PyObject *
python_cloudi_poll(PyObject * self, PyObject *);

static PyMethodDef python_cloudi_instance_object_methods[] = {
    {"subscribe",
     python_cloudi_subscribe, METH_VARARGS,
     "Subscribe to a service name with a callback function."},
    {"unsubscribe",
     python_cloudi_unsubscribe, METH_VARARGS,
     "Completely unsubscribe from a service name."},
    {"send_async",
     (PyCFunction) python_cloudi_send_async, METH_VARARGS | METH_KEYWORDS,
     "Send a request asynchronously."},
    {"send_sync",
     (PyCFunction) python_cloudi_send_sync, METH_VARARGS | METH_KEYWORDS,
     "Send a request synchronously."},
    {"mcast_async",
     (PyCFunction) python_cloudi_mcast_async, METH_VARARGS | METH_KEYWORDS,
     "Send a multicast request asynchronously."},
    {"forward_async",
     python_cloudi_forward_async, METH_VARARGS,
     "Forward a request asynchronously."},
    {"forward_sync",
     python_cloudi_forward_sync, METH_VARARGS,
     "Forward a request synchronously."},
    {"return_async",
     python_cloudi_return_async, METH_VARARGS,
     "Return a response asynchronously."},
    {"return_sync",
     python_cloudi_return_sync, METH_VARARGS,
     "Return a response synchronously."},
    {"recv_async",
     (PyCFunction) python_cloudi_recv_async, METH_VARARGS | METH_KEYWORDS,
     "Receive an asynchronous response synchronously."},
    {"prefix",
     python_cloudi_prefix, METH_VARARGS,
     "Provide the service name prefix."},
    {"timeout_async",
     python_cloudi_timeout_async, METH_VARARGS,
     "Provide the default asynchronous timeout."},
    {"timeout_sync",
     python_cloudi_timeout_sync, METH_VARARGS,
     "Provide the default synchronous timeout."},
    {"poll",
     python_cloudi_poll, METH_VARARGS,
     "Handle incoming requests."},
    {NULL} // Sentinel
};

static PyTypeObject python_cloudi_instance_type = {
    PyObject_HEAD_INIT(NULL)
    0,                                       // ob_size
    "libcloudi_py.cloudi_c",                 // tp_name
    sizeof(python_cloudi_instance_object),   // tp_basicsize
    0,                                       // tp_itemsize
    python_cloudi_instance_object_dealloc,   // tp_dealloc
    0,                                       // tp_print
    0,                                       // tp_getattr
    0,                                       // tp_setattr
    0,                                       // tp_compare
    0,                                       // tp_repr
    0,                                       // tp_as_number
    0,                                       // tp_as_sequence
    0,                                       // tp_as_mapping
    0,                                       // tp_hash 
    0,                                       // tp_call
    0,                                       // tp_str
    0,                                       // tp_getattro
    0,                                       // tp_setattro
    0,                                       // tp_as_buffer
    Py_TPFLAGS_DEFAULT,                      // tp_flags
    "CloudI::API C++ Python wrapper",        // tp_doc
    0,                                       // tp_traverse
    0,                                       // tp_clear
    0,                                       // tp_richcompare
    0,                                       // tp_weaklistoffset
    0,                                       // tp_iter
    0,                                       // tp_iternext
    python_cloudi_instance_object_methods,   // tp_methods
    0,                                       // tp_members
    0,                                       // tp_getset
    0,                                       // tp_base
    0,                                       // tp_dict
    0,                                       // tp_descr_get
    0,                                       // tp_descr_set
    0,                                       // tp_dictoffset
    python_cloudi_instance_object_init,      // tp_init
    0,                                       // tp_alloc
    python_cloudi_instance_object_new,       // tp_new
    0,                                       // tp_free
    0,                                       // tp_is_gc
    0,                                       // tp_bases
    0,                                       // tp_mro
    0,                                       // tp_cache
    0,                                       // tp_subclasses
    0,                                       // tp_weaklist
    0,                                       // tp_del
    0                                        // tp_version_tag
#ifdef COUNT_ALLOCS
     ,
    0,                                       // tp_allocs
    0,                                       // tp_frees
    0,                                       // tp_maxalloc
    0,                                       // tp_prev
    0                                        // tp_next
#endif 
};

static PyMethodDef python_cloudi_methods[] = {
    {NULL} // Sentinel
};

PyMODINIT_FUNC
initlibcloudi_py(void)
{
    if (PyType_Ready(&python_cloudi_instance_type) < 0)
        return;

    PyObject * m = Py_InitModule3("libcloudi_py", python_cloudi_methods,
                                  "Python interface to the C++ CloudI API");
    if (m == NULL)
        return;

    Py_INCREF(&python_cloudi_instance_type);
    PyModule_AddObject(m, "cloudi_c",
                       (PyObject *) &python_cloudi_instance_type);

    python_cloudi_assert = PyErr_NewException(
        const_cast<char *>("libcloudi_py.assert"), NULL, NULL);
    Py_INCREF(python_cloudi_assert);
    PyModule_AddObject(m, "assert", python_cloudi_assert);

    python_cloudi_error = PyErr_NewException(
        const_cast<char *>("libcloudi_py.error"), NULL, NULL);
    Py_INCREF(python_cloudi_error);
    PyModule_AddObject(m, "error", python_cloudi_error);

    python_cloudi_invalid_input = PyErr_NewException(
        const_cast<char *>("libcloudi_py.invalid_input"), NULL, NULL);
    Py_INCREF(python_cloudi_invalid_input);
    PyModule_AddObject(m, "invalid_input", python_cloudi_invalid_input);
}

static void
python_error(int value)
{
    switch (value)
    {
        case CloudI::API::return_value::timeout:
            PyErr_SetString(python_cloudi_error, "timeout");
            return;
        case CloudI::API::return_value::error_function_parameter:
            PyErr_SetString(python_cloudi_error, "function_parameter");
            return;
        case CloudI::API::return_value::error_read_underflow:
            PyErr_SetString(python_cloudi_error, "read_underflow");
            return;
        case CloudI::API::return_value::error_ei_decode:
            PyErr_SetString(python_cloudi_error, "ei_decode");
            return;
        case CloudI::API::return_value::invalid_input:
            PyErr_SetString(python_cloudi_error, "cloudi_invalid_input");
            return;
        case CloudI::API::return_value::out_of_memory:
            PyErr_SetString(python_cloudi_error, "cloudi_out_of_memory");
            return;
        case CloudI::API::return_value::error_read_EAGAIN:
            PyErr_SetString(python_cloudi_error, "read_EAGAIN");
            return;
        case CloudI::API::return_value::error_read_EBADF:
            PyErr_SetString(python_cloudi_error, "read_EBADF");
            return;
        case CloudI::API::return_value::error_read_EFAULT:
            PyErr_SetString(python_cloudi_error, "read_EFAULT");
            return;
        case CloudI::API::return_value::error_read_EINTR:
            PyErr_SetString(python_cloudi_error, "read_EINTR");
            return;
        case CloudI::API::return_value::error_read_EINVAL:
            PyErr_SetString(python_cloudi_error, "read_EINVAL");
            return;
        case CloudI::API::return_value::error_read_EIO:
            PyErr_SetString(python_cloudi_error, "read_EIO");
            return;
        case CloudI::API::return_value::error_read_EISDIR:
            PyErr_SetString(python_cloudi_error, "read_EISDIR");
            return;
        case CloudI::API::return_value::error_read_null:
            PyErr_SetString(python_cloudi_error, "read_null");
            return;
        case CloudI::API::return_value::error_read_overflow:
            PyErr_SetString(python_cloudi_error, "read_overflow");
            return;
        case CloudI::API::return_value::error_read_unknown:
            PyErr_SetString(python_cloudi_error, "read_unknown");
            return;
        case CloudI::API::return_value::error_write_EAGAIN:
            PyErr_SetString(python_cloudi_error, "write_EAGAIN");
            return;
        case CloudI::API::return_value::error_write_EBADF:
            PyErr_SetString(python_cloudi_error, "write_EBADF");
            return;
        case CloudI::API::return_value::error_write_EFAULT:
            PyErr_SetString(python_cloudi_error, "write_EFAULT");
            return;
        case CloudI::API::return_value::error_write_EFBIG:
            PyErr_SetString(python_cloudi_error, "write_EFBIG");
            return;
        case CloudI::API::return_value::error_write_EINTR:
            PyErr_SetString(python_cloudi_error, "write_EINTR");
            return;
        case CloudI::API::return_value::error_write_EINVAL:
            PyErr_SetString(python_cloudi_error, "write_EINVAL");
            return;
        case CloudI::API::return_value::error_write_EIO:
            PyErr_SetString(python_cloudi_error, "write_EIO");
            return;
        case CloudI::API::return_value::error_write_ENOSPC:
            PyErr_SetString(python_cloudi_error, "write_ENOSPC");
            return;
        case CloudI::API::return_value::error_write_EPIPE:
            PyErr_SetString(python_cloudi_error, "write_EPIPE");
            return;
        case CloudI::API::return_value::error_write_null:
            PyErr_SetString(python_cloudi_error, "write_null");
            return;
        case CloudI::API::return_value::error_write_overflow:
            PyErr_SetString(python_cloudi_error, "write_overflow");
            return;
        case CloudI::API::return_value::error_write_unknown:
            PyErr_SetString(python_cloudi_error, "write_unknown");
            return;
        case CloudI::API::return_value::error_ei_encode:
            PyErr_SetString(python_cloudi_error, "ei_encode");
            return;
        case CloudI::API::return_value::error_poll_EBADF:
            PyErr_SetString(python_cloudi_error, "poll_EBADF");
            return;
        case CloudI::API::return_value::error_poll_EFAULT:
            PyErr_SetString(python_cloudi_error, "poll_EFAULT");
            return;
        case CloudI::API::return_value::error_poll_EINTR:
            PyErr_SetString(python_cloudi_error, "poll_EINTR");
            return;
        case CloudI::API::return_value::error_poll_EINVAL:
            PyErr_SetString(python_cloudi_error, "poll_EINVAL");
            return;
        case CloudI::API::return_value::error_poll_ENOMEM:
            PyErr_SetString(python_cloudi_error, "poll_ENOMEM");
            return;
        case CloudI::API::return_value::error_poll_ERR:
            PyErr_SetString(python_cloudi_error, "poll_ERR");
            return;
        case CloudI::API::return_value::error_poll_HUP:
            PyErr_SetString(python_cloudi_error, "poll_HUP");
            return;
        case CloudI::API::return_value::error_poll_NVAL:
            PyErr_SetString(python_cloudi_error, "poll_NVAL");
            return;
        case CloudI::API::return_value::error_poll_unknown:
            PyErr_SetString(python_cloudi_error, "poll_unknown");
            return;
        default:
            PyErr_Format(python_cloudi_error, "unknown (%d)", value);
            return;
    }
}

class callback_function : public CloudI::API::function_object_c
{
    public:
        callback_function(PyObject * f) : m_f(f)
        {
            Py_INCREF(m_f);
        }
        virtual ~callback_function() throw()
        {
            Py_DECREF(m_f);
        }
        callback_function(callback_function const & o) : m_f(o.m_f)
        {
            Py_INCREF(m_f);
        }
        static PyThreadState * thread_state_save;
        virtual void operator () (CloudI::API const & api,
                                  int const command,
                                  char const * const name,
                                  char const * const pattern,
                                  void const * const request_info,
                                  uint32_t const request_info_size,
                                  void const * const request,
                                  uint32_t const request_size,
                                  uint32_t timeout,
                                  int8_t priority,
                                  char const * const trans_id,
                                  char const * const pid,
                                  uint32_t const pid_size)
        {
            PyObject * args = Py_BuildValue("(i,s,s,s#,s#,I,i,s#,s#)",
                                            command, name, pattern,
                                            request_info, request_info_size,
                                            request, request_size, timeout,
                                            static_cast<int>(priority),
                                            trans_id, 16, pid, pid_size);
            if (! args)
                return;
            PyObject * result;
            {
                PyThreadState * _save = callback_function::thread_state_save;
                Py_BLOCK_THREADS;
                result = PyObject_CallObject(m_f, args);
                Py_UNBLOCK_THREADS;
            }
            bool done = (result == NULL);
            Py_XDECREF(result);
            Py_DECREF(args);
            if (! done)
            {
                if (command == CloudI::API::ASYNC)
                {
                    try
                    {
                        api.return_async(name, pattern, "", 0, "", 0,
                                         timeout, trans_id, pid, pid_size);
                    }
                    catch (CloudI::API::return_async_exception const &)
                    {
                    }
                }
                else if (command == CloudI::API::SYNC)
                {
                    try
                    {
                        api.return_sync(name, pattern, "", 0, "", 0,
                                        timeout, trans_id, pid, pid_size);
                    }
                    catch (CloudI::API::return_sync_exception const &)
                    {
                    }
                }
            }
        }

    private:
        PyObject * const m_f;
};
PyThreadState * callback_function::thread_state_save = 0;

static PyObject *
python_cloudi_subscribe(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * pattern;
    PyObject * f;
    if (! PyArg_ParseTuple(args, "sO:subscribe", &pattern, &f))
    {
        return NULL;
    }
    if (! f || ! PyCallable_Check(f))
    {
        PyErr_SetString(python_cloudi_error, "subscribe: not_callable");
        return NULL;
    }
    int result = object->api->subscribe(pattern, new callback_function(f));
    if (result != 0)
    {
        python_error(result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_unsubscribe(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * pattern;
    if (! PyArg_ParseTuple(args, "s:unsubscribe", &pattern))
    {
        return NULL;
    }
    int result = object->api->unsubscribe(pattern);
    if (result != 0)
    {
        python_error(result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_send_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * name;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout = object->api->timeout_async();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    int8_t priority = object->api->priority_default();
    static char const * kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs, "ss#|Is#B:send_async",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size, &timeout,
                                      &request_info, &request_info_size,
                                      &priority))
    {
        return NULL;
    }
    int result;
    Py_BEGIN_ALLOW_THREADS;
    callback_function::thread_state_save = _save;
    result = object->api->send_async(name, request_info, request_info_size,
                                     request, request_size, timeout, priority);
    Py_END_ALLOW_THREADS;
    callback_function::thread_state_save = 0;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(result);
        return NULL;
    }
    PY_ASSERT(object->api->get_trans_id_count() == 1);
    return Py_BuildValue("s#", object->api->get_trans_id(0), 16);
}

static PyObject *
python_cloudi_send_sync(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * name;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout = object->api->timeout_sync();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    int8_t priority = object->api->priority_default();
    static char const * kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs, "ss#|Is#B:send_sync",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size, &timeout,
                                      &request_info, &request_info_size,
                                      &priority))
    {
        return NULL;
    }
    int result;
    Py_BEGIN_ALLOW_THREADS;
    callback_function::thread_state_save = _save;
    result = object->api->send_sync(name, request_info, request_info_size,
                                    request, request_size,
                                    timeout, priority);
    Py_END_ALLOW_THREADS;
    callback_function::thread_state_save = 0;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(result);
        return NULL;
    }
    PY_ASSERT(object->api->get_trans_id_count() == 1);
    return Py_BuildValue("(s#,s#,s#)",
                         object->api->get_response_info(),
                         object->api->get_response_info_size(),
                         object->api->get_response(),
                         object->api->get_response_size(),
                         object->api->get_trans_id(0), 16);
}

static PyObject *
python_cloudi_mcast_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * name;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout = object->api->timeout_async();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    int8_t priority = object->api->priority_default();
    static char const * kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs, "ss#|Is#B:mcast_async",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size, &timeout,
                                      &request_info, &request_info_size,
                                      &priority))
    {
        return NULL;
    }
    int result;
    Py_BEGIN_ALLOW_THREADS;
    callback_function::thread_state_save = _save;
    result = object->api->mcast_async(name, request_info, request_info_size,
                                      request, request_size,
                                      timeout, priority);
    Py_END_ALLOW_THREADS;
    callback_function::thread_state_save = 0;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(result);
        return NULL;
    }
    return Py_BuildValue("s#", object->api->get_trans_id(0),
                         object->api->get_trans_id_count() * 16);
}

static PyObject *
python_cloudi_forward_async(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * name;
    char const * request_info;
    uint32_t request_info_size = 0;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout;
    int8_t priority;
    char const * trans_id;
    uint32_t trans_id_size = 0;
    char const * pid;
    uint32_t pid_size = 0;
    if (! PyArg_ParseTuple(args, "ss#s#IBs#s#:forward_async",
                           &name, &request_info, &request_info_size,
                           &request, &request_size, &timeout, &priority,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    try
    {
        int result = object->api->forward_async(name,
                                                request_info, request_info_size,
                                                request, request_size,
                                                timeout, priority,
                                                trans_id, pid, pid_size);
        if (result != 0)
        {
            python_error(result);
            return NULL;
        }
    }
    catch (CloudI::API::return_async_exception const &)
    {
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_forward_sync(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * name;
    char const * request_info;
    uint32_t request_info_size = 0;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout;
    int8_t priority;
    char const * trans_id;
    uint32_t trans_id_size = 0;
    char const * pid;
    uint32_t pid_size = 0;
    if (! PyArg_ParseTuple(args, "ss#s#IBs#s#:forward_sync",
                           &name, &request_info, &request_info_size,
                           &request, &request_size, &timeout, &priority,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    try
    {
        int result = object->api->forward_sync(name,
                                               request_info, request_info_size,
                                               request, request_size,
                                               timeout, priority,
                                               trans_id, pid, pid_size);
        if (result != 0)
        {
            python_error(result);
            return NULL;
        }
    }
    catch (CloudI::API::return_sync_exception const &)
    {
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_return_async(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * name;
    char const * pattern;
    char const * response_info;
    uint32_t response_info_size = 0;
    char const * response;
    uint32_t response_size = 0;
    uint32_t timeout;
    char const * trans_id;
    uint32_t trans_id_size = 0;
    char const * pid;
    uint32_t pid_size = 0;
    if (! PyArg_ParseTuple(args, "sss#s#Is#s#:return_async",
                           &name, &pattern, &response_info, &response_info_size,
                           &response, &response_size, &timeout,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    try
    {
        int result = object->api->return_async(name, pattern,
                                               response_info,
                                               response_info_size,
                                               response, response_size, timeout,
                                               trans_id, pid, pid_size);
        if (result != 0)
        {
            python_error(result);
            return NULL;
        }
    }
    catch (CloudI::API::return_async_exception const &)
    {
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_return_sync(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * name;
    char const * pattern;
    char const * response_info;
    uint32_t response_info_size = 0;
    char const * response;
    uint32_t response_size = 0;
    uint32_t timeout;
    char const * trans_id;
    uint32_t trans_id_size = 0;
    char const * pid;
    uint32_t pid_size = 0;
    if (! PyArg_ParseTuple(args, "sss#s#Is#s#:return_sync",
                           &name, &pattern, &response_info, &response_info_size,
                           &response, &response_size, &timeout,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    try
    {
        int result = object->api->return_sync(name, pattern,
                                              response_info,
                                              response_info_size,
                                              response, response_size, timeout,
                                              trans_id, pid, pid_size);
        if (result != 0)
        {
            python_error(result);
            return NULL;
        }
    }
    catch (CloudI::API::return_sync_exception const &)
    {
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_recv_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    uint32_t timeout = object->api->timeout_sync();
    char const * trans_id = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
    uint32_t trans_id_size = 0;
    static char const * kwlist[] = {
        "timeout", "trans_id", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs, "|Is#:recv_async",
                                      const_cast<char**>(kwlist),
                                      &timeout, &trans_id, &trans_id_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    int result;
    Py_BEGIN_ALLOW_THREADS;
    callback_function::thread_state_save = _save;
    result = object->api->recv_async(timeout, trans_id);
    Py_END_ALLOW_THREADS;
    callback_function::thread_state_save = 0;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(result);
        return NULL;
    }
    PY_ASSERT(object->api->get_trans_id_count() == 1);
    return Py_BuildValue("(s#,s#,s#)",
                         object->api->get_response_info(),
                         object->api->get_response_info_size(),
                         object->api->get_response(),
                         object->api->get_response_size(),
                         object->api->get_trans_id(0), 16);
}

static PyObject *
python_cloudi_prefix(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    return Py_BuildValue("s", object->api->prefix());
}

static PyObject *
python_cloudi_timeout_async(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    return Py_BuildValue("I", object->api->timeout_async());
}

static PyObject *
python_cloudi_timeout_sync(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    return Py_BuildValue("I", object->api->timeout_sync());
}

static PyObject *
python_cloudi_poll(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    int result;
    Py_BEGIN_ALLOW_THREADS;
    callback_function::thread_state_save = _save;
    result = object->api->poll();
    Py_END_ALLOW_THREADS;
    callback_function::thread_state_save = 0;
    if (result != 0)
    {
        python_error(result);
        return NULL;
    }
    Py_RETURN_NONE;
}

