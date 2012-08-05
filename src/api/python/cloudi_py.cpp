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

typedef struct {
    PyObject_HEAD;
    CloudI::API * api;
} python_cloudi_instance_object;

static void
python_cloudi_instance_object_dealloc(PyObject* self)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    if (object->api != 0)
        delete object->api;
    PyObject_Del(self);
}

static PyTypeObject python_cloudi_instance_type = {
    PyObject_HEAD_INIT(NULL)
    0,
    "cloudi_c",
    sizeof(python_cloudi_instance_object),
    0,
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
    0,                                       // tp_doc
    0,                                       // tp_traverse
    0,                                       // tp_clear
    0,                                       // tp_richcompare
    0,                                       // tp_weaklistoffset
    0,                                       // tp_iter
    0,                                       // tp_iternext
    0,                                       // tp_methods
    0,                                       // tp_members
    0,                                       // tp_getset
    0,                                       // tp_base
    0,                                       // tp_dict
    0,                                       // tp_descr_get
    0,                                       // tp_descr_set
    0,                                       // tp_dictoffset
    0,                                       // tp_init
    0,                                       // tp_alloc
    0,                                       // tp_new
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

static PyObject *
python_cloudi_initialize(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_destroy(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_subscribe(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_unsubscribe(PyObject * self, PyObject * args);

static PyMethodDef python_cloudi_methods[] = {
    {"initialize",
     python_cloudi_initialize, METH_VARARGS,
     "Initialize a new CloudI API thread instance."},
    {"destroy",
     python_cloudi_destroy, METH_VARARGS,
     "Destroy a CloudI API thread instance."},
    {"subscribe",
     python_cloudi_subscribe, METH_VARARGS,
     "Subscribe to a service name with a callback function."},
    {"unsubscribe",
     python_cloudi_unsubscribe, METH_VARARGS,
     "Completely unsubscribe from a service name."},
    {NULL, NULL, 0, NULL} // Sentinel
};

static PyObject *python_cloudi_error;

PyMODINIT_FUNC
initlibcloudi_py(void)
{
    PyObject * m;
    m = Py_InitModule("libcloudi_py", python_cloudi_methods);
    if (m == NULL)
        return;

    python_cloudi_error = PyErr_NewException(
        const_cast<char *>("libcloudi_py.error"), NULL, NULL);
    Py_INCREF(python_cloudi_error);
    PyModule_AddObject(m, "error", python_cloudi_error);
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

static PyObject *
python_cloudi_initialize(PyObject *, PyObject * args)
{
    unsigned int thread_index;
    python_cloudi_instance_object * object;

    if (! PyArg_ParseTuple(args, "I:initialize", &thread_index))
        return NULL;
    object = PyObject_New(python_cloudi_instance_object,
                          &python_cloudi_instance_type);
    try
    {
        object->api = new CloudI::API(thread_index);
    }
    catch (CloudI::API::invalid_input_exception const *)
    {
        python_error(CloudI::API::return_value::invalid_input);
        return NULL;
    }
    return (PyObject *) object;
}

static PyObject *
python_cloudi_destroy(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    delete object->api;
    object->api = 0;
    Py_XDECREF(object);
    Py_RETURN_NONE;
}

class callback_function : public CloudI::API::function_object2
{
    public:
        callback_function(PyObject * f) : m_f(f)
        {
            Py_XINCREF(m_f);
        }
        virtual ~callback_function() throw()
        {
            Py_XDECREF(m_f);
        }
        callback_function(callback_function const & o) : m_f(o.m_f)
        {
            Py_XINCREF(m_f);
        }
        virtual void operator () (CloudI::API const &,
                                  int const,
                                  char const * const,
                                  char const * const,
                                  void const * const,
                                  uint32_t const,
                                  void const * const,
                                  uint32_t const,
                                  uint32_t,
                                  int8_t,
                                  char const * const,
                                  char const * const,
                                  uint32_t const) const
        {
            //Py_BEGIN_BLOCK_THREADS;
            // XXX do stuff here
            //Py_BEGIN_UNBLOCK_THREADS;
        }

    private:
        PyObject * const m_f;
};

static PyObject *
python_cloudi_subscribe(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    char const * pattern;
    PyObject * f;
    if (! PyArg_ParseTuple(args, "sO:subscribe", &pattern, &f))
        return NULL;
    if (! PyCallable_Check(f))
    {
        PyErr_SetString(python_cloudi_error, "subscribe: not_callable");
        return NULL;
    }
    int result = object->api->subscribe(pattern, callback_function(f));
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
        return NULL;
    int result = object->api->unsubscribe(pattern);
    if (result != 0)
    {
        python_error(result);
        return NULL;
    }
    Py_RETURN_NONE;
}

// Py_BEGIN_ALLOW_THREADS
// Py_END_ALLOW_THREADS

