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
#include "cloudi.h"

static PyObject *python_cloudi_error;

typedef struct {
    PyObject_HEAD;
    cloudi_instance_t * p;
} python_cloudi_instance_object;

static void
python_cloudi_instance_object_dealloc(PyObject* self)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    if (object->p != 0)
    {
        cloudi_destroy(object->p);
        free(object->p);
    }
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
};

static PyMethodDef python_cloudi_methods[];

PyMODINIT_FUNC
initcloudi_c(void)
{
    PyObject * m;
    m = Py_InitModule("cloudi_c", python_cloudi_methods);
    if (m == NULL)
        return;

    python_cloudi_error = PyErr_NewException("cloudi_c.error", NULL, NULL);
    Py_INCREF(python_cloudi_error);
    PyModule_AddObject(m, "error", python_cloudi_error);
}

static void
python_error(int value)
{
    switch (value)
    {
        case cloudi_timeout:
            PyErr_SetString(python_cloudi_error, "timeout");
            return;
        case cloudi_error_function_parameter:
            PyErr_SetString(python_cloudi_error, "function_parameter");
            return;
        case cloudi_error_read_underflow:
            PyErr_SetString(python_cloudi_error, "read_underflow");
            return;
        case cloudi_error_ei_decode:
            PyErr_SetString(python_cloudi_error, "ei_decode");
            return;
        case cloudi_invalid_input:
            PyErr_SetString(python_cloudi_error, "cloudi_invalid_input");
            return;
        case cloudi_out_of_memory:
            PyErr_SetString(python_cloudi_error, "cloudi_out_of_memory");
            return;
        case cloudi_error_read_EAGAIN:
            PyErr_SetString(python_cloudi_error, "read_EAGAIN");
            return;
        case cloudi_error_read_EBADF:
            PyErr_SetString(python_cloudi_error, "read_EBADF");
            return;
        case cloudi_error_read_EFAULT:
            PyErr_SetString(python_cloudi_error, "read_EFAULT");
            return;
        case cloudi_error_read_EINTR:
            PyErr_SetString(python_cloudi_error, "read_EINTR");
            return;
        case cloudi_error_read_EINVAL:
            PyErr_SetString(python_cloudi_error, "read_EINVAL");
            return;
        case cloudi_error_read_EIO:
            PyErr_SetString(python_cloudi_error, "read_EIO");
            return;
        case cloudi_error_read_EISDIR:
            PyErr_SetString(python_cloudi_error, "read_EISDIR");
            return;
        case cloudi_error_read_null:
            PyErr_SetString(python_cloudi_error, "read_null");
            return;
        case cloudi_error_read_overflow:
            PyErr_SetString(python_cloudi_error, "read_overflow");
            return;
        case cloudi_error_read_unknown:
            PyErr_SetString(python_cloudi_error, "read_unknown");
            return;
        case cloudi_error_write_EAGAIN:
            PyErr_SetString(python_cloudi_error, "write_EAGAIN");
            return;
        case cloudi_error_write_EBADF:
            PyErr_SetString(python_cloudi_error, "write_EBADF");
            return;
        case cloudi_error_write_EFAULT:
            PyErr_SetString(python_cloudi_error, "write_EFAULT");
            return;
        case cloudi_error_write_EFBIG:
            PyErr_SetString(python_cloudi_error, "write_EFBIG");
            return;
        case cloudi_error_write_EINTR:
            PyErr_SetString(python_cloudi_error, "write_EINTR");
            return;
        case cloudi_error_write_EINVAL:
            PyErr_SetString(python_cloudi_error, "write_EINVAL");
            return;
        case cloudi_error_write_EIO:
            PyErr_SetString(python_cloudi_error, "write_EIO");
            return;
        case cloudi_error_write_ENOSPC:
            PyErr_SetString(python_cloudi_error, "write_ENOSPC");
            return;
        case cloudi_error_write_EPIPE:
            PyErr_SetString(python_cloudi_error, "write_EPIPE");
            return;
        case cloudi_error_write_null:
            PyErr_SetString(python_cloudi_error, "write_null");
            return;
        case cloudi_error_write_overflow:
            PyErr_SetString(python_cloudi_error, "write_overflow");
            return;
        case cloudi_error_write_unknown:
            PyErr_SetString(python_cloudi_error, "write_unknown");
            return;
        case cloudi_error_ei_encode:
            PyErr_SetString(python_cloudi_error, "ei_encode");
            return;
        case cloudi_error_poll_EBADF:
            PyErr_SetString(python_cloudi_error, "poll_EBADF");
            return;
        case cloudi_error_poll_EFAULT:
            PyErr_SetString(python_cloudi_error, "poll_EFAULT");
            return;
        case cloudi_error_poll_EINTR:
            PyErr_SetString(python_cloudi_error, "poll_EINTR");
            return;
        case cloudi_error_poll_EINVAL:
            PyErr_SetString(python_cloudi_error, "poll_EINVAL");
            return;
        case cloudi_error_poll_ENOMEM:
            PyErr_SetString(python_cloudi_error, "poll_ENOMEM");
            return;
        case cloudi_error_poll_ERR:
            PyErr_SetString(python_cloudi_error, "poll_ERR");
            return;
        case cloudi_error_poll_HUP:
            PyErr_SetString(python_cloudi_error, "poll_HUP");
            return;
        case cloudi_error_poll_NVAL:
            PyErr_SetString(python_cloudi_error, "poll_NVAL");
            return;
        case cloudi_error_poll_unknown:
            PyErr_SetString(python_cloudi_error, "poll_unknown");
            return;
        default:
            PyErr_Format(python_cloudi_error, "unknown (%d)", value);
            return;
    }
}

static PyObject *
python_cloudi_initialize(PyObject * self, PyObject * args)
{
    unsigned int thread_index;
    python_cloudi_instance_object * object;
    int result;

    if (! PyArg_ParseTuple(args, "I", &thread_index))
        return NULL;
    object = PyObject_New(python_cloudi_instance_object,
                          &python_cloudi_instance_type);
    object->p = (cloudi_instance_t *) malloc(sizeof(cloudi_instance_t));
    result = cloudi_initialize(object->p, thread_index);
    if (result != 0)
    {
        python_error(result);
        return NULL;
    }
    return (PyObject *) object;
}

static PyObject *
python_cloudi_destroy(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    cloudi_destroy(object->p);
    free(object->p);
    object->p = 0;
    Py_RETURN_NONE;
}

// Py_BEGIN_ALLOW_THREADS
// Py_END_ALLOW_THREADS

static PyMethodDef python_cloudi_methods[] = {
    {"initialize",
     python_cloudi_initialize, METH_VARARGS,
     "Initialize a new CloudI API thread instance."},
    {"destroy",
     python_cloudi_destroy, METH_VARARGS,
     "Destroy a CloudI API thread instance."},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

