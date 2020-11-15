/* -*-Mode:C;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=c fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * MIT License
 *
 * Copyright (c) 2012-2020 Michael Truog <mjtruog at protonmail dot com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#if defined(__clang__)
// clang compiler specific
#elif defined(__GNUC__)
// GCC compiler specific
#if __GNUC__ >= 8
#pragma GCC diagnostic ignored "-Wcast-function-type"
#endif
#endif
#include <Python.h>
#if PY_MAJOR_VERSION >= 3
#define PYTHON_VERSION_3_COMPATIBLE
#endif
#if (PY_MAJOR_VERSION > 3) || \
    ((PY_MAJOR_VERSION == 3) && (PY_MINOR_VERSION >= 3))
#define PYTHON_VERSION_3_3_COMPATIBLE
#endif
#if (PY_MAJOR_VERSION > 3) || \
    ((PY_MAJOR_VERSION == 3) && (PY_MINOR_VERSION >= 4))
#define PYTHON_VERSION_3_4_COMPATIBLE
#endif
#if (PY_MAJOR_VERSION == 3) && (PY_MINOR_VERSION == 8)
#define PYTHON_VERSION_3_8
#endif
#if (PY_MAJOR_VERSION > 3) || \
    ((PY_MAJOR_VERSION == 3) && (PY_MINOR_VERSION >= 8))
#define PYTHON_VERSION_3_8_COMPATIBLE
#endif
#include "cloudi.hpp"
#include <limits>
#include <string>
#include <cstring>

#ifdef NDEBUG
#define PY_ASSERT(X)
#else
#define PY_ASSERT(X)                                                       \
    if (! (X))                                                             \
    {                                                                      \
        PyErr_Format(PyExc_AssertionError,                                 \
                     "%s:%d: PY_ASSERT(%s) failed!",                       \
                     __FILE__, __LINE__, #X);                              \
        return NULL;                                                       \
    }
#endif

typedef struct {
    PyObject_HEAD
    CloudI::API * api;
    PyThreadState * thread_state;
    PyObject * invalid_input_exception;
    PyObject * message_decoding_exception;
    PyObject * terminate_exception;
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
    if (object->thread_state != 0)
    {
        // should never happen
        PyEval_RestoreThread(object->thread_state);
        object->thread_state = 0;
    }
    PyObject_Del(self);
}

static PyObject *
python_cloudi_instance_object_new(PyTypeObject * type, PyObject *, PyObject *)
{
    python_cloudi_instance_object * self;
    self = (python_cloudi_instance_object *) type->tp_alloc(type, 0);
    if (self != NULL)
    {
        self->api = 0;
        self->thread_state = 0;
    }
    return (PyObject *) self;
}

static void
python_terminate_exception(PyObject * terminate_exception,
                           uint32_t const timeout)
{
    PyObject * timeout_value = Py_BuildValue("I", timeout);
    PyObject_SetAttrString(terminate_exception,
                           "timeout", timeout_value);
    Py_DECREF(timeout_value);
    PyErr_SetString(terminate_exception, "Terminate");
}

static void
python_error(python_cloudi_instance_object * const object, int const value)
{
    switch (value)
    {
        case CloudI::API::return_value::terminate:
            python_terminate_exception(object->terminate_exception,
                                       object->api->timeout_terminate());
            return;
        case CloudI::API::return_value::timeout:
            PyErr_SetString(object->message_decoding_exception,
                            "timeout");
            return;
        case CloudI::API::return_value::error_function_parameter:
            PyErr_SetString(object->message_decoding_exception,
                            "function_parameter");
            return;
        case CloudI::API::return_value::error_read_underflow:
            PyErr_SetString(object->message_decoding_exception,
                            "read_underflow");
            return;
        case CloudI::API::return_value::error_ei_decode:
            PyErr_SetString(object->message_decoding_exception,
                            "ei_decode");
            return;
        case CloudI::API::return_value::invalid_input:
            PyErr_SetString(object->message_decoding_exception,
                            "cloudi_invalid_input");
            return;
        case CloudI::API::return_value::out_of_memory:
            PyErr_SetString(object->message_decoding_exception,
                            "cloudi_out_of_memory");
            return;
        case CloudI::API::return_value::error_read_EAGAIN:
            PyErr_SetString(object->message_decoding_exception,
                            "read_EAGAIN");
            return;
        case CloudI::API::return_value::error_read_EBADF:
            PyErr_SetString(object->message_decoding_exception,
                            "read_EBADF");
            return;
        case CloudI::API::return_value::error_read_EFAULT:
            PyErr_SetString(object->message_decoding_exception,
                            "read_EFAULT");
            return;
        case CloudI::API::return_value::error_read_EINTR:
            PyErr_SetString(object->message_decoding_exception,
                            "read_EINTR");
            return;
        case CloudI::API::return_value::error_read_EINVAL:
            PyErr_SetString(object->message_decoding_exception,
                            "read_EINVAL");
            return;
        case CloudI::API::return_value::error_read_EIO:
            PyErr_SetString(object->message_decoding_exception,
                            "read_EIO");
            return;
        case CloudI::API::return_value::error_read_EISDIR:
            PyErr_SetString(object->message_decoding_exception,
                            "read_EISDIR");
            return;
        case CloudI::API::return_value::error_read_null:
            PyErr_SetString(object->message_decoding_exception,
                            "read_null");
            return;
        case CloudI::API::return_value::error_read_overflow:
            PyErr_SetString(object->message_decoding_exception,
                            "read_overflow");
            return;
        case CloudI::API::return_value::error_read_unknown:
            PyErr_SetString(object->message_decoding_exception,
                            "read_unknown");
            return;
        case CloudI::API::return_value::error_write_EAGAIN:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EAGAIN");
            return;
        case CloudI::API::return_value::error_write_EBADF:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EBADF");
            return;
        case CloudI::API::return_value::error_write_EFAULT:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EFAULT");
            return;
        case CloudI::API::return_value::error_write_EFBIG:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EFBIG");
            return;
        case CloudI::API::return_value::error_write_EINTR:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EINTR");
            return;
        case CloudI::API::return_value::error_write_EINVAL:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EINVAL");
            return;
        case CloudI::API::return_value::error_write_EIO:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EIO");
            return;
        case CloudI::API::return_value::error_write_ENOSPC:
            PyErr_SetString(object->message_decoding_exception,
                            "write_ENOSPC");
            return;
        case CloudI::API::return_value::error_write_EPIPE:
            PyErr_SetString(object->message_decoding_exception,
                            "write_EPIPE");
            return;
        case CloudI::API::return_value::error_write_null:
            PyErr_SetString(object->message_decoding_exception,
                            "write_null");
            return;
        case CloudI::API::return_value::error_write_overflow:
            PyErr_SetString(object->message_decoding_exception,
                            "write_overflow");
            return;
        case CloudI::API::return_value::error_write_unknown:
            PyErr_SetString(object->message_decoding_exception,
                            "write_unknown");
            return;
        case CloudI::API::return_value::error_ei_encode:
            PyErr_SetString(object->message_decoding_exception,
                            "ei_encode");
            return;
        case CloudI::API::return_value::error_poll_EBADF:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_EBADF");
            return;
        case CloudI::API::return_value::error_poll_EFAULT:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_EFAULT");
            return;
        case CloudI::API::return_value::error_poll_EINTR:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_EINTR");
            return;
        case CloudI::API::return_value::error_poll_EINVAL:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_EINVAL");
            return;
        case CloudI::API::return_value::error_poll_ENOMEM:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_ENOMEM");
            return;
        case CloudI::API::return_value::error_poll_ERR:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_ERR");
            return;
        case CloudI::API::return_value::error_poll_NVAL:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_NVAL");
            return;
        case CloudI::API::return_value::error_poll_unknown:
            PyErr_SetString(object->message_decoding_exception,
                            "poll_unknown");
            return;
        default:
            PyErr_Format(object->message_decoding_exception,
                         "unknown (%d)", value);
            return;
    }
}

static int
python_cloudi_instance_object_init(PyObject * self, PyObject * args, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    unsigned int thread_index;
    PyObject * invalid_input_exception;
    PyObject * message_decoding_exception;
    PyObject * terminate_exception;
    if (! PyArg_ParseTuple(args, "IOOO:__init__()",
                           &thread_index,
                           &invalid_input_exception,
                           &message_decoding_exception,
                           &terminate_exception))
    {
        return -1;
    }
    try
    {
        object->api = new CloudI::API(thread_index, true);
        object->thread_state = 0;
        object->invalid_input_exception = invalid_input_exception;
        object->message_decoding_exception = message_decoding_exception;
        object->terminate_exception = terminate_exception;
    }
    catch (CloudI::API::invalid_input_exception const & e)
    {
        PyErr_SetString(invalid_input_exception, e.what());
        return -1;
    }
    catch (CloudI::API::terminate_exception const & e)
    {
        python_terminate_exception(terminate_exception, e.timeout());
        return -1;
    }
    return 0;
}

static PyObject *
python_cloudi_subscribe(PyObject * self, PyObject * args);
static PyObject *
python_cloudi_subscribe_count(PyObject * self, PyObject * args);
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
python_cloudi_process_index(PyObject * self, PyObject *);
static PyObject *
python_cloudi_process_count(PyObject * self, PyObject *);
static PyObject *
python_cloudi_process_count_max(PyObject * self, PyObject *);
static PyObject *
python_cloudi_process_count_min(PyObject * self, PyObject *);
static PyObject *
python_cloudi_prefix(PyObject * self, PyObject *);
static PyObject *
python_cloudi_timeout_initialize(PyObject * self, PyObject *);
static PyObject *
python_cloudi_timeout_async(PyObject * self, PyObject *);
static PyObject *
python_cloudi_timeout_sync(PyObject * self, PyObject *);
static PyObject *
python_cloudi_timeout_terminate(PyObject * self, PyObject *);
static PyObject *
python_cloudi_poll(PyObject * self, PyObject *);
static PyObject *
python_cloudi_shutdown(PyObject * self, PyObject * args, PyObject * kwargs);

static PyMethodDef python_cloudi_instance_object_methods[] = {
    {"subscribe",
     python_cloudi_subscribe, METH_VARARGS,
     "Subscribe to a service name with a callback function."},
    {"subscribe_count",
     python_cloudi_subscribe_count, METH_VARARGS,
     "Determine how may service name pattern subscriptions have occurred."},
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
    {"process_index",
     python_cloudi_process_index, METH_VARARGS,
     "Provide the service instance process index."},
    {"process_count",
     python_cloudi_process_count, METH_VARARGS,
     "Provide the service instance process count."},
    {"process_count_max",
     python_cloudi_process_count_max, METH_VARARGS,
     "Provide the service instance maximum process count."},
    {"process_count_min",
     python_cloudi_process_count_min, METH_VARARGS,
     "Provide the service instance minimum process count."},
    {"prefix",
     python_cloudi_prefix, METH_VARARGS,
     "Provide the service name prefix."},
    {"timeout_initialize",
     python_cloudi_timeout_initialize, METH_VARARGS,
     "Provide the initialization timeout."},
    {"timeout_async",
     python_cloudi_timeout_async, METH_VARARGS,
     "Provide the default asynchronous timeout."},
    {"timeout_sync",
     python_cloudi_timeout_sync, METH_VARARGS,
     "Provide the default synchronous timeout."},
    {"timeout_terminate",
     python_cloudi_timeout_terminate, METH_VARARGS,
     "Provide the termination timeout."},
    {"poll",
     python_cloudi_poll, METH_VARARGS,
     "Handle incoming requests."},
    {"shutdown",
     (PyCFunction) python_cloudi_shutdown, METH_VARARGS | METH_KEYWORDS,
     "Shutdown the service successfully."},
    {NULL, NULL, 0, NULL} // Sentinel
};

static PyTypeObject python_cloudi_instance_type = {
    PyVarObject_HEAD_INIT(NULL, 0)
    "libcloudi_py.cloudi_c",                 // tp_name
    sizeof(python_cloudi_instance_object),   // tp_basicsize
    0,                                       // tp_itemsize
    python_cloudi_instance_object_dealloc,   // tp_dealloc
    0,                                       // tp_vectorcall_offset
    0,                                       // tp_getattr
    0,                                       // tp_setattr
    0,                                       // tp_as_async
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
#ifdef PYTHON_VERSION_3_4_COMPATIBLE
    ,
    0                                        // tp_finalize
#endif
#ifdef PYTHON_VERSION_3_8_COMPATIBLE
    ,
    0                                        // tp_vectorcall
#endif
#ifdef PYTHON_VERSION_3_8
    ,
    0                                        // deprecated tp_print
#endif
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
    {NULL, NULL, 0, NULL} // Sentinel
};

#ifdef PYTHON_VERSION_3_COMPATIBLE
#define MODINIT_FUNC_DECLARE(name) PyMODINIT_FUNC PyInit_##name(void)
#define MODINIT_FUNC_RETURN_NULL return 0
static struct PyModuleDef python_cloudi_module = {
    PyModuleDef_HEAD_INIT,
    "libcloudi_py",
    "Python interface to the C++ CloudI API",
    sizeof(python_cloudi_instance_object),
    python_cloudi_methods,
    0,
    0,
    0,
    0
};
#else
#define MODINIT_FUNC_DECLARE(name) PyMODINIT_FUNC init##name(void)
#define MODINIT_FUNC_RETURN_NULL return
#endif

MODINIT_FUNC_DECLARE(libcloudi_py)
{
#ifdef PYTHON_VERSION_3_COMPATIBLE
    PyObject * m = PyModule_Create(&python_cloudi_module);
#else
    PyObject * m = Py_InitModule3("libcloudi_py", python_cloudi_methods,
                                  "Python interface to the C++ CloudI API");
#endif
    if (m == NULL)
        MODINIT_FUNC_RETURN_NULL;

    if (PyType_Ready(&python_cloudi_instance_type) < 0)
        MODINIT_FUNC_RETURN_NULL;

    Py_INCREF(&python_cloudi_instance_type);
    PyModule_AddObject(m, "cloudi_c",
                       (PyObject *) &python_cloudi_instance_type);

#ifdef PYTHON_VERSION_3_COMPATIBLE
    return m;
#else
    return;
#endif
}

// provide custom macro equivalents of (no nested scope):
// Py_BLOCK_THREADS
// Py_UNBLOCK_THREADS
// Py_BEGIN_ALLOW_THREADS
// Py_END_ALLOW_THREADS

#define THREADS_BEGIN       object->thread_state = PyEval_SaveThread()
#define THREADS_END         PyEval_RestoreThread(object->thread_state); \
                            object->thread_state = 0

#ifdef PYTHON_VERSION_3_COMPATIBLE
#define BUILDVALUE_BYTES "y#"
#else
#define BUILDVALUE_BYTES "s#"
#endif

class callback : public CloudI::API::function_object_c
{
    public:
        callback(PyObject * f, python_cloudi_instance_object * o) :
            m_f(f), object(o)
        {
            Py_INCREF(m_f);
        }
        virtual ~callback() throw()
        {
            Py_DECREF(m_f);
        }
        callback(callback const & o) :
            CloudI::API::function_object_c(o),
            m_f(o.m_f), object(o.object)
        {
            Py_INCREF(m_f);
        }

        virtual void operator () (CloudI::API const & api,
                                  int const request_type,
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
            THREADS_END;
            PyObject * args = Py_BuildValue("(i,s,s,"
                                            BUILDVALUE_BYTES ","
                                            BUILDVALUE_BYTES ",I,i,"
                                            BUILDVALUE_BYTES ","
                                            BUILDVALUE_BYTES ")",
                                            request_type, name, pattern,
                                            request_info, request_info_size,
                                            request, request_size, timeout,
                                            static_cast<int>(priority),
                                            trans_id, 16, pid, pid_size);
            if (! args)
            {
                PyErr_Print();
                THREADS_BEGIN;
                return;
            }
            PyObject * result = PyObject_CallObject(m_f, args);
            Py_DECREF(args);
            if (result == NULL)
            {
                PyTypeObject * exception =
                    reinterpret_cast<PyTypeObject *>(PyErr_Occurred());
                if (exception == NULL ||
                    exception->tp_name == NULL)
                {
                    THREADS_BEGIN;
                    return;
                }
                char const * exception_name = exception->tp_name;
                bool const return_sync_exception =
                    (::strcmp(exception_name, "ReturnSyncException") == 0);
                bool const return_async_exception =
                    (::strcmp(exception_name, "ReturnAsyncException") == 0);
                bool const forward_sync_exception =
                    (::strcmp(exception_name, "ForwardSyncException") == 0);
                bool const forward_async_exception =
                    (::strcmp(exception_name, "ForwardAsyncException") == 0);
                bool exception_invalid = false;
                int immediate_exit_code = 0;
                if (request_type == CloudI::API::SYNC &&
                    return_sync_exception)
                {
                    PyErr_Clear();
                }
                else if (request_type == CloudI::API::SYNC &&
                         forward_sync_exception)
                {
                    PyErr_Clear();
                }
                else if (request_type == CloudI::API::ASYNC &&
                         return_async_exception)
                {
                    PyErr_Clear();
                }
                else if (request_type == CloudI::API::ASYNC &&
                         forward_async_exception)
                {
                    PyErr_Clear();
                }
                else
                {
                    if (PyErr_ExceptionMatches(object->terminate_exception))
                    {
                        PyErr_Clear();
                    }
                    else if (PyErr_ExceptionMatches(
                                 object->message_decoding_exception) ||
                             PyErr_ExceptionMatches(
                                 object->invalid_input_exception) ||
                             PyErr_ExceptionMatches(PyExc_AssertionError) ||
                             PyErr_ExceptionMatches(PyExc_SystemExit))
                    {
                        immediate_exit_code = 1;
                        PyErr_WriteUnraisable(m_f);
                    }
                    else if (PyErr_ExceptionMatches(PyExc_Exception))
                    {
                        PyErr_WriteUnraisable(m_f);
                    }
                    else
                    {
                        immediate_exit_code = 1;
                        PyErr_WriteUnraisable(m_f);
                    }
                    exception_invalid = true;
                }
                THREADS_BEGIN;

                if (exception_invalid)
                {
                    if (immediate_exit_code)
                        ::exit(immediate_exit_code);
                    return;
                }

                // return from the callback
                // (the CloudI API return or forward  function was
                //  already called with valid data)
                if (return_async_exception)
                {
                    throw CloudI::API::return_async_exception();
                }
                else if (return_sync_exception)
                {
                    throw CloudI::API::return_sync_exception();
                }
                else if (forward_async_exception)
                {
                    throw CloudI::API::forward_async_exception();
                }
                else if (forward_sync_exception)
                {
                    throw CloudI::API::forward_sync_exception();
                }
                assert(false);
            }
            else
            {
                // data to return from the callback may have been
                // returned by the python callback function
                char * response_info = 0;
                uint32_t response_info_size = 0;
                char * response = 0;
                uint32_t response_size = 0;
                bool result_invalid = false;
                if (PyTuple_Check(result) &&
                    PyTuple_Size(result) == 2)
                {
                    Py_ssize_t response_info_size_tmp = 0;
                    Py_ssize_t response_size_tmp = 0;
                    if (! PyArg_ParseTuple(result,
                                           BUILDVALUE_BYTES
                                           BUILDVALUE_BYTES,
                                           &response_info,
                                           &response_info_size_tmp,
                                           &response,
                                           &response_size_tmp))
                    {
                        PyErr_Print();
                        result_invalid = true;
                    }
                    else if (response_info_size_tmp < 0 ||
                             static_cast<uint32_t>(response_info_size_tmp) >
                             std::numeric_limits<uint32_t>::max() ||
                             response_size_tmp < 0 ||
                             static_cast<uint32_t>(response_size_tmp) >
                             std::numeric_limits<uint32_t>::max())
                    {
                        result_invalid = true;
                    }
                    else
                    {
                        response_info_size =
                            static_cast<uint32_t>(response_info_size_tmp);
                        response_size =
                            static_cast<uint32_t>(response_size_tmp);
                    }
                }
                else if (PyBytes_Check(result))
                {
                    Py_ssize_t response_size_tmp = 0;
                    if (PyBytes_AsStringAndSize(result,
                                                &response,
                                                &response_size_tmp))
                    {
                        PyErr_Print();
                        result_invalid = true;
                    }
                    else if (response_size_tmp < 0 ||
                             static_cast<uint32_t>(response_size_tmp) >
                             std::numeric_limits<uint32_t>::max())
                    {
                        result_invalid = true;
                    }
                    else
                    {
                        response_size =
                            static_cast<uint32_t>(response_size_tmp);
                    }
                }
                else if (PyUnicode_Check(result))
                {
                    Py_ssize_t response_size_tmp = 0;
#ifdef PYTHON_VERSION_3_3_COMPATIBLE
                    response =
                        const_cast<char *>(PyUnicode_AsUTF8AndSize(
                            result, &response_size_tmp));
                    if (response == NULL)
                    {
                        PyErr_Print();
                        result_invalid = true;
                    }
#else // before python 3.3
                    PyObject * result_new = PyUnicode_AsUTF8String(result);
                    if (result_new == NULL)
                    {
                        PyErr_Print();
                        result_invalid = true;
                    }
                    else
                    {
                        response_size_tmp =
                            PyUnicode_GET_DATA_SIZE(result_new);
                        response =
                            const_cast<char *>(PyUnicode_AS_DATA(result_new));
                        Py_DECREF(result_new);
                    }
#endif
                    if (response_size_tmp < 0 ||
                        static_cast<uint32_t>(response_size_tmp) >
                        std::numeric_limits<uint32_t>::max())
                    {
                        result_invalid = true;
                    }
                    else
                    {
                        response_size =
                            static_cast<uint32_t>(response_size_tmp);
                    }
                }
                else
                {
                    result_invalid = true;
                }
                Py_DECREF(result);
                THREADS_BEGIN;

                // return from the callback
                if (result_invalid)
                {
                    // allow empty response to automatically be sent
                }
                else if (request_type == CloudI::API::ASYNC)
                {
                    api.return_async(name, pattern,
                                     response_info, response_info_size,
                                     response, response_size,
                                     timeout, trans_id, pid, pid_size);
                }
                else if (request_type == CloudI::API::SYNC)
                {
                    api.return_sync(name, pattern,
                                    response_info, response_info_size,
                                    response, response_size,
                                    timeout, trans_id, pid, pid_size);
                }
                else
                {
                    // allow empty response to automatically be sent
                }
            }
        }

    private:
        PyObject * const m_f;
        python_cloudi_instance_object * const object;

};

static PyObject *
python_cloudi_subscribe(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    char const * pattern;
    PyObject * f;
    if (! PyArg_ParseTuple(args, "sO:subscribe", &pattern, &f))
    {
        return NULL;
    }
    if (! f || ! PyCallable_Check(f))
    {
        PyErr_SetString(object->message_decoding_exception,
                        "subscribe: not_callable");
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->subscribe(pattern, new callback(f, object));
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_subscribe_count(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    char const * pattern;
    if (! PyArg_ParseTuple(args, "s:subscribe_count", &pattern))
    {
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->subscribe_count(pattern);
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    return Py_BuildValue("I", api->get_subscribe_count());
}

static PyObject *
python_cloudi_unsubscribe(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    char const * pattern;
    if (! PyArg_ParseTuple(args, "s:unsubscribe", &pattern))
    {
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->unsubscribe(pattern);
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_send_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    char const * name;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout = api->timeout_async();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    int8_t priority = api->priority_default();
    static char const * kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "s" BUILDVALUE_BYTES "|I"
                                      BUILDVALUE_BYTES "B:send_async",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size, &timeout,
                                      &request_info, &request_info_size,
                                      &priority))
    {
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->send_async(name, request_info, request_info_size,
                             request, request_size, timeout, priority);
    THREADS_END;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(object, result);
        return NULL;
    }
    PY_ASSERT(api->get_trans_id_count() == 1);
    return Py_BuildValue(BUILDVALUE_BYTES, api->get_trans_id(0), 16);
}

static PyObject *
python_cloudi_send_sync(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    char const * name;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout = api->timeout_sync();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    int8_t priority = api->priority_default();
    static char const * kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "s" BUILDVALUE_BYTES "|I"
                                      BUILDVALUE_BYTES "B:send_sync",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size, &timeout,
                                      &request_info, &request_info_size,
                                      &priority))
    {
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->send_sync(name, request_info, request_info_size,
                            request, request_size, timeout, priority);
    THREADS_END;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(object, result);
        return NULL;
    }
    PY_ASSERT(api->get_trans_id_count() == 1);
    return Py_BuildValue("(" BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ")",
                         api->get_response_info(),
                         api->get_response_info_size(),
                         api->get_response(),
                         api->get_response_size(),
                         api->get_trans_id(0), 16);
}

static PyObject *
python_cloudi_mcast_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    char const * name;
    char const * request;
    uint32_t request_size = 0;
    uint32_t timeout = api->timeout_async();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    int8_t priority = api->priority_default();
    static char const * kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "s" BUILDVALUE_BYTES "|I"
                                      BUILDVALUE_BYTES "B:mcast_async",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size, &timeout,
                                      &request_info, &request_info_size,
                                      &priority))
    {
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->mcast_async(name, request_info, request_info_size,
                              request, request_size, timeout, priority);
    THREADS_END;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(object, result);
        return NULL;
    }
    return Py_BuildValue(BUILDVALUE_BYTES, api->get_trans_id(0),
                         api->get_trans_id_count() * 16);
}

static PyObject *
python_cloudi_forward_async(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
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
    if (! PyArg_ParseTuple(args,
                           "s" BUILDVALUE_BYTES BUILDVALUE_BYTES "IB"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":forward_async",
                           &name, &request_info, &request_info_size,
                           &request, &request_size, &timeout, &priority,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->forward_async(name,
                                    request_info, request_info_size,
                                    request, request_size,
                                    timeout, priority,
                                    trans_id, pid, pid_size);
    }
    catch (CloudI::API::forward_async_exception const &)
    {
    }
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_forward_sync(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
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
    if (! PyArg_ParseTuple(args,
                           "s" BUILDVALUE_BYTES BUILDVALUE_BYTES "IB"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":forward_sync",
                           &name, &request_info, &request_info_size,
                           &request, &request_size, &timeout, &priority,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->forward_sync(name,
                                   request_info, request_info_size,
                                   request, request_size,
                                   timeout, priority,
                                   trans_id, pid, pid_size);
    }
    catch (CloudI::API::forward_sync_exception const &)
    {
    }
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_return_async(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
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
    if (! PyArg_ParseTuple(args,
                           "ss" BUILDVALUE_BYTES BUILDVALUE_BYTES "I"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":return_async",
                           &name, &pattern, &response_info, &response_info_size,
                           &response, &response_size, &timeout,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->return_async(name, pattern,
                                   response_info, response_info_size,
                                   response, response_size,
                                   timeout, trans_id, pid, pid_size);
    }
    catch (CloudI::API::return_async_exception const &)
    {
    }
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_return_sync(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
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
    if (! PyArg_ParseTuple(args,
                           "ss" BUILDVALUE_BYTES BUILDVALUE_BYTES "I"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":return_sync",
                           &name, &pattern, &response_info, &response_info_size,
                           &response, &response_size, &timeout,
                           &trans_id, &trans_id_size, &pid, &pid_size))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->return_sync(name, pattern,
                                  response_info, response_info_size,
                                  response, response_size,
                                  timeout, trans_id, pid, pid_size);
    }
    catch (CloudI::API::return_sync_exception const &)
    {
    }
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_recv_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    uint32_t timeout = api->timeout_sync();
    char const * trans_id = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
    uint32_t trans_id_size = 16;
    bool consume = true;
    static char const * kwlist[] = {
        "timeout", "trans_id", "consume", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "|I" BUILDVALUE_BYTES "b:recv_async",
                                      const_cast<char**>(kwlist),
                                      &timeout, &trans_id, &trans_id_size,
                                      &consume))
    {
        return NULL;
    }
    PY_ASSERT(trans_id_size == 16);
    int result;
    THREADS_BEGIN;
    result = api->recv_async(timeout, trans_id, consume);
    THREADS_END;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            Py_RETURN_NONE;
        }
        python_error(object, result);
        return NULL;
    }
    PY_ASSERT(api->get_trans_id_count() == 1);
    return Py_BuildValue("(" BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ")",
                         api->get_response_info(),
                         api->get_response_info_size(),
                         api->get_response(),
                         api->get_response_size(),
                         api->get_trans_id(0), 16);
}

static PyObject *
python_cloudi_process_index(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->process_index());
}

static PyObject *
python_cloudi_process_count(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->process_count());
}

static PyObject *
python_cloudi_process_count_max(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->process_count_max());
}

static PyObject *
python_cloudi_process_count_min(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->process_count_min());
}

static PyObject *
python_cloudi_prefix(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("s", api->prefix());
}

static PyObject *
python_cloudi_timeout_initialize(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->timeout_initialize());
}

static PyObject *
python_cloudi_timeout_async(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->timeout_async());
}

static PyObject *
python_cloudi_timeout_sync(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->timeout_sync());
}

static PyObject *
python_cloudi_timeout_terminate(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    return Py_BuildValue("I", api->timeout_terminate());
}

static PyObject *
python_cloudi_poll(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    int32_t timeout;
    if (! PyArg_ParseTuple(args, "i:poll", &timeout))
    {
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->poll(timeout);
    THREADS_END;
    if (result != 0)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            return Py_BuildValue("O", Py_True);
        }
        else
        {
            python_error(object, result);
            return NULL;
        }
    }
    return Py_BuildValue("O", Py_False);
}

static PyObject *
python_cloudi_shutdown(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * object =
        (python_cloudi_instance_object *) self;
    CloudI::API * api = object->api;
    char const * reason = "";
    static char const * kwlist[] = {
        "reason", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "|s:shutdown",
                                      const_cast<char**>(kwlist),
                                      &reason))
    {
        return NULL;
    }
    int result;
    THREADS_BEGIN;
    result = api->shutdown(reason);
    THREADS_END;
    if (result != 0)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

