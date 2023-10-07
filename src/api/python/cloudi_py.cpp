/* -*-Mode:C;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=c fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * MIT License
 *
 * Copyright (c) 2012-2023 Michael Truog <mjtruog at protonmail dot com>
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
#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include "cloudi.hpp"
#include <limits>
#include <utility>
#include <string>
#include <cstring>
#include <vector>
#include <algorithm>

#define PY_ASSERT_RETURN(X, RETURN)                                        \
    if (! (X))                                                             \
    {                                                                      \
        PyErr_Format(PyExc_AssertionError,                                 \
                     "%s:%d: PY_ASSERT(%s) failed!",                       \
                     __FILE__, __LINE__, #X);                              \
        return RETURN;                                                     \
    }
#define PY_ASSERT(X)        PY_ASSERT_RETURN(X, NULL)
#define PY_ASSERT_VOID(X)   PY_ASSERT_RETURN(X, )

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
#if (PY_MAJOR_VERSION > 3) || \
    ((PY_MAJOR_VERSION == 3) && (PY_MINOR_VERSION >= 9))
#define PYTHON_VERSION_3_9_COMPATIBLE
#endif
#if (PY_MAJOR_VERSION > 3) || \
    ((PY_MAJOR_VERSION == 3) && (PY_MINOR_VERSION >= 12))
#define PYTHON_VERSION_3_12_COMPATIBLE
#endif

#if __cplusplus >= 202002L
#define CXX20
#endif

class callback;
typedef std::vector<callback *> callbacks_t;

typedef struct {
    PyObject_HEAD
    CloudI::API * m_api;
    PyThreadState * m_thread_state;
    PyObject * m_invalid_input_exception;
    PyObject * m_message_decoding_exception;
    PyObject * m_terminate_exception;
    PyObject * m_return_sync_exception;
    PyObject * m_return_async_exception;
    PyObject * m_forward_sync_exception;
    PyObject * m_forward_async_exception;
    callbacks_t * m_callbacks;
} python_cloudi_instance_object;

namespace
{
#ifdef CXX20
    constexpr bool overflow_uint32(Py_ssize_t const value) noexcept
    {
        return (std::in_range<uint32_t>(value) == false);
    }
    constexpr bool overflow_uint32(unsigned int const value) noexcept
    {
        return (std::in_range<uint32_t>(value) == false);
    }
#else
    bool overflow_uint32(Py_ssize_t const value)
    {
        return (value < 0 ||
                static_cast<size_t>(value) >
                std::numeric_limits<uint32_t>::max());
    }
    bool overflow_uint32(unsigned int const value)
    {
        return (value > std::numeric_limits<uint32_t>::max());
    }
#endif // CXX20
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
            python_terminate_exception(object->m_terminate_exception,
                                       object->m_api->timeout_terminate());
            return;
        case CloudI::API::return_value::timeout:
            PyErr_SetString(object->m_message_decoding_exception,
                            "timeout");
            return;
        case CloudI::API::return_value::error_function_parameter:
            PyErr_SetString(object->m_message_decoding_exception,
                            "function_parameter");
            return;
        case CloudI::API::return_value::error_read_underflow:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_underflow");
            return;
        case CloudI::API::return_value::error_ei_decode:
            PyErr_SetString(object->m_message_decoding_exception,
                            "ei_decode");
            return;
        case CloudI::API::return_value::invalid_input:
            PyErr_SetString(object->m_message_decoding_exception,
                            "cloudi_invalid_input");
            return;
        case CloudI::API::return_value::out_of_memory:
            PyErr_SetString(object->m_message_decoding_exception,
                            "cloudi_out_of_memory");
            return;
        case CloudI::API::return_value::error_read_EAGAIN:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_EAGAIN");
            return;
        case CloudI::API::return_value::error_read_EBADF:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_EBADF");
            return;
        case CloudI::API::return_value::error_read_EFAULT:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_EFAULT");
            return;
        case CloudI::API::return_value::error_read_EINTR:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_EINTR");
            return;
        case CloudI::API::return_value::error_read_EINVAL:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_EINVAL");
            return;
        case CloudI::API::return_value::error_read_EIO:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_EIO");
            return;
        case CloudI::API::return_value::error_read_EISDIR:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_EISDIR");
            return;
        case CloudI::API::return_value::error_read_null:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_null");
            return;
        case CloudI::API::return_value::error_read_overflow:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_overflow");
            return;
        case CloudI::API::return_value::error_read_unknown:
            PyErr_SetString(object->m_message_decoding_exception,
                            "read_unknown");
            return;
        case CloudI::API::return_value::error_write_EAGAIN:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EAGAIN");
            return;
        case CloudI::API::return_value::error_write_EBADF:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EBADF");
            return;
        case CloudI::API::return_value::error_write_EFAULT:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EFAULT");
            return;
        case CloudI::API::return_value::error_write_EFBIG:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EFBIG");
            return;
        case CloudI::API::return_value::error_write_EINTR:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EINTR");
            return;
        case CloudI::API::return_value::error_write_EINVAL:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EINVAL");
            return;
        case CloudI::API::return_value::error_write_EIO:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EIO");
            return;
        case CloudI::API::return_value::error_write_ENOSPC:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_ENOSPC");
            return;
        case CloudI::API::return_value::error_write_EPIPE:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_EPIPE");
            return;
        case CloudI::API::return_value::error_write_null:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_null");
            return;
        case CloudI::API::return_value::error_write_overflow:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_overflow");
            return;
        case CloudI::API::return_value::error_write_unknown:
            PyErr_SetString(object->m_message_decoding_exception,
                            "write_unknown");
            return;
        case CloudI::API::return_value::error_ei_encode:
            PyErr_SetString(object->m_message_decoding_exception,
                            "ei_encode");
            return;
        case CloudI::API::return_value::error_poll_EBADF:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_EBADF");
            return;
        case CloudI::API::return_value::error_poll_EFAULT:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_EFAULT");
            return;
        case CloudI::API::return_value::error_poll_EINTR:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_EINTR");
            return;
        case CloudI::API::return_value::error_poll_EINVAL:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_EINVAL");
            return;
        case CloudI::API::return_value::error_poll_ENOMEM:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_ENOMEM");
            return;
        case CloudI::API::return_value::error_poll_ERR:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_ERR");
            return;
        case CloudI::API::return_value::error_poll_NVAL:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_NVAL");
            return;
        case CloudI::API::return_value::error_poll_unknown:
            PyErr_SetString(object->m_message_decoding_exception,
                            "poll_unknown");
            return;
        default:
            PyErr_Format(object->m_message_decoding_exception,
                         "unknown (%d)", value);
            return;
    }
}

static void
python_cloudi_instance_object_dealloc(PyObject * self);
static int
python_cloudi_instance_object_traverse(PyObject *self,
                                       visitproc visit, void *arg);
static int
python_cloudi_instance_object_clear(PyObject *self);
static int
python_cloudi_instance_object_init(PyObject * self, PyObject * args,
                                   PyObject *);
static PyObject *
python_cloudi_instance_object_new(PyTypeObject * type, PyObject *, PyObject *);

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
python_cloudi_priority_default(PyObject * self, PyObject *);
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
    {"priority_default",
     python_cloudi_priority_default, METH_VARARGS,
     "Provide the default priority."},
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
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_HAVE_GC, // tp_flags
    "CloudI::API C++ Python wrapper",        // tp_doc
    python_cloudi_instance_object_traverse,  // tp_traverse
    python_cloudi_instance_object_clear,     // tp_clear
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
#ifdef __clang__
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif
    ,
    0                                        // deprecated tp_print
#ifdef __clang__
#pragma clang diagnostic pop
#endif
#endif
#if ! defined(PYTHON_VERSION_3_9_COMPATIBLE) && defined(COUNT_ALLOCS)
    ,
    0,                                       // tp_allocs
    0,                                       // tp_frees
    0,                                       // tp_maxalloc
    0,                                       // tp_prev
    0                                        // tp_next
#endif
#ifdef PYTHON_VERSION_3_12_COMPATIBLE
    ,
    0                                        // tp_watched
#endif
};

static PyMethodDef python_cloudi_methods[] = {
    {NULL, NULL, 0, NULL} // Sentinel
};

static int python_cloudi_slot_exec(PyObject * module)
{
    PyObject * const instance_type =
        reinterpret_cast<PyObject *>(&python_cloudi_instance_type);
    Py_INCREF(instance_type);
    if (PyModule_AddObject(module, "cloudi_c", instance_type) < 0)
    {
        Py_DECREF(instance_type);
        return -1;
    }
    return 0;
}

#ifdef PYTHON_VERSION_3_12_COMPATIBLE
static PyModuleDef_Slot python_cloudi_slots[] = {
    {Py_mod_multiple_interpreters, Py_MOD_PER_INTERPRETER_GIL_SUPPORTED},
    {Py_mod_exec, reinterpret_cast<void *>(&python_cloudi_slot_exec)},
    {0, NULL} // Sentinel
};
#endif

#ifdef PYTHON_VERSION_3_COMPATIBLE
#define MODINIT_FUNC_DECLARE(name) PyMODINIT_FUNC PyInit_##name(void)
#define MODINIT_FUNC_RETURN(module) return module
static struct PyModuleDef python_cloudi_module = {
    PyModuleDef_HEAD_INIT,
    "libcloudi_py",                             // m_name
    "Python interface to the C++ CloudI API",   // m_doc
    sizeof(python_cloudi_instance_object),      // m_size
    python_cloudi_methods,                      // m_methods
#ifdef PYTHON_VERSION_3_12_COMPATIBLE
    python_cloudi_slots,                        // m_slots
#else
    0,                                          // m_slots
#endif
    0,                                          // m_traverse
    0,                                          // m_clear
    0                                           // m_free
};
#else
#define MODINIT_FUNC_DECLARE(name) PyMODINIT_FUNC init##name(void)
#define MODINIT_FUNC_RETURN(module) return
#endif

MODINIT_FUNC_DECLARE(libcloudi_py)
{
    if (PyType_Ready(&python_cloudi_instance_type) < 0)
        MODINIT_FUNC_RETURN(0);

#ifdef PYTHON_VERSION_3_12_COMPATIBLE
    PyObject * module = PyModuleDef_Init(&python_cloudi_module);
#else
#ifdef PYTHON_VERSION_3_COMPATIBLE
    PyObject * module = PyModule_Create(&python_cloudi_module);
#else
    PyObject * module = Py_InitModule3("libcloudi_py", python_cloudi_methods,
                                       "Python interface to the "
                                       "C++ CloudI API");
#endif
    if (module == NULL)
        MODINIT_FUNC_RETURN(0);

    if (python_cloudi_slot_exec(module) < 0)
        MODINIT_FUNC_RETURN(0);
#endif
    MODINIT_FUNC_RETURN(module);
}

// provide custom macro equivalents of (no nested scope):
// Py_BLOCK_THREADS
// Py_UNBLOCK_THREADS
// Py_BEGIN_ALLOW_THREADS
// Py_END_ALLOW_THREADS
//
// only called for
// "long-running computations which don’t need access to Python objects"
//
#define THREADS_BEGIN       object->m_thread_state = PyEval_SaveThread()
#define THREADS_END         PyEval_RestoreThread(object->m_thread_state); \
                            object->m_thread_state = 0

#ifdef PYTHON_VERSION_3_COMPATIBLE
#define BUILDVALUE_BYTES "y#"
#else
#define BUILDVALUE_BYTES "s#"
#endif

class callback : public CloudI::API::function_object_c
{
    private:
        typedef enum
        {
            CALLBACK_ERROR,
            CALLBACK_RETURN_SYNC,
            CALLBACK_RETURN_ASYNC,
            CALLBACK_FORWARD_SYNC,
            CALLBACK_FORWARD_ASYNC
        } exception_success_t;

    public:
        callback(PyObject * f, python_cloudi_instance_object * object) :
            m_f(f), m_object(object)
        {
            Py_INCREF(m_f);
            callbacks_t * const callbacks = m_object->m_callbacks;
            callbacks->push_back(this);
        }
        virtual ~callback() throw()
        {
            callbacks_t * const callbacks = m_object->m_callbacks;
            std::remove(callbacks->begin(), callbacks->end(), this);
            if (m_f)
            {
                Py_DECREF(m_f);
            }
        }
        callback(callback const & o) :
            CloudI::API::function_object_c(o),
            m_f(o.m_f), m_object(o.m_object)
        {
            if (m_f)
            {
                Py_INCREF(m_f);
            }
            callbacks_t * const callbacks = m_object->m_callbacks;
            callbacks->push_back(this);
        }

        static int traverse(python_cloudi_instance_object * object,
                            visitproc visit, void *arg)
        {
            callbacks_t * const callbacks = object->m_callbacks;
            for (callbacks_t::iterator itr = callbacks->begin();
                 itr != callbacks->end(); ++itr)
            {
                callback * const callback = *itr;
                assert(callback->m_object == object);
                Py_VISIT(callback->m_f);
            }
            return 0;
        }

        static int clear(python_cloudi_instance_object * object)
        {
            callbacks_t * const callbacks = object->m_callbacks;
            for (callbacks_t::iterator itr = callbacks->begin();
                 itr != callbacks->end(); ++itr)
            {
                callback * const callback = *itr;
                assert(callback->m_object == object);
                Py_CLEAR(callback->m_f);
            }
            return 0;
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
                                  char const * const source,
                                  uint32_t const source_size)
        {
            if (m_f == NULL)
            {
                return;
            }
            python_cloudi_instance_object * const object = m_object;
            Py_ssize_t const request_info_size_tmp = request_info_size;
            Py_ssize_t const request_size_tmp = request_size;
            unsigned int const timeout_tmp = timeout;
            int const priority_tmp = priority;
            Py_ssize_t const trans_id_size_tmp = 16;
            Py_ssize_t const source_size_tmp = source_size;
            PY_ASSERT_VOID(static_cast<uint32_t>(request_info_size_tmp) ==
                           request_info_size);
            PY_ASSERT_VOID(static_cast<uint32_t>(request_size_tmp) ==
                           request_size);
            PY_ASSERT_VOID(timeout_tmp == timeout);
            PY_ASSERT_VOID(static_cast<uint32_t>(source_size_tmp) ==
                           source_size);
            PyObject * result = NULL;

            THREADS_END;
            PyObject * args = Py_BuildValue("(i,s,s,"
                                            BUILDVALUE_BYTES ","
                                            BUILDVALUE_BYTES ",I,i,"
                                            BUILDVALUE_BYTES ","
                                            BUILDVALUE_BYTES ")",
                                            request_type, name, pattern,
                                            request_info, request_info_size_tmp,
                                            request, request_size_tmp,
                                            timeout_tmp, priority_tmp,
                                            trans_id, trans_id_size_tmp,
                                            source, source_size_tmp);
            if (args)
            {
                result = PyObject_CallObject(m_f, args);
                Py_DECREF(args);
            }
            if (result == NULL)
            {
                PyTypeObject * exception =
                    reinterpret_cast<PyTypeObject *>(PyErr_Occurred());
                PY_ASSERT_VOID(exception && exception->tp_name);

                exception_success_t exception_success = CALLBACK_ERROR;
                if (PyErr_ExceptionMatches(object->m_return_sync_exception))
                {
                    exception_success = CALLBACK_RETURN_SYNC;
                }
                else if (PyErr_ExceptionMatches(
                             object->m_return_async_exception))
                {
                    exception_success = CALLBACK_RETURN_ASYNC;
                }
                else if (PyErr_ExceptionMatches(
                             object->m_forward_sync_exception))
                {
                    exception_success = CALLBACK_FORWARD_SYNC;
                }
                else if (PyErr_ExceptionMatches(
                             object->m_forward_async_exception))
                {
                    exception_success = CALLBACK_FORWARD_ASYNC;
                }
                int immediate_exit_code = 0;
                if ((request_type == CloudI::API::ASYNC &&
                     ((exception_success == CALLBACK_RETURN_ASYNC) ||
                      (exception_success == CALLBACK_FORWARD_ASYNC))) ||
                    (request_type == CloudI::API::SYNC &&
                     ((exception_success == CALLBACK_RETURN_SYNC) ||
                      (exception_success == CALLBACK_FORWARD_SYNC))))
                {
                    PyErr_Clear();
                }
                else
                {
                    exception_success = CALLBACK_ERROR;
                    if (PyErr_ExceptionMatches(object->m_terminate_exception))
                    {
                        PyErr_Clear();
                    }
                    else if (PyErr_ExceptionMatches(
                                 object->m_message_decoding_exception) ||
                             PyErr_ExceptionMatches(
                                 object->m_invalid_input_exception) ||
                             PyErr_ExceptionMatches(PyExc_AssertionError) ||
                             PyErr_ExceptionMatches(PyExc_MemoryError) ||
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
                }
                THREADS_BEGIN;

                switch (exception_success)
                {
                    case CALLBACK_ERROR:
                        if (immediate_exit_code)
                            ::exit(immediate_exit_code);
                        // allow empty response to automatically be sent
                        return;
                    // return from the callback
                    // (the CloudI API return or forward function was
                    //  already called with valid data)
                    case CALLBACK_RETURN_ASYNC:
                        throw CloudI::API::return_async_exception();
                    case CALLBACK_RETURN_SYNC:
                        throw CloudI::API::return_sync_exception();
                    case CALLBACK_FORWARD_ASYNC:
                        throw CloudI::API::forward_async_exception();
                    case CALLBACK_FORWARD_SYNC:
                        throw CloudI::API::forward_sync_exception();
                    default:
                        PY_ASSERT_VOID(false);
                        break;
                }
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
                PyObject * result_new = NULL;
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
                    else if (overflow_uint32(response_info_size_tmp) ||
                             overflow_uint32(response_size_tmp))
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
                    else if (overflow_uint32(response_size_tmp))
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
                    result_new = PyUnicode_AsUTF8String(result);
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
                    }
#endif
                    if (overflow_uint32(response_size_tmp))
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

                // return from the callback
                if (result_invalid)
                {
                    // allow empty response to automatically be sent
                }
                else if (request_type == CloudI::API::ASYNC)
                {
                    try
                    {
                        api.return_async(name, pattern,
                                         response_info, response_info_size,
                                         response, response_size,
                                         timeout, trans_id,
                                         source, source_size);
                    }
                    catch (CloudI::API::return_async_exception const & e)
                    {
                        Py_DECREF(result);
                        if (result_new)
                        {
                            Py_DECREF(result_new);
                        }
                        THREADS_BEGIN;
                        throw e;
                    }
                }
                else if (request_type == CloudI::API::SYNC)
                {
                    try
                    {
                        api.return_sync(name, pattern,
                                        response_info, response_info_size,
                                        response, response_size,
                                        timeout, trans_id,
                                        source, source_size);
                    }
                    catch (CloudI::API::return_sync_exception const & e)
                    {
                        Py_DECREF(result);
                        if (result_new)
                        {
                            Py_DECREF(result_new);
                        }
                        THREADS_BEGIN;
                        throw e;
                    }
                }
                else
                {
                    // allow empty response to automatically be sent
                }
                Py_DECREF(result);
                if (result_new)
                {
                    Py_DECREF(result_new);
                }
                THREADS_BEGIN;
            }
        }

    private:
        PyObject * m_f;
        python_cloudi_instance_object * const m_object;

};

static void
python_cloudi_instance_object_dealloc(PyObject * self)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    if (object->m_thread_state)
    {
        // should never happen
        PyEval_RestoreThread(object->m_thread_state);
    }
    PyObject_GC_UnTrack(self);
    if (object->m_api)
    {
        if (object->m_callbacks)
        {
            callback::clear(object);
            delete object->m_callbacks;
            object->m_callbacks = 0;
        }
        delete object->m_api;
        object->m_api = 0;
    }
    object->m_thread_state = 0;
    PyObject_GC_Del(self);
}

static int
python_cloudi_instance_object_traverse(PyObject *self,
                                       visitproc visit, void *arg)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    return callback::traverse(object, visit, arg);
}

static int
python_cloudi_instance_object_clear(PyObject *self)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    return callback::clear(object);
}

static int
python_cloudi_instance_object_init(PyObject * self, PyObject * args,
                                   PyObject *)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    unsigned int thread_index;
    PyObject * invalid_input_exception;
    PyObject * message_decoding_exception;
    PyObject * terminate_exception;
    PyObject * return_sync_exception;
    PyObject * return_async_exception;
    PyObject * forward_sync_exception;
    PyObject * forward_async_exception;
    if (! PyArg_ParseTuple(args, "IOOOOOOO:__init__()",
                           &thread_index,
                           &invalid_input_exception,
                           &message_decoding_exception,
                           &terminate_exception,
                           &return_sync_exception,
                           &return_async_exception,
                           &forward_sync_exception,
                           &forward_async_exception))
    {
        PyErr_Print();
        return -1;
    }
    try
    {
        object->m_api = new CloudI::API(thread_index, true);
        object->m_thread_state = 0;
        object->m_invalid_input_exception = invalid_input_exception;
        object->m_message_decoding_exception = message_decoding_exception;
        object->m_terminate_exception = terminate_exception;
        object->m_return_sync_exception = return_sync_exception;
        object->m_return_async_exception = return_async_exception;
        object->m_forward_sync_exception = forward_sync_exception;
        object->m_forward_async_exception = forward_async_exception;
        object->m_callbacks = new callbacks_t();
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
    PyObject_GC_Track(self);
    return 0;
}

static PyObject *
python_cloudi_instance_object_new(PyTypeObject * type, PyObject *, PyObject *)
{
    python_cloudi_instance_object * self =
        PyObject_GC_New(python_cloudi_instance_object, type);
    if (self)
    {
        self->m_api = 0;
        self->m_thread_state = 0;
        self->m_callbacks = 0;
    }
    return reinterpret_cast<PyObject *>(self);
}

static PyObject *
python_cloudi_subscribe(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * pattern = NULL;
    PyObject * f = NULL;
    if (! PyArg_ParseTuple(args, "sO:subscribe", &pattern, &f))
    {
        PyErr_Print();
        return NULL;
    }
    if (! f || ! PyCallable_Check(f))
    {
        PyErr_SetString(object->m_invalid_input_exception,
                        "subscribe: not_callable");
        return NULL;
    }
    int result = 0;
    THREADS_BEGIN;
    result = api->subscribe(pattern, new callback(f, object));
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_subscribe_count(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * pattern = NULL;
    if (! PyArg_ParseTuple(args, "s:subscribe_count", &pattern))
    {
        PyErr_Print();
        return NULL;
    }
    int result = 0;
    THREADS_BEGIN;
    result = api->subscribe_count(pattern);
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    unsigned int const subscribe_count_tmp = api->get_subscribe_count();
    return Py_BuildValue("I", subscribe_count_tmp);
}

static PyObject *
python_cloudi_unsubscribe(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * pattern = NULL;
    if (! PyArg_ParseTuple(args, "s:unsubscribe", &pattern))
    {
        PyErr_Print();
        return NULL;
    }
    int result = 0;
    THREADS_BEGIN;
    result = api->unsubscribe(pattern);
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_send_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * name = NULL;
    char const * request = NULL;
    uint32_t request_size = 0;
    Py_ssize_t request_size_tmp = 0;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = api->timeout_async();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    Py_ssize_t request_info_size_tmp = 0;
    int8_t priority = api->priority_default();
    static char const * const kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "s" BUILDVALUE_BYTES "|I"
                                      BUILDVALUE_BYTES "B:send_async",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size_tmp,
                                      &timeout_tmp,
                                      &request_info, &request_info_size_tmp,
                                      &priority))
    {
        PyErr_Print();
        return NULL;
    }
    if (overflow_uint32(request_info_size_tmp) ||
        overflow_uint32(request_size_tmp) ||
        overflow_uint32(timeout_tmp))
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTupleAndKeywords");
        return NULL;
    }
    request_info_size = static_cast<uint32_t>(request_info_size_tmp);
    request_size = static_cast<uint32_t>(request_size_tmp);
    timeout = timeout_tmp;
    int result = 0;
    THREADS_BEGIN;
    result = api->send_async(name, request_info, request_info_size,
                             request, request_size, timeout, priority);
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    PY_ASSERT(api->get_trans_id_count() == 1);
    Py_ssize_t const trans_id_size_tmp = 16;
    return Py_BuildValue(BUILDVALUE_BYTES,
                         api->get_trans_id(0), trans_id_size_tmp);
}

static PyObject *
python_cloudi_send_sync(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * name = NULL;
    char const * request = NULL;
    uint32_t request_size = 0;
    Py_ssize_t request_size_tmp = 0;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = api->timeout_sync();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    Py_ssize_t request_info_size_tmp = 0;
    int8_t priority = api->priority_default();
    static char const * const kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "s" BUILDVALUE_BYTES "|I"
                                      BUILDVALUE_BYTES "B:send_sync",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size_tmp,
                                      &timeout_tmp,
                                      &request_info, &request_info_size_tmp,
                                      &priority))
    {
        PyErr_Print();
        return NULL;
    }
    if (overflow_uint32(request_info_size_tmp) ||
        overflow_uint32(request_size_tmp) ||
        overflow_uint32(timeout_tmp))
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTupleAndKeywords");
        return NULL;
    }
    request_info_size = static_cast<uint32_t>(request_info_size_tmp);
    request_size = static_cast<uint32_t>(request_size_tmp);
    timeout = timeout_tmp;
    int result = 0;
    THREADS_BEGIN;
    result = api->send_sync(name, request_info, request_info_size,
                            request, request_size, timeout, priority);
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    PY_ASSERT(api->get_trans_id_count() == 1);
    Py_ssize_t const response_info_size_tmp = api->get_response_info_size();
    Py_ssize_t const response_size_tmp = api->get_response_size();
    Py_ssize_t const trans_id_size_tmp = 16;
    PY_ASSERT(static_cast<uint32_t>(response_info_size_tmp) ==
              api->get_response_info_size());
    PY_ASSERT(static_cast<uint32_t>(response_size_tmp) ==
              api->get_response_size());
    return Py_BuildValue("(" BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ")",
                         api->get_response_info(), response_info_size_tmp,
                         api->get_response(), response_size_tmp,
                         api->get_trans_id(0), trans_id_size_tmp);
}

static PyObject *
python_cloudi_mcast_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * name = NULL;
    char const * request = NULL;
    uint32_t request_size = 0;
    Py_ssize_t request_size_tmp = 0;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = api->timeout_async();
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    Py_ssize_t request_info_size_tmp = 0;
    int8_t priority = api->priority_default();
    static char const * const kwlist[] = {
        "timeout", "request_info", "priority", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "s" BUILDVALUE_BYTES "|I"
                                      BUILDVALUE_BYTES "B:mcast_async",
                                      const_cast<char**>(kwlist),
                                      &name, &request, &request_size_tmp,
                                      &timeout_tmp,
                                      &request_info, &request_info_size_tmp,
                                      &priority))
    {
        PyErr_Print();
        return NULL;
    }
    if (overflow_uint32(request_info_size_tmp) ||
        overflow_uint32(request_size_tmp) ||
        overflow_uint32(timeout_tmp))
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTupleAndKeywords");
        return NULL;
    }
    request_info_size = static_cast<uint32_t>(request_info_size_tmp);
    request_size = static_cast<uint32_t>(request_size_tmp);
    timeout = timeout_tmp;
    int result = 0;
    THREADS_BEGIN;
    result = api->mcast_async(name, request_info, request_info_size,
                              request, request_size, timeout, priority);
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_ssize_t const trans_ids_size_tmp = api->get_trans_id_count() * 16;
    return Py_BuildValue(BUILDVALUE_BYTES,
                         api->get_trans_id(0), trans_ids_size_tmp);
}

static PyObject *
python_cloudi_forward_async(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * name = NULL;
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    Py_ssize_t request_info_size_tmp = 0;
    char const * request = NULL;
    uint32_t request_size = 0;
    Py_ssize_t request_size_tmp = 0;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = 0;
    int8_t priority = 0;
    char const * trans_id = NULL;
    Py_ssize_t trans_id_size_tmp = 0;
    char const * source = NULL;
    uint32_t source_size = 0;
    Py_ssize_t source_size_tmp = 0;
    if (! PyArg_ParseTuple(args,
                           "s" BUILDVALUE_BYTES BUILDVALUE_BYTES "IB"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":forward_async",
                           &name, &request_info, &request_info_size_tmp,
                           &request, &request_size_tmp,
                           &timeout_tmp, &priority,
                           &trans_id, &trans_id_size_tmp,
                           &source, &source_size_tmp))
    {
        PyErr_Print();
        return NULL;
    }
    if (overflow_uint32(request_info_size_tmp) ||
        overflow_uint32(request_size_tmp) ||
        overflow_uint32(timeout_tmp) ||
        overflow_uint32(source_size_tmp))
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTuple");
        return NULL;
    }
    request_info_size = static_cast<uint32_t>(request_info_size_tmp);
    request_size = static_cast<uint32_t>(request_size_tmp);
    timeout = timeout_tmp;
    source_size = static_cast<uint32_t>(source_size_tmp);
    PY_ASSERT(trans_id_size_tmp == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->forward_async(name,
                                    request_info, request_info_size,
                                    request, request_size,
                                    timeout, priority,
                                    trans_id, source, source_size);
    }
    catch (CloudI::API::forward_async_exception const &)
    {
    }
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_forward_sync(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * name = NULL;
    char const * request_info = NULL;
    uint32_t request_info_size = 0;
    Py_ssize_t request_info_size_tmp = 0;
    char const * request = NULL;
    uint32_t request_size = 0;
    Py_ssize_t request_size_tmp = 0;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = 0;
    int8_t priority = 0;
    char const * trans_id = NULL;
    Py_ssize_t trans_id_size_tmp = 0;
    char const * source = NULL;
    uint32_t source_size = 0;
    Py_ssize_t source_size_tmp = 0;
    if (! PyArg_ParseTuple(args,
                           "s" BUILDVALUE_BYTES BUILDVALUE_BYTES "IB"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":forward_sync",
                           &name, &request_info, &request_info_size_tmp,
                           &request, &request_size_tmp,
                           &timeout_tmp, &priority,
                           &trans_id, &trans_id_size_tmp,
                           &source, &source_size_tmp))
    {
        PyErr_Print();
        return NULL;
    }
    if (overflow_uint32(request_info_size_tmp) ||
        overflow_uint32(request_size_tmp) ||
        overflow_uint32(timeout_tmp) ||
        overflow_uint32(source_size_tmp))
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTuple");
        return NULL;
    }
    request_info_size = static_cast<uint32_t>(request_info_size_tmp);
    request_size = static_cast<uint32_t>(request_size_tmp);
    timeout = timeout_tmp;
    source_size = static_cast<uint32_t>(source_size_tmp);
    PY_ASSERT(trans_id_size_tmp == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->forward_sync(name,
                                   request_info, request_info_size,
                                   request, request_size,
                                   timeout, priority,
                                   trans_id, source, source_size);
    }
    catch (CloudI::API::forward_sync_exception const &)
    {
    }
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_return_async(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * name = NULL;
    char const * pattern = NULL;
    char const * response_info = NULL;
    uint32_t response_info_size = 0;
    Py_ssize_t response_info_size_tmp = 0;
    char const * response = NULL;
    uint32_t response_size = 0;
    Py_ssize_t response_size_tmp = 0;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = 0;
    char const * trans_id = NULL;
    Py_ssize_t trans_id_size_tmp = 0;
    char const * source = NULL;
    uint32_t source_size = 0;
    Py_ssize_t source_size_tmp = 0;
    if (! PyArg_ParseTuple(args,
                           "ss" BUILDVALUE_BYTES BUILDVALUE_BYTES "I"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":return_async",
                           &name, &pattern,
                           &response_info, &response_info_size_tmp,
                           &response, &response_size_tmp, &timeout_tmp,
                           &trans_id, &trans_id_size_tmp,
                           &source, &source_size_tmp))
    {
        PyErr_Print();
        return NULL;
    }
    if (overflow_uint32(response_info_size_tmp) ||
        overflow_uint32(response_size_tmp) ||
        overflow_uint32(timeout_tmp) ||
        overflow_uint32(source_size_tmp))
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTuple");
        return NULL;
    }
    response_info_size = static_cast<uint32_t>(response_info_size_tmp);
    response_size = static_cast<uint32_t>(response_size_tmp);
    timeout = timeout_tmp;
    source_size = static_cast<uint32_t>(source_size_tmp);
    PY_ASSERT(trans_id_size_tmp == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->return_async(name, pattern,
                                   response_info, response_info_size,
                                   response, response_size,
                                   timeout, trans_id, source, source_size);
    }
    catch (CloudI::API::return_async_exception const &)
    {
    }
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_return_sync(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * name = NULL;
    char const * pattern = NULL;
    char const * response_info = NULL;
    uint32_t response_info_size = 0;
    Py_ssize_t response_info_size_tmp = 0;
    char const * response = NULL;
    uint32_t response_size = 0;
    Py_ssize_t response_size_tmp = 0;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = 0;
    char const * trans_id = NULL;
    Py_ssize_t trans_id_size_tmp = 0;
    char const * source = NULL;
    uint32_t source_size = 0;
    Py_ssize_t source_size_tmp = 0;
    if (! PyArg_ParseTuple(args,
                           "ss" BUILDVALUE_BYTES BUILDVALUE_BYTES "I"
                           BUILDVALUE_BYTES BUILDVALUE_BYTES ":return_sync",
                           &name, &pattern,
                           &response_info, &response_info_size_tmp,
                           &response, &response_size_tmp, &timeout_tmp,
                           &trans_id, &trans_id_size_tmp,
                           &source, &source_size_tmp))
    {
        PyErr_Print();
        return NULL;
    }
    if (overflow_uint32(response_info_size_tmp) ||
        overflow_uint32(response_size_tmp) ||
        overflow_uint32(timeout_tmp) ||
        overflow_uint32(source_size_tmp))
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTuple");
        return NULL;
    }
    response_info_size = static_cast<uint32_t>(response_info_size_tmp);
    response_size = static_cast<uint32_t>(response_size_tmp);
    timeout = timeout_tmp;
    source_size = static_cast<uint32_t>(source_size_tmp);
    PY_ASSERT(trans_id_size_tmp == 16);
    int result = 0;
    THREADS_BEGIN;
    try
    {
        result = api->return_sync(name, pattern,
                                  response_info, response_info_size,
                                  response, response_size,
                                  timeout, trans_id, source, source_size);
    }
    catch (CloudI::API::return_sync_exception const &)
    {
    }
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

static PyObject *
python_cloudi_recv_async(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    uint32_t timeout = 0;
    unsigned int timeout_tmp = api->timeout_sync();
    char const * trans_id = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
    Py_ssize_t trans_id_size_tmp = 16;
    unsigned char consume_tmp = 1;
    static char const * const kwlist[] = {
        "timeout", "trans_id", "consume", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "|I" BUILDVALUE_BYTES "b:recv_async",
                                      const_cast<char**>(kwlist),
                                      &timeout_tmp, &trans_id,
                                      &trans_id_size_tmp, &consume_tmp))
    {
        PyErr_Print();
        return NULL;
    }
    timeout = timeout_tmp;
    if (timeout != timeout_tmp)
    {
        PyErr_SetString(PyExc_OverflowError, "PyArg_ParseTupleAndKeywords");
        return NULL;
    }
    PY_ASSERT(trans_id_size_tmp == 16);
    int result = 0;
    THREADS_BEGIN;
    result = api->recv_async(timeout, trans_id, consume_tmp != 0);
    THREADS_END;
    if (result)
    {
        PY_ASSERT(result != CloudI::API::return_value::timeout);
        python_error(object, result);
        return NULL;
    }
    Py_ssize_t const response_info_size_tmp = api->get_response_info_size();
    Py_ssize_t const response_size_tmp = api->get_response_size();
    PY_ASSERT(static_cast<uint32_t>(response_info_size_tmp) ==
              api->get_response_info_size());
    PY_ASSERT(static_cast<uint32_t>(response_size_tmp) ==
              api->get_response_size());
    PY_ASSERT(api->get_trans_id_count() == 1);
    return Py_BuildValue("(" BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ","
                         BUILDVALUE_BYTES ")",
                         api->get_response_info(), response_info_size_tmp,
                         api->get_response(), response_size_tmp,
                         api->get_trans_id(0), trans_id_size_tmp);
}

#define UNSIGNED_INT_GETTER(NAME)                                          \
static PyObject *                                                          \
python_cloudi_##NAME(PyObject * self, PyObject *)                          \
{                                                                          \
    python_cloudi_instance_object * const object =                         \
        reinterpret_cast<python_cloudi_instance_object *>(self);           \
    CloudI::API const * const api = object->m_api;                         \
    unsigned int const NAME##_tmp = api->NAME();                           \
    return Py_BuildValue("I", NAME##_tmp);                                 \
}

UNSIGNED_INT_GETTER(process_index)
UNSIGNED_INT_GETTER(process_count)
UNSIGNED_INT_GETTER(process_count_max)
UNSIGNED_INT_GETTER(process_count_min)

static PyObject *
python_cloudi_prefix(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API const * const api = object->m_api;
    return Py_BuildValue("s", api->prefix());
}

UNSIGNED_INT_GETTER(timeout_initialize)
UNSIGNED_INT_GETTER(timeout_async)
UNSIGNED_INT_GETTER(timeout_sync)
UNSIGNED_INT_GETTER(timeout_terminate)

static PyObject *
python_cloudi_priority_default(PyObject * self, PyObject *)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API const * const api = object->m_api;
    int const priority_tmp = api->priority_default();
    return Py_BuildValue("i", priority_tmp);
}

static PyObject *
python_cloudi_poll(PyObject * self, PyObject * args)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    int timeout = -1;
    if (! PyArg_ParseTuple(args, "i:poll", &timeout))
    {
        PyErr_Print();
        return NULL;
    }
    int result = 0;
    THREADS_BEGIN;
    result = api->poll(timeout);
    THREADS_END;
    if (result)
    {
        if (result == CloudI::API::return_value::timeout)
        {
            return Py_BuildValue("O", Py_True);
        }
        python_error(object, result);
        return NULL;
    }
    return Py_BuildValue("O", Py_False);
}

static PyObject *
python_cloudi_shutdown(PyObject * self, PyObject * args, PyObject * kwargs)
{
    python_cloudi_instance_object * const object =
        reinterpret_cast<python_cloudi_instance_object *>(self);
    CloudI::API * const api = object->m_api;
    char const * reason = "";
    static char const * const kwlist[] = {
        "reason", NULL};
    if (! PyArg_ParseTupleAndKeywords(args, kwargs,
                                      "|s:shutdown",
                                      const_cast<char**>(kwlist),
                                      &reason))
    {
        PyErr_Print();
        return NULL;
    }
    int result = 0;
    THREADS_BEGIN;
    result = api->shutdown(reason);
    THREADS_END;
    if (result)
    {
        python_error(object, result);
        return NULL;
    }
    Py_RETURN_NONE;
}

