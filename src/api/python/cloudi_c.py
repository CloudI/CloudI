#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2012-2017 Michael Truog <mjtruog at gmail dot com>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

__all__ = [
    'API',
    'invalid_input_exception',
    'message_decoding_exception',
    'terminate_exception',
]

import sys, os, socket
import libcloudi_py

class API(object):
    ASYNC  =  1
    SYNC   = -1

    def __init__(self, thread_index):
        self.__timeout_terminate = 1000 # TIMEOUT_TERMINATE_MIN
        exception = None
        try:
            self.__api = libcloudi_py.cloudi_c(thread_index)
        except Exception as e:
            exception = e
        if exception is not None:
            self.__rethrow_exception(exception)
        self.__timeout_terminate = self.__api.timeout_terminate()

    @staticmethod
    def thread_count():
        s = os.getenv('CLOUDI_API_INIT_THREAD_COUNT')
        if s is None:
            raise invalid_input_exception()
        return int(s)

    def subscribe(self, pattern, Function):
        self.__api.subscribe(pattern, Function)

    def subscribe_count(self, pattern):
        try:
            return self.__api.subscribe_count(pattern)
        except Exception as e:
            exception = e
        self.__rethrow_exception(exception)

    def unsubscribe(self, pattern):
        self.__api.unsubscribe(pattern)

    def send_async(self, name, request,
                   timeout=None, request_info=None, priority=None):
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        try:
            return self.__api.send_async(name, request, **kwargs)
        except Exception as e:
            exception = e
        self.__rethrow_exception(exception)

    def send_sync(self, name, request,
                  timeout=None, request_info=None, priority=None):
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        try:
            return self.__api.send_sync(name, request, **kwargs)
        except Exception as e:
            exception = e
        self.__rethrow_exception(exception)

    def mcast_async(self, name, request,
                    timeout=None, request_info=None, priority=None):
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        exception = None
        try:
            trans_ids = self.__api.mcast_async(name, request, **kwargs)
        except Exception as e:
            exception = e
        if exception is not None:
            self.__rethrow_exception(exception)
        if trans_ids is None:
            return tuple()
        return tuple([
            trans_ids[i:i + 16] for i in range(0, len(trans_ids), 16)
        ])

    def forward_(self, request_type, name, request_info, request,
                 timeout, priority, trans_id, pid):
        if request_type == API.ASYNC:
            self.forward_async(name, request_info, request,
                               timeout, priority, trans_id, pid)
        elif request_type == API.SYNC:
            self.forward_sync(name, request_info, request,
                              timeout, priority, trans_id, pid)
        else:
            raise invalid_input_exception()

    def forward_async(self, name, request_info, request,
                      timeout, priority, trans_id, pid):
        self.__api.forward_async(name, request_info, request,
                                 timeout, priority, trans_id, pid)
        raise forward_async_exception()

    def forward_sync(self, name, request_info, request,
                     timeout, priority, trans_id, pid):
        self.__api.forward_sync(name, request_info, request,
                                timeout, priority, trans_id, pid)
        raise forward_sync_exception()

    def return_(self, request_type, name, pattern, response_info, response,
                timeout, trans_id, pid):
        if request_type == API.ASYNC:
            self.return_async(name, pattern, response_info, response,
                              timeout, trans_id, pid)
        elif request_type == API.SYNC:
            self.return_sync(name, pattern, response_info, response,
                             timeout, trans_id, pid)
        else:
            raise invalid_input_exception()

    def return_async(self, name, pattern, response_info, response,
                     timeout, trans_id, pid):
        self.__api.return_async(name, pattern, response_info, response,
                                timeout, trans_id, pid)
        raise return_async_exception()

    def return_sync(self, name, pattern, response_info, response,
                    timeout, trans_id, pid):
        self.__api.return_sync(name, pattern, response_info, response,
                               timeout, trans_id, pid)
        raise return_sync_exception()

    def recv_async(self, timeout=None, trans_id=None, consume=None):
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if trans_id is not None:
            kwargs['trans_id'] = trans_id
        if consume is not None:
            kwargs['consume'] = consume
        try:
            return self.__api.recv_async(**kwargs)
        except Exception as e:
            exception = e
        self.__rethrow_exception(exception)

    def process_index(self):
        return self.__api.process_index()

    def process_count(self):
        return self.__api.process_count()

    def process_count_max(self):
        return self.__api.process_count_max()

    def process_count_min(self):
        return self.__api.process_count_min()

    def prefix(self):
        return self.__api.prefix()

    def timeout_initialize(self):
        return self.__api.timeout_initialize()

    def timeout_async(self):
        return self.__api.timeout_async()

    def timeout_sync(self):
        return self.__api.timeout_sync()

    def timeout_terminate(self):
        return self.__timeout_terminate

    def poll(self, timeout=-1):
        if timeout is None:
            timeout = -1
        exception = None
        try:
            return self.__api.poll(timeout)
        except Exception as e:
            exception = e
        if exception is not None:
            self.__rethrow_exception(exception)

    def __rethrow_exception(self, exception):
        if isinstance(exception, libcloudi_py.message_decoding_exception):
            raise message_decoding_exception(str(exception))
        elif isinstance(exception, libcloudi_py.invalid_input_exception):
            raise invalid_input_exception(str(exception))
        elif isinstance(exception, libcloudi_py.terminate_exception):
            raise terminate_exception(self.__timeout_terminate)
        else:
            raise exception

    def __text_key_value_parse(self, text):
        result = {}
        data = text.split(b'\0')
        for i in range(0, len(data) - 1, 2):
            key = data[i]
            current = result.get(key, None)
            if current is None:
                result[key] = data[i + 1]
            elif type(current) == list:
                current.append(data[i + 1])
            else:
                result[key] = [current, data[i + 1]]
        return result

    def info_key_value_parse(self, message_info):
        return self.__text_key_value_parse(message_info)

class invalid_input_exception(Exception):
    def __init__(self, message=None):
        if message is None:
            message = 'Invalid Input'
        Exception.__init__(self, message)

class return_sync_exception(Exception):
    def __init__(self):
        Exception.__init__(self, 'Synchronous Call Return Invalid')

class return_async_exception(Exception):
    def __init__(self):
        Exception.__init__(self, 'Asynchronous Call Return Invalid')

class forward_sync_exception(Exception):
    def __init__(self):
        Exception.__init__(self, 'Synchronous Call Forward Invalid')

class forward_async_exception(Exception):
    def __init__(self):
        Exception.__init__(self, 'Asynchronous Call Forward Invalid')

class message_decoding_exception(Exception):
    def __init__(self, message):
        Exception.__init__(self, message)

class terminate_exception(Exception):
    def __init__(self, timeout):
        Exception.__init__(self, 'Terminate')
        self.__timeout = timeout

    def timeout(self):
        return self.__timeout

# force unbuffered stdout/stderr handling without external configuration
if sys.stderr.__class__.__name__ != '_unbuffered':
    class _unbuffered(object):
        def __init__(self, stream):
            if int(sys.version[0]) >= 3:
                import io
                self.__stream = io.TextIOWrapper(
                    stream.buffer,
                    encoding='UTF-8',
                    errors=stream.errors,
                    newline=stream.newlines,
                    line_buffering=stream.line_buffering,
                    write_through=False,
                )
            else:
                import codecs
                self.__stream = codecs.getwriter('UTF-8')(stream)

        def write(self, data):
            self.__stream.write(data)
            self.__stream.flush()

        def __getattr__(self, attr):
            return getattr(self.__stream, attr)

    sys.stdout = _unbuffered(sys.stdout)
    sys.stderr = _unbuffered(sys.stderr)

