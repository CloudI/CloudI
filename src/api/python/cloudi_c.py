#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# BSD LICENSE
# 
# Copyright (c) 2012-2015, Michael Truog <mjtruog at gmail dot com>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#     * All advertising materials mentioning features or use of this
#       software must display the following acknowledgment:
#         This product includes software developed by Michael Truog
#     * The name of the author may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
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

    def forward_(self, command, name, request_info, request,
                 timeout, priority, trans_id, pid):
        if command == API.ASYNC:
            self.forward_async(name, request_info, request,
                               timeout, priority, trans_id, pid)
        elif command == API.SYNC:
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

    def return_(self, command, name, pattern, response_info, response,
                timeout, trans_id, pid):
        if command == API.ASYNC:
            self.return_async(name, pattern, response_info, response,
                              timeout, trans_id, pid)
        elif command == API.SYNC:
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

    def request_http_qs_parse(self, request):
        return self.__text_key_value_parse(request)

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

