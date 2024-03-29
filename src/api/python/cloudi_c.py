#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2012-2023 Michael Truog <mjtruog at protonmail dot com>
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
"""
Python/C CloudI API <https://cloudi.org/api.html#1_Intro>.
Example usage is available in the
integration tests <https://cloudi.org/tutorials.html#cloudi_examples>.
"""

import sys
import os
import traceback
import inspect
from functools import partial
import libcloudi_py

__all__ = [
    'API',
    'InvalidInputException',
    'MessageDecodingException',
    'TerminateException',
    'FatalError',
]

if sys.version_info[0] >= 3:
    def _function_argc(function):
        args, _, _, _, _, _, _ = inspect.getfullargspec(function)
        return len(args)
else:
    def _function_argc(function):
        # pylint: disable=deprecated-method
        args, _, _, _ = inspect.getargspec(function)
        return len(args)

# pylint: disable=too-many-instance-attributes
# pylint: disable=too-many-public-methods
# pylint: disable=useless-object-inheritance
class API(object):
    """
    CloudI API object for use in a single thread of execution
    """

    ASYNC = 1
    SYNC = -1

    def __init__(self, thread_index):
        self.__api = libcloudi_py.cloudi_c(thread_index,
                                           InvalidInputException,
                                           MessageDecodingException,
                                           TerminateException,
                                           ReturnSyncException,
                                           ReturnAsyncException,
                                           ForwardSyncException,
                                           ForwardAsyncException)

    @staticmethod
    def thread_count():
        """
        returns the thread count from the service configuration
        """
        return API.__getenv_to_uint('CLOUDI_API_INIT_THREAD_COUNT')

    def subscribe(self, pattern, function):
        """
        subscribes to a service name pattern with a callback
        """
        if _function_argc(function) != 10:
            # self + arguments for a member function
            #  api + arguments for a static function
            raise InvalidInputException()
        if not inspect.ismethod(function):
            function = partial(function, self)
        self.__api.subscribe(pattern, function)

    def subscribe_count(self, pattern):
        """
        returns the number of subscriptions for a single service name pattern
        """
        return self.__api.subscribe_count(pattern)

    def unsubscribe(self, pattern):
        """
        unsubscribes from a service name pattern once
        """
        self.__api.unsubscribe(pattern)

    def send_async(self, name, request,
                   timeout=None, request_info=None, priority=None):
        """
        sends an asynchronous service request
        """
        # pylint: disable=too-many-arguments
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        return self.__api.send_async(name, request, **kwargs)

    def send_sync(self, name, request,
                  timeout=None, request_info=None, priority=None):
        """
        sends a synchronous service request
        """
        # pylint: disable=too-many-arguments
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        return self.__api.send_sync(name, request, **kwargs)

    def mcast_async(self, name, request,
                    timeout=None, request_info=None, priority=None):
        """
        sends asynchronous service requests to all subscribers
        of the matching service name pattern
        """
        # pylint: disable=too-many-arguments
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        trans_ids = self.__api.mcast_async(name, request, **kwargs)
        return tuple([
            trans_ids[i:i + 16] for i in range(0, len(trans_ids), 16)
        ])

    def forward_(self, request_type, name, request_info, request,
                 timeout, priority, trans_id, source):
        """
        forwards a service request to a different service name
        """
        # pylint: disable=too-many-arguments
        if request_type == API.ASYNC:
            self.forward_async(name, request_info, request,
                               timeout, priority, trans_id, source)
        elif request_type == API.SYNC:
            self.forward_sync(name, request_info, request,
                              timeout, priority, trans_id, source)
        else:
            raise InvalidInputException()

    def forward_async(self, name, request_info, request,
                      timeout, priority, trans_id, source):
        """
        forwards an asynchronous service request to a different service name
        """
        # pylint: disable=too-many-arguments
        self.__api.forward_async(name, request_info, request,
                                 timeout, priority, trans_id, source)
        raise ForwardAsyncException()

    def forward_sync(self, name, request_info, request,
                     timeout, priority, trans_id, source):
        """
        forwards a synchronous service request to a different service name
        """
        # pylint: disable=too-many-arguments
        self.__api.forward_sync(name, request_info, request,
                                timeout, priority, trans_id, source)
        raise ForwardSyncException()

    def return_(self, request_type, name, pattern, response_info, response,
                timeout, trans_id, source):
        """
        provides a response to a service request
        """
        # pylint: disable=too-many-arguments
        if request_type == API.ASYNC:
            self.return_async(name, pattern, response_info, response,
                              timeout, trans_id, source)
        elif request_type == API.SYNC:
            self.return_sync(name, pattern, response_info, response,
                             timeout, trans_id, source)
        else:
            raise InvalidInputException()

    def return_async(self, name, pattern, response_info, response,
                     timeout, trans_id, source):
        """
        provides a response to an asynchronous service request
        """
        # pylint: disable=too-many-arguments
        self.__api.return_async(name, pattern, response_info, response,
                                timeout, trans_id, source)
        raise ReturnAsyncException()

    def return_sync(self, name, pattern, response_info, response,
                    timeout, trans_id, source):
        """
        provides a response to a synchronous service request
        """
        # pylint: disable=too-many-arguments
        self.__api.return_sync(name, pattern, response_info, response,
                               timeout, trans_id, source)
        raise ReturnSyncException()

    def recv_async(self, timeout=None, trans_id=None, consume=None):
        """
        blocks to receive an asynchronous service request response
        """
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if trans_id is not None:
            kwargs['trans_id'] = trans_id
        if consume is not None:
            kwargs['consume'] = consume
        return self.__api.recv_async(**kwargs)

    def process_index(self):
        """
        returns the 0-based index of this process in the service instance
        """
        return self.__api.process_index()

    @staticmethod
    def process_index_():
        """
        returns the 0-based index of this process in the service instance
        """
        return API.__getenv_to_uint('CLOUDI_API_INIT_PROCESS_INDEX')

    def process_count(self):
        """
        returns the current process count based on the service configuration
        """
        return self.__api.process_count()

    def process_count_max(self):
        """
        returns the count_process_dynamic maximum count
        """
        return self.__api.process_count_max()

    @staticmethod
    def process_count_max_():
        """
        returns the count_process_dynamic maximum count
        """
        return API.__getenv_to_uint('CLOUDI_API_INIT_PROCESS_COUNT_MAX')

    def process_count_min(self):
        """
        returns the count_process_dynamic minimum count
        """
        return self.__api.process_count_min()

    @staticmethod
    def process_count_min_():
        """
        returns the count_process_dynamic minimum count
        """
        return API.__getenv_to_uint('CLOUDI_API_INIT_PROCESS_COUNT_MIN')

    def prefix(self):
        """
        returns the service name pattern prefix from the service configuration
        """
        return self.__api.prefix()

    def timeout_initialize(self):
        """
        returns the service initialization timeout
        """
        return self.__api.timeout_initialize()

    @staticmethod
    def timeout_initialize_():
        """
        returns the service initialization timeout
        """
        return API.__getenv_to_uint('CLOUDI_API_INIT_TIMEOUT_INITIALIZE')

    def timeout_async(self):
        """
        returns the default asynchronous service request send timeout
        """
        return self.__api.timeout_async()

    def timeout_sync(self):
        """
        returns the default synchronous service request send timeout
        """
        return self.__api.timeout_sync()

    def timeout_terminate(self):
        """
        returns the service termination timeout
        """
        return self.__api.timeout_terminate()

    @staticmethod
    def timeout_terminate_():
        """
        returns the service termination timeout
        """
        return API.__getenv_to_uint('CLOUDI_API_INIT_TIMEOUT_TERMINATE')

    def priority_default(self):
        """
        returns the default service request send priority
        """
        return self.__api.priority_default()

    def poll(self, timeout=-1):
        """
        blocks to process incoming CloudI service requests
        """
        return self.__api.poll(timeout)

    def shutdown(self, reason=None):
        """
        shutdown the service successfully
        """
        kwargs = {}
        if reason is not None:
            kwargs['reason'] = reason
        self.__api.shutdown(**kwargs)

    @staticmethod
    def __text_pairs_parse(text):
        pairs = {}
        data = text.split(b'\0')
        for i in range(0, len(data) - 1, 2):
            key = data[i]
            current = pairs.get(key, None)
            if current is None:
                pairs[key] = data[i + 1]
            elif isinstance(current, list):
                current.append(data[i + 1])
            else:
                pairs[key] = [current, data[i + 1]]
        return pairs

    @staticmethod
    def __text_pairs_new(pairs, response):
        text_segments = []
        for key, values in pairs.items():
            if isinstance(values, bytes):
                text_segments.append(key)
                text_segments.append(values)
            else:
                assert not isinstance(values, str)
                for value in values:
                    text_segments.append(key)
                    text_segments.append(value)
        if response and text_segments == []:
            return b'\0'
        text_segments.append(b'')
        return b'\0'.join(text_segments)

    @staticmethod
    def info_key_value_parse(info):
        """
        decode service request info key/value data
        """
        return API.__text_pairs_parse(info)

    @staticmethod
    def info_key_value_new(pairs, response=True):
        """
        encode service response info key/value data
        """
        return API.__text_pairs_new(pairs, response)

    @staticmethod
    def __getenv_to_uint(name):
        value_str = os.getenv(name)
        if value_str is None:
            raise InvalidInputException()
        value = int(value_str)
        if value < 0:
            raise InvalidInputException()
        return value

class InvalidInputException(Exception):
    """
    Invalid Input
    """
    def __init__(self, message=None):
        if message is None:
            message = 'Invalid Input'
        Exception.__init__(self, message)

class ReturnSyncException(Exception):
    """
    Synchronous Call Return Invalid
    """
    def __init__(self):
        Exception.__init__(self, 'Synchronous Call Return Invalid')

class ReturnAsyncException(Exception):
    """
    Asynchronous Call Return Invalid
    """
    def __init__(self):
        Exception.__init__(self, 'Asynchronous Call Return Invalid')

class ForwardSyncException(Exception):
    """
    Synchronous Call Forward Invalid
    """
    def __init__(self):
        Exception.__init__(self, 'Synchronous Call Forward Invalid')

class ForwardAsyncException(Exception):
    """
    Asynchronous Call Forward Invalid
    """
    def __init__(self):
        Exception.__init__(self, 'Asynchronous Call Forward Invalid')

class MessageDecodingException(Exception):
    """
    Message Decoding Error
    """
    def __init__(self, message):
        Exception.__init__(self, message)

class TerminateException(Exception):
    """
    Terminate
    """
    def __init__(self, timeout):
        Exception.__init__(self, 'Terminate')
        self.__timeout = timeout

    def timeout(self):
        """
        return the termination timeout
        """
        return self.__timeout

class FatalError(BaseException):
    """
    Fatal Error
    """
    def __init__(self, message):
        BaseException.__init__(self, message)


# force unbuffered stdout/stderr handling without external configuration
if sys.stderr.__class__.__name__ != '_unbuffered':
    class _unbuffered(object):
        # pylint: disable=too-few-public-methods
        def __init__(self, stream):
            # pylint: disable=import-outside-toplevel
            if sys.version_info[0] >= 3:
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
                self.encoding = 'UTF-8'
                self.__stream = codecs.getwriter(self.encoding)(stream)

        def write(self, data):
            """
            unbuffered write function
            """
            self.__stream.write(data)
            self.__stream.flush()

        def __getattr__(self, attr):
            return getattr(self.__stream, attr)

    sys.stdout = _unbuffered(sys.stdout)
    sys.stderr = _unbuffered(sys.stderr)
