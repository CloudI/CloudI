#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2012-2018 Michael Truog <mjtruog at protonmail dot com>
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
import inspect
from functools import partial
import libcloudi_py

__all__ = [
    'API',
    'InvalidInputException',
    'MessageDecodingException',
    'TerminateException',
    # XXX backwards-compatibility
    'invalid_input_exception',
    'message_decoding_exception',
    'terminate_exception',
]

# pylint: disable=too-many-instance-attributes
# pylint: disable=too-many-public-methods
class API(object):
    """
    CloudI API object for use in a single thread of execution
    """

    ASYNC = 1
    SYNC = -1

    def __init__(self, thread_index):
        # pylint: disable=broad-except
        self.__timeout_terminate = 1000 # TIMEOUT_TERMINATE_MIN
        try:
            self.__api = libcloudi_py.cloudi_c(thread_index)
        except Exception as exception:
            self.__rethrow_exception(exception)
        self.__timeout_terminate = self.__api.timeout_terminate()

    @staticmethod
    def thread_count():
        """
        returns the thread count from the service configuration
        """
        thread_count = os.getenv('CLOUDI_API_INIT_THREAD_COUNT')
        if thread_count is None:
            raise InvalidInputException()
        return int(thread_count)

    def subscribe(self, pattern, function):
        """
        subscribes to a service name pattern with a callback
        """
        args, _, _, _ = inspect.getargspec(function)
        if len(args) != 10:
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
        # pylint: disable=broad-except
        try:
            return self.__api.subscribe_count(pattern)
        except Exception as exception:
            self.__rethrow_exception(exception)

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
        # pylint: disable=broad-except
        # pylint: disable=too-many-arguments
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        try:
            return self.__api.send_async(name, request, **kwargs)
        except Exception as exception:
            self.__rethrow_exception(exception)

    def send_sync(self, name, request,
                  timeout=None, request_info=None, priority=None):
        """
        sends a synchronous service request
        """
        # pylint: disable=broad-except
        # pylint: disable=too-many-arguments
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if request_info is not None:
            kwargs['request_info'] = request_info
        if priority is not None:
            kwargs['priority'] = priority
        try:
            return self.__api.send_sync(name, request, **kwargs)
        except Exception as exception:
            self.__rethrow_exception(exception)

    def mcast_async(self, name, request,
                    timeout=None, request_info=None, priority=None):
        """
        sends asynchronous service requests to all subscribers
        of the matching service name pattern
        """
        # pylint: disable=broad-except
        # pylint: disable=too-many-arguments
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
        except Exception as exception:
            self.__rethrow_exception(exception)
        if trans_ids is None:
            return tuple()
        return tuple([
            trans_ids[i:i + 16] for i in range(0, len(trans_ids), 16)
        ])

    def forward_(self, request_type, name, request_info, request,
                 timeout, priority, trans_id, pid):
        """
        forwards a service request to a different service name
        """
        # pylint: disable=too-many-arguments
        if request_type == API.ASYNC:
            self.forward_async(name, request_info, request,
                               timeout, priority, trans_id, pid)
        elif request_type == API.SYNC:
            self.forward_sync(name, request_info, request,
                              timeout, priority, trans_id, pid)
        else:
            raise InvalidInputException()

    def forward_async(self, name, request_info, request,
                      timeout, priority, trans_id, pid):
        """
        forwards an asynchronous service request to a different service name
        """
        # pylint: disable=too-many-arguments
        self.__api.forward_async(name, request_info, request,
                                 timeout, priority, trans_id, pid)
        raise ForwardAsyncException()

    def forward_sync(self, name, request_info, request,
                     timeout, priority, trans_id, pid):
        """
        forwards a synchronous service request to a different service name
        """
        # pylint: disable=too-many-arguments
        self.__api.forward_sync(name, request_info, request,
                                timeout, priority, trans_id, pid)
        raise ForwardSyncException()

    def return_(self, request_type, name, pattern, response_info, response,
                timeout, trans_id, pid):
        """
        provides a response to a service request
        """
        # pylint: disable=too-many-arguments
        if request_type == API.ASYNC:
            self.return_async(name, pattern, response_info, response,
                              timeout, trans_id, pid)
        elif request_type == API.SYNC:
            self.return_sync(name, pattern, response_info, response,
                             timeout, trans_id, pid)
        else:
            raise InvalidInputException()

    def return_async(self, name, pattern, response_info, response,
                     timeout, trans_id, pid):
        """
        provides a response to an asynchronous service request
        """
        # pylint: disable=too-many-arguments
        self.__api.return_async(name, pattern, response_info, response,
                                timeout, trans_id, pid)
        raise ReturnAsyncException()

    def return_sync(self, name, pattern, response_info, response,
                    timeout, trans_id, pid):
        """
        provides a response to a synchronous service request
        """
        # pylint: disable=too-many-arguments
        self.__api.return_sync(name, pattern, response_info, response,
                               timeout, trans_id, pid)
        raise ReturnSyncException()

    def recv_async(self, timeout=None, trans_id=None, consume=None):
        """
        blocks to receive an asynchronous service request response
        """
        # pylint: disable=broad-except
        kwargs = {}
        if timeout is not None:
            kwargs['timeout'] = timeout
        if trans_id is not None:
            kwargs['trans_id'] = trans_id
        if consume is not None:
            kwargs['consume'] = consume
        try:
            return self.__api.recv_async(**kwargs)
        except Exception as exception:
            self.__rethrow_exception(exception)

    def process_index(self):
        """
        returns the 0-based index of this process in the service instance
        """
        return self.__api.process_index()

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

    def process_count_min(self):
        """
        returns the count_process_dynamic minimum count
        """
        return self.__api.process_count_min()

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
        return self.__timeout_terminate

    def poll(self, timeout=-1):
        """
        blocks to process incoming CloudI service requests
        """
        # pylint: disable=broad-except
        if timeout is None:
            timeout = -1
        try:
            return self.__api.poll(timeout)
        except Exception as exception:
            self.__rethrow_exception(exception)

    def __rethrow_exception(self, exception):
        if isinstance(exception, libcloudi_py.message_decoding_exception):
            raise MessageDecodingException(str(exception))
        elif isinstance(exception, libcloudi_py.invalid_input_exception):
            raise InvalidInputException(str(exception))
        elif isinstance(exception, libcloudi_py.terminate_exception):
            raise TerminateException(self.__timeout_terminate)
        else:
            raise exception

    def __text_key_value_parse(self, text):
        # pylint: disable=no-self-use
        result = {}
        data = text.split(b'\0')
        for i in range(0, len(data) - 1, 2):
            key = data[i]
            current = result.get(key, None)
            if current is None:
                result[key] = data[i + 1]
            elif isinstance(current, list):
                current.append(data[i + 1])
            else:
                result[key] = [current, data[i + 1]]
        return result

    def info_key_value_parse(self, message_info):
        """
        parses "text_pairs" in service request info
        """
        return self.__text_key_value_parse(message_info)

class InvalidInputException(Exception):
    """
    Invalid Input
    """
    def __init__(self, message=None):
        if message is None:
            message = 'Invalid Input'
        Exception.__init__(self, message)
# XXX backwards-compatibility
invalid_input_exception = InvalidInputException

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
# XXX backwards-compatibility
message_decoding_exception = MessageDecodingException

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
# XXX backwards-compatibility
terminate_exception = TerminateException

# force unbuffered stdout/stderr handling without external configuration
if sys.stderr.__class__.__name__ != '_unbuffered':
    class _unbuffered(object):
        # pylint: disable=too-few-public-methods
        def __init__(self, stream):
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
                self.__stream = codecs.getwriter('UTF-8')(stream)

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

