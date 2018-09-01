#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2018 Michael Truog <mjtruog at protonmail dot com>
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
Python CloudI API <https://cloudi.org/api.html#1_Intro>.
Example usage is available in the
integration tests <https://cloudi.org/tutorials.html#cloudi_examples>.
"""

import sys
import os
import struct
import socket
import select
import collections
import traceback
import inspect
from functools import partial
from timeit import default_timer
from erlang import (binary_to_term, term_to_binary,
                    OtpErlangAtom, OtpErlangBinary)

if sys.version_info[0] >= 3:
    TypeUnicode = str
else:
    TypeUnicode = unicode

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

_MESSAGE_INIT = 1
_MESSAGE_SEND_ASYNC = 2
_MESSAGE_SEND_SYNC = 3
_MESSAGE_RECV_ASYNC = 4
_MESSAGE_RETURN_ASYNC = 5
_MESSAGE_RETURN_SYNC = 6
_MESSAGE_RETURNS_ASYNC = 7
_MESSAGE_KEEPALIVE = 8
_MESSAGE_REINIT = 9
_MESSAGE_SUBSCRIBE_COUNT = 10
_MESSAGE_TERM = 11

# pylint: disable=too-many-instance-attributes
# pylint: disable=too-many-public-methods
class API(object):
    """
    CloudI API object for use in a single thread of execution
    """

    ASYNC = 1
    SYNC = -1

    def __init__(self, thread_index):
        protocol_str = os.getenv('CLOUDI_API_INIT_PROTOCOL')
        if protocol_str is None:
            raise InvalidInputException()
        buffer_size_str = os.getenv('CLOUDI_API_INIT_BUFFER_SIZE')
        if buffer_size_str is None:
            raise InvalidInputException()
        if protocol_str == 'tcp':
            self.__s = socket.fromfd(
                thread_index + 3, socket.AF_INET, socket.SOCK_STREAM
            )
            self.__use_header = True
        elif protocol_str == 'udp':
            self.__s = socket.fromfd(
                thread_index + 3, socket.AF_INET, socket.SOCK_DGRAM
            )
            self.__use_header = False
        elif protocol_str == 'local':
            self.__s = socket.fromfd(
                thread_index + 3, socket.AF_UNIX, socket.SOCK_STREAM
            )
            self.__use_header = True
        else:
            raise InvalidInputException()
        self.__initialization_complete = False
        self.__terminate = False
        self.__size = int(buffer_size_str)
        self.__callbacks = {}
        self.__timeout_terminate = 1000 # TIMEOUT_TERMINATE_MIN
        self.__send(term_to_binary(OtpErlangAtom(b'init')))
        (self.__process_index,
         self.__process_count,
         self.__process_count_max,
         self.__process_count_min,
         self.__prefix,
         self.__timeout_initialize,
         self.__timeout_async, self.__timeout_sync,
         self.__timeout_terminate,
         self.__priority_default) = self.__poll_request(None, False)

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
        key = self.__prefix + pattern
        value = self.__callbacks.get(key, None)
        if value is None:
            self.__callbacks[key] = collections.deque([function])
        else:
            value.append(function)
        self.__send(term_to_binary((OtpErlangAtom(b'subscribe'),
                                    pattern)))

    def subscribe_count(self, pattern):
        """
        returns the number of subscriptions for a single service name pattern
        """
        self.__send(term_to_binary((OtpErlangAtom(b'subscribe_count'),
                                    pattern)))
        return self.__poll_request(None, False)

    def unsubscribe(self, pattern):
        """
        unsubscribes from a service name pattern once
        """
        key = self.__prefix + pattern
        value = self.__callbacks.get(key, None)
        assert value is not None
        value.popleft()
        if len(value) == 0:
            del self.__callbacks[key]
        self.__send(term_to_binary((OtpErlangAtom(b'unsubscribe'),
                                    pattern)))

    def send_async(self, name, request,
                   timeout=None, request_info=None, priority=None):
        """
        sends an asynchronous service request
        """
        # pylint: disable=too-many-arguments
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = b''
        if priority is None:
            priority = self.__priority_default
        self.__send(term_to_binary((OtpErlangAtom(b'send_async'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority)))
        return self.__poll_request(None, False)

    def send_sync(self, name, request,
                  timeout=None, request_info=None, priority=None):
        """
        sends a synchronous service request
        """
        # pylint: disable=too-many-arguments
        if timeout is None:
            timeout = self.__timeout_sync
        if request_info is None:
            request_info = b''
        if priority is None:
            priority = self.__priority_default
        self.__send(term_to_binary((OtpErlangAtom(b'send_sync'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority)))
        return self.__poll_request(None, False)

    def mcast_async(self, name, request,
                    timeout=None, request_info=None, priority=None):
        """
        sends asynchronous service requests to all subscribers
        of the matching service name pattern
        """
        # pylint: disable=too-many-arguments
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = b''
        if priority is None:
            priority = self.__priority_default
        self.__send(term_to_binary((OtpErlangAtom(b'mcast_async'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority)))
        return self.__poll_request(None, False)

    def forward_(self, request_type, name, request_info, request,
                 timeout, priority, trans_id, pid):
        """
        forwards a service request to a different service name
        """
        # pylint: disable=too-many-arguments
        if request_type == API.ASYNC:
            self.forward_async(name,
                               request_info, request,
                               timeout, priority, trans_id, pid)
        elif request_type == API.SYNC:
            self.forward_sync(name,
                              request_info, request,
                              timeout, priority, trans_id, pid)
        else:
            raise InvalidInputException()

    def forward_async(self, name, request_info, request,
                      timeout, priority, trans_id, pid):
        """
        forwards an asynchronous service request to a different service name
        """
        # pylint: disable=too-many-arguments
        self.__send(term_to_binary((OtpErlangAtom(b'forward_async'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority,
                                    OtpErlangBinary(trans_id), pid)))
        raise ForwardAsyncException()

    def forward_sync(self, name, request_info, request,
                     timeout, priority, trans_id, pid):
        """
        forwards a synchronous service request to a different service name
        """
        # pylint: disable=too-many-arguments
        self.__send(term_to_binary((OtpErlangAtom(b'forward_sync'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority,
                                    OtpErlangBinary(trans_id), pid)))
        raise ForwardSyncException()

    def return_(self, request_type, name, pattern, response_info, response,
                timeout, trans_id, pid):
        """
        provides a response to a service request
        """
        # pylint: disable=too-many-arguments
        if request_type == API.ASYNC:
            self.return_async(name, pattern,
                              response_info, response,
                              timeout, trans_id, pid)
        elif request_type == API.SYNC:
            self.return_sync(name, pattern,
                             response_info, response,
                             timeout, trans_id, pid)
        else:
            raise InvalidInputException()

    def return_async(self, name, pattern, response_info, response,
                     timeout, trans_id, pid):
        """
        provides a response to an asynchronous service request
        """
        # pylint: disable=too-many-arguments
        self.__send(term_to_binary((OtpErlangAtom(b'return_async'),
                                    name, pattern,
                                    OtpErlangBinary(response_info),
                                    OtpErlangBinary(response), timeout,
                                    OtpErlangBinary(trans_id), pid)))
        raise ReturnAsyncException()

    def return_sync(self, name, pattern, response_info, response,
                    timeout, trans_id, pid):
        """
        provides a response to a synchronous service request
        """
        # pylint: disable=too-many-arguments
        self.__send(term_to_binary((OtpErlangAtom(b'return_sync'),
                                    name, pattern,
                                    OtpErlangBinary(response_info),
                                    OtpErlangBinary(response), timeout,
                                    OtpErlangBinary(trans_id), pid)))
        raise ReturnSyncException()

    def recv_async(self, timeout=None, trans_id=None, consume=True):
        """
        blocks to receive an asynchronous service request response
        """
        if timeout is None:
            timeout = self.__timeout_sync
        if trans_id is None:
            trans_id = b'\0' * 16
        self.__send(term_to_binary((OtpErlangAtom(b'recv_async'), timeout,
                                    OtpErlangBinary(trans_id), consume)))
        return self.__poll_request(None, False)

    def process_index(self):
        """
        returns the 0-based index of this process in the service instance
        """
        return self.__process_index

    def process_count(self):
        """
        returns the current process count based on the service configuration
        """
        return self.__process_count

    def process_count_max(self):
        """
        returns the count_process_dynamic maximum count
        """
        return self.__process_count_max

    def process_count_min(self):
        """
        returns the count_process_dynamic minimum count
        """
        return self.__process_count_min

    def prefix(self):
        """
        returns the service name pattern prefix from the service configuration
        """
        return self.__prefix

    def timeout_initialize(self):
        """
        returns the service initialization timeout
        """
        return self.__timeout_initialize

    def timeout_async(self):
        """
        returns the default asynchronous service request send timeout
        """
        return self.__timeout_async

    def timeout_sync(self):
        """
        returns the default synchronous service request send timeout
        """
        return self.__timeout_sync

    def timeout_terminate(self):
        """
        returns the service termination timeout
        """
        return self.__timeout_terminate

    def __null_response(self, request_type, name, pattern,
                        request_info, request,
                        timeout, priority, trans_id, pid):
        # pylint: disable=no-self-use
        # pylint: disable=too-many-arguments
        # pylint: disable=unused-argument
        return b''

    def __callback(self, command, name, pattern, request_info, request,
                   timeout, priority, trans_id, pid):
        # pylint: disable=too-many-arguments
        # pylint: disable=bare-except
        # pylint: disable=too-many-return-statements
        # pylint: disable=too-many-branches
        # pylint: disable=too-many-statements
        function_queue = self.__callbacks.get(pattern, None)
        if function_queue is None:
            function = self.__null_response
        else:
            function = function_queue.popleft()
            function_queue.append(function)
        if command == _MESSAGE_SEND_ASYNC:
            try:
                response = function(API.ASYNC, name, pattern,
                                    request_info, request,
                                    timeout, priority, trans_id, pid)
                if isinstance(response, tuple):
                    response_info, response = response
                    if not (isinstance(response_info, bytes) or
                            isinstance(response_info, TypeUnicode)):
                        response_info = b''
                else:
                    response_info = b''
                if not (isinstance(response, bytes) or
                        isinstance(response_info, TypeUnicode)):
                    response = b''
            except InvalidInputException as exception:
                raise exception
            except MessageDecodingException as exception:
                raise exception
            except TerminateException as exception:
                raise exception
            except ReturnAsyncException:
                return
            except ReturnSyncException:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except ForwardAsyncException:
                return
            except ForwardSyncException:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except:
                traceback.print_exc(file=sys.stderr)
                response_info = b''
                response = b''
            try:
                self.return_async(name, pattern,
                                  response_info, response,
                                  timeout, trans_id, pid)
            except ReturnAsyncException:
                pass
            return
        elif command == _MESSAGE_SEND_SYNC:
            try:
                response = function(API.SYNC, name, pattern,
                                    request_info, request,
                                    timeout, priority, trans_id, pid)
                if isinstance(response, tuple):
                    response_info, response = response
                    if not (isinstance(response_info, bytes) or
                            isinstance(response_info, TypeUnicode)):
                        response_info = b''
                else:
                    response_info = b''
                if not (isinstance(response, bytes) or
                        isinstance(response_info, TypeUnicode)):
                    response = b''
            except InvalidInputException as exception:
                raise exception
            except MessageDecodingException as exception:
                raise exception
            except TerminateException as exception:
                raise exception
            except ReturnSyncException:
                return
            except ReturnAsyncException:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except ForwardSyncException:
                return
            except ForwardAsyncException:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except:
                traceback.print_exc(file=sys.stderr)
                response_info = b''
                response = b''
            try:
                self.return_sync(name, pattern,
                                 response_info, response,
                                 timeout, trans_id, pid)
            except ReturnSyncException:
                pass
            return
        else:
            raise MessageDecodingException()

    def __handle_events(self, external, data, data_size, j, command=None):
        # pylint: disable=too-many-arguments
        if command is None:
            if j > data_size:
                raise MessageDecodingException()
            i, j = j, j + 4
            command = struct.unpack(b'=I', data[i:j])[0]
        while True:
            if command == _MESSAGE_TERM:
                self.__terminate = True
                if external:
                    return False
                else:
                    raise TerminateException(self.__timeout_terminate)
            elif command == _MESSAGE_REINIT:
                i, j = j, j + 4 + 4 + 4 + 1
                (self.__process_count,
                 self.__timeout_async, self.__timeout_sync,
                 self.__priority_default) = struct.unpack(
                     b'=IIIb', data[i:j]
                 )
            elif command == _MESSAGE_KEEPALIVE:
                self.__send(term_to_binary(OtpErlangAtom(b'keepalive')))
            else:
                raise MessageDecodingException()
            if j > data_size:
                raise MessageDecodingException()
            elif j == data_size:
                return True
            i, j = j, j + 4
            command = struct.unpack(b'=I', data[i:j])[0]

    def __poll_request(self, timeout, external):
        # pylint: disable=too-many-locals
        # pylint: disable=too-many-return-statements
        # pylint: disable=too-many-branches
        # pylint: disable=too-many-statements
        if self.__terminate:
            return False
        elif external and not self.__initialization_complete:
            self.__send(term_to_binary(OtpErlangAtom(b'polling')))
            self.__initialization_complete = True

        poll_timer = None
        if timeout is None or timeout < 0:
            timeout_value = None
        elif timeout == 0:
            timeout_value = 0.0
        elif timeout > 0:
            poll_timer = default_timer()
            timeout_value = timeout * 0.001
        fd_in, _, fd_except = select.select([self.__s], [], [self.__s],
                                            timeout_value)
        if len(fd_except) > 0:
            return False
        if len(fd_in) == 0:
            return True

        data = b''
        data = self.__recv(data)
        data_size = len(data)
        if data_size == 0:
            return False # socket was closed
        i, j = 0, 4

        while True:
            command = struct.unpack(b'=I', data[i:j])[0]
            if command == _MESSAGE_INIT:
                i, j = j, j + 4 + 4 + 4 + 4 + 4
                (process_index, process_count,
                 process_count_max, process_count_min,
                 prefix_size) = struct.unpack(b'=IIIII', data[i:j])
                i, j = j, j + prefix_size + 4 + 4 + 4 + 4 + 1
                (prefix, _, timeout_initialize,
                 timeout_async, timeout_sync, timeout_terminate,
                 priority_default) = struct.unpack(
                     '=%dscIIIIb' % (prefix_size - 1), data[i:j]
                 )
                if j != data_size:
                    assert external is False
                    self.__handle_events(external, data, data_size, j)
                return (process_index, process_count,
                        process_count_max, process_count_min,
                        prefix.decode('utf-8'), timeout_initialize,
                        timeout_sync, timeout_async, timeout_terminate,
                        priority_default)
            elif (command == _MESSAGE_SEND_ASYNC or
                  command == _MESSAGE_SEND_SYNC):
                i, j = j, j + 4
                name_size = struct.unpack(b'=I', data[i:j])[0]
                i, j = j, j + name_size + 4
                (name, _,
                 pattern_size) = struct.unpack('=%dscI' % (name_size - 1),
                                               data[i:j])
                i, j = j, j + pattern_size + 4
                (pattern, _,
                 request_info_size) = struct.unpack(
                     '=%dscI' % (pattern_size - 1), data[i:j]
                 )
                i, j = j, j + request_info_size + 1 + 4
                (request_info, _,
                 request_size) = struct.unpack(
                     '=%dscI' % request_info_size, data[i:j]
                 )
                i, j = j, j + request_size + 1 + 4 + 1 + 16 + 4
                (request, _, request_timeout, priority, trans_id,
                 pid_size) = struct.unpack(
                     '=%dscIb16sI' % request_size, data[i:j]
                 )
                i, j = j, j + pid_size
                pid = data[i:j]
                if j != data_size:
                    assert external is True
                    if not self.__handle_events(external, data, data_size, j):
                        return False
                data = b''
                self.__callback(command,
                                name.decode('utf-8'),
                                pattern.decode('utf-8'),
                                request_info, request,
                                request_timeout, priority, trans_id,
                                binary_to_term(pid))
            elif (command == _MESSAGE_RECV_ASYNC or
                  command == _MESSAGE_RETURN_SYNC):
                i, j = j, j + 4
                response_info_size = struct.unpack(b'=I', data[i:j])[0]
                i, j = j, j + response_info_size + 1 + 4
                (response_info, _,
                 response_size) = struct.unpack(
                     '=%dscI' % response_info_size, data[i:j]
                 )
                i, j = j, j + response_size + 1 + 16
                (response, _,
                 trans_id) = struct.unpack(
                     '=%dsc16s' % response_size, data[i:j]
                 )
                if j != data_size:
                    assert external is False
                    self.__handle_events(external, data, data_size, j)
                return (response_info, response, trans_id)
            elif command == _MESSAGE_RETURN_ASYNC:
                i, j = j, j + 16
                trans_id = data[i:j]
                if j != data_size:
                    assert external is False
                    self.__handle_events(external, data, data_size, j)
                return trans_id
            elif command == _MESSAGE_RETURNS_ASYNC:
                i, j = j, j + 4
                trans_id_count = struct.unpack(b'=I', data[i:j])[0]
                i, j = j, j + 16 * trans_id_count
                trans_ids = struct.unpack(
                    b'=' + b'16s' * trans_id_count, data[i:j]
                )
                if j != data_size:
                    assert external is False
                    self.__handle_events(external, data, data_size, j)
                return trans_ids
            elif command == _MESSAGE_SUBSCRIBE_COUNT:
                i, j = j, j + 4
                count = struct.unpack(b'=I', data[i:j])[0]
                if j != data_size:
                    assert external is False
                    self.__handle_events(external, data, data_size, j)
                return count
            elif command == _MESSAGE_TERM:
                if not self.__handle_events(external, data, data_size, j,
                                            command=command):
                    return False
                assert False
            elif command == _MESSAGE_REINIT:
                i, j = j, j + 4 + 4 + 4 + 1
                (self.__process_count,
                 self.__timeout_async, self.__timeout_sync,
                 self.__priority_default) = struct.unpack(
                     b'=IIIb', data[i:j]
                 )
                if j == data_size:
                    data = b''
                elif j < data_size:
                    i, j = j, j + 4
                    continue
                else:
                    raise MessageDecodingException()
            elif command == _MESSAGE_KEEPALIVE:
                self.__send(term_to_binary(OtpErlangAtom(b'keepalive')))
                if j == data_size:
                    data = b''
                elif j < data_size:
                    i, j = j, j + 4
                    continue
                else:
                    raise MessageDecodingException()
            else:
                raise MessageDecodingException()

            if poll_timer is not None:
                poll_timer_new = default_timer()
                elapsed = max(0, int((poll_timer_new -
                                      poll_timer) * 1000.0))
                poll_timer = poll_timer_new
                if elapsed >= timeout:
                    timeout = 0
                else:
                    timeout -= elapsed
            if timeout_value is not None:
                if timeout == 0:
                    return True
                elif timeout > 0:
                    timeout_value = timeout * 0.001
            fd_in, _, fd_except = select.select([self.__s], [], [self.__s],
                                                timeout_value)
            if len(fd_except) > 0:
                return False
            if len(fd_in) == 0:
                return True

            data = self.__recv(data)
            data_size = len(data)
            if data_size == 0:
                return False # socket was closed
            i, j = 0, 4

    def poll(self, timeout=-1):
        """
        blocks to process incoming CloudI service requests
        """
        return self.__poll_request(timeout, True)

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

    def __send(self, data):
        if self.__use_header:
            data = struct.pack(b'>I', len(data)) + data
        self.__s.sendall(data)

    def __recv(self, data_old):
        data = b''
        if self.__use_header:
            i = 0
            while i < 4:
                fragment = self.__s.recv(4 - i)
                data += fragment
                i += len(fragment)
            total = struct.unpack(b'>I', data)[0]
            data = data_old
            i = 0
            while i < total:
                fragment = self.__s.recv(min(total - i, self.__size))
                data += fragment
                i += len(fragment)
        else:
            data = data_old
            ready = True
            while ready is True:
                fragment = self.__s.recv(self.__size)
                data += fragment
                ready = (len(fragment) == self.__size)
                if ready:
                    fd_in, _, _ = select.select([self.__s], [], [], 0)
                    ready = (len(fd_in) > 0)
        return data

class InvalidInputException(Exception):
    """
    Invalid Input
    """
    def __init__(self):
        Exception.__init__(self, 'Invalid Input')
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
    def __init__(self):
        Exception.__init__(self, 'Message Decoding Error')
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

