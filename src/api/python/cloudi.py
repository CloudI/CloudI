#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
]

import sys, os, types, struct, socket, select, threading, inspect, \
       collections, traceback
from timeit import default_timer
from erlang import (binary_to_term, term_to_binary,
                    OtpErlangAtom, OtpErlangBinary)

assert sys.version >= '2.7'

_MESSAGE_INIT           = 1
_MESSAGE_SEND_ASYNC     = 2
_MESSAGE_SEND_SYNC      = 3
_MESSAGE_RECV_ASYNC     = 4
_MESSAGE_RETURN_ASYNC   = 5
_MESSAGE_RETURN_SYNC    = 6
_MESSAGE_RETURNS_ASYNC  = 7
_MESSAGE_KEEPALIVE      = 8

class API(object):
    ASYNC  =  1
    SYNC   = -1

    def __init__(self, thread_index):
        protocol_str = os.getenv('CLOUDI_API_INIT_PROTOCOL')
        if protocol_str is None:
            raise invalid_input_exception()
        buffer_size_str = os.getenv('CLOUDI_API_INIT_BUFFER_SIZE')
        if buffer_size_str is None:
            raise invalid_input_exception()
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
            raise invalid_input_exception()
        self.__initializtion_complete = False
        self.__size = int(buffer_size_str)
        self.__callbacks = {}
        self.__send(term_to_binary(OtpErlangAtom('init')))
        (self.__prefix,
         self.__timeout_async, self.__timeout_sync,
         self.__priority_default,
         self.__request_timeout_adjustment) = self.__poll_request(False)

    @staticmethod
    def thread_count():
        s = os.getenv('CLOUDI_API_INIT_THREAD_COUNT')
        if s is None:
            raise invalid_input_exception()
        return int(s)

    def subscribe(self, pattern, Function):
        args, varargs, varkw, defaults = inspect.getargspec(Function)
        if len(args) != 10: # self + arguments, so a non-static method
            raise invalid_input_exception()
        key = self.__prefix + pattern
        value = self.__callbacks.get(key, None)
        if value is None:
            self.__callbacks[key] = collections.deque([Function])
        else:
            value.append(Function)
        self.__send(term_to_binary((OtpErlangAtom('subscribe'), pattern)))

    def unsubscribe(self, pattern):
        key = self.__prefix + pattern
        value = self.__callbacks.get(key, None)
        assert value is not None
        value.popleft()
        if len(value) == 0:
            del self.__callbacks[key]
        self.__send(term_to_binary((OtpErlangAtom('unsubscribe'), pattern)))

    def send_async(self, name, request,
                   timeout=None, request_info=None, priority=None):
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = ''
        if priority is None:
            priority = self.__priority_default
        self.__send(term_to_binary((OtpErlangAtom('send_async'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority)))
        return self.__poll_request(False)

    def send_sync(self, name, request,
                  timeout=None, request_info=None, priority=None):
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = ''
        if priority is None:
            priority = self.__priority_default
        self.__send(term_to_binary((OtpErlangAtom('send_sync'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority)))
        return self.__poll_request(False)

    def mcast_async(self, name, request,
                    timeout=None, request_info=None, priority=None):
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = ''
        if priority is None:
            priority = self.__priority_default
        self.__send(term_to_binary((OtpErlangAtom('mcast_async'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority)))
        return self.__poll_request(False)

    def forward_(self, command, name, request_info, request,
                 timeout, priority, trans_id, pid):
        if command == API.ASYNC:
            self.forward_async(name,
                               request_info, request,
                               timeout, priority, trans_id, pid)
        elif command == API.SYNC:
            self.forward_sync(name, pattern,
                              request_info, request,
                              timeout, priority, trans_id, pid)
        else:
            raise invalid_input_exception()

    def forward_async(self, name, request_info, request,
                      timeout, priority, trans_id, pid):
        if self.__request_timeout_adjustment:
            if timeout == self.__request_timeout:
                elapsed = max(0, int(default_timer() - self.__request_timer))
                if elapsed > timeout:
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom('forward_async'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority,
                                    OtpErlangBinary(trans_id), pid)))
        raise forward_async_exception()

    def forward_sync(self, name, request_info, request,
                     timeout, priority, trans_id, pid):
        if self.__request_timeout_adjustment:
            if timeout == self.__request_timeout:
                elapsed = max(0, int(default_timer() - self.__request_timer))
                if elapsed > timeout:
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom('forward_sync'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority,
                                    OtpErlangBinary(trans_id), pid)))
        raise forward_sync_exception()

    def return_(self, command, name, pattern, response_info, response,
                timeout, trans_id, pid):
        if command == API.ASYNC:
            self.return_async(name, pattern,
                              response_info, response,
                              timeout, trans_id, pid)
        elif command == API.SYNC:
            self.return_sync(name, pattern,
                             response_info, response,
                             timeout, trans_id, pid)
        else:
            raise invalid_input_exception()

    def return_async(self, name, pattern, response_info, response,
                     timeout, trans_id, pid):
        if self.__request_timeout_adjustment:
            if timeout == self.__request_timeout:
                elapsed = max(0, int(default_timer() - self.__request_timer))
                if elapsed > timeout:
                    response_info = ''
                    response = ''
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom('return_async'),
                                    name, pattern,
                                    OtpErlangBinary(response_info),
                                    OtpErlangBinary(response), timeout,
                                    OtpErlangBinary(trans_id), pid)))
        raise return_async_exception()

    def return_sync(self, name, pattern, response_info, response,
                    timeout, trans_id, pid):
        if self.__request_timeout_adjustment:
            if timeout == self.__request_timeout:
                elapsed = max(0, int(default_timer() - self.__request_timer))
                if elapsed > timeout:
                    response_info = ''
                    response = ''
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom('return_sync'),
                                    name, pattern,
                                    OtpErlangBinary(response_info),
                                    OtpErlangBinary(response), timeout,
                                    OtpErlangBinary(trans_id), pid)))
        raise return_sync_exception()

    def recv_async(self, timeout=None, trans_id=None, consume=True):
        if timeout is None:
            timeout = self.__timeout_sync
        if trans_id is None:
            trans_id = chr(0) * 16
        self.__send(term_to_binary((OtpErlangAtom('recv_async'), timeout,
                                    OtpErlangBinary(trans_id), consume)))
        return self.__poll_request(False)

    def prefix(self):
        return self.__prefix

    def timeout_async(self):
        return self.__timeout_async

    def timeout_sync(self):
        return self.__timeout_sync

    def __callback(self, command, name, pattern, request_info, request,
                   timeout, priority, trans_id, pid):
        if self.__request_timeout_adjustment:
            self.__request_timer = default_timer()
            self.__request_timeout = timeout
        function_queue = self.__callbacks.get(pattern, None)
        assert function_queue is not None
        function = function_queue.popleft()
        function_queue.append(function)
        if command == _MESSAGE_SEND_ASYNC:
            try:
                response = function(API.ASYNC, name, pattern,
                                    request_info, request,
                                    timeout, priority, trans_id, pid)
                if type(response) == tuple:
                    response_info, response = response
                    if not (type(response_info) == bytes or
                            type(response_info) == str or
                            type(response_info) == unicode):
                        response_info = ''
                else:
                    response_info = ''
                if not (type(response) == bytes or
                        type(response) == str or
                        type(response) == unicode):
                    response = ''
            except return_async_exception:
                return
            except return_sync_exception:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except forward_async_exception:
                return
            except forward_sync_exception:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except:
                traceback.print_exc(file=sys.stderr)
                response_info = ''
                response = ''
            try:
                self.return_async(name, pattern,
                                  response_info, response,
                                  timeout, trans_id, pid)
            except return_async_exception:
                pass
            return
        elif command == _MESSAGE_SEND_SYNC:
            try:
                response = function(API.SYNC, name, pattern,
                                    request_info, request,
                                    timeout, priority, trans_id, pid)
                if type(response) == tuple:
                    response_info, response = response
                    if not (type(response_info) == bytes or
                            type(response_info) == str or
                            type(response_info) == unicode):
                        response_info = ''
                else:
                    response_info = ''
                if not (type(response) == bytes or
                        type(response) == str or
                        type(response) == unicode):
                    response = ''
            except return_sync_exception:
                return
            except return_async_exception:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except forward_sync_exception:
                return
            except forward_async_exception:
                traceback.print_exc(file=sys.stderr)
                assert False
                return
            except:
                traceback.print_exc(file=sys.stderr)
                response_info = ''
                response = ''
            try:
                self.return_sync(name, pattern,
                                 response_info, response,
                                 timeout, trans_id, pid)
            except return_sync_exception:
                pass
            return
        else:
            raise message_decoding_exception()

    def __poll_request(self, external):
        if external and not self.__initializtion_complete:
            self.__send(term_to_binary(OtpErlangAtom('polling')))
            self.__initializtion_complete = True
        ready = False
        while ready == False:
            IN, OUT, EXCEPT = select.select([self.__s],[],[self.__s])
            if len(EXCEPT) > 0:
                return None
            if len(IN) > 0:
                ready = True

        data = ''
        data = self.__recv(data)

        if len(data) == 0:
            return None # socket was closed

        while True:
            i, j = 0, 4
            command = struct.unpack('=I', data[i:j])[0]
            if command == _MESSAGE_INIT:
                i, j = j, j + 4
                prefixSize = struct.unpack('=I', data[i:j])[0]
                i, j = j, j + prefixSize + 4 + 4 + 1 + 1
                (prefix, nullTerminator, timeoutAsync, timeoutSync,
                 priorityDefault,
                 requestTimeoutAdjustment) = struct.unpack(
                    '=%dscIIbB' % (prefixSize - 1), data[i:j]
                )
                if j != len(data):
                    raise message_decoding_exception()
                return (prefix, timeoutSync, timeoutAsync,
                        priorityDefault, bool(requestTimeoutAdjustment))
            elif (command == _MESSAGE_SEND_ASYNC or
                  command == _MESSAGE_SEND_SYNC):
                i, j = j, j + 4
                nameSize = struct.unpack('=I', data[i:j])[0]
                i, j = j, j + nameSize + 4
                (name, nullTerminator,
                 patternSize) = struct.unpack('=%dscI' % (nameSize - 1),
                                              data[i:j])
                i, j = j, j + patternSize + 4
                (pattern, nullTerminator,
                 request_infoSize) = struct.unpack('=%dscI' % (patternSize - 1),
                                                  data[i:j])
                i, j = j, j + request_infoSize + 1 + 4
                (request_info, nullTerminator,
                 requestSize) = struct.unpack('=%dscI' % request_infoSize,
                                              data[i:j])
                i, j = j, j + requestSize + 1 + 4 + 1 + 16 + 4
                (request, nullTerminator, timeout, priority, trans_id,
                 pidSize) = struct.unpack('=%dscIb16sI' % requestSize,
                                          data[i:j])
                i, j = j, j + pidSize
                pid = struct.unpack('=%ds' % pidSize, data[i:j])[0]
                if j != len(data):
                    raise message_decoding_exception()
                data = ''
                self.__callback(command, name, pattern,
                                request_info, request,
                                timeout, priority, trans_id,
                                binary_to_term(pid))
            elif (command == _MESSAGE_RECV_ASYNC or
                  command == _MESSAGE_RETURN_SYNC):
                i, j = j, j + 4
                responseInfoSize = struct.unpack('=I', data[i:j])[0]
                i, j = j, j + responseInfoSize + 1 + 4
                (response_info, nullTerminator,
                 responseSize) = struct.unpack('=%dscI' % responseInfoSize,
                                               data[i:j])
                i, j = j, j + responseSize + 1 + 16
                if j != len(data):
                    raise message_decoding_exception()
                (response, nullTerminator,
                 trans_id) = struct.unpack('=%dsc16s' % responseSize, data[i:j])
                return (response_info, response, trans_id)
            elif command == _MESSAGE_RETURN_ASYNC:
                i, j = j, j + 16
                if j != len(data):
                    raise message_decoding_exception()
                return struct.unpack('=16s', data[i:j])[0]
            elif command == _MESSAGE_RETURNS_ASYNC:
                i, j = j, j + 4
                transIdCount = struct.unpack('=I', data[i:j])[0]
                i, j = j, j + 16 * transIdCount
                if j != len(data):
                    raise message_decoding_exception()
                return struct.unpack('=' + '16s' * transIdCount, data[i:j])
            elif command == _MESSAGE_KEEPALIVE:
                self.__send(term_to_binary(OtpErlangAtom('keepalive')))
                if j < len(data):
                    raise message_decoding_exception()
                data = data[j:]
                if len(data) > 0:
                    IN, OUT, EXCEPT = select.select([self.__s],[],[],0)
                    if len(IN) == 0:
                        continue
            else:
                raise message_decoding_exception()

            ready = False
            while ready == False:
                IN, OUT, EXCEPT = select.select([self.__s],[],[self.__s])
                if len(EXCEPT) > 0:
                    return None
                if len(IN) > 0:
                    ready = True
    
            data = self.__recv(data)
    
            if len(data) == 0:
                return None # socket was closed

    def poll(self):
        return self.__poll_request(True)

    def __binary_key_value_parse(self, binary):
        result = {}
        data = binary.split(chr(0))
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
        return self.__binary_key_value_parse(request)

    def info_key_value_parse(self, message_info):
        return self.__binary_key_value_parse(message_info)

    def __send(self, data):
        if self.__use_header:
            data = struct.pack('>I', len(data)) + data
        self.__s.sendall(data)

    def __recv(self, data):
        if self.__use_header:
            while len(data) < 4:
                fragment = self.__s.recv(self.__size)
                data += fragment
            total = struct.unpack('>I', data[:4])[0]
            data = data[4:]
            while len(data) < total:
                fragment = self.__s.recv(self.__size)
                data += fragment
        else:
            ready = True
            while ready == True:
                fragment = self.__s.recv(self.__size)
                data += fragment
                ready = (len(fragment) == self.__size)
                if ready:
                    IN, OUT, EXCEPT = select.select([self.__s],[],[],0)
                    ready = (len(IN) > 0)
        return data

class invalid_input_exception(Exception):
    def __init__(self):
        Exception.__init__(self, 'Invalid Input')

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
    def __init__(self):
        Exception.__init__(self, 'Message Decoding Error')

# force unbuffered stdout/stderr handling without external configuration
class _unbuffered(object):
    def __init__(self, stream):
        self.__stream = stream

    def write(self, data):
        self.__stream.write(data)
        self.__stream.flush()

    def __getattr__(self, attr):
        return getattr(self.__stream, attr)

sys.stdout = _unbuffered(sys.stdout)
sys.stderr = _unbuffered(sys.stderr)

