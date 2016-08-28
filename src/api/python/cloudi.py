#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2016, Michael Truog <mjtruog at gmail dot com>
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

import sys, os, types, struct, socket, select, inspect, collections, traceback
from timeit import default_timer
from erlang import (binary_to_term, term_to_binary,
                    OtpErlangAtom, OtpErlangBinary)

_MESSAGE_INIT                = 1
_MESSAGE_SEND_ASYNC          = 2
_MESSAGE_SEND_SYNC           = 3
_MESSAGE_RECV_ASYNC          = 4
_MESSAGE_RETURN_ASYNC        = 5
_MESSAGE_RETURN_SYNC         = 6
_MESSAGE_RETURNS_ASYNC       = 7
_MESSAGE_KEEPALIVE           = 8
_MESSAGE_REINIT              = 9
_MESSAGE_SUBSCRIBE_COUNT     = 10
_MESSAGE_TERM                = 11

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
         self.__priority_default,
         self.__request_timeout_adjustment) = self.__poll_request(None, False)

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
        self.__send(term_to_binary((OtpErlangAtom(b'subscribe'),
                                    pattern)))

    def subscribe_count(self, pattern):
        self.__send(term_to_binary((OtpErlangAtom(b'subscribe_count'),
                                    pattern)))
        return self.__poll_request(None, False)

    def unsubscribe(self, pattern):
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
                elapsed = max(0, int((default_timer() -
                                      self.__request_timer) * 1000.0))
                if elapsed > timeout:
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom(b'forward_async'), name,
                                    OtpErlangBinary(request_info),
                                    OtpErlangBinary(request),
                                    timeout, priority,
                                    OtpErlangBinary(trans_id), pid)))
        raise forward_async_exception()

    def forward_sync(self, name, request_info, request,
                     timeout, priority, trans_id, pid):
        if self.__request_timeout_adjustment:
            if timeout == self.__request_timeout:
                elapsed = max(0, int((default_timer() -
                                      self.__request_timer) * 1000.0))
                if elapsed > timeout:
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom(b'forward_sync'), name,
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
                elapsed = max(0, int((default_timer() -
                                      self.__request_timer) * 1000.0))
                if elapsed > timeout:
                    response_info = b''
                    response = b''
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom(b'return_async'),
                                    name, pattern,
                                    OtpErlangBinary(response_info),
                                    OtpErlangBinary(response), timeout,
                                    OtpErlangBinary(trans_id), pid)))
        raise return_async_exception()

    def return_sync(self, name, pattern, response_info, response,
                    timeout, trans_id, pid):
        if self.__request_timeout_adjustment:
            if timeout == self.__request_timeout:
                elapsed = max(0, int((default_timer() -
                                      self.__request_timer) * 1000.0))
                if elapsed > timeout:
                    response_info = b''
                    response = b''
                    timeout = 0
                else:
                    timeout -= elapsed
        self.__send(term_to_binary((OtpErlangAtom(b'return_sync'),
                                    name, pattern,
                                    OtpErlangBinary(response_info),
                                    OtpErlangBinary(response), timeout,
                                    OtpErlangBinary(trans_id), pid)))
        raise return_sync_exception()

    def recv_async(self, timeout=None, trans_id=None, consume=True):
        if timeout is None:
            timeout = self.__timeout_sync
        if trans_id is None:
            trans_id = b'\0' * 16
        self.__send(term_to_binary((OtpErlangAtom(b'recv_async'), timeout,
                                    OtpErlangBinary(trans_id), consume)))
        return self.__poll_request(None, False)

    def process_index(self):
        return self.__process_index

    def process_count(self):
        return self.__process_count

    def process_count_max(self):
        return self.__process_count_max

    def process_count_min(self):
        return self.__process_count_min

    def prefix(self):
        return self.__prefix

    def timeout_initialize(self):
        return self.__timeout_initialize

    def timeout_async(self):
        return self.__timeout_async

    def timeout_sync(self):
        return self.__timeout_sync

    def timeout_terminate(self):
        return self.__timeout_terminate

    def __null_response(self, command, name, pattern, request_info, request,
                        timeout, priority, trans_id, pid):
        return b''

    def __callback(self, command, name, pattern, request_info, request,
                   timeout, priority, trans_id, pid):
        if self.__request_timeout_adjustment:
            self.__request_timer = default_timer()
            self.__request_timeout = timeout
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
                if type(response) == tuple:
                    response_info, response = response
                    if not (type(response_info) == bytes or
                            type(response_info) == str or
                            type(response_info) == unicode):
                        response_info = b''
                else:
                    response_info = b''
                if not (type(response) == bytes or
                        type(response) == str or
                        type(response) == unicode):
                    response = b''
            except invalid_input_exception as e:
                raise e
            except message_decoding_exception as e:
                raise e
            except terminate_exception as e:
                raise e
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
                response_info = b''
                response = b''
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
                        response_info = b''
                else:
                    response_info = b''
                if not (type(response) == bytes or
                        type(response) == str or
                        type(response) == unicode):
                    response = b''
            except invalid_input_exception as e:
                raise e
            except message_decoding_exception as e:
                raise e
            except terminate_exception as e:
                raise e
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
                response_info = b''
                response = b''
            try:
                self.return_sync(name, pattern,
                                 response_info, response,
                                 timeout, trans_id, pid)
            except return_sync_exception:
                pass
            return
        else:
            raise message_decoding_exception()

    def __handle_events(self, external, data, data_size, j, command=None):
        if command is None:
            if j > data_size:
                raise message_decoding_exception()
            i, j = j, j + 4
            command = struct.unpack(b'=I', data[i:j])[0]
        while True:
            if command == _MESSAGE_TERM:
                self.__terminate = True
                if external:
                    return False
                else:
                    raise terminate_exception(self.__timeout_terminate)
            elif command == _MESSAGE_REINIT:
                i, j = j, j + 4
                self.__process_count = struct.unpack(b'=I', data[i:j])[0]
            elif command == _MESSAGE_KEEPALIVE:
                self.__send(term_to_binary(OtpErlangAtom(b'keepalive')))
            else:
                raise message_decoding_exception()
            if j > data_size:
                raise message_decoding_exception()
            elif j == data_size:
                return True
            i, j = j, j + 4
            command = struct.unpack(b'=I', data[i:j])[0]

    def __poll_request(self, timeout, external):
        if self.__terminate:
            return False
        elif external and not self.__initialization_complete:
            self.__send(term_to_binary(OtpErlangAtom(b'polling')))
            self.__initialization_complete = True

        poll_timer = None
        if timeout is None or timeout < 0:
            timeout_value = None
        elif timeout == 0:
            timeout_value = 0
        elif timeout > 0:
            poll_timer = default_timer()
            timeout_value = timeout * 0.001
        IN, OUT, EXCEPT = select.select([self.__s],[],[self.__s],
                                        timeout_value)
        if len(EXCEPT) > 0:
            return False
        if len(IN) == 0:
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
                i, j = j, j + prefix_size + 4 + 4 + 4 + 4 + 1 + 1
                (prefix, null_terminator, timeout_initialize,
                 timeout_async, timeout_sync, timeout_terminate,
                 priority_default,
                 request_timeout_adjustment) = struct.unpack(
                    '=%dscIIIIbB' % (prefix_size - 1), data[i:j]
                )
                if j != data_size:
                    assert external == False
                    self.__handle_events(external, data, data_size, j)
                return (process_index, process_count,
                        process_count_max, process_count_min,
                        prefix.decode('utf-8'), timeout_initialize,
                        timeout_sync, timeout_async, timeout_terminate,
                        priority_default, bool(request_timeout_adjustment))
            elif (command == _MESSAGE_SEND_ASYNC or
                  command == _MESSAGE_SEND_SYNC):
                i, j = j, j + 4
                name_size = struct.unpack(b'=I', data[i:j])[0]
                i, j = j, j + name_size + 4
                (name, null_terminator,
                 pattern_size) = struct.unpack('=%dscI' % (name_size - 1),
                                               data[i:j])
                i, j = j, j + pattern_size + 4
                (pattern, null_terminator,
                 request_info_size) = struct.unpack(
                    '=%dscI' % (pattern_size - 1), data[i:j]
                )
                i, j = j, j + request_info_size + 1 + 4
                (request_info, null_terminator,
                 request_size) = struct.unpack(
                    '=%dscI' % request_info_size, data[i:j]
                )
                i, j = j, j + request_size + 1 + 4 + 1 + 16 + 4
                (request, null_terminator, request_timeout, priority, trans_id,
                 pid_size) = struct.unpack(
                    '=%dscIb16sI' % request_size, data[i:j]
                )
                i, j = j, j + pid_size
                pid = data[i:j]
                if j != data_size:
                    assert external == True
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
                (response_info, null_terminator,
                 response_size) = struct.unpack(
                    '=%dscI' % response_info_size, data[i:j]
                )
                i, j = j, j + response_size + 1 + 16
                (response, null_terminator,
                 trans_id) = struct.unpack(
                    '=%dsc16s' % response_size, data[i:j]
                )
                if j != data_size:
                    assert external == False
                    self.__handle_events(external, data, data_size, j)
                return (response_info, response, trans_id)
            elif command == _MESSAGE_RETURN_ASYNC:
                i, j = j, j + 16
                trans_id = data[i:j]
                if j != data_size:
                    assert external == False
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
                    assert external == False
                    self.__handle_events(external, data, data_size, j)
                return trans_ids
            elif command == _MESSAGE_SUBSCRIBE_COUNT:
                i, j = j, j + 4
                count = struct.unpack(b'=I', data[i:j])[0]
                if j != data_size:
                    assert external == False
                    self.__handle_events(external, data, data_size, j)
                return count
            elif command == _MESSAGE_TERM:
                if not self.__handle_events(external, data, data_size, j,
                                            command=command):
                    return False
                assert False
            elif command == _MESSAGE_REINIT:
                i, j = j, j + 4
                self.__process_count = struct.unpack(b'=I', data[i:j])[0]
                if j == data_size:
                    pass
                elif j < data_size:
                    i, j = j, j + 4
                    continue
                else:
                    raise message_decoding_exception()
            elif command == _MESSAGE_KEEPALIVE:
                self.__send(term_to_binary(OtpErlangAtom(b'keepalive')))
                if j == data_size:
                    pass
                elif j < data_size:
                    i, j = j, j + 4
                    continue
                else:
                    raise message_decoding_exception()
            else:
                raise message_decoding_exception()

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
            IN, OUT, EXCEPT = select.select([self.__s],[],[self.__s],
                                            timeout_value)
            if len(EXCEPT) > 0:
                return False
            if len(IN) == 0:
                return True
    
            data = self.__recv(data)
            data_size = len(data)
            if data_size == 0:
                return False # socket was closed
            i, j = 0, 4

    def poll(self, timeout=-1):
        return self.__poll_request(timeout, True)

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

    def __send(self, data):
        if self.__use_header:
            data = struct.pack(b'>I', len(data)) + data
        self.__s.sendall(data)

    def __recv(self, data):
        if self.__use_header:
            while len(data) < 4:
                fragment = self.__s.recv(self.__size)
                data += fragment
            total = struct.unpack(b'>I', data[:4])[0]
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

