#!/usr/bin/env python
# -*- coding: utf-8; Mode: python; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# BSD LICENSE
# 
# Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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

__all__ = ["API"]

import sys, os, types, struct, socket, select, threading, inspect
from erlang import (binary_to_term, term_to_binary,
                    OtpErlangAtom, OtpErlangBinary)

_MESSAGE_INIT           = 1
_MESSAGE_SEND_ASYNC     = 2
_MESSAGE_SEND_SYNC      = 3
_MESSAGE_RECV_ASYNC     = 4
_MESSAGE_RETURN_ASYNC   = 5
_MESSAGE_RETURN_SYNC    = 6
_MESSAGE_RETURNS_ASYNC  = 7
_MESSAGE_KEEPALIVE      = 8

class API(object):
    __ASYNC  =  1
    __SYNC   = -1

    def __init__(self, index, protocol, size):
        self.__s = socket.fromfd(index + 3, socket.AF_INET, protocol)
        self.__size = size
        self.__callbacks = {}
        self.__s.sendall(term_to_binary(OtpErlangAtom("init")))
        self.__prefix, self.__timeout_async, self.__timeout_sync = self.poll()

    def __del__(self):
        self.__s.close()

    def subscribe(self, name, Function):
        args, varargs, varkw, defaults = inspect.getargspec(Function)
        assert len(args) == 9 # self + arguments, so a non-static method
        self.__callbacks[self.__prefix + name] = Function
        self.__s.sendall(term_to_binary((OtpErlangAtom("subscribe"), name)))

    def unsubscribe(self, name):
        del self.__callbacks[self.__prefix + name]
        self.__s.sendall(term_to_binary((OtpErlangAtom("unsubscribe"), name)))

    def send_async(self, name, request,
                   timeout=None, request_info=None, priority=None):
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = ''
        if priority is None:
            priority = 0
        self.__s.sendall(term_to_binary((OtpErlangAtom("send_async"), name,
                                         OtpErlangBinary(request_info),
                                         OtpErlangBinary(request),
                                         timeout, priority)))
        return self.poll()

    def send_sync(self, name, request,
                  timeout=None, request_info=None, priority=None):
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = ''
        if priority is None:
            priority = 0
        self.__s.sendall(term_to_binary((OtpErlangAtom("send_sync"), name,
                                         OtpErlangBinary(request_info),
                                         OtpErlangBinary(request),
                                         timeout, priority)))
        return self.poll()

    def mcast_async(self, name, request,
                    timeout=None, request_info=None, priority=None):
        if timeout is None:
            timeout = self.__timeout_async
        if request_info is None:
            request_info = ''
        if priority is None:
            priority = 0
        self.__s.sendall(term_to_binary((OtpErlangAtom("mcast_async"), name,
                                         OtpErlangBinary(request_info),
                                         OtpErlangBinary(request),
                                         timeout, priority)))
        return self.poll()

    def forward_(self, command, name, request_info, request,
                 timeout, priority, transId, pid):
        if command == API.__ASYNC:
            self.forward_async(name, request_info, request,
                               timeout, priority, transId, pid)
        elif command == API.__SYNC:
            self.forward_sync(name, request_info, request,
                              timeout, priority, transId, pid)
        else:
            assert False

    def forward_async(self, name, request_info, request,
                      timeout, priority, transId, pid):
        self.__s.sendall(term_to_binary((OtpErlangAtom("forward_async"), name,
                                         OtpErlangBinary(request_info),
                                         OtpErlangBinary(request),
                                         timeout, priority,
                                         OtpErlangBinary(transId), pid)))
        raise _return_async_exception()

    def forward_sync(self, name, request_info, request,
                     timeout, priority, transId, pid):
        self.__s.sendall(term_to_binary((OtpErlangAtom("forward_sync"), name,
                                         OtpErlangBinary(request_info),
                                         OtpErlangBinary(request),
                                         timeout, priority,
                                         OtpErlangBinary(transId), pid)))
        raise _return_sync_exception()

    def return_(self, command, name, response_info, response,
                timeout, transId, pid):
        if command == API.__ASYNC:
            self.return_async(name, response_info, response,
                              timeout, transId, pid)
        elif command == API.__SYNC:
            self.return_sync(name, response_info, response,
                             timeout, transId, pid)
        else:
            assert False

    def return_async(self, name, response_info, response,
                     timeout, transId, pid):
        self.__s.sendall(term_to_binary((OtpErlangAtom("return_async"), name,
                                         OtpErlangBinary(response_info),
                                         OtpErlangBinary(response), timeout,
                                         OtpErlangBinary(transId), pid)))
        raise _return_async_exception()

    def return_sync(self, name, response_info, response,
                    timeout, transId, pid):
        self.__s.sendall(term_to_binary((OtpErlangAtom("return_sync"), name,
                                         OtpErlangBinary(response_info),
                                         OtpErlangBinary(response), timeout,
                                         OtpErlangBinary(transId), pid)))
        raise _return_sync_exception()

    def recv_async(self, timeout, transId):
        self.__s.sendall(term_to_binary((OtpErlangAtom("recv_async"), timeout,
                                         OtpErlangBinary(transId))))
        return self.poll()

    def __callback(self, command, name, requestInfo, request,
                   timeout, priority, transId, pid):
        function = self.__callbacks.get(name, None)
        assert function is not None
        if command == _MESSAGE_SEND_ASYNC:
            try:
                response = function(API.__ASYNC, name, requestInfo, request,
                                    timeout, priority, transId, pid)
                if type(response) == types.TupleType:
                    responseInfo, response = response
                else:
                    responseInfo = ''
            except _return_async_exception:
                return
            except _return_sync_exception:
                assert False
                return
            except:
                # exception is ignored at this level
                responseInfo = ''
                response = ''
            self.return_async(name, responseInfo, response,
                              timeout, transId, pid)
        elif command == _MESSAGE_SEND_SYNC:
            try:
                response = function(API.__SYNC, name, requestInfo, request,
                                    timeout, priority, transId, pid)
                if type(response) == types.TupleType:
                    responseInfo, response = response
                else:
                    responseInfo = ''
            except _return_sync_exception:
                return
            except _return_async_exception:
                assert False
                return
            except:
                # exception is ignored at this level
                responseInfo = ''
                response = ''
            self.return_sync(name, responseInfo, response,
                             timeout, transId, pid)
        else:
            assert False

    def poll(self):
        ready = False
        while ready == False:
            IN, OUT, EXCEPT = select.select([self.__s],[],[self.__s])
            if len(EXCEPT) > 0:
                return None
            if len(IN) > 0:
                ready = True

        data = ''
        while ready == True:
            fragment = self.__s.recv(self.__size)
            data += fragment
            ready = (len(fragment) == self.__size)
            if ready:
                IN, OUT, EXCEPT = select.select([self.__s],[],[],0)
                ready == (len(IN) > 0)

        if len(data) == 0:
            return None # socket was closed

        while True:
            i, j = 0, 4
            command = struct.unpack("=I", data[i:j])[0]
            if command == _MESSAGE_INIT:
                i, j = j, j + 4
                prefixSize = struct.unpack("=I", data[i:j])[0]
                i, j = j, j + prefixSize + 4 + 4
                (prefix, nullTerminator, timeoutAsync,
                 timeoutSync) = struct.unpack("=%dscII" % (prefixSize - 1),
                                               data[i:j])
                assert j == len(data)
                return (prefix, timeoutSync, timeoutAsync)
            elif (command == _MESSAGE_SEND_ASYNC or
                  command == _MESSAGE_SEND_SYNC):
                i, j = j, j + 4
                nameSize = struct.unpack("=I", data[i:j])[0]
                i, j = j, j + nameSize + 4
                (name, nullTerminator,
                 requestInfoSize) = struct.unpack("=%dscI" % (nameSize - 1),
                                                  data[i:j])
                i, j = j, j + requestInfoSize + 1 + 4
                (requestInfo, nullTerminator,
                 requestSize) = struct.unpack("=%dscI" % requestInfoSize,
                                              data[i:j])
                i, j = j, j + requestSize + 1 + 4 + 1 + 16 + 4
                (request, nullTerminator, timeout, priority, transId,
                 pidSize) = struct.unpack("=%dscIb16sI" % requestSize,
                                          data[i:j])
                i, j = j, j + pidSize
                pid = struct.unpack("=%ds" % pidSize, data[i:j])[0]
                assert j == len(data)
                data = ''
                self.__callback(command, name, requestInfo, request,
                                timeout, priority, transId,
                                binary_to_term(pid))
            elif (command == _MESSAGE_RECV_ASYNC or
                  command == _MESSAGE_RETURN_SYNC):
                i, j = j, j + 4
                responseInfoSize = struct.unpack("=I", data[i:j])[0]
                i, j = j, j + requestInfoSize + 1 + 4
                (responseInfo, nullTerminator,
                 responseSize) = struct.unpack("=%dscI" % responseInfoSize,
                                               data[i:j])
                i, j = j, j + responseSize + 1 + 16
                assert j == len(data)
                (response, nullTerminator,
                 transId) = struct.unpack("=%dsc16s" % responseSize, data[i:j])
                return (responseInfo, response, transId)
            elif command == _MESSAGE_RETURN_ASYNC:
                i, j = j, j + 16
                assert j == len(data)
                return struct.unpack("=16s", data[i:j])[0]
            elif command == _MESSAGE_RETURNS_ASYNC:
                i, j = j, j + 4
                transIdCount = struct.unpack("=I", data[i:j])[0]
                i, j = j, j + 16 * transIdCount
                assert j == len(data)
                return struct.unpack("=" + "16s" * transIdCount, data[i:j])
            elif command == _MESSAGE_KEEPALIVE:
                self.__s.sendall(term_to_binary(OtpErlangAtom("keepalive")))
                assert j >= len(data)
                data = data[j:]
                if len(data) > 0:
                    IN, OUT, EXCEPT = select.select([self.__s],[],[],0)
                    if len(IN) == 0:
                        continue
            else:
                assert False

            ready = False
            while ready == False:
                IN, OUT, EXCEPT = select.select([self.__s],[],[self.__s])
                if len(EXCEPT) > 0:
                    return None
                if len(IN) > 0:
                    ready = True
    
            while ready == True:
                fragment = self.__s.recv(self.__size)
                data += fragment
                ready = (len(fragment) == self.__size)
                if ready:
                    IN, OUT, EXCEPT = select.select([self.__s],[],[],0)
                    ready == (len(IN) > 0)
    
            if len(data) == 0:
                return None # socket was closed

    def request_http_qs_parse(self, request):
        result = {}
        data = request.split(chr(0))
        for i in xrange(0, len(data) - 1, 2):
            result[data[i]] = data[i + 1]
        return result


class _return_sync_exception(SystemExit):
    def __init__(self):
        pass

class _return_async_exception(SystemExit):
    def __init__(self):
        pass

class _Task(threading.Thread):
    def __init__(self, index, protocol, size):
        threading.Thread.__init__(self)
        self.__api = API(index, protocol, size)

    def foobar(self, command, name, request, timeout, transId, pid):
        print "got foobar"
        self.__api.return_(command, name, "bye", timeout, transId, pid)

    def run(self):
        self.__api.subscribe("foobar", self.foobar)

        running = True
        while running:
            result = self.__api.poll()
            if result is None:
                running = False
            else:
                print "(python) received:",result
        print "exited thread"

if __name__ == '__main__':
    if len(sys.argv) != 4:
        print >> sys.stderr, "Usage: %s thread_count protocol buffer_size" % (
                                 sys.argv[0],
                             )
        sys.exit(-1)
    thread_count = int(sys.argv[1])
    if sys.argv[2] == "udp":
        protocol = socket.SOCK_DGRAM
    else:
        protocol = socket.SOCK_STREAM
    buffer_size = int(sys.argv[3])
    assert thread_count >= 1
    
    threads = [_Task(i, protocol, buffer_size) for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    
