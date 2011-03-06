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

import sys, os, struct, socket, select, threading, inspect
from erlang import (binary_to_term, term_to_binary,
                    OtpErlangAtom, OtpErlangBinary)

_MESSAGE_INIT           = 1
_MESSAGE_SEND_ASYNC     = 2
_MESSAGE_SEND_SYNC      = 3
_MESSAGE_RECV_ASYNC     = 4
_MESSAGE_RETURN_ASYNC   = 5
_MESSAGE_RETURN_SYNC    = 6
_MESSAGE_RETURNS_ASYNC  = 7

class API(object):
    __ASYNC  =  1
    __SYNC   = -1

    def __init__(self, index, protocol, size):
        self.__s = socket.fromfd(index + 3, socket.AF_INET, protocol)
        self.__size = size
        self.__callbacks = {}
        self.__s.send(term_to_binary(OtpErlangAtom("init")))
        self.__prefix, self.__timeout_async, self.__timeout_sync = self.poll()

    def __del__(self):
        self.__s.close()

    def subscribe(self, name, Function):
        args, varargs, varkw, defaults = inspect.getargspec(Function)
        assert len(args) == 7 # self + arguments, so a non-static method
        self.__callbacks[self.__prefix + name] = Function
        self.__s.send(term_to_binary((OtpErlangAtom("subscribe"), name)))

    def unsubscribe(self, name):
        del self.__callbacks[self.__prefix + name]
        self.__s.send(term_to_binary((OtpErlangAtom("unsubscribe"), name)))

    def send_async(self, name, request):
        return self.send_async(name, request, self.__timeout_async)

    def send_async(self, name, request, timeout):
        self.__s.send(term_to_binary((OtpErlangAtom("send_async"), name,
                                      OtpErlangBinary(request), timeout)))
        return self.poll()

    def send_sync(self, name, request):
        return self.send_sync(name, request, self.__timeout_sync)

    def send_sync(self, name, request, timeout):
        self.__s.send(term_to_binary((OtpErlangAtom("send_sync"), name,
                                      OtpErlangBinary(request), timeout)))
        return self.poll()

    def mcast_async(self, name, request):
        return self.mcast_async(name, request, self.__timeout_async)

    def mcast_async(self, name, request, timeout):
        self.__s.send(term_to_binary((OtpErlangAtom("mcast_async"), name,
                                      OtpErlangBinary(request), timeout)))
        return self.poll()

    def forward_(self, command, name, request, timeout, transId, pid):
        if command == API.__SYNC:
            self.forward_sync(name, request, timeout, transId, pid)
        elif command == API.__ASYNC:
            self.forward_async(name, request, timeout, transId, pid)
        else:
            assert False

    def forward_async(self, name, request, timeout, transId, pid):
        self.__s.send(term_to_binary((OtpErlangAtom("forward_async"), name,
                                      OtpErlangBinary(request), timeout,
                                      OtpErlangBinary(transId), pid)))
        raise _return_async_exception()

    def forward_sync(self, name, request, timeout, transId, pid):
        self.__s.send(term_to_binary((OtpErlangAtom("forward_sync"), name,
                                      OtpErlangBinary(request), timeout,
                                      OtpErlangBinary(transId), pid)))
        raise _return_sync_exception()

    def return_(self, command, name, response, timeout, transId, pid):
        if command == API.__SYNC:
            self.return_sync(name, response, timeout, transId, pid)
        elif command == API.__ASYNC:
            self.return_async(name, response, timeout, transId, pid)
        else:
            assert False

    def return_async(self, name, response, timeout, transId, pid):
        self.__s.send(term_to_binary((OtpErlangAtom("return_async"), name,
                                      OtpErlangBinary(response), timeout,
                                      OtpErlangBinary(transId), pid)))
        raise _return_async_exception()

    def return_sync(self, name, response, timeout, transId, pid):
        self.__s.send(term_to_binary((OtpErlangAtom("return_sync"), name,
                                      OtpErlangBinary(response), timeout,
                                      OtpErlangBinary(transId), pid)))
        raise _return_sync_exception()

    def recv_async(self, timeout, transId):
        self.__s.send(term_to_binary((OtpErlangAtom("recv_async"), timeout,
                                      OtpErlangBinary(transId))))
        return self.poll()

    def __callback(self, command, name, request, timeout, transId, pid):
        function = self.__callbacks.get(name, None)
        assert function is not None
        if command == _MESSAGE_SEND_ASYNC:
            try:
                response = function(API.__ASYNC, name, request,
                                    timeout, transId, pid)
                self.return_async(name, response, timeout, transId, pid)
            except _return_async_exception:
                return
            except _return_sync_exception:
                assert False
        elif command == _MESSAGE_SEND_SYNC:
            try:
                response = function(API.__SYNC, name, request,
                                    timeout, transId, pid)
                self.return_sync(name, response, timeout, transId, pid)
            except _return_sync_exception:
                return
            except _return_async_exception:
                assert False
        else:
            assert False

    def poll(self):
        while True:
            ready = False
            while ready == False:
                IN, OUT, EXCEPT = select.select([self.__s],[],[])
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

            i, j = 0, 4
            (command,) = struct.unpack("=I", data[i:j])
            if command == _MESSAGE_INIT:
                i, j = j, j + 4
                (prefixSize,) = struct.unpack("=I", data[i:j])
                i, j = j, j + prefixSize + 4 + 4
                (prefix, null_terminator, timeoutSync,
                 timeoutAsync) = struct.unpack("=%dscII" % (prefixSize - 1),
                                               data[i:j])
                return (prefix, timeoutSync, timeoutAsync)
            elif (command == _MESSAGE_SEND_ASYNC or
                  command == _MESSAGE_SEND_SYNC):
                i, j = j, j + 4
                (nameSize,) = struct.unpack("=I", data[i:j])
                i, j = j, j + nameSize + 4
                (name, null_terminator,
                 requestSize) = struct.unpack("=%dscI" % (nameSize - 1),
                                              data[i:j])
                i, j = j, j + requestSize + 4 + 16 + 4
                (request, timeout, transId,
                 pidSize) = struct.unpack("=%dsI16sI" % requestSize, data[i:j])
                i, j = j, j + pidSize
                (pid,) = struct.unpack("=%ds" % pidSize, data[i:j])
                self.__callback(command, name, request, timeout,
                                transId, binary_to_term(pid))
            elif (command == _MESSAGE_RECV_ASYNC or
                  command == _MESSAGE_RETURN_SYNC):
                i, j = j, j + 4
                (responseSize,) = struct.unpack("=I", data[i:j])
                i, j = j, j + responseSize + 16
                (response,
                 transId) = struct.unpack("=%ds16s" % responseSize, data[i:j])
                return (response, transId)
            elif command == _MESSAGE_RETURN_ASYNC:
                i, j = j, j + 16
                (transId,) = struct.unpack("=16s", data[i:j])
                return transId
            elif command == _MESSAGE_RETURNS_ASYNC:
                i, j = j, j + 4
                (transIdCount,) = struct.unpack("=I", data[i:j])
                transIdList = []
                for count in range(transIdCount):
                    i, j = j, j + 16
                    (transId,) = struct.unpack("=16s", data[i:j])
                    transIdList.append(transId)
                return transIdList

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
    
