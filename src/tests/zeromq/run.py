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
# CONTRIBUTORS 'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES,
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

import sys, os
cwd = os.path.dirname(os.path.abspath(__file__))
sys.path.append(
    os.path.sep.join(
        cwd.split(os.path.sep)[:-2] + ['api', 'python']
    )
)

import threading, socket
from cloudi import API

class _Task(threading.Thread):
    def __init__(self, index, protocol, size):
        threading.Thread.__init__(self)
        self.__api = API(index, protocol, size)
        self.__index = index

    def zigzag_finish(self, command, name, requestInfo, request,
                      timeout, priority, transId, pid):
        print 'Got to CloudI finish from ZeroMQ zig-zag:',request
        self.__api.return_(command, name, '', 'done', timeout, transId, pid)

    def chain_inproc_finish(self, command, name, requestInfo, request,
                            timeout, priority, transId, pid):
        print 'chain_inproc_finish'
        self.__api.return_(command, name, '', 'done', timeout, transId, pid)

    def chain_ipc_finish(self, command, name, requestInfo, request,
                         timeout, priority, transId, pid):
        print 'chain_ipc_finish'
        self.__api.return_(command, name, '', 'done', timeout, transId, pid)

    def run(self):
        # sends outside of a callback must occur before the subscriptions
        # so that the sends going out do not conflict with the
        # sends coming in to call local callbacks
        if self.__index == 0:
            self.__api.send_async('/tests/zeromq/zigzag_start', 'magic')
            self.__api.send_async('/tests/zeromq/chain_inproc_start', 'inproc')
            self.__api.send_async('/tests/zeromq/chain_ipc_start', 'ipc')
        self.__api.subscribe('zigzag_finish', self.zigzag_finish)
        self.__api.subscribe('chain_inproc_finish', self.chain_inproc_finish)
        self.__api.subscribe('chain_ipc_finish', self.chain_ipc_finish)
        running = True
        while running:
            result = self.__api.poll()
            if result is None:
                running = False
            else:
                print '(python) received:',result
        print 'exited thread'

if __name__ == '__main__':
    if len(sys.argv) != 4:
        print >> sys.stderr, 'Usage: %s thread_count protocol buffer_size' % (
                                 sys.argv[0],
                             )
        sys.exit(-1)
    thread_count = int(sys.argv[1])
    if sys.argv[2] == 'udp':
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
    
