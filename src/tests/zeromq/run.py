#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
    def __init__(self, thread_index):
        threading.Thread.__init__(self)
        self.__api = API(thread_index)
        self.__index = thread_index

    def run(self):
        # Unable to receive messages in the 1st thread, because ZeroMQ usage
        # removes the association between Erlang pids and CloudI API requests
        # (i.e., otherwise the sending thread could receive the sent
        #  message, with this test ZeroMQ configuration)
        if self.__index == 0:
            print 'zeromq zigzag start'
            self.__api.send_async('/tests/zeromq/zigzag_start',
                                  'magic', request_info='amazing')
            print 'zeromq chain_inproc start'
            self.__api.send_async('/tests/zeromq/chain_inproc_start',
                                  'inproc', request_info='process')
            print 'zeromq chain_ipc start'
            self.__api.send_async('/tests/zeromq/chain_ipc_start',
                                  'ipc', request_info='pipes')
        else:
            self.__api.subscribe('zigzag_finish',
                                 self.zigzag_finish)
            self.__api.subscribe('chain_inproc_finish',
                                 self.chain_inproc_finish)
            self.__api.subscribe('chain_ipc_finish',
                                 self.chain_ipc_finish)
        result = self.__api.poll()
        print 'exited thread:', result

    def zigzag_finish(self, command, name, pattern,
                      request_info, request,
                      timeout, priority, trans_id, pid):
        assert request_info == 'amazing'
        assert request == 'magic'
        print 'zeromq zigzag end'
        self.__api.return_(command, name, pattern,
                           '', 'done', timeout, trans_id, pid)

    def chain_inproc_finish(self, command, name, pattern,
                            request_info, request,
                            timeout, priority, trans_id, pid):
        assert request_info == 'process'
        assert request == 'inproc'
        print 'zeromq chain_inproc end'
        self.__api.return_(command, name, pattern,
                           '', 'done', timeout, trans_id, pid)

    def chain_ipc_finish(self, command, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        assert request_info == 'pipes'
        assert request == 'ipc'
        print 'zeromq chain_ipc end'
        self.__api.return_(command, name, pattern,
                           '', 'done', timeout, trans_id, pid)

if __name__ == '__main__':
    thread_count = API.thread_count()
    assert thread_count >= 1
    
    threads = [_Task(i) for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

