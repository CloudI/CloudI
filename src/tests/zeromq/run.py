#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2017 Michael Truog <mjtruog at protonmail dot com>
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

import sys, threading, socket, traceback
from cloudi import API, terminate_exception

class _Task(threading.Thread):
    def __init__(self, thread_index):
        threading.Thread.__init__(self)
        self.__api = API(thread_index)
        self.__index = thread_index

    def run(self):
        try:
            # Unable to receive messages in the 1st thread,
            # because ZeroMQ usage removes the association between
            # Erlang pids and CloudI API requests
            # (i.e., otherwise the sending thread could receive the sent
            #  message, with this test ZeroMQ configuration)
            if self.__index == 0:
                print('zeromq zigzag start')
                self.__api.send_async('/tests/zeromq/zigzag_start',
                                      b'magic', request_info=b'amazing')
                print('zeromq chain_inproc start')
                self.__api.send_async('/tests/zeromq/chain_inproc_start',
                                      b'inproc', request_info=b'process')
                print('zeromq chain_ipc start')
                self.__api.send_async('/tests/zeromq/chain_ipc_start',
                                      b'ipc', request_info=b'pipes')
            else:
                self.__api.subscribe('zigzag_finish',
                                     self.zigzag_finish)
                self.__api.subscribe('chain_inproc_finish',
                                     self.chain_inproc_finish)
                self.__api.subscribe('chain_ipc_finish',
                                     self.chain_ipc_finish)
            result = self.__api.poll()
            assert result == False
        except terminate_exception:
            pass
        except:
            traceback.print_exc(file=sys.stderr)
        print('terminate zeromq python')

    def zigzag_finish(self, command, name, pattern,
                      request_info, request,
                      timeout, priority, trans_id, pid):
        assert request_info == b'amazing'
        assert request == b'magic'
        print('zeromq zigzag end')
        self.__api.return_(command, name, pattern,
                           b'', b'done', timeout, trans_id, pid)

    def chain_inproc_finish(self, command, name, pattern,
                            request_info, request,
                            timeout, priority, trans_id, pid):
        assert request_info == b'process'
        assert request == b'inproc'
        print('zeromq chain_inproc end')
        self.__api.return_(command, name, pattern,
                           b'', b'done', timeout, trans_id, pid)

    def chain_ipc_finish(self, command, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        assert request_info == b'pipes'
        assert request == b'ipc'
        print('zeromq chain_ipc end')
        self.__api.return_(command, name, pattern,
                           b'', b'done', timeout, trans_id, pid)

if __name__ == '__main__':
    thread_count = API.thread_count()
    assert thread_count >= 1
    
    threads = [_Task(i) for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

