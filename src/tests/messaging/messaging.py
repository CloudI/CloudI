#!/usr/bin/env python
# -*- coding: utf-8; Mode: python; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# BSD LICENSE
# 
# Copyright (c) 2012, Michael Truog <mjtruog at gmail dot com>
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

import sys, os
sys.path.append(
    os.path.sep.join(
        os.path.dirname(os.path.abspath(__file__))
               .split(os.path.sep)[:-2] + ['api', 'python']
    )
)

import threading, socket, types
from cloudi import API

class _Task(threading.Thread):
    def __init__(self, thread_index):
        threading.Thread.__init__(self)
        self.__api = API(thread_index)
        self.__index = thread_index

    def __f_abcd(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + 'a/b/c/d')
        assert request == 'test1'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f_abc_(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + 'a/b/c/*')
        assert request == 'test2' or request == 'test3'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f_ab_d(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + 'a/b/*/d')
        assert request == 'test4' or request == 'test5'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f_a_cd(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + 'a/*/c/d')
        assert request == 'test6' or request == 'test7'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f__bcd(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + '*/b/c/d')
        assert request == 'test8' or request == 'test9'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f_ab__(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + 'a/b/*')
        assert request == 'test10'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f_a__d(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + 'a/*/d')
        assert request == 'test11'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f___cd(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + '*/c/d')
        assert request == 'test12'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f_a___(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + 'a/*')
        assert request == 'test13'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f____d(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + '*/d')
        assert request == 'test14'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __f_____(self, command, name, pattern, requestInfo, request,
                 timeout, priority, transId, pid):
        assert pattern == (self.__api.prefix() + '*')
        assert request == 'test15'
        self.__api.return_(command, name, pattern,
                           '', request, timeout, transId, pid)

    def __sequence1(self, command, name, pattern, requestInfo, request,
                    timeout, priority, transId, pid):
        print 'messaging sequence1 start'
        assert request == 'start'
        test1_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/d',  'test1'
        )
        test2_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/z',  'test2'
        )
        test3_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/dd', 'test3'
        )
        test4_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/z/d',  'test4'
        )
        test5_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/cc/d', 'test5'
        )
        test6_id = self.__api.send_async(
            self.__api.prefix() + 'a/z/c/d',  'test6'
        )
        test7_id = self.__api.send_async(
            self.__api.prefix() + 'a/bb/c/d', 'test7'
        )
        test8_id = self.__api.send_async(
            self.__api.prefix() + 'z/b/c/d',  'test8'
        )
        test9_id = self.__api.send_async(
            self.__api.prefix() + 'aa/b/c/d', 'test9'
        )
        test10_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/czd',  'test10'
        )
        test11_id = self.__api.send_async(
            self.__api.prefix() + 'a/bzc/d',  'test11'
        )
        test12_id = self.__api.send_async(
            self.__api.prefix() + 'azb/c/d',  'test12'
        )
        test13_id = self.__api.send_async(
            self.__api.prefix() + 'a/bzczd',  'test13'
        )
        test14_id = self.__api.send_async(
            self.__api.prefix() + 'azbzc/d',  'test14'
        )
        test15_id = self.__api.send_async(
            self.__api.prefix() + 'azbzczd',  'test15'
        )
        # n.b., depends on cloudi_constants.hrl having
        # RECV_ASYNC_STRATEGY == recv_async_select_oldest
        (tmp, test1_check, test1_id_check) = self.__api.recv_async()
        assert test1_check == 'test1'
        assert test1_id_check == test1_id
        (tmp, test2_check, test2_id_check) = self.__api.recv_async()
        assert test2_check == 'test2'
        assert test2_id_check == test2_id
        (tmp, test3_check, test3_id_check) = self.__api.recv_async()
        assert test3_check == 'test3'
        assert test3_id_check == test3_id
        (tmp, test4_check, test4_id_check) = self.__api.recv_async()
        assert test4_check == 'test4'
        assert test4_id_check == test4_id
        (tmp, test5_check, test5_id_check) = self.__api.recv_async()
        assert test5_check == 'test5'
        assert test5_id_check == test5_id
        (tmp, test6_check, test6_id_check) = self.__api.recv_async()
        assert test6_check == 'test6'
        assert test6_id_check == test6_id
        (tmp, test7_check, test7_id_check) = self.__api.recv_async()
        assert test7_check == 'test7'
        assert test7_id_check == test7_id
        (tmp, test8_check, test8_id_check) = self.__api.recv_async()
        assert test8_check == 'test8'
        assert test8_id_check == test8_id
        (tmp, test9_check, test9_id_check) = self.__api.recv_async()
        assert test9_check == 'test9'
        assert test9_id_check == test9_id
        (tmp, test10_check, test10_id_check) = self.__api.recv_async()
        assert test10_check == 'test10'
        assert test10_id_check == test10_id
        (tmp, test11_check, test11_id_check) = self.__api.recv_async()
        assert test11_check == 'test11'
        assert test11_id_check == test11_id
        (tmp, test12_check, test12_id_check) = self.__api.recv_async()
        assert test12_check == 'test12'
        assert test12_id_check == test12_id
        (tmp, test13_check, test13_id_check) = self.__api.recv_async()
        assert test13_check == 'test13'
        assert test13_id_check == test13_id
        (tmp, test14_check, test14_id_check) = self.__api.recv_async()
        assert test14_check == 'test14'
        assert test14_id_check == test14_id
        (tmp, test15_check, test15_id_check) = self.__api.recv_async()
        assert test15_check == 'test15'
        assert test15_id_check == test15_id
        print 'messaging sequence1 end'
        self.__api.return_(command, name, pattern,
                           '', 'end', timeout, transId, pid)

    def __sequence2(self, command, name, pattern, requestInfo, request,
                    timeout, priority, transId, pid):
        assert request == 'start'
        self.__api.return_(command, name, pattern,
                           '', 'end', timeout, transId, pid)

    def __sequence3(self, command, name, pattern, requestInfo, request,
                    timeout, priority, transId, pid):
        assert request == 'start'
        self.__api.return_(command, name, pattern,
                           '', 'end', timeout, transId, pid)

    def run(self):
        # sends outside of a callback function must occur before the
        # subscriptions so that the incoming requests do not interfere with
        # the outgoing sends (i.e., without subscriptions there will be no
        # incoming requests)
        if self.__index == 0:
            self.__api.send_async(self.__api.prefix() + 'sequence1', 'start')
            self.__api.send_async(self.__api.prefix() + 'sequence2', 'start')
            self.__api.send_async(self.__api.prefix() + 'sequence3', 'start')
        self.__api.subscribe('a/b/c/d', self.__f_abcd)
        self.__api.subscribe('a/b/c/*', self.__f_abc_)
        self.__api.subscribe('a/b/*/d', self.__f_ab_d)
        self.__api.subscribe('a/*/c/d', self.__f_a_cd)
        self.__api.subscribe('*/b/c/d', self.__f__bcd)
        self.__api.subscribe('a/b/*',   self.__f_ab__)
        self.__api.subscribe('a/*/d',   self.__f_a__d)
        self.__api.subscribe('*/c/d',   self.__f___cd)
        self.__api.subscribe('a/*',     self.__f_a___)
        self.__api.subscribe('*/d',     self.__f____d)
        self.__api.subscribe('*',       self.__f_____)
        self.__api.subscribe('sequence1', self.__sequence1)
        self.__api.subscribe('sequence2', self.__sequence2)
        self.__api.subscribe('sequence3', self.__sequence3)

        running = True
        while running:
            result = self.__api.poll()
            if result is None:
                running = False
            else:
                print '(python) received:',result
        print 'exited thread'

if __name__ == '__main__':
    thread_count = API.thread_count()
    assert thread_count >= 1
    
    threads = [_Task(i) for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    
