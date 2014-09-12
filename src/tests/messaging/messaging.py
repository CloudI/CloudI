#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
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

import threading, socket, types, traceback
from cloudi import API

class Task(threading.Thread):
    def __init__(self, api, thread_index, name):
        threading.Thread.__init__(self)
        self.__api = api
        self.__index = thread_index
        self.__name = name

    def run(self):
        try:
            self.__api.subscribe('a/b/c/d', self.__sequence1_abcd)
            self.__api.subscribe('a/b/c/*', self.__sequence1_abc_)
            self.__api.subscribe('a/b/*/d', self.__sequence1_ab_d)
            self.__api.subscribe('a/*/c/d', self.__sequence1_a_cd)
            self.__api.subscribe('*/b/c/d', self.__sequence1__bcd)
            self.__api.subscribe('a/b/*',   self.__sequence1_ab__)
            self.__api.subscribe('a/*/d',   self.__sequence1_a__d)
            self.__api.subscribe('*/c/d',   self.__sequence1___cd)
            self.__api.subscribe('a/*',     self.__sequence1_a___)
            self.__api.subscribe('*/d',     self.__sequence1____d)
            self.__api.subscribe('*',       self.__sequence1_____)
            self.__api.subscribe('sequence1', self.__sequence1)
            self.__api.subscribe('e', self.__sequence2_e1)
            self.__api.subscribe('e', self.__sequence2_e2)
            self.__api.subscribe('e', self.__sequence2_e3)
            self.__api.subscribe('e', self.__sequence2_e4)
            self.__api.subscribe('e', self.__sequence2_e5)
            self.__api.subscribe('e', self.__sequence2_e6)
            self.__api.subscribe('e', self.__sequence2_e7)
            self.__api.subscribe('e', self.__sequence2_e8)
            self.__api.subscribe('sequence2', self.__sequence2)
            self.__api.subscribe('f1', self.__sequence3_f1)
            self.__api.subscribe('f2', self.__sequence3_f2)
            self.__api.subscribe('g1', self.__sequence3_g1)
            self.__api.subscribe('sequence3', self.__sequence3)
            if self.__index == 0:
                # start sequence1
                self.__api.send_async(
                    self.__api.prefix() + 'sequence1', b'start',
                )
    
            result = self.__api.poll()
            print('exited thread: %d' % result)
        except:
            traceback.print_exc(file=sys.stderr)

    def __sequence1_abcd(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + 'a/b/c/d')
        assert request == b'test1'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_abc_(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + 'a/b/c/*')
        assert request == b'test2' or request == b'test3'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_ab_d(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + 'a/b/*/d')
        assert request == b'test4' or request == b'test5'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_a_cd(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + 'a/*/c/d')
        assert request == b'test6' or request == b'test7'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1__bcd(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + '*/b/c/d')
        assert request == b'test8' or request == b'test9'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_ab__(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + 'a/b/*')
        assert request == b'test10'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_a__d(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + 'a/*/d')
        assert request == b'test11'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1___cd(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + '*/c/d')
        assert request == b'test12'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_a___(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + 'a/*')
        assert request == b'test13'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1____d(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + '*/d')
        assert request == b'test14'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_____(self, command, name, pattern, request_info, request,
                         timeout, priority, trans_id, pid):
        assert pattern == (self.__api.prefix() + '*')
        assert request == b'test15'
        self.__api.return_(command, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1(self, command, name, pattern, request_info, request,
                    timeout, priority, trans_id, pid):
        # consume all the 'end' responses from all sequences handled
        # by this service
        while self.__api.recv_async(timeout=1000)[1] == b'end':
            pass
        print('messaging sequence1 start %s' % self.__name)
        assert request == b'start'
        # n.b., depends on cloudi_constants.hrl having
        # SERVICE_NAME_PATTERN_MATCHING defined
        test1_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/d',  b'test1'
        )
        test2_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/z',  b'test2'
        )
        test3_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/dd', b'test3'
        )
        test4_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/z/d',  b'test4'
        )
        test5_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/cc/d', b'test5'
        )
        test6_id = self.__api.send_async(
            self.__api.prefix() + 'a/z/c/d',  b'test6'
        )
        test7_id = self.__api.send_async(
            self.__api.prefix() + 'a/bb/c/d', b'test7'
        )
        test8_id = self.__api.send_async(
            self.__api.prefix() + 'z/b/c/d',  b'test8'
        )
        test9_id = self.__api.send_async(
            self.__api.prefix() + 'aa/b/c/d', b'test9'
        )
        test10_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/czd',  b'test10'
        )
        test11_id = self.__api.send_async(
            self.__api.prefix() + 'a/bzc/d',  b'test11'
        )
        test12_id = self.__api.send_async(
            self.__api.prefix() + 'azb/c/d',  b'test12'
        )
        test13_id = self.__api.send_async(
            self.__api.prefix() + 'a/bzczd',  b'test13'
        )
        test14_id = self.__api.send_async(
            self.__api.prefix() + 'azbzc/d',  b'test14'
        )
        test15_id = self.__api.send_async(
            self.__api.prefix() + 'azbzczd',  b'test15'
        )
        # n.b., depends on cloudi_constants.hrl having
        # RECV_ASYNC_STRATEGY == recv_async_select_oldest
        self.__api.recv_async(trans_id=test1_id, consume=False)
        (tmp, test1_check, test1_id_check) = self.__api.recv_async()
        assert test1_check == b'test1'
        assert test1_id_check == test1_id
        self.__api.recv_async(trans_id=test2_id, consume=False)
        (tmp, test2_check, test2_id_check) = self.__api.recv_async()
        assert test2_check == b'test2'
        assert test2_id_check == test2_id
        self.__api.recv_async(trans_id=test3_id, consume=False)
        (tmp, test3_check, test3_id_check) = self.__api.recv_async()
        assert test3_check == b'test3'
        assert test3_id_check == test3_id
        self.__api.recv_async(trans_id=test4_id, consume=False)
        (tmp, test4_check, test4_id_check) = self.__api.recv_async()
        assert test4_check == b'test4'
        assert test4_id_check == test4_id
        self.__api.recv_async(trans_id=test5_id, consume=False)
        (tmp, test5_check, test5_id_check) = self.__api.recv_async()
        assert test5_check == b'test5'
        assert test5_id_check == test5_id
        self.__api.recv_async(trans_id=test6_id, consume=False)
        (tmp, test6_check, test6_id_check) = self.__api.recv_async()
        assert test6_check == b'test6'
        assert test6_id_check == test6_id
        self.__api.recv_async(trans_id=test7_id, consume=False)
        (tmp, test7_check, test7_id_check) = self.__api.recv_async()
        assert test7_check == b'test7'
        assert test7_id_check == test7_id
        self.__api.recv_async(trans_id=test8_id, consume=False)
        (tmp, test8_check, test8_id_check) = self.__api.recv_async()
        assert test8_check == b'test8'
        assert test8_id_check == test8_id
        self.__api.recv_async(trans_id=test9_id, consume=False)
        (tmp, test9_check, test9_id_check) = self.__api.recv_async()
        assert test9_check == b'test9'
        assert test9_id_check == test9_id
        self.__api.recv_async(trans_id=test10_id, consume=False)
        (tmp, test10_check, test10_id_check) = self.__api.recv_async()
        assert test10_check == b'test10'
        assert test10_id_check == test10_id
        self.__api.recv_async(trans_id=test11_id, consume=False)
        (tmp, test11_check, test11_id_check) = self.__api.recv_async()
        assert test11_check == b'test11'
        assert test11_id_check == test11_id
        self.__api.recv_async(trans_id=test12_id, consume=False)
        (tmp, test12_check, test12_id_check) = self.__api.recv_async()
        assert test12_check == b'test12'
        assert test12_id_check == test12_id
        self.__api.recv_async(trans_id=test13_id, consume=False)
        (tmp, test13_check, test13_id_check) = self.__api.recv_async()
        assert test13_check == b'test13'
        assert test13_id_check == test13_id
        self.__api.recv_async(trans_id=test14_id, consume=False)
        (tmp, test14_check, test14_id_check) = self.__api.recv_async()
        assert test14_check == b'test14'
        assert test14_id_check == test14_id
        self.__api.recv_async(trans_id=test15_id, consume=False)
        (tmp, test15_check, test15_id_check) = self.__api.recv_async()
        assert test15_check == b'test15'
        assert test15_id_check == test15_id
        print('messaging sequence1 end %s' % self.__name)
        # start sequence2
        self.__api.send_async(self.__api.prefix() + 'sequence2', b'start')
        self.__api.return_(command, name, pattern,
                           b'', b'end', timeout, trans_id, pid)

    def __sequence2_e1(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'1', timeout, trans_id, pid)

    def __sequence2_e2(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'2', timeout, trans_id, pid)

    def __sequence2_e3(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'3', timeout, trans_id, pid)

    def __sequence2_e4(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'4', timeout, trans_id, pid)

    def __sequence2_e5(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'5', timeout, trans_id, pid)

    def __sequence2_e6(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'6', timeout, trans_id, pid)

    def __sequence2_e7(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'7', timeout, trans_id, pid)

    def __sequence2_e8(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', b'8', timeout, trans_id, pid)

    def __sequence2(self, command, name, pattern, request_info, request,
                    timeout, priority, trans_id, pid):
        print('messaging sequence2 start %s' % self.__name)
        assert request == b'start'
        while True:
            # the sending process is excluded from the services that receive
            # the asynchronous message, so in this case, the receiving thread
            # will not be called, despite the fact it has subscribed to 'e',
            # to prevent a process (in this case thread) from deadlocking
            # with itself.
            e_ids = self.__api.mcast_async(self.__api.prefix() + 'e', b' ')
            # 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
            # since 1 thread is sending the mcast_async, so 3 * 8 == 24
            if len(e_ids) == 24:
                e_check_list = []
                for e_id in e_ids:
                    (tmp,
                     e_check,
                     e_id_check) = self.__api.recv_async(trans_id=e_id)
                    assert e_id == e_id_check
                    e_check_list.append(e_check)
                e_check_list.sort()
                assert b''.join(e_check_list) == b'111222333444555666777888'
                break
            else:
                print('Waiting for %s services to initialize' % (
                    str(4 - len(e_ids) / 8.0),
                ))
                for e_id in e_ids:
                    (tmp,
                     e_check,
                     e_id_check) = self.__api.recv_async(trans_id=e_id)
                    assert e_id == e_id_check
                null_id = self.__api.recv_async(timeout=1000)[2]
                assert null_id == b'\0' * 16
        print('messaging sequence2 end %s' % self.__name)
        # start sequence3
        self.__api.send_async(self.__api.prefix() + 'sequence3', b'start')
        self.__api.return_(command, name, pattern,
                           b'', b'end', timeout, trans_id, pid)

    def __sequence3_f1(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        request_i = int(request)
        if request_i == 4:
            return b'done'
        request_new = request_i + 2 # two steps forward
        self.__api.forward_(command, self.__api.prefix() + 'f2', request_info,
                            ('%d' % request_new).encode('ascii'),
                            timeout, priority, trans_id, pid)
                           
    def __sequence3_f2(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        request_i = int(request)
        request_new = request_i - 1 # one step back
        self.__api.forward_(command, self.__api.prefix() + 'f1', request_info,
                            ('%d' % request_new).encode('ascii'),
                            timeout, priority, trans_id, pid)

    def __sequence3_g1(self, command, name, pattern, request_info, request,
                       timeout, priority, trans_id, pid):
        self.__api.return_(command, name, pattern,
                           b'', request + b'suffix', timeout, trans_id, pid)

    def __sequence3(self, command, name, pattern, request_info, request,
                    timeout, priority, trans_id, pid):
        print('messaging sequence3 start %s' % self.__name)
        assert request == b'start'
        test1_id = self.__api.send_async(
            self.__api.prefix() + 'f1',  b'0'
        )
        (tmp, test1_check, test1_id_check) = self.__api.recv_async(
            trans_id=test1_id
        )
        assert test1_id_check == test1_id
        assert test1_check == b'done'
        (tmp, test2_check, test2_id_check) = self.__api.send_sync(
            self.__api.prefix() + 'g1',  b'prefix_'
        )
        assert test2_check == b'prefix_suffix'
        print('messaging sequence3 end %s' % self.__name)
        # loop to find any infrequent problems, restart sequence1
        self.__api.send_async(
            self.__api.prefix() + 'sequence1', b'start',
        )
        self.__api.return_(command, name, pattern,
                           b'', b'end', timeout, trans_id, pid)

if __name__ == '__main__':
    thread_count = API.thread_count()
    assert thread_count >= 1
    
    threads = [Task(API(i), i, 'python') for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    
