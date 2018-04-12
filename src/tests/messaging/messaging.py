#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2012-2018 Michael Truog <mjtruog at protonmail dot com>
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
Messaging Integration Test with Python
"""

from __future__ import print_function
import sys
import threading
import traceback
from cloudi import API, TerminateException

class Task(threading.Thread):
    """
    messaging thread task
    """
    def __init__(self, api, thread_index, name, terminate):
        threading.Thread.__init__(self)
        self.__api = api
        self.__thread_index = thread_index
        self.__name = name
        self.__terminate_exception = terminate

    def run(self):
        """
        run the messaging thread
        """
        # pylint: disable=bare-except
        try:
            self.__api.subscribe('a/b/c/d', self.__sequence1_abcd)
            self.__api.subscribe('a/b/c/*', self.__sequence1_abc_)
            self.__api.subscribe('a/b/*/d', self.__sequence1_ab_d)
            self.__api.subscribe('a/*/c/d', self.__sequence1_a_cd)
            self.__api.subscribe('*/b/c/d', self.__sequence1__bcd)
            self.__api.subscribe('a/b/*', self.__sequence1_ab__)
            self.__api.subscribe('a/*/d', self.__sequence1_a__d)
            self.__api.subscribe('*/c/d', self.__sequence1___cd)
            self.__api.subscribe('a/*', self.__sequence1_a___)
            self.__api.subscribe('*/d', self.__sequence1____d)
            self.__api.subscribe('*', self.__sequence1_____)
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
            if self.__thread_index == 0:
                # start sequence1
                self.__api.send_async(
                    self.__api.prefix() + 'sequence1', b'start',
                )

            result = self.__api.poll()
            assert result is False
        except self.__terminate_exception:
            pass
        except:
            traceback.print_exc(file=sys.stderr)
        print('terminate messaging %s' % self.__name)

    def __sequence1_abcd(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + 'a/b/c/d')
        assert request == b'test1'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_abc_(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + 'a/b/c/*')
        assert request == b'test2' or request == b'test3'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_ab_d(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + 'a/b/*/d')
        assert request == b'test4' or request == b'test5'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_a_cd(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + 'a/*/c/d')
        assert request == b'test6' or request == b'test7'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1__bcd(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + '*/b/c/d')
        assert request == b'test8' or request == b'test9'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_ab__(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + 'a/b/*')
        assert request == b'test10'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_a__d(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + 'a/*/d')
        assert request == b'test11'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1___cd(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + '*/c/d')
        assert request == b'test12'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_a___(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + 'a/*')
        assert request == b'test13'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1____d(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + '*/d')
        assert request == b'test14'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1_____(self, request_type, name, pattern,
                         request_info, request,
                         timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        assert pattern == (self.__api.prefix() + '*')
        assert request == b'test15'
        self.__api.return_(request_type, name, pattern,
                           b'', request, timeout, trans_id, pid)

    def __sequence1(self, request_type, name, pattern,
                    request_info, request,
                    timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-locals
        # pylint: disable=too-many-statements

        # consume all the 'end' responses from all sequences handled
        # by this service
        while self.__api.recv_async(timeout=1000)[1] == b'end':
            pass
        print('messaging sequence1 start %s' % self.__name)
        assert request == b'start'
        test1_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/d', b'test1'
        )
        test2_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/z', b'test2'
        )
        test3_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/c/dd', b'test3'
        )
        test4_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/z/d', b'test4'
        )
        test5_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/cc/d', b'test5'
        )
        test6_id = self.__api.send_async(
            self.__api.prefix() + 'a/z/c/d', b'test6'
        )
        test7_id = self.__api.send_async(
            self.__api.prefix() + 'a/bb/c/d', b'test7'
        )
        test8_id = self.__api.send_async(
            self.__api.prefix() + 'z/b/c/d', b'test8'
        )
        test9_id = self.__api.send_async(
            self.__api.prefix() + 'aa/b/c/d', b'test9'
        )
        test10_id = self.__api.send_async(
            self.__api.prefix() + 'a/b/czd', b'test10'
        )
        test11_id = self.__api.send_async(
            self.__api.prefix() + 'a/bzc/d', b'test11'
        )
        test12_id = self.__api.send_async(
            self.__api.prefix() + 'azb/c/d', b'test12'
        )
        test13_id = self.__api.send_async(
            self.__api.prefix() + 'a/bzczd', b'test13'
        )
        test14_id = self.__api.send_async(
            self.__api.prefix() + 'azbzc/d', b'test14'
        )
        test15_id = self.__api.send_async(
            self.__api.prefix() + 'azbzczd', b'test15'
        )
        # n.b., depends on cloudi_core_i_constants.hrl having
        # RECV_ASYNC_STRATEGY == recv_async_select_oldest
        self.__api.recv_async(trans_id=test1_id, consume=False)
        (_, test1_check, test1_id_check) = self.__api.recv_async()
        assert test1_check == b'test1'
        assert test1_id_check == test1_id
        self.__api.recv_async(trans_id=test2_id, consume=False)
        (_, test2_check, test2_id_check) = self.__api.recv_async()
        assert test2_check == b'test2'
        assert test2_id_check == test2_id
        self.__api.recv_async(trans_id=test3_id, consume=False)
        (_, test3_check, test3_id_check) = self.__api.recv_async()
        assert test3_check == b'test3'
        assert test3_id_check == test3_id
        self.__api.recv_async(trans_id=test4_id, consume=False)
        (_, test4_check, test4_id_check) = self.__api.recv_async()
        assert test4_check == b'test4'
        assert test4_id_check == test4_id
        self.__api.recv_async(trans_id=test5_id, consume=False)
        (_, test5_check, test5_id_check) = self.__api.recv_async()
        assert test5_check == b'test5'
        assert test5_id_check == test5_id
        self.__api.recv_async(trans_id=test6_id, consume=False)
        (_, test6_check, test6_id_check) = self.__api.recv_async()
        assert test6_check == b'test6'
        assert test6_id_check == test6_id
        self.__api.recv_async(trans_id=test7_id, consume=False)
        (_, test7_check, test7_id_check) = self.__api.recv_async()
        assert test7_check == b'test7'
        assert test7_id_check == test7_id
        self.__api.recv_async(trans_id=test8_id, consume=False)
        (_, test8_check, test8_id_check) = self.__api.recv_async()
        assert test8_check == b'test8'
        assert test8_id_check == test8_id
        self.__api.recv_async(trans_id=test9_id, consume=False)
        (_, test9_check, test9_id_check) = self.__api.recv_async()
        assert test9_check == b'test9'
        assert test9_id_check == test9_id
        self.__api.recv_async(trans_id=test10_id, consume=False)
        (_, test10_check, test10_id_check) = self.__api.recv_async()
        assert test10_check == b'test10'
        assert test10_id_check == test10_id
        self.__api.recv_async(trans_id=test11_id, consume=False)
        (_, test11_check, test11_id_check) = self.__api.recv_async()
        assert test11_check == b'test11'
        assert test11_id_check == test11_id
        self.__api.recv_async(trans_id=test12_id, consume=False)
        (_, test12_check, test12_id_check) = self.__api.recv_async()
        assert test12_check == b'test12'
        assert test12_id_check == test12_id
        self.__api.recv_async(trans_id=test13_id, consume=False)
        (_, test13_check, test13_id_check) = self.__api.recv_async()
        assert test13_check == b'test13'
        assert test13_id_check == test13_id
        self.__api.recv_async(trans_id=test14_id, consume=False)
        (_, test14_check, test14_id_check) = self.__api.recv_async()
        assert test14_check == b'test14'
        assert test14_id_check == test14_id
        self.__api.recv_async(trans_id=test15_id, consume=False)
        (_, test15_check, test15_id_check) = self.__api.recv_async()
        assert test15_check == b'test15'
        assert test15_id_check == test15_id
        print('messaging sequence1 end %s' % self.__name)
        # start sequence2
        self.__api.send_async(self.__api.prefix() + 'sequence2', b'start')
        self.__api.return_(request_type, name, pattern,
                           b'', b'end', timeout, trans_id, pid)

    def __sequence2_e1(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'1', timeout, trans_id, pid)

    def __sequence2_e2(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'2', timeout, trans_id, pid)

    def __sequence2_e3(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'3', timeout, trans_id, pid)

    def __sequence2_e4(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'4', timeout, trans_id, pid)

    def __sequence2_e5(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'5', timeout, trans_id, pid)

    def __sequence2_e6(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'6', timeout, trans_id, pid)

    def __sequence2_e7(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'7', timeout, trans_id, pid)

    def __sequence2_e8(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', b'8', timeout, trans_id, pid)

    def __sequence2(self, request_type, name, pattern,
                    request_info, request,
                    timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-locals

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
                    (_,
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
                    (_,
                     e_check,
                     e_id_check) = self.__api.recv_async(trans_id=e_id)
                    assert e_id == e_id_check
                null_id = self.__api.recv_async(timeout=1000)[2]
                assert null_id == b'\0' * 16
        print('messaging sequence2 end %s' % self.__name)
        # start sequence3
        self.__api.send_async(self.__api.prefix() + 'sequence3', b'start')
        self.__api.return_(request_type, name, pattern,
                           b'', b'end', timeout, trans_id, pid)

    def __sequence3_f1(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        request_i = int(request)
        if request_i == 4:
            return b'done'
        request_new = request_i + 2 # two steps forward
        self.__api.forward_(request_type, self.__api.prefix() + 'f2',
                            request_info,
                            ('%d' % request_new).encode('ascii'),
                            timeout, priority, trans_id, pid)

    def __sequence3_f2(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        request_i = int(request)
        request_new = request_i - 1 # one step back
        self.__api.forward_(request_type, self.__api.prefix() + 'f1',
                            request_info,
                            ('%d' % request_new).encode('ascii'),
                            timeout, priority, trans_id, pid)

    def __sequence3_g1(self, request_type, name, pattern,
                       request_info, request,
                       timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        self.__api.return_(request_type, name, pattern,
                           b'', request + b'suffix', timeout, trans_id, pid)

    def __sequence3(self, request_type, name, pattern,
                    request_info, request,
                    timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        # pylint: disable=too-many-locals
        print('messaging sequence3 start %s' % self.__name)
        assert request == b'start'
        test1_id = self.__api.send_async(
            self.__api.prefix() + 'f1', b'0'
        )
        (_, test1_check, test1_id_check) = self.__api.recv_async(
            trans_id=test1_id
        )
        assert test1_id_check == test1_id
        assert test1_check == b'done'
        (_, test2_check, _) = self.__api.send_sync(
            self.__api.prefix() + 'g1', b'prefix_'
        )
        assert test2_check == b'prefix_suffix'
        print('messaging sequence3 end %s' % self.__name)
        # loop to find any infrequent problems, restart sequence1
        self.__api.send_async(
            self.__api.prefix() + 'sequence1', b'start',
        )
        self.__api.return_(request_type, name, pattern,
                           b'', b'end', timeout, trans_id, pid)

def _main():
    thread_count = API.thread_count()
    assert thread_count >= 1

    threads = [Task(API(thread_index), thread_index,
                    'python', TerminateException)
               for thread_index in range(thread_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

if __name__ == '__main__':
    _main()

