#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2022 Michael Truog <mjtruog at protonmail dot com>
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
HTTP Integration Test with Python
"""

from __future__ import print_function
import sys
import threading
import traceback
from cloudi import API, TerminateException

class _Task(threading.Thread):
    def __init__(self, thread_index):
        threading.Thread.__init__(self)
        self.__api = None
        self.__thread_index = thread_index

    def run(self):
        try:
            self.__api = API(self.__thread_index)
            self.__api.subscribe('text/post', self.__text)

            result = self.__api.poll()
            assert result is False
        except TerminateException:
            pass
        except Exception:
            traceback.print_exc(file=sys.stderr)
        print('terminate http python')

    def __text(self, request_type, name, pattern, request_info, request,
               timeout, priority, trans_id, source):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        print(b'(' + request + b')')
        assert request == b'Test Text'
        self.__api.return_(request_type, name, pattern, b'', b'Test Response',
                           timeout, trans_id, source)

def _main():
    thread_count = API.thread_count()
    assert thread_count >= 1

    threads = [_Task(thread_index)
               for thread_index in range(thread_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

if __name__ == '__main__':
    _main()
