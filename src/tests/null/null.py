#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2017-2023 Michael Truog <mjtruog at protonmail dot com>
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
Null Integration Test with Python
"""

from __future__ import print_function
import sys
import threading
import traceback
from cloudi import API, TerminateException

class Task(threading.Thread):
    """
    null thread task
    """
    def __init__(self, thread_index, name, api_class, terminate_exception):
        threading.Thread.__init__(self)
        self.__api = None
        self.__thread_index = thread_index
        self.__name = name
        self.__api_class = api_class
        self.__terminate_exception = terminate_exception

    def run(self):
        """
        run the null thread
        """
        # pylint: disable=broad-except
        try:
            self.__api = self.__api_class(self.__thread_index)
            self.__api.subscribe(self.__name + '/get', self.__request)

            result = self.__api.poll()
            assert result is False
        except self.__terminate_exception:
            pass
        except Exception:
            traceback.print_exc(file=sys.stderr)
        print('terminate null %s' % self.__name)

    def __request(self, request_type, name, pattern, request_info, request,
                  timeout, priority, trans_id, source):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        print('null %s' % self.__name)
        return None

def _main():
    thread_count = API.thread_count()
    assert thread_count >= 1

    threads = [Task(thread_index, 'python', API, TerminateException)
               for thread_index in range(thread_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

if __name__ == '__main__':
    _main()
