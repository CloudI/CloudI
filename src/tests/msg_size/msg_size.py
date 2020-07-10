#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
Message Size Integration Test with Python
"""

from __future__ import print_function
import sys
import threading
import struct
import traceback
from cloudi import API, TerminateException

_DESTINATION = '/tests/msg_size/erlang'

class Task(threading.Thread):
    """
    msg_size thread task
    """
    api_name = None

    def __init__(self, api, terminate):
        threading.Thread.__init__(self)
        self.__api = api
        self.__terminate_exception = terminate

    def run(self):
        """
        run the msg_size thread
        """
        try:
            self.__api.subscribe(Task.api_name, _request)

            result = self.__api.poll()
            assert result is False
        except self.__terminate_exception:
            pass
        except Exception:
            traceback.print_exc(file=sys.stderr)
        print('terminate msg_size %s' % Task.api_name)

def _request(api, request_type, name, pattern, request_info, request,
             timeout, priority, trans_id, pid):
    # pylint: disable=unused-argument
    # pylint: disable=too-many-arguments
    i = struct.unpack('=I', request[:4])[0]
    if i == 4294967295:
        i = 0
    else:
        i += 1
    request = struct.pack('=I', i) + request[4:]
    print('forward #%d %s to %s (with timeout %d ms)' % (
        i, Task.api_name, _DESTINATION, timeout,
    ))
    api.forward_(request_type, _DESTINATION, request_info, request,
                 timeout, priority, trans_id, pid)

def _main():
    thread_count = API.thread_count()
    assert thread_count >= 1

    Task.api_name = 'python'
    threads = [Task(API(thread_index), TerminateException)
               for thread_index in range(thread_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

if __name__ == '__main__':
    _main()
