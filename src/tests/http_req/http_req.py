#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2019 Michael Truog <mjtruog at protonmail dot com>
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
HTTP Request Integration Test with Python
"""

from __future__ import print_function
import sys
import threading
import traceback
from cloudi import API, TerminateException

class Task(threading.Thread):
    """
    http_req thread task
    """
    def __init__(self, api, name, exception):
        threading.Thread.__init__(self)
        self.__api = api
        self.__name = name
        self.__terminate_exception = exception

    def run(self):
        """
        run the http_req thread
        """
        # pylint: disable=bare-except
        try:
            assert self.__api.subscribe_count(self.__name + '.xml/get') == 0
            self.__api.subscribe(self.__name + '.xml/get', self.__request)
            assert self.__api.subscribe_count(self.__name + '.xml/get') == 1

            result = self.__api.poll()
            assert result is False
        except self.__terminate_exception:
            pass
        except:
            traceback.print_exc(file=sys.stderr)
        print('terminate http_req %s' % self.__name)

    def __request(self, request_type, name, pattern, request_info, request,
                  timeout, priority, trans_id, pid):
        # pylint: disable=unused-argument
        # pylint: disable=too-many-arguments
        http_qs = self.__api.info_key_value_parse(request)
        value = http_qs.get(b'value', None)
        if value is None:
            response = """\
<http_test><error>no value specified</error></http_test>"""
        else:
            if isinstance(value, list):
                value = value[0]
            response = """\
<http_test><value>%d</value></http_test>""" % (int(value),)
        self.__api.return_(request_type, name, pattern,
                           b'', response.encode('utf-8'),
                           timeout, trans_id, pid)

def _main():
    thread_count = API.thread_count()
    assert thread_count >= 1

    threads = [Task(API(thread_index), 'python', TerminateException)
               for thread_index in range(thread_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

if __name__ == '__main__':
    _main()
