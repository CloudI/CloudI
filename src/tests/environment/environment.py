#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2013-2020 Michael Truog <mjtruog at protonmail dot com>
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
Environment Integration Test with Python/C
"""

from __future__ import print_function
import sys
import os
import threading
import traceback
from cloudi_c import API, TerminateException

class _Task(threading.Thread):
    def __init__(self, api):
        threading.Thread.__init__(self)
        self.__api = api

    def run(self):
        try:
            self.__check_environment()
            # idle service with no subscriptions
            self.__api.shutdown('environment checked successfully')
            result = self.__api.poll()
            assert result is False
        except TerminateException:
            pass
        except Exception:
            traceback.print_exc(file=sys.stderr)
        print('terminate environment python_c')

    def __check_environment(self):
        # pylint: disable=no-self-use
        user = os.environ.get('USER')
        assert user is not None
        assert os.environ[user] == 'user'
        assert os.environ[user + '_' + user] == 'user_user'
        assert os.environ[user + user] == 'useruser'
        assert os.environ[user + '123' + user] == 'user123user'
        assert os.environ['USER_D'] == 'user_$'
        assert os.environ['USER_'] == 'user_'
        assert os.environ[user + "'check1'"] == "user'check1'"
        assert os.environ[user + '"check2"'] == 'user"check2"'
        assert os.environ[user + '/' + user + ' ' +
                          user + '`' + user] == 'user/user user`user'
        assert os.environ['À_UNICODE'] == 'true'
        assert os.environ['UNICODE_À'] == 'true'
        assert os.environ['UNICODE_CHARACTER'] == 'À'
        assert sys.argv[1:] == ['À','À','À','À']

def _main():
    thread_count = API.thread_count()
    assert thread_count >= 1

    threads = [_Task(API(thread_index))
               for thread_index in range(thread_count)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()

if __name__ == '__main__':
    _main()
