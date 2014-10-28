#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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

import threading, types, traceback
from cloudi import API, terminate_exception

class Task(threading.Thread):
    def __init__(self, api, name, exception):
        threading.Thread.__init__(self)
        self.__api = api
        self.__name = name
        self.__terminate_exception = exception

    def run(self):
        try:
            assert self.__api.subscribe_count(self.__name + '.xml/get') == 0
            self.__api.subscribe(self.__name + '.xml/get', self.request)
            assert self.__api.subscribe_count(self.__name + '.xml/get') == 1

            result = self.__api.poll()
            assert result == False
        except self.__terminate_exception:
            pass
        except:
            traceback.print_exc(file=sys.stderr)
        print('terminate http_req %s' % self.__name)

    def request(self, command, name, pattern, request_info, request,
                timeout, priority, trans_id, pid):
        http_qs = self.__api.request_http_qs_parse(request)
        value = http_qs.get(b'value', None)
        if value is None:
            response = """\
<http_test><error>no value specified</error></http_test>"""
        else:
            if type(value) == list:
                value = value[0]
            response = """\
<http_test><value>%d</value></http_test>""" % (int(value),)
        self.__api.return_(command, name, pattern,
                           b'', response.encode('utf-8'),
                           timeout, trans_id, pid)

if __name__ == '__main__':
    thread_count = API.thread_count()
    assert thread_count >= 1
    
    threads = [Task(API(i), 'python', terminate_exception)
               for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

