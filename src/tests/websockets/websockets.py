#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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

import threading, time, traceback
from cloudi_c import API

class Task(threading.Thread):
    def __init__(self, api):
        threading.Thread.__init__(self)
        self.__api = api

    def run(self):
        try:
            self.__api.subscribe('bounce/get', self.__request)
            self.__api.subscribe('bounce/delay', self.__delay)
            self.__api.subscribe('bounce/websocket/connect',
                                 self.__connect)
            self.__api.subscribe('bounce/websocket/disconnect',
                                 self.__disconnect)

            result = self.__api.poll()
            print >> sys.stderr, 'exited thread:', result
        except:
            traceback.print_exc(file=sys.stdout)

    def __connect(self, command, name, pattern, request_info, request,
                  timeout, priority, trans_id, pid):
        assert request == 'CONNECT'
        print "connect:", self.__api.info_key_value_parse(request_info)
        return ''

    def __disconnect(self, command, name, pattern, request_info, request,
                     timeout, priority, trans_id, pid):
        assert request == 'DISCONNECT'
        print "disconnect:", self.__api.info_key_value_parse(request_info)
        return ''

    def __request(self, command, name, pattern, request_info, request,
                  timeout, priority, trans_id, pid):
        # send the request to self
        self.__api.send_async(self.__api.prefix() + 'bounce/delay',
                              request)
        return request

    def __delay(self, command, name, pattern, request_info, request,
                timeout, priority, trans_id, pid):
        time.sleep(1.0)
        assert name[-6:] == '/delay'
        trans_ids = self.__api.mcast_async(name[:-6] + '/websocket',
                                           'notification: got "' +
                                           request + '" 1 second ago')
        if len(trans_ids) == 0:
            print 'websockets: (no websockets connected?)'
        else:
            for check in trans_ids:
                (tmp, response, tmp) = self.__api.recv_async(trans_id=check)
                print 'websockets:', response

if __name__ == '__main__':
    thread_count = API.thread_count()
    assert thread_count >= 1
    
    threads = [Task(API(i)) for i in range(thread_count)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()

