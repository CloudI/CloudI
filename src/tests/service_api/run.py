#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2016, Michael Truog <mjtruog at gmail dot com>
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
               .split(os.path.sep)[:-2] + ['service_api', 'python']
    )
)

import time, urllib2

if __name__ == '__main__':
    from cloudi_service_api import CloudI
    obj = CloudI()
    assert(obj.nodes_add('[foobar1@hostX, foobar2@hostY]') == 'ok')
    assert(obj.nodes_remove('[foobar1@hostX, foobar2@hostY]') == 'ok')
    assert(obj.nodes_remove(
        '[foobar1@hostX, foobar2@hostY]'
    ) == '{error,{node_not_found,foobar1@hostX}}')

    assert(obj.acl_remove('[all]') == 'ok')
    assert(obj.acl_add('[{all, [database, tests]}]') == 'ok')

    # start extra instances of the http_req test
    services_added = obj.services_add("""\
[[{prefix, "/json_rpc/1/"},
  {module, cloudi_service_http_req}],
 [{prefix, "/json_rpc/2/"},
  {module, cloudi_service_http_req}],
 [{prefix, "/json_rpc/3/"},
  {module, cloudi_service_http_req}],
 [{prefix, "/json_rpc/4/"},
  {module, cloudi_service_http_req}]]
""")
    assert(type(services_added) == list) # returns the list of new ServiceIds
    assert(len(services_added) == 4)

    url = 'http://localhost:6464'
    assert('<http_test><value>5</value></http_test>' ==
           urllib2.urlopen(url + '/json_rpc/1/erlang.xml?value=5').read())
    assert('<http_test><value>6</value></http_test>' ==
           urllib2.urlopen(url + '/json_rpc/2/erlang.xml?value=6').read())
    assert('<http_test><value>7</value></http_test>' ==
           urllib2.urlopen(url + '/json_rpc/3/erlang.xml?value=7').read())
    assert('<http_test><value>8</value></http_test>' ==
           urllib2.urlopen(url + '/json_rpc/4/erlang.xml?value=8').read())

    # remove the extra instances of the http_req test
    assert(obj.services_remove('["%s", "%s", "%s", "%s"]' % (
        services_added[0], services_added[1],
        services_added[2], services_added[3],
    )) == 'ok')

