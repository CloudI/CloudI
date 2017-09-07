#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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
  {module, cloudi_service_test_http_req}],
 [{prefix, "/json_rpc/2/"},
  {module, cloudi_service_test_http_req}],
 [{prefix, "/json_rpc/3/"},
  {module, cloudi_service_test_http_req}],
 [{prefix, "/json_rpc/4/"},
  {module, cloudi_service_test_http_req}]]
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

