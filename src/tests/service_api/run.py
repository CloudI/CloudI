#!/usr/bin/env python
# -*- coding: utf-8; Mode: python; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# BSD LICENSE
# 
# Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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

import time

if __name__ == '__main__':
    from cloudi_service_api import CloudI
    obj = CloudI()
    assert obj.nodes_add('[foobar1@hostX, foobar2@hostY]') == 'ok'
    assert obj.nodes_remove('[foobar1@hostX, foobar2@hostY]') == 'ok'
    # removing entries that do not exist, does not fail,
    # since the request is valid (despite the fact it is pointless)
    assert obj.nodes_remove('[foobar1@hostX, foobar2@hostY]') == 'ok'

    assert obj.acl_remove('[all]') == 'ok'
    assert obj.acl_add('[{all, [database, tests]}]') == 'ok'

    # remove the hexpi services
    services = obj.services()
    assert obj.services_remove('[%s, %s]' % (
        str(services[1][0]), str(services[4][0]),
    )) == 'ok'

    # start the C flood test
    assert obj.services_add("""\
[{external,
    "/tests/flood/",
    "tests/flood/service/flood", "1 tcp 16384",
    [{"LD_LIBRARY_PATH", "api/c/lib/"},
    {"DYLD_LIBRARY_PATH", "api/c/lib/"}],
    none, tcp, 16384,
    5000, 5000, 5000, [api], undefined, 1, 1, 5, 300, []},
 {internal,
     "/tests/flood/",
     cloudi_service_flood,
     [{flood, "/tests/flood/c", <<"DATA">>, 1000}],
     lazy_closest,
     5000, 5000, 5000, [api], undefined, 2, 5, 300, []}]""") == 'ok'

    print 'waiting 20 seconds...'
    time.sleep(20)

    # stop the C flood test
    services = obj.services()
    assert obj.services_remove('[%s, %s]' % (
        str(services[14][0]), str(services[15][0]),
    )) == 'ok'

