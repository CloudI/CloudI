# -*- coding: utf-8; Mode: python; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

import sys, os

sys.path.append(
    os.path.sep.join(
        os.path.dirname(os.path.abspath(__file__))
               .split(os.path.sep) + ['jsonrpclib']
    )
)

import jsonrpclib

class CloudI(object):

    # initialize with configuration file defaults
    def __init__(self, host = 'localhost', port = 8081):
        address = 'http://%s:%d/cloudi/api/json_rpc/' % (host, port)
        self.__server = jsonrpclib.Server(address)

    def __getattr__(self, name):
        return self.__server.__getattr__(name)

