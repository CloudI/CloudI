#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
"""
CloudI Service API <https://cloudi.org/api.html#2_Intro>.
"""

# pylint: disable=wrong-import-position
import sys
import os
_FILE_DIRECTORY = os.path.dirname(os.path.abspath(__file__)).split(os.path.sep)
sys.path.extend([
    os.path.sep.join(_FILE_DIRECTORY + ['jsonrpclib']),
    os.path.sep.join(_FILE_DIRECTORY[:-2] + ['api', 'python']),
])
import jsonrpclib
import erlang

class _ServiceDescription(object):
    # pylint: disable=too-few-public-methods
    def __init__(self, *args):
        self.__args = args

    def __str__(self):
        return str(self.__args)

class CloudI(object):
    """
    CloudI Service API object (communicating with JSON-RPC)
    """
    # pylint: disable=too-few-public-methods

    # initialize with configuration file defaults
    def __init__(self, host='localhost', port=6464):
        address = 'http://%s:%d/cloudi/api/rpc.json' % (host, port)
        self.__server = jsonrpclib.Server(address)

    def __getattr__(self, name):
        if name == 'services':
            return self.__services
        elif name == 'services_add':
            return self.__services_add
        elif name == 'services_search':
            return self.__services_search
        return self.__server.__getattr__(name)

    def __services(self):
        raw = self.__server.services()
        return [
            (uuid_string,
             _ServiceDescription(*service_configuration))
            for uuid_string, service_configuration in erlang.consult(raw)
        ]

    def __services_add(self, id_list):
        raw = self.__server.services_add(id_list)
        return [
            uuid_string
            for uuid_string in erlang.consult(raw)
        ]

    def __services_search(self, id_list):
        raw = self.__server.services_search(id_list)
        return [
            (uuid_string,
             _ServiceDescription(*service_configuration))
            for uuid_string, service_configuration in erlang.consult(raw)
        ]

