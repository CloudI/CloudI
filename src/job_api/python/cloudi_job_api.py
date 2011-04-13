import sys, os

sys.path.append(
    os.path.sep.join(
        os.path.dirname(os.path.abspath(__file__))
               .split(os.path.sep)[:-2] + ["external", "jsonrpclib"]
    )
)

import jsonrpclib

class cloudi_job_api(object):

    # initialize with configuration file defaults
    def __init__(self, host = 'localhost', port = 8081):
        address = 'http://%s:%d/cloudi/api/json_rpc/' % (host, port)
        self.__server = jsonrpclib.Server(address)

    def __getattr__(self, name):
        return self.__server.__getattr__(name)

