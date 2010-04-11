import jsonrpclib

class cloud_api(object):
    def __init__(self, host, port):
        address = 'http://%s:%d/jsonrpc/%s' % (
            host, port, self.__class__.__name__,
        )
        self.__server = jsonrpclib.Server(address)

    def __getattr__(self, name):
        return self.__server.__getattr__(name)

