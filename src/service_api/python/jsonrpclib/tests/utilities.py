#!/usr/bin/python
# -- Content-Encoding: UTF-8 --
"""
Tests utility classes

:license: Apache License 2.0
"""

# JSON-RPC library
from jsonrpclib.SimpleJSONRPCServer import SimpleJSONRPCServer

# Standard library
import threading

# ------------------------------------------------------------------------------
# Test methods


def subtract(minuend, subtrahend):
    """
    Using the keywords from the JSON-RPC v2 doc
    """
    return minuend - subtrahend


def add(x, y):
    return x + y


def update(*args):
    return args


def summation(*args):
    return sum(args)


def notify_hello(*args):
    return args


def get_data():
    return ['hello', 5]


def ping():
    return True

# ------------------------------------------------------------------------------
# Server utility class


class UtilityServer(object):
    """
    Utility start/stop server
    """
    def __init__(self):
        """
        Sets up members
        """
        self._server = None
        self._thread = None

    def start(self, addr, port):
        """
        Starts the server

        :param addr: A binding address
        :param port: A listening port
        :return: This object (for in-line calls)
        """
        # Create the server
        self._server = server = SimpleJSONRPCServer((addr, port),
                                                    logRequests=False)

        # Register test methods
        server.register_function(summation, 'sum')
        server.register_function(summation, 'notify_sum')
        server.register_function(notify_hello)
        server.register_function(subtract)
        server.register_function(update)
        server.register_function(get_data)
        server.register_function(add)
        server.register_function(ping)
        server.register_function(summation, 'namespace.sum')

        # Serve in a thread
        self._thread = threading.Thread(target=server.serve_forever)
        self._thread.daemon = True
        self._thread.start()

        # Allow an in-line instantiation
        return self

    def get_port(self):
        """
        Retrieves the port this server is listening to
        """
        return self._server.socket.getsockname()[1]

    def stop(self):
        """
        Stops the server and waits for its thread to finish
        """
        self._server.shutdown()
        self._server.server_close()
        self._thread.join()

        self._server = None
        self._thread = None
