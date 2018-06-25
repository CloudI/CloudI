#!/usr/bin/python
# -- Content-Encoding: UTF-8 --
"""
Tests that verify that the client and server parts of jsonrpclib
talk to each other properly.

:license: Apache License 2.0
"""

# Tests utilities
from tests.utilities import UtilityServer

# JSON-RPC library
import jsonrpclib

# Standard library
import json
import unittest

# ------------------------------------------------------------------------------


class InternalTests(unittest.TestCase):
    """
    These tests verify that the client and server portions of
    jsonrpclib talk to each other properly.
    """
    def setUp(self):
        # Set up the server
        self.server = UtilityServer().start('', 0)
        self.port = self.server.get_port()

        # Prepare the client
        self.history = jsonrpclib.history.History()

    def tearDown(self):
        """
        Post-test clean up
        """
        # Stop the server
        self.server.stop()

    def get_client(self):
        return jsonrpclib.ServerProxy('http://localhost:{0}'.format(self.port),
                                      history=self.history)

    def get_multicall_client(self):
        server = self.get_client()
        return jsonrpclib.MultiCall(server)

    def test_connect(self):
        client = self.get_client()
        result = client.ping()
        self.assertTrue(result)

    def test_single_args(self):
        client = self.get_client()
        result = client.add(5, 10)
        self.assertTrue(result == 15)

    def test_single_kwargs(self):
        client = self.get_client()
        result = client.add(x=5, y=10)
        self.assertTrue(result == 15)

    def test_single_kwargs_and_args(self):
        client = self.get_client()
        self.assertRaises(jsonrpclib.ProtocolError,
                          client.add, (5,), {'y': 10})

    def test_single_notify(self):
        client = self.get_client()
        result = client._notify.add(5, 10)
        self.assertTrue(result is None)

    def test_single_namespace(self):
        client = self.get_client()
        client.namespace.sum(1, 2, 4)
        request = json.loads(self.history.request)
        response = json.loads(self.history.response)
        verify_request = {
            "jsonrpc": "2.0", "params": [1, 2, 4],
            "id": "5", "method": "namespace.sum"
        }
        verify_response = {"jsonrpc": "2.0", "result": 7,
                           "id": request['id']}
        verify_request['id'] = request['id']
        self.assertTrue(verify_request == request)
        self.assertTrue(verify_response == response)

    def test_multicall_success(self):
        multicall = self.get_multicall_client()
        multicall.ping()
        multicall.add(5, 10)
        multicall.namespace.sum(5, 10, 15)
        correct = [True, 15, 30]

        for i, result in enumerate(multicall()):
            self.assertTrue(result == correct[i])

    def test_multicall_success_2(self):
        multicall = self.get_multicall_client()
        for i in range(3):
            multicall.add(5, i)
        result = multicall()
        self.assertTrue(result[2] == 7)

    def test_multicall_failure(self):
        multicall = self.get_multicall_client()
        multicall.ping()
        multicall.add(x=5, y=10, z=10)
        raises = [None, jsonrpclib.ProtocolError]
        result = multicall()
        for i in range(2):
            if not raises[i]:
                result[i]
            else:
                def func():
                    return result[i]
                self.assertRaises(raises[i], func)
