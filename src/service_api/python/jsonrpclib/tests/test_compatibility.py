#!/usr/bin/python
# -- Content-Encoding: UTF-8 --
"""
Tests JSON-RPC compatibility

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


class TestCompatibility(unittest.TestCase):
    """
    Tests JSON-RPC compatibility
    """
    def setUp(self):
        """
        Pre-test set up
        """
        # Set up the server
        self.server = UtilityServer().start('', 0)
        self.port = self.server.get_port()

        # Set up the client
        self.history = jsonrpclib.history.History()
        self.client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), history=self.history)

    def tearDown(self):
        """
        Post-test clean up
        """
        # Close the client
        self.client("close")()

        # Stop the server
        self.server.stop()

    # Version 2.0 Tests
    def test_positional(self):
        """ Positional arguments in a single call """
        result = self.client.subtract(23, 42)
        self.assertTrue(result == -19)
        result = self.client.subtract(42, 23)
        self.assertTrue(result == 19)
        request = json.loads(self.history.request)
        response = json.loads(self.history.response)
        verify_request = {
            "jsonrpc": "2.0", "method": "subtract",
            "params": [42, 23], "id": request['id']
        }
        verify_response = {
            "jsonrpc": "2.0", "result": 19, "id": request['id']
        }
        self.assertTrue(request == verify_request)
        self.assertTrue(response == verify_response)

    def test_named(self):
        """ Named arguments in a single call """
        result = self.client.subtract(subtrahend=23, minuend=42)
        self.assertTrue(result == 19)
        result = self.client.subtract(minuend=42, subtrahend=23)
        self.assertTrue(result == 19)
        request = json.loads(self.history.request)
        response = json.loads(self.history.response)
        verify_request = {
            "jsonrpc": "2.0", "method": "subtract",
            "params": {"subtrahend": 23, "minuend": 42},
            "id": request['id']
        }
        verify_response = {
            "jsonrpc": "2.0", "result": 19, "id": request['id']
        }
        self.assertTrue(request == verify_request)
        self.assertTrue(response == verify_response)

    def test_notification(self):
        """ Testing a notification (response should be null) """
        result = self.client._notify.update(1, 2, 3, 4, 5)
        self.assertTrue(result is None)
        request = json.loads(self.history.request)
        response = self.history.response
        verify_request = {
            "jsonrpc": "2.0", "method": "update", "params": [1, 2, 3, 4, 5]
        }
        verify_response = ''
        self.assertTrue(request == verify_request)
        self.assertTrue(response == verify_response)

    def test_non_existent_method(self):
        self.assertRaises(jsonrpclib.ProtocolError, self.client.foobar)
        request = json.loads(self.history.request)
        response = json.loads(self.history.response)
        verify_request = {
            "jsonrpc": "2.0", "method": "foobar", "id": request['id']
        }
        verify_response = {
            "jsonrpc": "2.0",
            "error":
                {"code": -32601, "message": response['error']['message']},
            "id": request['id']
        }
        self.assertTrue(request == verify_request)
        self.assertTrue(response == verify_response)

    def test_special_method(self):
        self.assertRaises(AttributeError, getattr, self.client, '__special_method__')
        self.assertIsNone(self.history.request)

    def test_invalid_json(self):
        invalid_json = '{"jsonrpc": "2.0", "method": "foobar, ' + \
            '"params": "bar", "baz]'
        self.client._run_request(invalid_json)
        response = json.loads(self.history.response)
        verify_response = json.loads(
            '{"jsonrpc": "2.0", "error": {"code": -32700,' +
            ' "message": "Parse error."}, "id": null}'
        )
        verify_response['error']['message'] = response['error']['message']
        self.assertTrue(response == verify_response)

    def test_invalid_request(self):
        invalid_request = '{"jsonrpc": "2.0", "method": 1, "params": "bar"}'
        self.client._run_request(invalid_request)
        response = json.loads(self.history.response)
        verify_response = json.loads(
            '{"jsonrpc": "2.0", "error": {"code": -32600, ' +
            '"message": "Invalid Request."}, "id": null}'
        )
        verify_response['error']['message'] = response['error']['message']
        self.assertTrue(response == verify_response)

    def test_batch_invalid_json(self):
        invalid_request = '[ {"jsonrpc": "2.0", "method": "sum", ' + \
            '"params": [1,2,4], "id": "1"},{"jsonrpc": "2.0", "method" ]'
        self.client._run_request(invalid_request)
        response = json.loads(self.history.response)
        verify_response = json.loads(
            '{"jsonrpc": "2.0", "error": {"code": -32700,' +
            '"message": "Parse error."}, "id": null}'
        )
        verify_response['error']['message'] = response['error']['message']
        self.assertTrue(response == verify_response)

    def test_empty_array(self):
        invalid_request = '[]'
        self.client._run_request(invalid_request)
        response = json.loads(self.history.response)
        verify_response = json.loads(
            '{"jsonrpc": "2.0", "error": {"code": -32600, ' +
            '"message": "Invalid Request."}, "id": null}'
        )
        verify_response['error']['message'] = response['error']['message']
        self.assertTrue(response == verify_response)

    def test_nonempty_array(self):
        invalid_request = '[1,2]'
        request_obj = json.loads(invalid_request)
        self.client._run_request(invalid_request)
        response = json.loads(self.history.response)
        self.assertTrue(len(response) == len(request_obj))
        for resp in response:
            verify_resp = json.loads(
                '{"jsonrpc": "2.0", "error": {"code": -32600, ' +
                '"message": "Invalid Request."}, "id": null}'
            )
            verify_resp['error']['message'] = resp['error']['message']
            self.assertTrue(resp == verify_resp)

    def test_batch(self):
        multicall = jsonrpclib.MultiCall(self.client)
        multicall.sum(1, 2, 4)
        multicall._notify.notify_hello(7)
        multicall.subtract(42, 23)
        multicall.foo.get(name='myself')
        multicall.get_data()
        job_requests = [j.request() for j in multicall._job_list]
        job_requests.insert(3, '{"foo": "boo"}')
        json_requests = '[%s]' % ','.join(job_requests)
        requests = json.loads(json_requests)
        responses = self.client._run_request(json_requests)

        verify_requests = json.loads("""[
            {"jsonrpc": "2.0", "method": "sum", "params": [1,2,4], "id": "1"},
            {"jsonrpc": "2.0", "method": "notify_hello", "params": [7]},
            {"jsonrpc": "2.0", "method": "subtract",
             "params": [42,23], "id": "2"},
            {"foo": "boo"},
            {"jsonrpc": "2.0", "method": "foo.get",
             "params": {"name": "myself"}, "id": "5"},
            {"jsonrpc": "2.0", "method": "get_data", "id": "9"}
        ]""")

        # Thankfully, these are in order so testing is pretty simple.
        verify_responses = json.loads("""[
            {"jsonrpc": "2.0", "result": 7, "id": "1"},
            {"jsonrpc": "2.0", "result": 19, "id": "2"},
            {"jsonrpc": "2.0",
             "error": {"code": -32600, "message": "Invalid Request."},
             "id": null},
            {"jsonrpc": "2.0",
             "error": {"code": -32601, "message": "Method not found."},
             "id": "5"},
            {"jsonrpc": "2.0", "result": ["hello", 5], "id": "9"}
        ]""")

        self.assertTrue(len(requests) == len(verify_requests))
        self.assertTrue(len(responses) == len(verify_responses))

        responses_by_id = {}
        response_i = 0

        for i in range(len(requests)):
            verify_request = verify_requests[i]
            request = requests[i]
            response = None
            if request.get('method') != 'notify_hello':
                req_id = request.get('id')
                if 'id' in verify_request:
                    verify_request['id'] = req_id
                verify_response = verify_responses[response_i]
                verify_response['id'] = req_id
                responses_by_id[req_id] = verify_response
                response_i += 1
                response = verify_response
            self.assertTrue(request == verify_request)

        for response in responses:
            verify_response = responses_by_id.get(response.get('id'))
            if 'error' in verify_response:
                verify_response['error']['message'] = \
                    response['error']['message']
            self.assertTrue(response == verify_response)

    def test_batch_notifications(self):
        multicall = jsonrpclib.MultiCall(self.client)
        multicall._notify.notify_sum(1, 2, 4)
        multicall._notify.notify_hello(7)
        result = multicall()
        self.assertTrue(len(result) == 0)
        valid_request = json.loads(
            '[{"jsonrpc": "2.0", "method": "notify_sum", ' +
            '"params": [1,2,4]},{"jsonrpc": "2.0", ' +
            '"method": "notify_hello", "params": [7]}]'
        )
        request = json.loads(self.history.request)
        self.assertTrue(len(request) == len(valid_request))
        for i in range(len(request)):
            req = request[i]
            valid_req = valid_request[i]
            self.assertTrue(req == valid_req)
        self.assertTrue(self.history.response == '')
