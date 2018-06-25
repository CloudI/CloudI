#!/usr/bin/python
# -- Content-Encoding: UTF-8 --
"""
Tests the History class

:license: Apache License 2.0
"""

# JSON-RPC library
from jsonrpclib.history import History

# Standard library
try:
    import unittest2 as unittest
except ImportError:
    import unittest


# ------------------------------------------------------------------------------


class HistoryTests(unittest.TestCase):
    """
    Tests the methods of the History class
    """

    def test_basic(self):
        """
        Tests basic access to history
        """
        history = History()

        # Empty history
        self.assertIsNone(history.request)
        self.assertIsNone(history.response)
        self.assertListEqual(history.requests, [])
        self.assertListEqual(history.responses, [])

        # First value
        req1 = object()
        res1 = object()
        history.add_request(req1)
        history.add_response(res1)

        self.assertListEqual(history.requests, [req1])
        self.assertListEqual(history.responses, [res1])
        self.assertIs(history.request, req1)
        self.assertIs(history.response, res1)

        # Second value
        req2 = object()
        res2 = object()
        history.add_request(req2)
        history.add_response(res2)

        self.assertListEqual(history.requests, [req1, req2])
        self.assertListEqual(history.responses, [res1, res2])
        self.assertIs(history.request, req2)
        self.assertIs(history.response, res2)

    def test_clear(self):
        """
        Ensures that the clear() method doesn't create new history lists
        """
        # Fill some history
        history = History()
        history.add_request(1)
        history.add_response(1)

        # Keep track of lists
        original_requests = history.requests
        original_responses = history.responses

        # Clear
        history.clear()

        # Check aftermath
        self.assertListEqual(history.requests, [])
        self.assertListEqual(history.responses, [])
        self.assertIs(history.requests, original_requests)
        self.assertIs(history.responses, original_responses)
        self.assertIsNone(history.request)
        self.assertIsNone(history.response)
