#!/usr/bin/python
# -- Content-Encoding: UTF-8 --
"""
Tests to verify the additional headers feature

:license: Apache License 2.0
"""

# Tests utilities
from tests.utilities import UtilityServer

# JSON-RPC library
from jsonrpclib.utils import from_bytes

# Standard library
import contextlib
import re
import sys
import unittest
import jsonrpclib

try:
    # Python 2
    from StringIO import StringIO

except ImportError:
    # Python 3
    from io import StringIO

# ------------------------------------------------------------------------------


class HeadersTests(unittest.TestCase):
    """
    These tests verify functionality of additional headers.
    """
    REQUEST_LINE = "^send: POST"

    def setUp(self):
        """
        Sets up the test
        """
        # Set up the server
        self.server = UtilityServer().start('', 0)
        self.port = self.server.get_port()

    def tearDown(self):
        """
        Post-test clean up
        """
        # Stop the server
        self.server.stop()

    @contextlib.contextmanager
    def captured_headers(self, check_duplicates=True):
        """
        Captures the request headers. Yields the {header : value} dictionary,
        where keys are in lower case.

        :param check_duplicates: If True, raises an error if a header appears
                                 twice
        """
        # Redirect the standard output, to catch jsonrpclib verbose messages
        stdout = sys.stdout
        sys.stdout = f = StringIO()
        headers = {}
        yield headers
        sys.stdout = stdout

        # Extract the sent request content
        request_lines = f.getvalue().splitlines()
        request_lines = list(filter(lambda l: l.startswith("send:"),
                                    request_lines))
        request_line = request_lines[0].split("send: ")[-1]

        # Convert it to a string
        try:
            # Use eval to convert the representation into a string
            request_line = from_bytes(eval(request_line))
        except:
            # Keep the received version
            pass

        # Extract headers
        raw_headers = request_line.splitlines()[1:-1]
        raw_headers = map(lambda h: re.split(r":\s?", h, 1), raw_headers)
        for header, value in raw_headers:
            header = header.lower()
            if check_duplicates and header in headers:
                raise KeyError("Header defined twice: {0}".format(header))
            headers[header] = value

    def test_should_extract_headers(self):
        # given
        client = jsonrpclib.ServerProxy('http://localhost:{0}'
                                        .format(self.port), verbose=1)

        # when
        with self.captured_headers() as headers:
            response = client.ping()
            self.assertTrue(response)

        # then
        self.assertTrue(len(headers) > 0)
        self.assertTrue('content-type' in headers)
        self.assertEqual(headers['content-type'], 'application/json-rpc')

    def test_should_add_additional_headers(self):
        # given
        client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), verbose=1,
            headers={'X-My-Header': 'Test'})

        # when
        with self.captured_headers() as headers:
            response = client.ping()
            self.assertTrue(response)

        # then
        self.assertTrue('x-my-header' in headers)
        self.assertEqual(headers['x-my-header'], 'Test')

    def test_should_add_additional_headers_to_notifications(self):
        # given
        client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), verbose=1,
            headers={'X-My-Header': 'Test'})

        # when
        with self.captured_headers() as headers:
            client._notify.ping()

        # then
        self.assertTrue('x-my-header' in headers)
        self.assertEqual(headers['x-my-header'], 'Test')

    def test_should_override_headers(self):
        # given
        client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), verbose=1,
            headers={'User-Agent': 'jsonrpclib test', 'Host': 'example.com'})

        # when
        with self.captured_headers(False) as headers:
            response = client.ping()
            self.assertTrue(response)

        # then
        self.assertEqual(headers['user-agent'], 'jsonrpclib test')
        self.assertEqual(headers['host'], 'example.com')

    def test_should_not_override_content_length(self):
        # given
        client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), verbose=1,
            headers={'Content-Length': 'invalid value'})

        # when
        with self.captured_headers() as headers:
            response = client.ping()
            self.assertTrue(response)

        # then
        self.assertTrue('content-length' in headers)
        self.assertNotEqual(headers['content-length'], 'invalid value')

    def test_should_convert_header_values_to_basestring(self):
        # given
        client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), verbose=1,
            headers={'X-Test': 123})

        # when
        with self.captured_headers() as headers:
            response = client.ping()
            self.assertTrue(response)

        # then
        self.assertTrue('x-test' in headers)
        self.assertEqual(headers['x-test'], '123')

    def test_should_add_custom_headers_to_methods(self):
        # given
        client = jsonrpclib.ServerProxy('http://localhost:{0}'
                                        .format(self.port), verbose=1)

        # when
        with self.captured_headers() as headers:
            with client._additional_headers({'X-Method': 'Method'}) as cl:
                response = cl.ping()

            self.assertTrue(response)

        # then
        self.assertTrue('x-method' in headers)
        self.assertEqual(headers['x-method'], 'Method')

    def test_should_override_global_headers(self):
        # given
        client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), verbose=1,
            headers={'X-Test': 'Global'})

        # when
        with self.captured_headers() as headers:
            with client._additional_headers({'X-Test': 'Method'}) as cl:
                response = cl.ping()
                self.assertTrue(response)

        # then
        self.assertTrue('x-test' in headers)
        self.assertEqual(headers['x-test'], 'Method')

    def test_should_restore_global_headers(self):
        # given
        client = jsonrpclib.ServerProxy(
            'http://localhost:{0}'.format(self.port), verbose=1,
            headers={'X-Test': 'Global'})

        # when
        with self.captured_headers() as headers:
            with client._additional_headers({'X-Test': 'Method'}) as cl:
                response = cl.ping()
                self.assertTrue(response)

        self.assertTrue('x-test' in headers)
        self.assertEqual(headers['x-test'], 'Method')

        with self.captured_headers() as headers:
            response = cl.ping()
            self.assertTrue(response)

        # then
        self.assertTrue('x-test' in headers)
        self.assertEqual(headers['x-test'], 'Global')

    def test_should_allow_to_nest_additional_header_blocks(self):
        # given
        client = jsonrpclib.ServerProxy('http://localhost:{0}'
                                        .format(self.port), verbose=1)

        # when
        with client._additional_headers({'X-Level-1': '1'}) as cl_level1:
            with self.captured_headers() as headers1:
                response = cl_level1.ping()
                self.assertTrue(response)

            with cl_level1._additional_headers({'X-Level-2': '2'}) as cl:
                with self.captured_headers() as headers2:
                    response = cl.ping()
                    self.assertTrue(response)

        # then
        self.assertTrue('x-level-1' in headers1)
        self.assertEqual(headers1['x-level-1'], '1')

        self.assertTrue('x-level-1' in headers2)
        self.assertEqual(headers1['x-level-1'], '1')
        self.assertTrue('x-level-2' in headers2)
        self.assertEqual(headers2['x-level-2'], '2')
