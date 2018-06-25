#!/usr/bin/python
# -- Content-Encoding: UTF-8 --
"""
Tests the Config class

:license: Apache License 2.0
"""

# JSON-RPC library
from jsonrpclib.config import Config

# Standard library
try:
    import unittest2 as unittest
except ImportError:
    import unittest

# ------------------------------------------------------------------------------


class ConfigTests(unittest.TestCase):
    """
    Tests the methods of the Config class
    """
    def compare_config(self, config1, config2):
        """
        Compares the given configuration objects
        """
        self.assertIsNot(config1, config2)
        for member in ('version', 'use_jsonclass', 'content_type',
                       'user_agent', 'ignore_attribute'):
            self.assertEqual(getattr(config1, member),
                             getattr(config2, member))

        # Local classes
        self.assertIsNot(config1.classes, config2.classes)
        self.assertDictEqual(config1.classes, config2.classes)

        # Custom type serializers
        self.assertIsNot(config1.serialize_handlers,
                         config2.serialize_handlers)
        self.assertDictEqual(config1.serialize_handlers,
                             config2.serialize_handlers)

    def test_copy(self):
        """
        Tests the configuration copy
        """
        # Basic configuration
        config1 = Config()
        self.compare_config(config1, config1.copy())

        # Custom values
        config1.version = 1.0
        config1.content_type = "text/plain"
        config1.user_agent = "test_agent"
        config1.serialize_method = "_new_method"
        config1.ignore_attribute = "_new_method"
        self.compare_config(config1, config1.copy())

        # Handlers
        def handler(obj, serialize_method, ignore_attribute, ignore, config):
            pass

        config1.serialize_handlers[int] = handler
        self.compare_config(config1, config1.copy())

        # Local classes
        class A:
            pass
        class B:
            pass

        config1.classes.add(A)
        config1.classes.add(B, "like_B")

        self.assertIs(config1.classes["A"], A)
        self.assertIs(config1.classes["like_B"], B)
        self.assertNotIn("B", config1.classes)
        self.compare_config(config1, config1.copy())
