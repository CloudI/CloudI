#!/usr/bin/python
# -- Content-Encoding: UTF-8 --
"""
Tests the jsonclass module, for bean (de)serialization

TODO: test custom serialization

:license: Apache License 2.0
"""

# JSON-RPC library
from jsonrpclib.jsonclass import dump, load
import jsonrpclib.config

# Standard library
import datetime
import sys
try:
    import unittest2 as unittest
except ImportError:
    import unittest

try:
    import enum


    class Color(enum.Enum):
        BLUE = 1
        RED = 2
except ImportError:
    enum = None

# ------------------------------------------------------------------------------


class Bean(object):
    """
    Sample bean
    """
    def __init__(self):
        """
        Sets up members
        """
        self.public = 42
        self._protected = [1, 2, 3]
        self.__private = True

    def __eq__(self, other):
        """
        Checks equality
        """
        return self.public == other.public \
            and self._protected == other._protected \
            and self.__private == other.__private


class InheritanceBean(Bean):
    """
    Child bean
    """
    def __init__(self):
        """
        Sets up members
        """
        Bean.__init__(self)
        self.first = True
        self._second = [1, 2, 3]
        self.__third = {1: "string", "string": 1}

    def __eq__(self, other):
        """
        Checks equality
        """
        return Bean.__eq__(self, other) and self.first == other.first \
            and self._second == other._second \
            and self.__third == other.__third


class SlotBean(object):
    """
    Sample bean using slots: private fields are not usable (yet)
    """
    __slots__ = ('public', '_protected')

    def __init__(self):
        """
        Sets up members
        """
        self.public = 42
        self._protected = [1, 2, 3]

    def __eq__(self, other):
        """
        Checks equality
        """
        return self.public == other.public \
            and self._protected == other._protected


class InheritanceSlotBean(SlotBean):
    """
    Child bean using slots
    """
    __slots__ = ('first', '_second')

    def __init__(self):
        """
        Sets up members
        """
        SlotBean.__init__(self)
        self.first = True
        self._second = [1, 2, 3]

    def __eq__(self, other):
        """
        Checks equality
        """
        return SlotBean.__eq__(self, other) \
            and self.first == other.first \
            and self._second == other._second


class SecondInheritanceSlotBean(InheritanceSlotBean):
    """
    Grand-child bean using slots
    """
    __slots__ = ('third', '_fourth')

    def __init__(self):
        """
        Sets up members
        """
        InheritanceSlotBean.__init__(self)
        self.third = False
        self._fourth = [4, 5, 6]

    def __eq__(self, other):
        """
        Checks equality
        """
        return InheritanceSlotBean.__eq__(self, other) \
            and self.third == other.third \
            and self._fourth == other._fourth

# ------------------------------------------------------------------------------


class SerializationTests(unittest.TestCase):
    """
    Checks the behavior of jsonclass
    """
    def setUp(self):
        """
        Tests initialization
        """
        # Compatibility issue between Python 2 & 3
        if sys.version_info[0] < 3:
            self.assertCountEqual = self.assertItemsEqual

    def test_primitive(self):
        """
        Tests dump & load of primitive types
        """
        for value in (42, 42.12, "string", True, False, None):
            # Dump..
            serialized = dump(value)
            # Reload...
            deserialized = load(serialized)

            self.assertIs(type(serialized), type(value),
                          "Type changed during serialization")
            self.assertIs(type(deserialized), type(value),
                          "Type changed during deserialization")

            self.assertEqual(serialized, value,
                             "Value changed during serialization")
            self.assertEqual(deserialized, value,
                             "Value changed during deserialization")

    def test_iterable(self):
        """
        Tests dump & load of iterable types
        """
        tuple_values = (42, 42.12, "string", True, False, None)
        list_values = list(tuple_values)
        set_values = set(tuple_values)
        frozen_values = frozenset(tuple_values)

        for iterable in (tuple_values, list_values, set_values, frozen_values):
            # Dump...
            serialized = dump(iterable)
            # Reload...
            deserialized = load(serialized)

            self.assertIs(type(serialized), list,
                          "Dumped iterable should be a list")
            self.assertIs(type(deserialized), list,
                          "Loaded iterable should be a list")

            # Check content
            self.assertCountEqual(deserialized, tuple_values,
                                  "Values order changed")

    def test_dictionary(self):
        """
        Tests dump & load of dictionaries
        """
        dictionary = {'int': 42,
                      'float': 42.2,
                      None: "string",
                      True: False,
                      42.1: None,
                      'dict': {"sub": 1},
                      "list": [1, 2, 3]}

        # Dump it
        serialized = dump(dictionary)
        # Reload it
        deserialized = load(serialized)

        self.assertDictEqual(deserialized, dictionary)

    def test_object(self):
        """
        Tests dump & load of a custom type
        """
        types = {Bean: ('public', '_protected', '_Bean__private'),
                 InheritanceBean: ('public', '_protected', 'first', '_second'),
                 SlotBean: ('public', '_protected'),
                 InheritanceSlotBean: ('public', '_protected',
                                       'first', '_second'),
                 SecondInheritanceSlotBean: ('public', '_protected',
                                             'first', '_second',
                                             'third', '_fourth'), }

        for clazz, fields in types.items():
            # Prepare the bean
            data = clazz()

            # Dump it...
            serialized = dump(data)

            # Check serialized content
            self.assertIn('__jsonclass__', serialized)
            for field in fields:
                self.assertIn(field, serialized)

            # Check class name
            self.assertEqual(serialized['__jsonclass__'][0],
                             '{0}.{1}'.format(clazz.__module__,
                                              clazz.__name__))

            # Reload it
            deserialized = load(serialized)

            # Dictionary is left as-is
            self.assertIn('__jsonclass__', serialized,
                          "Serialized dictionary has been modified")
            self.assertFalse(hasattr(deserialized, '__jsonclass__'),
                             "The deserialized bean shouldn't have a "
                             "__jsonclass__ attribute")

            # Check deserialized value
            self.assertIs(type(deserialized), type(data))
            self.assertEqual(deserialized, data,
                             "Source and deserialized bean are not equal")

    def test_config_custom(self):
        """
        Tests configured custom serializer
        """
        # Get the current time object
        now = datetime.datetime.now()

        # Check if it is correctly serialized
        std_serialized = dump(now)
        self.assertEqual(std_serialized['__jsonclass__'][0],
                         'datetime.datetime')

        # Configure a custom serializer
        def datetime_serializer(obj, serialize_method, ignore_attribute,
                                ignore, config):
            """
            Custom datetime serializer (returns an ISO date string)
            """
            self.assertIs(type(obj), datetime.datetime)
            return obj.isoformat()

        handlers = {datetime.datetime: datetime_serializer}
        config = jsonrpclib.config.Config(serialize_handlers=handlers)

        # Dump with out configuration
        custom_serialized = dump(now, config=config)

        # This should be a raw string
        self.assertEqual(custom_serialized, now.isoformat())

    def test_enum(self):
        """
        Tests the serialization of enumerations
        """
        if enum is None:
            self.skipTest("enum package not available.")

        for data in (Color.BLUE, Color.RED):
            # Serialization
            enum_serialized = dump(data)
            self.assertIn(
                Color.__name__, enum_serialized['__jsonclass__'][0])
            self.assertEqual(
                data.value, enum_serialized['__jsonclass__'][1][0])

            # Loading
            result = load(enum_serialized)
            self.assertEqual(data, result)

        # Embedded
        data = [Color.BLUE, Color.RED]
        serialized = dump(data)
        result = load(serialized)
        self.assertListEqual(data, result)
