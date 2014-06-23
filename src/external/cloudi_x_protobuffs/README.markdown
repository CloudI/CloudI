# README

[![Build Status](https://secure.travis-ci.org/basho/erlang_protobuffs.png?branch=master)](http://travis-ci.org/basho/erlang_protobuffs)

This module is a composite of other open source modules and original code to
make interfacing with the Protocol Buffers protocol easy.

## Encode / Decode

Encoding is simple.

    1> protobuffs:encode(1, 1, uint32).
    ["\b",[1]]
    2> erlang:iolist_to_binary([
        protobuffs:encode(1, <<"Nick">>, string),
        protobuffs:encode(2, 25, uint32)
    ]).
    <<10,4,78,105,99,107,16,25>>

Decoding is simple too.

    1> protobuffs:decode(<<8, 1>>, uint32).
    {{1, 1}, <<>>}
    2> protobuffs:decode(<<10,4,78,105,99,107,16,25>>, bytes).
    {{1, <<"Nick">>}, <<16,25>>}
    3> protobuffs:decode(<<16,25>>, bytes).
    {{2, 25}, <<>>}

## Using .proto Files

The main objective of this module is to allow developers to use .proto files
easily. This module provides very basic functionality to do so.

Consider the `test/erlang_protobuffs_SUITE_data/proto/simple.proto` file.

    message Person {
    	required string name = 1;
    	required string address = 2;
    	required string phone_number = 3;
    	required int32 age = 4;
        optional Location location = 5;
    }

    message Location
    {
        required string region = 1;
        required string country = 2;
    }

From that file we can create an Erlang module that can encode and decode the
Person message into records.

    1> protobuffs_compile:scan_file("simple.proto").
    ok
	2> simple_pb:decode_person(<<10,4,78,105,99,107,18,13,77,111,...>>).
	{person,<<"Nick">>,<<"Mountain View">>, <<"+1 (000) 555-1234">>,25,undefined}
	3> simple_pb:encode_person({person, <<"Nick">>, <<"Mountain View">>,
	    <<"+1 (000) 555-1234">>,25, undefined}).
    [[["\n",[4],<<"Nick">>],
      [[18],"\r",<<"Mountain View">>],
      [[26],[17],<<"+1 (000) 555-1234">>],
      [" ",[25]],
      []]]

How cool is that? From .proto files, we create modules that export encode and
decode functions for the messages defined.

## Deep lists

You might have noticed that the examples above produce deep lists
(also known as iolists), not binaries, when encoding messages. Since
we assume most messages will be sent to another computer over the
network or written to disk, we can delay flattening the encoding until
the last instant, i.e. writing to the port that will send the message.
All ports accept deep lists, so there's no reason to flatten them in
our encoding or application code. If you absolutely must have a
binary, use the `iolist_to_binary/1` BIF on the encoded message.

Ref:
[Erlang Efficiency Guide](http://www.erlang.org/doc/efficiency_guide/listHandling.html#id64578)

## no_debug_info

The protobuffs_compile module relies on the pokemon_pb module being compiled
with debug info. This is because pokemon_pb serves as a template for generated
_pb modules. Running protobuffs_compile:scan_file/1 reads the erlang forms from
the pokemon_pb.beam file and expands and alters those forms to create the generated
module.

## Building with rebar
To compile
    %>./rebar compile

To run all tests
    %>./rebar eunit
    %>./rebar ct

Se rebar doc for more information.

## CREDITS

Some of the protobuffs.erl module came from code written by Brian Buchanan.

Some of the protobuffs\_compile.erl module came from code written by Tim
Fletcher.

The rest of it and it's test suite was written by Nick Gerakines. Major
contributions have been made by Jacob Vorreuter.
