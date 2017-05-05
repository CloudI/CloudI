# MessagePack Erlang

[![Travis](https://secure.travis-ci.org/msgpack/msgpack-erlang.png)](https://travis-ci.org/msgpack/msgpack-erlang)
[![Drone.io](https://drone.io/github.com/msgpack/msgpack-erlang/status.png)](https://drone.io/github.com/msgpack/msgpack-erlang)
[![hex.pm version](https://img.shields.io/hexpm/v/msgpack.svg)](https://hex.pm/packages/msgpack)

## Prerequisites for runtime

[Erlang/OTP](http://erlang.org/), >= 17.0 Also based on
[the new msgpack spec 0b8f5a](https://github.com/msgpack/msgpack/blob/0b8f5ac67cdd130f4d4d4fe6afb839b989fdb86a/spec.md).

## edit rebar.config to use in your application

```erlang
{deps, [
  {msgpack, ".*",
    {git, "git://github.com/msgpack/msgpack-erlang.git", {branch, "master"}}}
]}.
```

Or as it is [now published at hex.pm](https://hex.pm/packages/msgpack), just

```erlang
{deps, [msgpack]}.
```

might work.

## Simple deserialization

```erlang
Ham = msgpack:pack(Spam),
{ok, Spam} = msgpack:unpack(Ham).
```

## Stream deserialization

```erlang
{Term0, Rest0} = msgpack:unpack_stream(Binary),
{Term1, Rest1} = msgpack:unpack_stream(Rest0),
...
```

## Options, for packing and unpacking

### `{spec, new|old}`

Both for packing and unpacking. Default is `new`. Major difference
between old and new spec is:

- raw family (`0xa0~0xbf`, `0xda`, `0xdb`) becomes new str family
- `0xd9` is new as str8
- new bin space (`0xc4, 0xc5, 0xc6` as bin8, bin16, bin32)
- new ext space (`0xc7, 0xc8, 0xc9` as ext8, ext16, ext32)
- new fixext space (`0xd4, 0xd5, 0xd6, 0xd7, 0xd8` as fixext1, fixext2, fixext4, fixext8, fixext16),

The default is new spec. Old spec mode does not handle these new types but
returns error. To use
[old spec](https://github.com/msgpack/msgpack/blob/master/spec-old.md)
mode, this option is explicitly added.

```erlang
OldHam = msgpack:pack(Spam, [{spec, old}]),
{ok, Spam} = msgpack:unpack(OldHam, [{spec, old}]).
```

### `{allow_atom, none|pack}`

Only in packing. Atoms are packed as binaries. Default value is `pack`.
Otherwise, any term including atoms throws badarg.

### `{known_atoms, [atom()]}`

Both in packing and unpacking. In packing, if an atom is in this list
a binary is encoded as a binary. In unpacking, msgpacked binaries are
decoded as atoms with `erlang:binary_to_existing_atom/2` with encoding
`utf8`. Default value is an empty list.

Even if `allow_atom` is `none`, known atoms are packed.

### `{unpack_str, as_binary|as_list}`

A switch to choose decoded term style of `str` type when *unpacking*.
Only available at new spec. Default is `as_list`.

```
mode        as_binary    as_list
-----------+------------+-------
bin         binary()     binary()
str         binary()     string()
```

### `{validate_string, boolean()}`

Only in unpacking, UTF-8 validation at unpacking from `str` type will
be enabled. Default value is `false`.

### `{pack_str, from_binary|from_list|none}`

A switch to choose packing of `string()` when packing. Only available
at new spec. Default is `from_list` for symmetry with `unpack_str`
option.

```
mode        from_list    from_binary    none
-----------+------------+--------------+-----------------
binary()    bin          str*/bin       bin
string()    str*/array   array of int   array of int
list()      array        array          array
```

But the default option pays the cost of performance for symmetry. If
the overhead of UTF-8 validation is unacceptable, choosing `none` as
the option would be the best.

- \* Tries to pack as `str` if it is a valid `string()`.

### `{map_format, map|jiffy|jsx}`

Both at packing and unpacking. Default value is `map`.

```erlang
msgpack:pack(#{ <<"key">> => <<"value">> }, []).
msgpack:pack({[{<<"key">>, <<"value">>}]}, [{format, jiffy}]),
msgpack:pack([{<<"key">>, <<"value">>}], [{format, jsx}]).
```


### `{ext, {msgpack_ext_packer(), msgpack_ext_unpacker()}|module()}`

At both. The default behaviour in case of facing ext data at decoding
is to ignore them as its length is known.

Now msgpack-erlang supports ext type. Now you can serialize everything
with your original (de)serializer. That will enable us to handle
erlang- native types like `pid()`, `ref()` contained in `tuple()`. See
`test/msgpack_ext_example_tests.erl` for example code.

```erlang
Packer = fun({ref, Ref}, Opt) when is_reference(Ref) -> {ok, {12, term_to_binary(Ref)}} end,
Unpacker = fun(12, Bin) -> {ok, {ref, binary_to_term(Bin)}} end,
Ref = make_ref(),
Opt = [{ext,{Packer,Unpacker}}],
{ok, {ref, Ref}} = msgpack:unpack(msgpack:pack({ref, Ref}, Opt), Opt).
```

## Misc

### Float type

The Float type of Message Pack represents IEEE 754 floating point number, so it includes Nan and Infinity.
In unpacking, msgpack-erlang returns `nan`, `positive_infinity` and `negative_infinity`.

## License

Apache License 2.0

# Release Notes

## 0.7.0

- Support `nan`, `positive_infinity` and `negative_infinity`

## 0.6.0

- Support OTP 19.0

## 0.5.0

- Renewed optional arguments to pack/unpack interface. This is
  incompatible change from 0.4 series.

## 0.4.0

- Deprecate `nil`
- Moved to rebar3
- Promote default map unpacker as default format when OTP is >= 17
- Added QuickCheck tests
- Since this version OTP older than R16B03-1 are no more supported

## 0.3.5 / 0.3.4

- 0.3 series will be the last versions that supports R16B or older
  versions of OTP.
- OTP 18.0 support
- Promote default map unpacker as default format when OTP is >= 18

## 0.3.3

- Add OTP 17 series to Travis-CI tests
- Fix wrong numbering for ext types
- Allow packing maps even when {format,map} is not set
- Fix Dialyzer invalid contract warning
- Proper use of null for jiffy-style encoding/decoding

## 0.3.2

- set back default style as jiffy
- fix bugs around nil/null handling

## 0.3.0

- supports map new in 17.0
- jiffy-style maps will be deprecated in near future
- set default style as map

## 0.2.8

0.2 series works with OTP 17.0, R16, R15, and with MessagePack's new
and old format. But does not support `map` type introduced in
OTP 17.0.

It also supports JSX-compatible mode.
