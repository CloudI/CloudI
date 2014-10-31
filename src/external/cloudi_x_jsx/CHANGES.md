v2.1.1

* faster generation of json via iolists
* `repeat_keys` option

v2.1

* force the end of streams with `end_json` in addition to `end_stream`
* support for encoding erlang datetime tuples to iso8601 format
* allow a single trailing comma in objects and arrays

v2.0.4

* more typespec adjustments

v2.0.3

* update some typespecs to make them more comprehensive

v2.0.2

* fixes travis-ci spec

v2.0.1

* fix regression in output of empty objects/arrays

v2.0

* jsx is much more pragmatic by default; common json errors are silently
    ignored (and fixed). stricter parsing must be enabled with options
* add support for encoding otp 17.0's new maps data type
* removed `pre_encode` and `post_decode` options in favour of making jsx
    functions easier to wrap and customize
* streaming behavior is now disabled by default and must be requested explicitly
* removed deprecated function names (`to_json`, `to_term`, `term_to_json`, etc) 
* expanded test coverage
    

v1.4.5

* various fixes to typespecs uncovered by dialyzer
* allow integer keys during encoding
* convert atoms (other than `true`, `false` and `null`) to strings during encoding

v1.4.4

* typespec for `json_term/0` fixed
* incorrect boolean shortcircuiting fixed in multibyte escape processing

v1.4.3

* add empty rebar.config for mix build tool
* add `attempt_atom` option for decoding json objects
* fix a bug related to multibyte codepoints and streaming input
* add a missing error state in the encoder

v1.4.2

* build apparatus cleaned up and streamlined
* new `{raw, <<"json goes here">>}` intermediate form to support direct generation of json
* bugfixes involving inappropriate exceptions from jsx functions

v1.4.1

* fixes a bug with interaction between `dirty_strings` and even numbers of escape characters
* performance enhancements

v1.4

* radically refactored decoder
* `dirty_strings` now behaves intuitively in decoding. bad codepoints, bad utf8, illegal characters and escapes (except `"` and `'` if `single_quoted_strings` is enabled) are ignored completely
* `incomplete_handler` & `error_handler` are now available for use, see documentation in README

v1.3.3

* `pre_encode` now orders input in the order you'd expect

v1.3.2

* `pre_encode` is now able to handle tuples *correctly*

v1.3.1

* `pre_encode` is now able to handle tuples

v1.3

* introduces `prettify/1` and `minify/1`, shortcuts for `format/2`
* introduce `encode/1,2` and `decode/1,2` as primary interface to built in tokenizers. `to_json/1,2` and `to_term/1,2` remain accessible but not advertised
* new `parser/3` function exposes syntactic analysis stage for use with user defined tokenizers
* improved documentation

v1.2.1

* fixes incorrect handling of escaped forward slashes, thanks bob ippolito

v1.2

* rewritten handling of string escaping to improve performance
* `pre_encode` and `post_decode` hooks, see README
* `relax` option

v1.1.2

* add `dirty_strings` option
* more fixes for invalid unicode in strings

v1.1.1

* fixes bug regarding handling of invalid unicode in R14Bxx

v1.1

* improvements to string escaping and json generation performance

v1.0.2

* fixes to function specs
* rewritten README
* `comments` option

v1.0.1

* rebar fix
