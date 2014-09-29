#`erlang_term`

## PURPOSE

To provide the in-memory size of Erlang terms, ignoring where the Erlang terms
are stored.

Size information should match the [Erlang Efficiency Guide memory information](http://www.erlang.org/doc/efficiency_guide/advanced.html#id68923):

* Small integer: 1 word
  * On 32-bit architectures: -134217729 < i < 134217728 (28 bits)
  * On 64-bit architectures: -576460752303423489 < i < 576460752303423488 (60 bits)
* Big integer: 3..N words
* Atom: 1 word
* Float:
  * On 32-bit architectures: 4 words
  * On 64-bit architectures: 3 words
* Binary: 3..6 + data
* List: 1 word + 1 word per element + the size of each element
* Tuple: 2 words + the size of each element
* Pid:
  * From local node: 1 word
  * From remote node: 5 words
* Port:
  * From local node: 1 word
  * From remote node: 5 words
* Reference:
  * On 32-bit architectures:
    * From local node: 5 words
    * From remote node: 7 words
  * On 64-bit architectures:
    * From local node: 4 words
    * From remote node: 6 words
* Fun: 9..13 words + size of environment


## LICENSE

BSD license


## CONTACT

Michael Truog <mjtruog at gmail dot com>


## THANKS

* Sverker Eriksson [`erts_debug:flat_size/1` info](http://erlang.org/pipermail/erlang-bugs/2014-September/004607.html)

