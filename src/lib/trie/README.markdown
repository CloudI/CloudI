Erlang Trie Implementation
==========================

The data structure is only for storing keys as strings (lists of integers), but is able to get performance close to the process dictionary when doing key lookups (based on [results here](http://okeuday.livejournal.com/20025.html) with [the benchmark here](http://github.com/okeuday/erlbench)).  So, this data structure is (currently) the quickest for lookups on key-value pairs where all keys are strings, if you ignore the process dictionary (which many argue should never be used).

The implementation stores leaf nodes as the string suffix because it is a [PATRICIA trie](https://xlinux.nist.gov/dads/HTML/patriciatree.html) (PATRICIA - Practical Algorithm to Retrieve Information Coded in Alphanumeric, D.R.Morrison (1968)).  Storing leaf nodes this way helps avoid single child leafs (compressing the tree a little bit).

The full OTP dict API is supported in addition to other functions.  Functions like foldl, iter, itera, and foreach traverse in alphabetical order.  Functions like map and foldr traverse in reverse alphabetical order.  There are also functions like `find_prefix`, `is_prefix`, and `is_prefixed` that check if a prefix exists within the trie.  The functions with a `"_similar"` suffix like `find_similar`, `foldl_similar`, and `foldr_similar` all operate with trie elements that share a common prefix with the supplied string.

The trie data structure supports string patterns.  The functions `find_match/2`, `fold_match/4`, and `pattern_parse/2` utilize patterns that contain a`"*"`wildcard character(s) (equivalent to ".+" regex while`"**"`is forbidden).  The function `find_match/2` operates on a trie filled with patterns when supplied a string non-pattern, while the function `fold_match/4` operates on a trie without patterns when supplied a string pattern.  The functions `find_match2/2` and `pattern2_parse/2` add `"?"` as an additional wildcard character (with `"**"`, `"??"`, `"*?"` and `"?*"` forbidden) that consumes greedily to the next character (`"?"` must not be the last character in the pattern).

The btrie data structure was added because many people wanted a quick associative data structure for binary keys.  However, other alternatives provide better efficiency, so the btrie is best used for functions that can not be found elsewhere (or perhaps extra-long keys)... more testing would be needed to determine the best use-cases of the btrie.

Author
------

Michael Truog (mjtruog at protonmail dot com)

License
-------

MIT License
