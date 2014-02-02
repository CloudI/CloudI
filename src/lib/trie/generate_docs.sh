#!/bin/sh
FILES_CHANGED=`git status --porcelain 2>/dev/null| grep "^?? src\/" | wc -l`
if [ ${FILES_CHANGED} -ne 0 ]; then
    echo "Commit changes!"
    exit 1
fi
sed -e "/-include(\"trie.hrl\")\./ r src/trie.hrl" \
    -e "/-include(\"trie.hrl\")\./d" \
    src/trie.erl > src/trie.erl.doc
sed -e "/-include(\"trie.hrl\")\./ r src/trie.hrl" \
    -e "/-include(\"trie.hrl\")\./d" \
    src/btrie.erl > src/btrie.erl.doc
mv src/trie.erl.doc src/trie.erl
mv src/btrie.erl.doc src/btrie.erl
rebar doc
git checkout src

