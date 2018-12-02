#!/bin/bash

cd $(dirname $(realpath $0))

make clean
make

# fix internal doc links
sed -i 's/https:\/\/github\.com\/doc\/\([a-zA-Z_-]*\)\.md/\1.html/g' *.html
sed -i 's/\"\([a-zA-Z_-]*\)\.md\([a-zA-Z_-#]*\)\"/\"\1.html\2\"/g' *.html
sed -i 's/\"doc\/\([a-zA-Z_-]*\)\.md\"/\"\1.html\"/g' *.html
sed -i 's/\"\([a-zA-Z_-]*\)\.md\"/\"\1.html\"/g' *.html

# fix external doc links
sed -i 's/maps\.html\#/http:\/\/erlang.org\/doc\/man\/maps\.html#/g' *.html
sed -i 's/unicode\.html\#/http:\/\/erlang.org\/doc\/man\/unicode\.html#/g' *.html
sed -i 's/maps\.md\#/http:\/\/erlang.org\/doc\/man\/maps\.html#/g' *.html
sed -i 's/unicode\.md\#/http:\/\/erlang.org\/doc\/man\/unicode\.html#/g' *.html

# cleans up the indentation of code blocks
sed -i 's/      <a/<a/g' index.html
