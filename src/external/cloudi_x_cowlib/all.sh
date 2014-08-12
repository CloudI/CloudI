#!/bin/sh

KERL_INSTALL_PATH=~/erlang
KERL_RELEASES="r15b r15b01 r15b02 r15b03 r16b r16b01 r16b02 r16b03-1 17.0 17.1.2"

make build-ct-suites

for rel in $KERL_RELEASES
do
	echo
	echo "    TESTING $rel"
	echo
	. $KERL_INSTALL_PATH/$rel/activate
	CT_OPTS="-label $rel" make tests
done

xdg-open logs/all_runs.html
