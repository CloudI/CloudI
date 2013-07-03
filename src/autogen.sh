#!/bin/sh
DIRECTORY=`dirname $0`
autoreconf --force --install -I config -I m4 $DIRECTORY/configure.ac

