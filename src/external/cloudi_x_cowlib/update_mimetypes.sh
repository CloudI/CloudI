#!/bin/sh

# based on Makefile gen target

GEN_URL="http://svn.apache.org/repos/asf/httpd/httpd/trunk/docs/conf/mime.types"
GEN_SRC="priv/cow_mimetypes.erl.src"
GEN_OUT="src/cow_mimetypes.erl"

cat $GEN_SRC \
  | head -n `grep -n "%% GENERATED" $GEN_SRC | cut -d : -f 1` \
  > $GEN_OUT
wget -qO - $GEN_URL \
  | grep -v ^# \
  | awk '{for (i=2; i<=NF; i++) if ($i != "") { \
   split($1, a, "/"); \
   print "all_ext(<<\"" $i "\">>) -> {<<\"" \
    a[1] "\">>, <<\"" a[2] "\">>, []};"}}' \
  | sort \
  | uniq -w 25 \
  >> $GEN_OUT
cat $GEN_SRC \
  | tail -n +`grep -n "%% GENERATED" $GEN_SRC | cut -d : -f 1` \
  >> $GEN_OUT
