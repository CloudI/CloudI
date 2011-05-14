#!/bin/sh

for FILE in `ls input`
do

  # -H "Transfer-Encoding: chunked"

  # text requests
  rm -f tmp/$FILE
  /usr/bin/env curl -H "Content-Type: text/plain" -s \
                    --data-ascii @input/$FILE -o tmp/$FILE \
                    --compressed \
                    http://localhost:6464/tests/http/$FILE

  if [ ! -f output/$FILE ]; then
    echo "output/$FILE missing"
    continue
  fi
  test -f tmp/$FILE && cmp -s tmp/$FILE output/$FILE
  if [ $? -ne 0 ]; then
    echo "input/$FILE request failed"
  fi

  # compressed requests
  if [ -f input.zip/$FILE ]; then
    rm -f tmp/$FILE
    /usr/bin/env curl -H "Content-Type: application/zip" -s \
                      --data-binary @input.zip/$FILE -o tmp/$FILE \
                      --compressed \
                      http://localhost:6464/tests/http/$FILE

    test -f tmp/$FILE && cmp -s tmp/$FILE output/$FILE
    if [ $? -ne 0 ]; then
      echo "input.zip/$FILE request failed"
    fi
  fi

done
