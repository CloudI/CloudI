#!/bin/sh

# test all the HTTP service implementations (python, java, ruby)
# and make sure their responses are the same while avoiding a
# byzantine failure
# (uses cloudi_service_quorum to make sure that less than 1/3rd of the
#  processes fail (the default configuration has 12 processes))

FAILED=0
for FILE in `ls input`
do

  # -H "Transfer-Encoding: chunked"

  # text requests
  rm -f tmp/$FILE
  /usr/bin/env curl -X POST -H "Content-Type: text/plain" -s \
                    --data-ascii @input/$FILE -o tmp/$FILE \
                    --compressed \
                    http://localhost:6466/byzantine/tests/http/$FILE

  if [ ! -f output/$FILE ]; then
    echo "output/$FILE missing"
    continue
  fi
  test -f tmp/$FILE && cmp -s tmp/$FILE output/$FILE
  if [ $? -ne 0 ]; then
    echo "input/$FILE request failed"
    FAILED=1
  fi

  # compressed requests
  if [ -f input.zip/$FILE ]; then
    rm -f tmp/$FILE
    /usr/bin/env curl -X POST -H "Content-Type: application/zip" -s \
                      --data-binary @input.zip/$FILE -o tmp/$FILE \
                      --compressed \
                      http://localhost:6466/byzantine/tests/http/$FILE

    test -f tmp/$FILE && cmp -s tmp/$FILE output/$FILE
    if [ $? -ne 0 ]; then
      echo "input.zip/$FILE request failed"
      FAILED=1
    fi
  fi

done

if [ $FAILED -eq 0 ]; then
  echo \
"All tests were successful! (no Byzantine failures handling HTTP requests)"
fi
