#!/bin/bash

# bash syntax to preserve CRLF
curl -v -X POST -H "Content-Type: multipart/form-data; boundary=----------------------------4ebf00fbcf09" -d $'------------------------------4ebf00fbcf09\r\nContent-Disposition: form-data; name="example1"; filename="example1.txt"\r\n\r\ntest1\r\n------------------------------4ebf00fbcf09\r\nContent-Disposition: form-data; name="example2"; filename="example2.txt"\r\n\r\ntest2\r\n------------------------------4ebf00fbcf09--\r\n'  http://localhost:6464/tests/echo

curl -v -X POST -F 'example3=test3' -F 'example4=test4' http://localhost:6464/tests/echo
