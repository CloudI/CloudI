#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2019 Michael Truog <mjtruog at protonmail dot com>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#
"""
Code Path CloudI Service API Integration Test
"""

import sys
import os
sys.path.append(
    os.path.sep.join(
        os.path.dirname(
            os.path.abspath(__file__)
        ).split(os.path.sep)[:-2] + ['service_api', 'python']
    )
)
from cloudi_service_api import CloudI # pylint: disable=wrong-import-position

def _main():
    obj = CloudI()
    assert obj.code_path_add('"/foo/bar"') == '{"success":false,"error":"bad_directory"}'

if __name__ == '__main__':
    _main()
