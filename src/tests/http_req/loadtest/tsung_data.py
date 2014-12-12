#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#     * All advertising materials mentioning features or use of this
#       software must display the following acknowledgment:
#         This product includes software developed by Michael Truog
#     * The name of the author may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#

import os, csv

def print_latency(file_path):
    initialize_min = 1e6
    initialize_max = 0
    values = {
        # can add 'tr_XXX's below (min min, max mean, max max)
        'page':    (initialize_min, initialize_max, initialize_max),
        #'request': (initialize_min, initialize_max, initialize_max),
        #'connect': (initialize_min, initialize_max, initialize_max),
    }
    f = open(file_path, 'rb')
    lines = csv.reader(f, delimiter=' ')
    for line in lines:
        if len(line) != 9:
            continue
        name = line[1]
        value = values.get(name)
        if value is not None:
            values[name] = (
                min(float(line[6]), value[0]), # min min
                max(float(line[3]), value[1]), # max (10 second) mean
                max(float(line[5]), value[2]), # max max
            )
    f.close()
    print('     name\tmin_min    \tmax_mean    \tmax_max')
    for name, value in values.items():
        (min_min, max_mean, max_max) = value
        print('%8s:\t%f\t%f\t%f' % (name, min_min, max_mean, max_max))

if __name__ == '__main__':
    file_name = 'tsung.log'
    if os.access(file_name, os.R_OK):
        print_latency(file_name)
    else:
        for root, dirs, files in os.walk('.'):
            if file_name in files:
                file_path = os.path.sep.join([root, file_name])
                print('%s:' % file_path)
                print_latency(file_path)

