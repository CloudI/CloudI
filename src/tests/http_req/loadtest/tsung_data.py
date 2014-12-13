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
from optparse import OptionParser

def print_metrics(file_path, host_name):
    initialize_min = 1e6
    initialize_max = 0
    initialize_count = 0
    values_min_mean_max = {
        # TRANSACTION LATENCY SUMMARY
        # ('tr_XXX' can be added below)
        'page':    (initialize_min, initialize_max, initialize_max),
        #'request': (initialize_min, initialize_max, initialize_max),
        #'connect': (initialize_min, initialize_max, initialize_max),
    }
    count_per_sec_stable_tolerance = 10 # % within current value
    count_per_sec_stable_count = 60 / 10 # 1 minutes min in 10 second incr
    count_per_sec_history = 10 # entries in list
    values_count_per_sec_max = {
        # MAX TRANSACTIONS PER SECOND
        # ('tr_XXX' can be added below)
        'page': [(initialize_max, initialize_count, initialize_count)],
        #'request': [(initialize_max, initialize_count, initialize_count)],
        #'connect': [(initialize_max, initialize_count, initialize_count)],
    }
    values_min_max_diff = {}
    values_max = {}
    if host_name is not None:
        # MEMORY CONSUMPTION
        values_min_max_diff['{freemem,"os_mon@%s"}' % host_name] = (
            initialize_min, initialize_max,
        )
        # CPU CONSUMPTION
        values_max['{cpu,"os_mon@%s"}' % host_name] = (
            initialize_max,
        )
        # LOAD AVERAGE
        values_max['{load,"os_mon@%s"}' % host_name] = (
            initialize_max,
        )
    count_per_sec_mult = 1.0 - count_per_sec_stable_tolerance / 100.0
    f = open(file_path, 'r')
    lines = csv.reader(f, delimiter=' ')
    for line in lines:
        if len(line) != 9:
            continue
        name = line[1]
        value_min_mean_max = values_min_mean_max.get(name)
        if value_min_mean_max is not None:
            values_min_mean_max[name] = (
                min(float(line[6]), value_min_mean_max[0]), # min min
                max(float(line[3]), value_min_mean_max[1]), # max (10 sec) mean
                max(float(line[5]), value_min_mean_max[2]), # max max
            )
        value_count_per_sec_max = values_count_per_sec_max.get(name)
        if value_count_per_sec_max is not None:
            updated = False
            insert_i = None
            count_per_sec = int(line[2]) / 10.0
            for i, value_count_per_sec in enumerate(value_count_per_sec_max):
                (count_per_sec_max, stable_count, count) = value_count_per_sec
                if count_per_sec > count_per_sec_max and insert_i is None:
                    insert_i = i
                if (count == int(line[8]) and # must be consecutive
                    count_per_sec > count_per_sec_max * count_per_sec_mult):
                    value_count_per_sec_max[i] = (
                        count_per_sec_max,
                        stable_count + 1,
                        int(line[2]) + count,
                    )
                    updated = True
            if insert_i is not None:
                # only looking at half of the plateau due to storing the max
                value_count_per_sec_max.insert(insert_i, (
                    count_per_sec,
                    1,
                    int(line[2]) + int(line[8]),
                ))
                updated = True
            if updated:
                values_count_per_sec_max[name] = (
                    value_count_per_sec_max[:count_per_sec_history]
                )
        value_min_max_diff = values_min_max_diff.get(name)
        if value_min_max_diff is not None:
            values_min_max_diff[name] = (
                min(float(line[6]), value_min_max_diff[0]), # min min
                max(float(line[5]), value_min_max_diff[1]), # max max
            )
        value_max = values_max.get(name)
        if value_max is not None:
            values_max[name] = (
                max(float(line[5]), value_max[0]), # max max
            )
    f.close()
    print('\t\tmin_min    \tmax_mean    \tmax_max\t   ' +
          '(trans latency)')
    for name, value_min_mean_max in values_min_mean_max.items():
        (min_min, max_mean, max_max) = value_min_mean_max
        print('%8s:\t%f\t%f\t%f\tmilliseconds' %
              (name, min_min, max_mean, max_max))
    print('\t\ttransactions_per_second_max\t\t   ' +
          '(trans throughput)')
    for name, value_count_per_sec_max in values_count_per_sec_max.items():
        count_per_sec_max_peak = value_count_per_sec_max[0][0]
        for count_per_sec_max, stable_count, count in value_count_per_sec_max:
            if stable_count < count_per_sec_stable_count:
                continue
            print('%8s:\t%f -%d%% during %ds\t(%f peak)' %
                  (name, count_per_sec_max, count_per_sec_stable_tolerance,
                   stable_count * 10, count_per_sec_max_peak))
            break
    if host_name is not None:
        print('\t\ttotal_megabytes\t\t\t\t   ' +
              '(memory)')
        (min_min,
         max_max) = values_min_max_diff['{freemem,"os_mon@%s"}' % host_name]
        print('%8s:\t%f' % ('memory', max_max - min_min))
        print('\t\tmax_max\t\t\t\t\t   ' +
              '(processor)')
        (max_max, ) = values_max['{cpu,"os_mon@%s"}' % host_name]
        print('%8s:\t%f %%' % ('cpu', max_max))
        (max_max, ) = values_max['{load,"os_mon@%s"}' % host_name]
        print('%8s:\t%f' % ('load', max_max))

if __name__ == '__main__':
    default_file_name = 'tsung.log'
    default_host_name = None
    parser = OptionParser()
    parser.add_option(
        '-n', '--file-name', dest='file_name',
        help='Tsung output log',
        metavar='FILENAME', default=default_file_name, type='string',
    )
    parser.add_option(
        '-s', '--server-name', dest='host_name',
        help='Monitored hostname (of the loadtest server)',
        metavar='HOSTNAME', default=default_host_name, type='string',
    )
    (options, args) = parser.parse_args()
    file_name = options.file_name
    host_name = options.host_name
    if os.access(file_name, os.R_OK):
        print_metrics(file_name, host_name)
    else:
        for root, dirs, files in os.walk('.'):
            if file_name in files:
                file_path = os.path.sep.join([root, file_name])
                print('%s:' % file_path)
                print_metrics(file_path, host_name)

