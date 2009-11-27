// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2009, Michael Truog
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//     * All advertising materials mentioning features or use of this
//       software must display the following acknowledgment:
//         This product includes software developed by Michael Truog
//     * The name of the author may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.
//
#include "cloud_work_interface.hpp"
#include <iostream>
#include <unistd.h>
#include <cstring>
#include <string>
#include <stdint.h>
#include <cassert>
#include <sys/time.h>
#include <time.h>

static void storeNow(char * dst);
static void storeNow(char * dst,
                     uint32_t megaSecs,
                     uint32_t secs,
                     uint32_t microSecs);
static bool performSleep(bool const & abortTask,
                         uint32_t sleepTime,
                         uint32_t sleepUnit);

static struct timespec lastEndTask[512];

extern "C"
{

void initialize()
{
    // global/"static" data is shared between multiple threads, beware!
    struct timespec startTime;
    clock_gettime(CLOCK_MONOTONIC, &startTime);
    for (size_t i = 0; i < (sizeof(lastEndTask) / sizeof(struct timespec)); ++i)
        lastEndTask[i] = startTime;
}

void deinitialize()
{
}

bool do_work(bool const & abortTask,
             uint32_t const,
             std::string const & machineName,
             std::string const &,
             uint32_t const id,
             uint32_t const, 
             boost::scoped_array<uint8_t> const & taskData,
             size_t const taskDataSize,
             DatabaseQueryVector const &,
             DatabaseQueryVector & queriesOut)
{
    struct timespec startTask;
    clock_gettime(CLOCK_MONOTONIC, &startTask);
    // time spent waiting before the task in milliseconds
    const double beforeTask =
        (static_cast<double>(
            startTask.tv_sec - lastEndTask[id].tv_sec) * 1.0e3 +
         static_cast<double>(
            startTask.tv_nsec - lastEndTask[id].tv_nsec) * 1.0e-6);

    // get task data
    assert(taskDataSize == (sizeof(uint32_t) * 5));
    uint32_t & sleepTime =
        *(reinterpret_cast<uint32_t *>(&taskData[0]));
    uint32_t & sleepUnit =
        *(reinterpret_cast<uint32_t *>(&taskData[sizeof(uint32_t)]));
    uint32_t & createTaskTimeMegaSecs = 
        *(reinterpret_cast<uint32_t *>(&taskData[sizeof(uint32_t) * 2]));
    uint32_t & createTaskTimeSecs = 
        *(reinterpret_cast<uint32_t *>(&taskData[sizeof(uint32_t) * 3]));
    uint32_t & createTaskTimeMicroSecs = 
        *(reinterpret_cast<uint32_t *>(&taskData[sizeof(uint32_t) * 4]));

    // time data for latency calculations are passed through
    // the work function while emulating the proper amount of work.

    std::string latency(
        sizeof(double) + sizeof(uint32_t) * 9 + machineName.size(), '\0');
    char * interTaskTime = const_cast<char *>(latency.c_str());
    *(reinterpret_cast<double *>(interTaskTime)) = beforeTask;
    char * createTaskTime = &interTaskTime[sizeof(double)];
    storeNow(createTaskTime,
             createTaskTimeMegaSecs,
             createTaskTimeSecs,
             createTaskTimeMicroSecs);
    char * getTaskTime = &createTaskTime[sizeof(uint32_t) * 3];
    storeNow(getTaskTime);
    if (! performSleep(abortTask, sleepTime, sleepUnit))
        return false;
    char * returnTaskTime = &getTaskTime[sizeof(uint32_t) * 3];
    storeNow(returnTaskTime);
    char * uniqueName = &returnTaskTime[sizeof(uint32_t) * 3];
    memcpy(uniqueName, machineName.c_str(), machineName.size());

    queriesOut.push_back(DatabaseQuery(latency.c_str(), latency.size()));

    clock_gettime(CLOCK_MONOTONIC, &(lastEndTask[id]));

    return true;
}

}

static void storeNow(char * dst)
{
    struct timeval now;
    int status = gettimeofday(&now, 0);
    assert(status == 0);
    storeNow(dst, (now.tv_sec / 1000000), (now.tv_sec % 1000000), now.tv_usec);
}

static void storeNow(char * dst,
                     uint32_t megaSecs,
                     uint32_t secs,
                     uint32_t microSecs)
{
    *(reinterpret_cast<uint32_t *>(&dst[0])) = megaSecs;
    *(reinterpret_cast<uint32_t *>(&dst[sizeof(uint32_t)])) = secs;
    *(reinterpret_cast<uint32_t *>(&dst[sizeof(uint32_t) * 2])) = microSecs;
}

// sleep to consistently represent a delay processing work
static bool performSleep(bool const & abortTask,
                         uint32_t sleepTime,
                         uint32_t sleepUnit)
{
    static uint32_t const secondsUnit =
        *(reinterpret_cast<uint32_t const *>("sec "));
    static uint32_t const microsecondsUnit =
        *(reinterpret_cast<uint32_t const *>("usec"));

    if (sleepUnit == secondsUnit)
    {
        while (sleepTime-- > 0)
        {
            sleep(1);
            if (abortTask)
                return false;
        }
    }
    else if (sleepUnit == microsecondsUnit)
    {
        if (sleepTime < 1000000)
        {
            usleep(sleepTime);
        }
        else
        {
            sleepTime /= 1000000;
            while (sleepTime-- > 0)
            {
                sleep(1);
                if (abortTask)
                    return false;
            }
        }
    }
    else
    {
        char pUnitName[5] = {0, 0, 0, 0, 0};
        *(reinterpret_cast<uint32_t *>(pUnitName)) = sleepUnit;
        std::clog << "unknown sleep unit \"" << pUnitName << "\"" << std::endl;
        return false;
    }
    return true;
}

