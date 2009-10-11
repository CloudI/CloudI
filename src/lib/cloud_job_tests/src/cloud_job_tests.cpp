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
#include "piqpr8_gmp.hpp"
#include "piqpr8_gmp_verify.hpp"
#include <sstream>
#include <iostream>
#include <string>
#include <stdint.h>
#include <cassert>

//#include <boost/random.hpp>
//// create random failures for testing purposes
//class RandomFailure
//{
//    public:
//        static int const oddsOfFailure = 10; // to 1
//        static int const maxConsecutiveFailures = 50; // (min of 0)
//
//        RandomFailure() : m_random(1, oddsOfFailure),
//            m_randomCount(0, maxConsecutiveFailures), m_count(10)
//        {
//            m_randomGenerator.seed(static_cast<int>(getpid()));
//            m_randomCountGenerator.seed(static_cast<int>(getpid()) / 2);
//        }
//        bool operator () ()
//        {
//            boost::lock_guard<boost::mutex> lock(m_mutex);
//            if (m_count > 0)
//            {
//                --m_count;
//                return true;
//            }
//            if (m_random(m_randomGenerator) == 1)
//            {
//                m_count = m_randomCount(m_randomCountGenerator);
//                return true;
//            }
//            else
//            {
//                return false;
//            }
//        }
//            
//    private:
//        boost::mutex m_mutex;
//        boost::hellekalek1995 m_randomGenerator;
//        boost::uniform_int<int> m_random;
//        boost::hellekalek1995 m_randomCountGenerator;
//        boost::uniform_int<int> m_randomCount;
//        int m_count;
//};
//
//static RandomFailure returnFalse;

extern "C"
{

void initialize()
{
    // global/"static" data is shared between multiple threads, beware!
    std::clog << "cloud_job_tests global/static data "
        "initialize()" << std::endl;
}

void deinitialize()
{
    std::clog << "cloud_job_tests global/static data "
        "deinitialize()" << std::endl;
}

bool do_work(bool const & abortTask,
             uint32_t const,
             uint32_t const,
             uint32_t const, 
             boost::scoped_array<uint8_t> const & taskData,
             size_t const taskDataSize,
             DatabaseQueryVector const &,
             DatabaseQueryVector & queriesOut)
{
    // uncomment for random failures that would be deterministic
    // based on the work code dependencies
    //if (returnFalse())
    //    return false;

    // get task data
    assert(taskDataSize > (sizeof(uint32_t) * 2));
    uint32_t & iterationCount =
        *(reinterpret_cast<uint32_t *>(&taskData[0]));
    uint32_t & digitStep =
        *(reinterpret_cast<uint32_t *>(&taskData[sizeof(uint32_t)]));
    char const * digitIndex = 
        reinterpret_cast<char const *>(&taskData[sizeof(uint32_t) +
                                                 sizeof(uint32_t)]);

    // perform the work
    std::ostringstream result;
    for (uint32_t i = 0; i < iterationCount; ++i)
    {
        if (abortTask)
        {
            return false;
        }
        else
        {
            std::string piSequence;
            if (! bbp_pi(abortTask, digitIndex, digitStep * i, piSequence))
                return false;
            result << piSequence;
        }
    }
    std::string const & piResult = result.str();
    if (! verify(digitIndex, piResult))
        return false;

    // create query to store the results of the work
    std::ostringstream resultQuery;
    resultQuery <<
        "INSERT INTO incoming_results_v3 "
        "(digit_index, data) "
        "VALUES "
        "(" << digitIndex << ", '" << piResult << "');";
    queriesOut.push_back(
        DatabaseQuery("cloud_data_pgsql.cloudi_tests", resultQuery.str()));
    queriesOut.push_back(
        DatabaseQuery("cloud_data_mysql.cloudi_tests", resultQuery.str()));
    resultQuery.str("");
    resultQuery << 
        "{set, \"" << digitIndex << "\", <<\"" << piResult << "\">>}";
    queriesOut.push_back(
        DatabaseQuery("cloud_data_memcached.cloudi_tests", resultQuery.str()));

    return true;
}

}

