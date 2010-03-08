// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2010, Michael Truog
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
#include <iostream>
#include <string>
#include <stdint.h>
#include <ctime>
#include <cstdlib>
#include <cstring>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <libpq-fe.h>
#include "query_cache.hpp"
#include "assert.hpp"

query_cache::query_cache(PGconn * connection,
            std::string const & query,
            uint32_t rows) : 
    m_connection(connection),
    m_result(0),
    m_query(query + " LIMIT $1::int4 OFFSET $2::int8"),
    m_limit(rows),
    m_offset(0),
    m_inter_limit(0),
    m_own_result(true)
{
    query_segment();
}

query_cache::query_cache(query_cache const & o) :
    m_connection(o.m_connection),
    m_result(o.m_result),
    m_query(o.m_query),
    m_limit(o.m_limit),
    m_offset(o.m_offset),
    m_inter_limit(o.m_inter_limit),
    m_own_result(false)
{
}

query_cache::~query_cache()
{
    if (m_own_result && m_result)
        PQclear(m_result);
}

bool query_cache::at_end() const
{
    assert(m_result);
    ssize_t const rows = PQntuples(m_result);
    if (rows < 0)
        return true; // error condition
    if (m_inter_limit == static_cast<size_t>(rows) &&
        (m_inter_limit != (m_limit - 1) || m_inter_limit == 0))
    {
        return true;
    }
    return false;
}

void query_cache::store_value(char const * const column, double & value)
{
    assert(m_result);
    int const fnum = PQfnumber(m_result, column);
    assert(fnum >= 0);
    uint32_t * value_chunks =
        reinterpret_cast<uint32_t *>(PQgetvalue(m_result,
                                                m_inter_limit,
                                                fnum));
    assert(value_chunks);
    uint32_t * value_result = reinterpret_cast<uint32_t *>(&value);
    value_result[0] = ntohl(value_chunks[1]);
    value_result[1] = ntohl(value_chunks[0]);
}

void query_cache::store_value(char const * const column, uint32_t & value)
{
    assert(m_result);
    int const fnum = PQfnumber(m_result, column);
    assert(fnum >= 0);
    uint32_t * value_chunk =
        reinterpret_cast<uint32_t *>(PQgetvalue(m_result,
                                                m_inter_limit,
                                                fnum));
    assert(value_chunk);
    value = ntohl(*value_chunk);
}

void query_cache::store_value(char const * const column, uint64_t & value)
{
    assert(m_result);
    int const fnum = PQfnumber(m_result, column);
    assert(fnum >= 0);
    uint32_t * value_chunks =
        reinterpret_cast<uint32_t *>(PQgetvalue(m_result,
                                                m_inter_limit,
                                                fnum));
    assert(value_chunks);
    uint32_t * value_result = reinterpret_cast<uint32_t *>(&value);
    value_result[0] = ntohl(value_chunks[1]);
    value_result[1] = ntohl(value_chunks[0]);
}

void query_cache::store_value(char const * const column, time_t & value)
{
    assert(m_result);
    int const fnum = PQfnumber(m_result, column);
    assert(fnum >= 0);
    uint32_t * value_chunks =
        reinterpret_cast<uint32_t *>(PQgetvalue(m_result,
                                                m_inter_limit,
                                                fnum));
    assert(value_chunks);
    if (sizeof(time_t) == sizeof(uint64_t))
    {
        uint32_t * value_result = reinterpret_cast<uint32_t *>(&value);
        value_result[0] = ntohl(value_chunks[1]);
        value_result[1] = ntohl(value_chunks[0]);
    }
    else if (sizeof(time_t) == sizeof(uint32_t))
    {
        uint32_t * value_result = reinterpret_cast<uint32_t *>(&value);
        *value_result = ntohl(value_chunks[1]);
    }
    else
    {
        // time_t assumption is wrong
        std::cerr << "(sizeof(time_t) == sizeof(uint64_t) ||"
                     " sizeof(time_t) == sizeof(uint32_t))"
                     " == false" << std::endl;
        exit(1);
    }
}

void query_cache::store_value(char const * const column, std::string & value)
{
    assert(m_result);
    int const fnum = PQfnumber(m_result, column);
    assert(fnum >= 0);
    char * text_value = reinterpret_cast<char *>(PQgetvalue(m_result,
                                                            m_inter_limit,
                                                            fnum));
    assert(text_value);
    size_t length = strlen(text_value);
    value.resize(length);
    memcpy(const_cast<char *>(value.c_str()),
           text_value, (length + 1) * sizeof(char));
}

void query_cache::query_segment()
{
    char const * param_values[2];
    int param_lengths[2];
    int param_formats[2];

    uint32_t network_order_limit = htonl(m_limit);
    uint32_t * offset = reinterpret_cast<uint32_t *>(&m_offset);
    uint32_t network_order_offset[2];
    network_order_offset[0] = htonl(offset[1]);
    network_order_offset[1] = htonl(offset[0]);
    param_values[0] = reinterpret_cast<char *>(&network_order_limit);
    param_lengths[0] = sizeof(network_order_limit);
    param_formats[0] = 1;
    param_values[1] = reinterpret_cast<char *>(network_order_offset);
    param_lengths[1] = sizeof(network_order_offset);
    param_formats[1] = 1;

    if (m_own_result && m_result)
        PQclear(m_result);
    m_result = PQexecParams(m_connection,
                            m_query.c_str(),
                            2,
                            0,  // backend deduces parameter types
                            param_values,
                            param_lengths,
                            param_formats,
                            1); // ask for binary results

    if (PQresultStatus(m_result) != PGRES_TUPLES_OK)
    {
        std::cerr << "query failed: " <<
            PQerrorMessage(m_connection) << std::endl;
        PQclear(m_result);
        PQfinish(m_connection);
        exit(1);
    }
}

