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
#ifndef QUERY_CACHE_HPP
#define QUERY_CACHE_HPP

// Cache for Postgres queries to reduce database traffic
// and avoid accesses to the raw data provided by the
// Postgres library interface.  The class encapsulates the dependencies
// on the Postgres library interface types, so they are not useable
// in the global namespace (after including this file).

#include <string>
#include <stdint.h>
#include <ctime>
#include "assert.hpp"

typedef struct pg_conn PGconn;
typedef struct pg_result PGresult;

// typical usage should be similar to:
// query_cache pgsql_data(connection,
//                        "SELECT extract(epoch from datetime)::int8, "
//                               "col0, col1, col2, col4 "
//                        "FROM amazing_pgsql_table ORDER BY datetime ASC",
//                        30000);
//
// (never use a ';' suffix on the query, since SQL is appended to the query
//  for OFFSET/LIMIT stepping through the data)

class query_cache
{
    public:
        query_cache(PGconn * connection,
                    std::string const & query,
                    uint32_t rows);
        query_cache(query_cache const & o);
        ~query_cache();

        query_cache & operator --()
        {
            if (m_inter_limit == 0)
            {
                assert(m_offset >= m_limit);
                m_offset -= m_limit;
                query_segment();
                m_inter_limit = m_limit - 1;
            }
            else
            {
                --m_inter_limit;
            }
            return *this;
        }

        query_cache & operator ++()
        {
            if (m_inter_limit == (m_limit - 1))
            {
                m_offset += m_limit;
                query_segment();
                m_inter_limit = 0;
            }
            else
            {
                ++m_inter_limit;
            }
            return *this;
        }

        query_cache operator --(int)
        {
            query_cache old(*this);
            if (m_inter_limit == 0)
            {
                assert(m_offset >= m_limit);
                m_offset -= m_limit;
                query_segment();
                m_inter_limit = m_limit - 1;
            }
            else
            {
                --m_inter_limit;
            }
            return old;
        }

        query_cache operator ++(int)
        {
            query_cache old(*this);
            if (m_inter_limit == (m_limit - 1))
            {
                m_offset += m_limit;
                query_segment();
                m_inter_limit = 0;
            }
            else
            {
                ++m_inter_limit;
            }
            return old;
        }

        void set_begin()
        {
            if (m_offset != 0)
            {
                m_offset = 0;
                m_inter_limit = 0;
                query_segment();
            }
            else
            {
                m_inter_limit = 0;
            }
        }

        bool at_begin() const
        {
            return ((m_offset == 0) && (m_inter_limit == 0));
        }

        bool at_end() const;
        
        template <typename T1>
        void get(char const * const column1, T1 & value1)
        {
            store_value(column1, value1);
        }

        template <typename T1,
                  typename T2>
        void get(char const * const column1, T1 & value1,
                 char const * const column2, T2 & value2)
        {
            store_value(column1, value1);
            store_value(column2, value2);
        }

        template <typename T1,
                  typename T2,
                  typename T3>
        void get(char const * const column1, T1 & value1,
                 char const * const column2, T2 & value2,
                 char const * const column3, T3 & value3)
        {
            store_value(column1, value1);
            store_value(column2, value2);
            store_value(column3, value3);
        }

        template <typename T1,
                  typename T2,
                  typename T3,
                  typename T4>
        void get(char const * const column1, T1 & value1,
                 char const * const column2, T2 & value2,
                 char const * const column3, T3 & value3,
                 char const * const column4, T4 & value4)
        {
            store_value(column1, value1);
            store_value(column2, value2);
            store_value(column3, value3);
            store_value(column4, value4);
        }

        template <typename T1,
                  typename T2,
                  typename T3,
                  typename T4,
                  typename T5>
        void get(char const * const column1, T1 & value1,
                 char const * const column2, T2 & value2,
                 char const * const column3, T3 & value3,
                 char const * const column4, T4 & value4,
                 char const * const column5, T5 & value5)
        {
            store_value(column1, value1);
            store_value(column2, value2);
            store_value(column3, value3);
            store_value(column4, value4);
            store_value(column5, value5);
        }

        template <typename T1,
                  typename T2,
                  typename T3,
                  typename T4,
                  typename T5,
                  typename T6>
        void get(char const * const column1, T1 & value1,
                 char const * const column2, T2 & value2,
                 char const * const column3, T3 & value3,
                 char const * const column4, T4 & value4,
                 char const * const column5, T5 & value5,
                 char const * const column6, T6 & value6)
        {
            store_value(column1, value1);
            store_value(column2, value2);
            store_value(column3, value3);
            store_value(column4, value4);
            store_value(column5, value5);
            store_value(column6, value6);
        }

        template <typename T1,
                  typename T2,
                  typename T3,
                  typename T4,
                  typename T5,
                  typename T6,
                  typename T7>
        void get(char const * const column1, T1 & value1,
                 char const * const column2, T2 & value2,
                 char const * const column3, T3 & value3,
                 char const * const column4, T4 & value4,
                 char const * const column5, T5 & value5,
                 char const * const column6, T6 & value6,
                 char const * const column7, T7 & value7)
        {
            store_value(column1, value1);
            store_value(column2, value2);
            store_value(column3, value3);
            store_value(column4, value4);
            store_value(column5, value5);
            store_value(column6, value6);
            store_value(column7, value7);
        }

        template <typename T1,
                  typename T2,
                  typename T3,
                  typename T4,
                  typename T5,
                  typename T6,
                  typename T7,
                  typename T8>
        void get(char const * const column1, T1 & value1,
                 char const * const column2, T2 & value2,
                 char const * const column3, T3 & value3,
                 char const * const column4, T4 & value4,
                 char const * const column5, T5 & value5,
                 char const * const column6, T6 & value6,
                 char const * const column7, T7 & value7,
                 char const * const column8, T8 & value8)
        {
            store_value(column1, value1);
            store_value(column2, value2);
            store_value(column3, value3);
            store_value(column4, value4);
            store_value(column5, value5);
            store_value(column6, value6);
            store_value(column7, value7);
            store_value(column8, value8);
        }

    private:
        void store_value(char const * const column, double & value);
        void store_value(char const * const column, uint32_t & value);
        void store_value(char const * const column, uint64_t & value);
        // extract(epoch from datetime)::int8 from TIMESTAMP column type
        void store_value(char const * const column, time_t & value);
        // TEXT column type
        void store_value(char const * const column, std::string & value);
        void query_segment();

        PGconn * m_connection;
        PGresult * m_result;
        std::string m_query;
        uint32_t const m_limit;
        uint64_t m_offset;
        uint32_t m_inter_limit; 
        bool m_own_result;
};

#endif // QUERY_CACHE_HPP

