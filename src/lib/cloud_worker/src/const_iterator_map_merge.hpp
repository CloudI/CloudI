// -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
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
#ifndef CONST_ITERATOR_MAP_MERGE_HPP
#define CONST_ITERATOR_MAP_MERGE_HPP

template <typename CONTENTS, typename T1, typename T2>
class const_iterator_map_merge
{
    public:
        const_iterator_map_merge(T1 const & t1, T2 const & t2) :
            m_t1begin(t1.begin()), m_t1itr(t1.begin()), m_t1end(t1.end()),
            m_t2begin(t2.begin()), m_t2itr(t2.begin()), m_t2end(t2.end()) {}
        CONTENTS const operator *() const
        {
            if (m_t1itr == m_t1end)
                return m_t2itr->second.get();
            else
                return m_t1itr->second.get();
        }
        const_iterator_map_merge operator ++(int) // postfix
        {
            const_iterator_map_merge<CONTENTS, T1, T2> o(*this);
            if (m_t1itr == m_t1end)
                ++m_t2itr;
            else
                ++m_t1itr;
            return o;
        }
        const_iterator_map_merge & operator ++()  // prefix
        {
            if (m_t1itr == m_t1end)
                ++m_t2itr;
            else
                ++m_t1itr;
            return *this;
        }
        friend bool operator ==(const_iterator_map_merge const & lhs,
                                const_iterator_map_merge const & rhs)
        {
            return (lhs.m_t1itr == rhs.m_t1itr && lhs.m_t2itr == rhs.m_t2itr);
        }
        friend bool operator !=(const_iterator_map_merge const & lhs,
                                const_iterator_map_merge const & rhs)
        {
            return (lhs.m_t1itr != rhs.m_t1itr || lhs.m_t2itr != rhs.m_t2itr);
        }
        void set_begin()
        {
            // sets the iterators back to their state in the constructor
            m_t1itr = m_t1begin;
            m_t2itr = m_t2begin;
        }
        void set_end()
        {
            m_t1itr = m_t1end;
            m_t2itr = m_t2end;
        }
    private:
        typename T1::const_iterator m_t1begin;
        typename T1::const_iterator m_t1itr;
        typename T1::const_iterator m_t1end;
        typename T2::const_iterator m_t2begin;
        typename T2::const_iterator m_t2itr;
        typename T2::const_iterator m_t2end;
};

#endif // CONST_ITERATOR_MAP_MERGE_HPP

