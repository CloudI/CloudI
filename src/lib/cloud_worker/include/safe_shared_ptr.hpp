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
#ifndef SAFE_SHARED_PTR_HPP
#define SAFE_SHARED_PTR_HPP

#include <memory>
#include <cassert>
#include "boost_thread.hpp"

// thread-safe reference counting that handles deletion
// with storage and lock contention overhead.
template <typename T>
class safe_shared_ptr
{
    public:
        typedef T element_type;

        explicit safe_shared_ptr(element_type * p = 0) throw() :
            m_pShared(new shared_data()), m_p(p) {}
        safe_shared_ptr(std::auto_ptr<T> const & o) throw() :
            m_pShared(new shared_data()),
            m_p(const_cast<std::auto_ptr<T> &>(o).release()) {}
        safe_shared_ptr(safe_shared_ptr const & o) throw() :
            m_pShared(const_cast<safe_shared_ptr &>(o).m_pShared),
            m_p(const_cast<safe_shared_ptr &>(o).m_p)
        {
            m_pShared->increment();
        }
        template <typename RELATED>
        safe_shared_ptr(std::auto_ptr<RELATED> const & o) throw() :
            m_pShared(new shared_data()),
            m_p(const_cast<std::auto_ptr<RELATED> &>(o).release()) {}
        template <typename RELATED>
        safe_shared_ptr(safe_shared_ptr<RELATED> const & o) throw() :
            m_pShared(const_cast<safe_shared_ptr<RELATED> &>(o).m_pShared),
            m_p(const_cast<safe_shared_ptr<RELATED> &>(o).m_p)
        {
            m_pShared->increment();
        }
        ~safe_shared_ptr()
        {
            if (m_pShared->decrement())
            {
                delete m_pShared;
                delete m_p;
            }
        }
        element_type & operator *() const throw()
        {
            assert(m_p != 0);
            return *const_cast<T *>(m_p);
        }
        element_type * operator ->() const throw()
        {
            assert(m_p != 0);
            return const_cast<T *>(m_p);
        }
        element_type * get() const throw() { return const_cast<T *>(m_p); }
    private:
        // disable the assignment operators
        safe_shared_ptr & operator =(
            safe_shared_ptr const & a) throw();
        template <typename RELATED>
        safe_shared_ptr & operator =(
            safe_shared_ptr<RELATED> const & a) throw();

        class shared_data
        {
            public:
                shared_data() : m_references(1) {}
                void increment() volatile
                {
                    boost::lock_guard<boost::mutex> lock(
                        const_cast<boost::mutex &>(m_mutex));
                    ++m_references;
                }
                bool decrement() volatile
                {
                    boost::lock_guard<boost::mutex> lock(
                        const_cast<boost::mutex &>(m_mutex));
                    // handle proper usage, safe_shared_ptr should not
                    // be stored as a C pointer or a C++ reference.
                    // so, return whether deletion should occur.
                    if (--m_references == 0)
                        return true;
                    return false;
                }
            private:
                volatile boost::mutex m_mutex;
                volatile size_t m_references;
        };

        volatile shared_data * m_pShared;
        volatile T * m_p;
};

#endif // SAFE_SHARED_PTR_HPP

