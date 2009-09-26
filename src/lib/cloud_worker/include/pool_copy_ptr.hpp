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
#ifndef POOL_COPY_PTR_HPP
#define POOL_COPY_PTR_HPP

#include <boost/pool/object_pool.hpp>
#include "boost_thread.hpp"
#include <cassert>

// thread-safe wrapper for boost::object_pool<T, UserAllocator>
template <
typename T,
typename UserAllocator =
    boost::default_user_allocator_new_delete
>
class safe_object_pool
{
    public:
        typedef T element_type;

        element_type * malloc()
        {
            boost::lock_guard<boost::mutex> lock(
                const_cast<boost::mutex &>(m_mutex));
            return m_pool.malloc();
        }
        void free(element_type * const chunk)
        {
            boost::lock_guard<boost::mutex> lock(
                const_cast<boost::mutex &>(m_mutex));
            m_pool.free(chunk);
        }
        void destroy(element_type * const chunk)
        {
            boost::lock_guard<boost::mutex> lock(
                const_cast<boost::mutex &>(m_mutex));
            m_pool.destroy(chunk);
        }
    private:
        boost::object_pool<T, UserAllocator> m_pool;
        volatile boost::mutex m_mutex;
};

// copy_ptr functionality with object memory pool allocation
template <
typename T,
typename POOL_T = 
    boost::object_pool<T, boost::default_user_allocator_new_delete>
>
class pool_copy_ptr
{
    public:
        typedef T element_type;

        explicit pool_copy_ptr(POOL_T & pool) throw() :
            m_pool(pool), m_p(0), m_constructed(false) {}
        ~pool_copy_ptr()
        {
            if (m_p)
            {
                if (m_constructed)
                    m_pool.destroy(m_p);
                else
                    m_pool.free(m_p);
            }
        }

        // allocate and construct the object
        template <typename ... ARG>
        void operator () (ARG&&... a)
        {
            
            reset(m_pool.malloc());
            if (m_p)
            {
                try
                {
                    new (m_p) element_type(std::forward<ARG>(a)...);
                }
                catch (...)
                {
                    // a constructor raised an exception
                    m_pool.free(m_p);
                    m_p = 0;
                    throw;
                }
                m_constructed = true;
            }
            // need to know if construction failed ASAP
            assert(m_p != 0 && m_constructed == true);
        }

        // handle const & conversion
      
        pool_copy_ptr(pool_copy_ptr const & o) throw() :
            m_pool(const_cast<pool_copy_ptr &>(o).m_pool),
            m_p(const_cast<pool_copy_ptr &>(o).release()),
            m_constructed(o.m_constructed) {}
        template <typename RELATED>
        pool_copy_ptr(pool_copy_ptr<RELATED> const & o) throw() :
            m_pool(const_cast<pool_copy_ptr<RELATED> &>(o).m_pool),
            m_p(const_cast<pool_copy_ptr<RELATED> &>(o).release()),
            m_constructed(o.m_constructed) {}
        pool_copy_ptr & operator =(pool_copy_ptr const & o) throw()
        {
            m_pool = &(const_cast<pool_copy_ptr &>(o).m_pool);
            reset(const_cast<pool_copy_ptr &>(o).release());
            m_constructed = o.m_constructed;
            return *this;
        }
        template <typename RELATED>
        pool_copy_ptr & operator =(pool_copy_ptr<RELATED> const & o) throw()
        {
            m_pool = &(const_cast<pool_copy_ptr<RELATED> &>(o).m_pool);
            reset(const_cast<pool_copy_ptr<RELATED> &>(o).release());
            m_constructed = o.m_constructed;
            return *this;
        }

        // handle non-const & conversion

        pool_copy_ptr(pool_copy_ptr & o) throw() :
            m_pool(o.m_pool), m_p(o.release()),
            m_constructed(o.m_constructed) {}
        template <typename RELATED>
        pool_copy_ptr(pool_copy_ptr<RELATED> & o) throw() :
            m_pool(o.m_pool), m_p(o.release()),
            m_constructed(o.m_constructed) {}
        pool_copy_ptr & operator =(pool_copy_ptr & o) throw()
        {
            m_pool = &(o.m_pool);
            reset(o.release());
            m_constructed = o.m_constructed;
            return *this;
        }
        template <typename RELATED>
        pool_copy_ptr & operator =(pool_copy_ptr<RELATED> & o) throw()
        {
            m_pool = &(o.m_pool);
            reset(o.release());
            m_constructed = o.m_constructed;
            return *this;
        }

        // operations

        element_type & operator *() const throw()
        {
            assert(m_p != 0 && m_constructed == true);
            return *m_p;
        }
        element_type * operator ->() const throw()
        {
            assert(m_p != 0 && m_constructed == true);
            return m_p;
        }
        element_type * get() const throw() { return m_p; }
        operator bool () const throw()
        {
            return (m_p != 0 && m_constructed == true);
        }
        bool operator! () const throw()
        {
            return (m_p == 0 || m_constructed == false);
        }
    private:
        element_type * release() throw()
        {
            element_type * t = m_p;
            m_p = 0;
            return t;
        }
        void reset(element_type * p = 0) throw()
        {
            if (p != m_p)
            {
                if (m_p)
                {
                    if (m_constructed)
                        m_pool.destroy(m_p);
                    else
                        m_pool.free(m_p);
                }
                m_p = p;
                m_constructed = false;
            }
        }

        POOL_T & m_pool;
        T * m_p;
        bool m_constructed;
};

// C++0x feature not yet implemented in g++ 4.3.2
//template <typename T>
//using safe_pool_copy_ptr = pool_copy_ptr< T, safe_object_pool<T> >;

#endif // POOL_COPY_PTR_HPP

