//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
#ifndef COPY_PTR_HPP
#define COPY_PTR_HPP

#include <memory>
#include "assert.hpp"

// The functionality of std::auto_ptr,
// with a goal similar to the c++0x unique_ptr
// (which copies rvalue references but not lvalue references).
// copy_ptr copies both rvalue references and lvalue references.
// Usage of copy_ptr assumes the object is only ever referenced by
// the newest copy of the object, and that assumption should be correct
// for general STL containers, because of efficiency concerns that minimize
// copying.  Well designed (i.e., functioning) code will not retain
// the state of a copy_ptr, it will only pass it to other code
// (scopes) until it goes out of scope.  This design practice is common
// to keep large scale systems manageable, because it minimizes side-effects.
// The system is then easier to debug, test and maintain (so the code
// should live longer).
//
// Copying lvalue references is normally seen as unsafe
// (in the context of random code), so be aware that copy_ptr usage
// implies safe usage of the copy constructor.
//
template <typename T>
class copy_ptr
{
    public:
        typedef T element_type;

        explicit copy_ptr(element_type * p = 0) throw() : m_p(p) {}
        ~copy_ptr() { delete m_p; }

        // handle const & conversion

        copy_ptr(std::auto_ptr<T> const & o) throw() :
            m_p(const_cast<std::auto_ptr<T> &>(o).release()) {}
        copy_ptr(copy_ptr const & o) throw() :
            m_p(const_cast<copy_ptr &>(o).release()) {}
        template <typename RELATED>
        copy_ptr(std::auto_ptr<RELATED> const & o) throw() :
            m_p(const_cast<std::auto_ptr<RELATED> &>(o).release()) {}
        template <typename RELATED>
        copy_ptr(copy_ptr<RELATED> const & o) throw() :
            m_p(const_cast<copy_ptr<RELATED> &>(o).release()) {}
        copy_ptr & operator =(std::auto_ptr<T> const & o) throw()
        {
            reset(const_cast<std::auto_ptr<T> &>(o).release());
            return *this;
        }
        copy_ptr & operator =(copy_ptr const & o) throw()
        {
            reset(const_cast<copy_ptr &>(o).release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(std::auto_ptr<RELATED> const & o) throw()
        {
            reset(const_cast<std::auto_ptr<RELATED> &>(o).release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(copy_ptr<RELATED> const & o) throw()
        {
            reset(const_cast<copy_ptr<RELATED> &>(o).release());
            return *this;
        }

        // handle non-const & conversion
        
        copy_ptr(std::auto_ptr<T> & o) throw() :
            m_p(o.release()) {}
        copy_ptr(copy_ptr & o) throw() :
            m_p(o.release()) {}
        template <typename RELATED>
        copy_ptr(std::auto_ptr<RELATED> & o) throw() :
            m_p(o.release()) {}
        template <typename RELATED>
        copy_ptr(copy_ptr<RELATED> & o) throw() :
            m_p(o.release()) {}
        copy_ptr & operator =(std::auto_ptr<T> & o) throw()
        {
            reset(o.release());
            return *this;
        }
        copy_ptr & operator =(copy_ptr & o) throw()
        {
            reset(o.release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(std::auto_ptr<RELATED> & o) throw()
        {
            reset(o.release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(copy_ptr<RELATED> & o) throw()
        {
            reset(o.release());
            return *this;
        }

        // operations

        element_type & operator *() const throw()
        {
            assert(m_p != 0);
            return *m_p;
        }
        element_type * operator ->() const throw()
        {
            assert(m_p != 0);
            return m_p;
        }
        element_type * get() const throw() { return m_p; }
        operator bool () const throw()
        {
            return (m_p != 0);
        }
        bool operator! () const throw()
        {
            return (m_p == 0);
        }
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
                delete m_p;
                m_p = p;
            }
        }
    private:
        T * m_p;
};

#endif // COPY_PTR_HPP

