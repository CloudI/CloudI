//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// MIT License
//
// Copyright (c) 2009-2017 Michael Truog <mjtruog at gmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
#if __cplusplus >= 201103L
// C++11 deprecated std::auto_ptr
// C++11 added std::unique_ptr
#define CXX11
#endif
template <typename T>
class copy_ptr
{
    public:
        typedef T element_type;

        explicit copy_ptr(element_type * p = 0) throw() : m_p(p) {}
        ~copy_ptr() { delete m_p; }

        // handle const & conversion

        copy_ptr(copy_ptr const & o) throw() :
            m_p(const_cast<copy_ptr &>(o).release()) {}
        template <typename RELATED>
        copy_ptr(copy_ptr<RELATED> const & o) throw() :
            m_p(const_cast<copy_ptr<RELATED> &>(o).release()) {}
        copy_ptr & operator =(copy_ptr const & o) throw()
        {
            reset(const_cast<copy_ptr &>(o).release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(copy_ptr<RELATED> const & o) throw()
        {
            reset(const_cast<copy_ptr<RELATED> &>(o).release());
            return *this;
        }
#ifdef CXX11
        copy_ptr(std::unique_ptr<T> const & o) throw() :
            m_p(const_cast<std::unique_ptr<T> &>(o).release()) {}
        template <typename RELATED>
        copy_ptr(std::unique_ptr<RELATED> const & o) throw() :
            m_p(const_cast<std::unique_ptr<RELATED> &>(o).release()) {}
        copy_ptr & operator =(std::unique_ptr<T> const & o) throw()
        {
            reset(const_cast<std::unique_ptr<T> &>(o).release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(std::unique_ptr<RELATED> const & o) throw()
        {
            reset(const_cast<std::unique_ptr<RELATED> &>(o).release());
            return *this;
        }
#else
        copy_ptr(std::auto_ptr<T> const & o) throw() :
            m_p(const_cast<std::auto_ptr<T> &>(o).release()) {}
        template <typename RELATED>
        copy_ptr(std::auto_ptr<RELATED> const & o) throw() :
            m_p(const_cast<std::auto_ptr<RELATED> &>(o).release()) {}
        copy_ptr & operator =(std::auto_ptr<T> const & o) throw()
        {
            reset(const_cast<std::auto_ptr<T> &>(o).release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(std::auto_ptr<RELATED> const & o) throw()
        {
            reset(const_cast<std::auto_ptr<RELATED> &>(o).release());
            return *this;
        }
#endif

        // handle non-const & conversion
        
        copy_ptr(copy_ptr & o) throw() :
            m_p(o.release()) {}
        template <typename RELATED>
        copy_ptr(copy_ptr<RELATED> & o) throw() :
            m_p(o.release()) {}
        copy_ptr & operator =(copy_ptr & o) throw()
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
#ifdef CXX11
        copy_ptr(std::unique_ptr<T> & o) throw() :
            m_p(o.release()) {}
        template <typename RELATED>
        copy_ptr(std::unique_ptr<RELATED> & o) throw() :
            m_p(o.release()) {}
        copy_ptr & operator =(std::unique_ptr<T> & o) throw()
        {
            reset(o.release());
            return *this;
        }
        template <typename RELATED>
        copy_ptr & operator =(std::unique_ptr<RELATED> & o) throw()
        {
            reset(o.release());
            return *this;
        }
#else
        copy_ptr(std::auto_ptr<T> & o) throw() :
            m_p(o.release()) {}
        template <typename RELATED>
        copy_ptr(std::auto_ptr<RELATED> & o) throw() :
            m_p(o.release()) {}
        copy_ptr & operator =(std::auto_ptr<T> & o) throw()
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
#endif

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
#ifdef CXX11
#undef CXX11
#endif

#endif // COPY_PTR_HPP

