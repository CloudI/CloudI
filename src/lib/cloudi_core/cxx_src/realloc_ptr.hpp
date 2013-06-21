//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2009-2011, Michael Truog
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
#ifndef REALLOC_PTR_HPP
#define REALLOC_PTR_HPP

#include <cstdlib>
#include <cstring>
#include "assert.hpp"

// the functionality of boost::scoped_array, however,
// use malloc/realloc/free to resize the array by powers of two.
// using realloc may be considered bad, but the hope is that the allocation
// might be extended in memory, rather than a completely new allocation.
// that is why the C++ new/delete are not used
// (currently no C++ realloc exists).
template <typename T>
class realloc_ptr
{
public:
    typedef T element_type;

    explicit realloc_ptr(size_t initialSize, size_t maxSize) :
        m_initialSize(greater_pow2(initialSize)),
        m_size(m_initialSize),
        m_maxSize(greater_pow2(maxSize)),
        m_p(reinterpret_cast<T *>(malloc(m_initialSize * sizeof(T)))) {}

    ~realloc_ptr() throw() { free(m_p); }

    T * release() throw()
    {
        T * t = m_p;
        m_p = reinterpret_cast<T *>(malloc(m_initialSize * sizeof(T)));
        return t;
    }

    size_t size() const { return m_size; }

    T & operator [](size_t i) const
    {
        assert(i < m_size);
        return m_p[i];
    }

    T * get() const { return m_p; }

    template <typename R>
    R * get() const
    {
        assert(sizeof(R) == sizeof(T));
        return reinterpret_cast<R *>(m_p);
    }

    bool copy(realloc_ptr & src, size_t iDst = 0)
    {
        assert(&src != this);
        if (! reserve(iDst + src.m_size))
            return false;
        memcpy(&(m_p[iDst]), src.m_p, src.m_size * sizeof(T));
        return true;
    }

    bool copy(realloc_ptr & src, size_t nSrc, size_t iDst)
    {
        assert(&src != this);
        if (! reserve(iDst + nSrc))
            return false;
        memcpy(&(m_p[iDst]), src.m_p, nSrc * sizeof(T));
        return true;
    }

    bool copy(realloc_ptr & src, size_t iSrc, size_t nSrc, size_t iDst)
    {
        assert(&src != this);
        if (! reserve(iDst + nSrc))
            return false;
        memcpy(&(m_p[iDst]), &(src.m_p[iSrc]), nSrc * sizeof(T));
        return true;
    }

    bool move(size_t iSrc, size_t nSrc, size_t iDst)
    {
        if (! reserve(iDst + nSrc))
            return false;
        memmove(&(m_p[iDst]), &(m_p[iSrc]), nSrc * sizeof(T));
        return true;
    }

    bool grow()
    {
        size_t const newSize = m_size << 1;
        if (newSize > m_maxSize)
            return false;
        T * tmp = reinterpret_cast<T *>(realloc(m_p, newSize * sizeof(T)));
        if (! tmp)
            return false;
        m_p = tmp;
        m_size = newSize;
        return true;
    }

    bool reserve(size_t size)
    {
        if (size < m_size)
            return true;
        if (size > m_maxSize)
            return false;
        size_t newSize = m_size;
        while (size >= newSize)
            newSize <<= 1;
        T * tmp = reinterpret_cast<T *>(realloc(m_p, newSize * sizeof(T)));
        if (! tmp)
            return false;
        m_p = tmp;
        m_size = newSize;
        return true;
    }

private:
    // find a value >= totalSize as a power of 2
    size_t greater_pow2(size_t n)
    {
        size_t const totalSize = n * sizeof(T);
        int bits = 0;
        for (size_t div2 = totalSize; div2 > 1; div2 >>= 1)
            bits++;
        size_t const value = (1 << bits);
        if (value == totalSize)
            return value;
        else
            return (value << 1);
    }

    size_t const m_initialSize;
    size_t m_size;
    size_t const m_maxSize;
    T * m_p;

    realloc_ptr(realloc_ptr const &);
    realloc_ptr & operator =(realloc_ptr const &);

};

#endif // REALLOC_PTR_HPP

