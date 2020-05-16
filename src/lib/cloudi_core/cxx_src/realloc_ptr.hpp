//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2009-2020 Michael Truog <mjtruog at protonmail dot com>
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
// (no C++ realloc exists, std::vector could be used instead but control
//  over the allocations would be lost).
template <typename T>
class realloc_ptr
{
public:
    typedef T element_type;

    explicit realloc_ptr(size_t const size_initial, size_t const size_max) :
        m_size_initial(greater_pow2(size_initial)),
        m_size_max(greater_pow2(size_max)),
        m_size(m_size_initial),
        m_p(reinterpret_cast<T *>(::malloc(m_size_initial * sizeof(T))))
    {
    }

    ~realloc_ptr() throw()
    {
        if (m_p)
            ::free(m_p);
    }

    T * release(bool const last = false) throw()
    {
        T * t = m_p;
        if (last)
            m_p = 0;
        else
            m_p = reinterpret_cast<T *>(::malloc(m_size_initial * sizeof(T)));
        return t;
    }

    size_t size() const
    {
        return m_size;
    }

    T & operator [](size_t const i) const
    {
        assert(i < m_size);
        return m_p[i];
    }

    T * get() const
    {
        return m_p;
    }

    template <typename R>
    R * get() const
    {
        assert(sizeof(R) == sizeof(T));
        return reinterpret_cast<R *>(m_p);
    }

    bool copy(realloc_ptr & src, size_t const dst_i = 0)
    {
        assert(&src != this);
        if (! reserve(dst_i + src.m_size))
            return false;
        ::memcpy(&(m_p[dst_i]), src.m_p, src.m_size * sizeof(T));
        return true;
    }

    bool copy(realloc_ptr & src,
              size_t const src_n, size_t const dst_i)
    {
        assert(&src != this);
        if (! reserve(dst_i + src_n))
            return false;
        ::memcpy(&(m_p[dst_i]), src.m_p, src_n * sizeof(T));
        return true;
    }

    bool copy(realloc_ptr & src,
              size_t const src_i, size_t const src_n, size_t const dst_i)
    {
        assert(&src != this);
        if (! reserve(dst_i + src_n))
            return false;
        ::memcpy(&(m_p[dst_i]), &(src.m_p[src_i]), src_n * sizeof(T));
        return true;
    }

    bool move(size_t const src_i, size_t const src_n, size_t const dst_i)
    {
        if (! reserve(dst_i + src_n))
            return false;
        ::memmove(&(m_p[dst_i]), &(m_p[src_i]), src_n * sizeof(T));
        return true;
    }

    bool grow()
    {
        size_t const size_new = m_size << 1;
        if (size_new > m_size_max)
            return false;
        T * tmp = reinterpret_cast<T *>(::realloc(m_p, size_new * sizeof(T)));
        if (! tmp)
            return false;
        m_p = tmp;
        m_size = size_new;
        return true;
    }

    bool reserve(size_t const size)
    {
        if (size < m_size)
            return true;
        if (size > m_size_max)
            return false;
        size_t size_new = m_size;
        while (size >= size_new)
            size_new <<= 1;
        T * tmp = reinterpret_cast<T *>(::realloc(m_p, size_new * sizeof(T)));
        if (! tmp)
            return false;
        m_p = tmp;
        m_size = size_new;
        return true;
    }

private:
    // find a value >= size_total as a power of 2
    size_t greater_pow2(size_t n)
    {
        size_t const size_total = n * sizeof(T);
        int bits = 0;
        for (size_t div2 = size_total; div2 > 1; div2 >>= 1)
            bits++;
        size_t const value = (1 << bits);
        if (value == size_total)
            return value;
        else
            return (value << 1);
    }

    size_t const m_size_initial;
    size_t const m_size_max;
    size_t m_size;
    T * m_p;

    realloc_ptr(realloc_ptr const &);
    realloc_ptr & operator =(realloc_ptr const &);

};

#endif // REALLOC_PTR_HPP

