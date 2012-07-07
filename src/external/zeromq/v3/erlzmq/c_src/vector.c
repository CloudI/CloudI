/* -*- coding: utf-8; Mode: C; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
 * ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
 *
 * BSD LICENSE
 *
 * Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *     * All advertising materials mentioning features or use of this
 *       software must display the following acknowledgment:
 *         This product includes software developed by Michael Truog
 *     * The name of the author may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
#include "vector.h"
#include <string.h>
#include <stdlib.h>

static size_t greater_pow2(size_t size);

int vector_initialize(vector_t * v,
                      size_t allocation_increment,
                      size_t allocation_size,
                      size_t allocation_size_max,
                      size_t element_size)
{
    v->allocation_size = greater_pow2(allocation_size);
    v->allocation_increment = greater_pow2(allocation_increment);
    v->allocation_size_max = allocation_size_max;
    v->p = malloc(v->allocation_size);
    v->element_size = element_size;
    v->count = 0;
    if (v->p)
        return VECTOR_SUCCESS;
    else
        return VECTOR_FAILURE;
}

void vector_destroy(vector_t * v)
{
    free(v->p);
}

int vector_reserve(vector_t * v, size_t count)
{
    size_t const size = v->element_size * count;
    size_t new_size = v->allocation_size;
    void * tmp;
    if (size < v->allocation_size)
        return VECTOR_SUCCESS;
    if (size > v->allocation_size_max)
        return VECTOR_FAILURE;
    if (v->allocation_increment == 0) {
        while (size > new_size)
            new_size <<= 1;
    }
    else {
        while (size > new_size)
            new_size += v->allocation_increment;
    }
    if (new_size > v->allocation_size_max)
        new_size = v->allocation_size_max;
    tmp = realloc(v->p, new_size);
    if (! tmp)
        return VECTOR_FAILURE;
    v->p = tmp;
    v->allocation_size = new_size;
    return VECTOR_SUCCESS;
}

int vector_copy(vector_t * v_dst, vector_t * v_src,
                size_t i_src, size_t count_src, size_t i_dst)
{
    if (v_dst->element_size != v_src->element_size)
        return VECTOR_FAILURE;
    if (count_src == 0)
        count_src = v_src->count;
    if (vector_reserve(v_dst, i_dst + count_src) == VECTOR_FAILURE)
        return VECTOR_FAILURE;
    memcpy(&(((char *) v_dst->p)[i_dst * v_dst->element_size]),
           &(((char *) v_src->p)[i_src * v_dst->element_size]),
           count_src * v_dst->element_size);
    if ((i_dst + count_src) > v_dst->count)
        v_dst->count = i_dst + count_src;
    return VECTOR_SUCCESS;
}

int vector_move(vector_t * v, size_t i_dst, size_t i_src, size_t count_src)
{
    if (count_src == 0)
        count_src = v->count - i_src;
    if (vector_reserve(v, i_dst + count_src) == VECTOR_FAILURE)
        return VECTOR_FAILURE;
    memmove(&(((char *) v->p)[i_dst * v->element_size]),
            &(((char *) v->p)[i_src * v->element_size]),
            count_src * v->element_size);
    if ((i_dst + count_src) > v->count)
        v->count = i_dst + count_src;
    return VECTOR_SUCCESS;
}

int vector_append_element(vector_t * v, void * p, size_t size)
{
    if (v->element_size != size)
        return VECTOR_FAILURE;
    if (vector_reserve(v, v->count + 1) == VECTOR_FAILURE)
        return VECTOR_FAILURE;
    memcpy(&(((char *) v->p)[v->count * v->element_size]), p, size);
    ++(v->count);
    return VECTOR_SUCCESS;
}

int vector_remove(vector_t * v, size_t i)
{
    if ((i + 1) == v->count) {
        --(v->count);
        return VECTOR_SUCCESS;
    }
    else {
        if (vector_move(v, i, i + 1, 0) == VECTOR_FAILURE)
            return VECTOR_FAILURE;
        --(v->count);
        return VECTOR_SUCCESS;
    }
}

/* find a value >= totalSize as a power of 2
 */
static size_t greater_pow2(size_t total_size)
{
    int bits = 0;
    size_t div2 = total_size;
    size_t value;
    for (; div2 > 1; div2 >>= 1)
        bits++;
    value = (1 << bits);
    if (value == total_size)
        return value;
    else
        return (value << 1);
}

