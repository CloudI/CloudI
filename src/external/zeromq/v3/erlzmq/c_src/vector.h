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
#ifndef VECTOR_H
#define VECTOR_H

#include <stddef.h>

typedef struct {

  size_t allocation_increment;
  size_t allocation_size;
  size_t allocation_size_max;
  void * p;

  size_t element_size;
  size_t count;

} vector_t;

#define vector_initialize_linear(TYPE, V, COUNT_INCR, COUNT, COUNT_MAX) \
    vector_initialize(V, COUNT_INCR * sizeof(TYPE), COUNT * sizeof(TYPE), \
                      COUNT_MAX * sizeof(TYPE), sizeof(TYPE))
#define vector_initialize_pow2(TYPE, V, COUNT, COUNT_MAX) \
    vector_initialize(V, 0, COUNT * sizeof(TYPE), \
                      COUNT_MAX * sizeof(TYPE), sizeof(TYPE))
int vector_initialize(vector_t * v,
                      size_t allocation_increment,
                      size_t allocation_size,
                      size_t allocation_size_max,
                      size_t element_size);
void vector_destroy(vector_t * v);
int vector_reserve(vector_t * v, size_t count);
#define vector_copy_all(DST, SRC)    vector_copy(DST, SRC, 0, 0, 0)
int vector_copy(vector_t * v_dst, vector_t * v_src,
                size_t i_src, size_t count_src, size_t i_dst);
int vector_move(vector_t * v, size_t i_dst, size_t i_src, size_t count_src);
#define vector_append(TYPE, V, OBJ) \
    vector_append_element(V, OBJ, sizeof(TYPE))
int vector_append_element(vector_t * v, void * p, size_t size);
int vector_remove(vector_t * v, size_t i);
#define vector_has(V, I)             ((I) < (V)->count)
#define vector_p(TYPE, V)            ((TYPE *) ((V)->p))
#define vector_get(TYPE, V, I)       (&(vector_p(TYPE, V)[I]))
#define vector_count(V)              ((V)->count)
#define vector_size(V)               ((V)->allocation_size)

#define VECTOR_FAILURE -1
#define VECTOR_SUCCESS  0

#endif /* VECTOR_H */

