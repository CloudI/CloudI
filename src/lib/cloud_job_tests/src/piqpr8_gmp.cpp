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
#include <sstream>
#include <cmath>
#include <cstdlib>
#include <gmp.h>
#include "piqpr8_gmp.hpp"

// An implementation of the original Bailey–Borwein–Plouffe formula
// for the constant PI in hexadecimal using the GNU GMP library
// based on the implementation by David H. Bailey found at:
// http://www.experimentalmath.info/bbp-codes/piqpr8.c

#define TEMPORARY_FLOATS 2
#define TEMPORARY_INTEGERS 4
void series(bool const & abortTask, mpf_t & rop, unsigned long m,
            mpz_t * const & tmpI, mpf_t * const & tmpF,
            mpz_t const & sixteen, mpz_t const & digit, mpf_t const & epsilon)
{
    // temporary local variables
    mpf_t & tmp1 = tmpF[0];
    mpf_t & t = tmpF[1];
    mpz_t & tmp2 = tmpI[0];
    mpz_t & k = tmpI[1];
    mpz_t & p = tmpI[2];
    mpz_t & ak = tmpI[3];

    mpf_set_ui(rop, 0);
    mpz_set_ui(k, 0);

    while (mpz_cmp(k, digit) < 0) // k < digit
    {
        // p = id - k;
        mpz_sub(p, digit, k);

        // ak = 8 * k + m;
        mpz_set(ak, k);
        mpz_mul_ui(ak, ak, 8);
        mpz_add_ui(ak, ak, m);

        // t = expm (p, ak);
        mpz_powm(tmp2, sixteen, p, ak);
        mpf_set_z(t, tmp2);

        // s = s + t / ak;
        mpf_set_z(tmp1, ak);
        mpf_div(tmp1, t, tmp1);
        mpf_add(rop, rop, tmp1);

        // s = s - (int) s;
        mpf_floor(tmp1, rop);
        mpf_sub(rop, rop, tmp1);

        // k++
        mpz_add_ui(k, k, 1);

        if (abortTask)
            return;
    }

    // ak = 8 * k + m;
    mpz_set(ak, k);
    mpz_mul_ui(ak, ak, 8);
    mpz_add_ui(ak, ak, m);

    // t = pow (16., (double) (id - k)) / ak;
    mpf_set_z(tmp1, ak);
    mpf_ui_div(t, 1, tmp1);

    while (mpf_cmp(t, epsilon) >= 0) // t >= epsilon
    {
        // s = s + t;
        mpf_add(rop, rop, t);

        // s = s - (int) s;
        mpf_floor(tmp1, rop);
        mpf_sub(rop, rop, tmp1);

        // k++
        mpz_add_ui(k, k, 1);

        // p = id - k;
        mpz_sub(p, digit, k);

        // ak = 8 * k + m;
        mpz_set(ak, k);
        mpz_mul_ui(ak, ak, 8);
        mpz_add_ui(ak, ak, m);

        // t = pow (16., (double) (id - k)) / ak;
        mpz_pow_ui(tmp2, sixteen, mpz_get_ui(p));
        mpz_mul(tmp2, tmp2, ak);
        mpf_set_z(t, tmp2);
        mpf_ui_div(t, 1, t);

        if (abortTask)
            return;
    }
}

void set(std::string & dest, mpz_t const & x)
{
    char number[256];
    mpz_get_str(number, 10, x);
    dest.assign(number);
}

void set(std::string & dest, mpf_t const & x)
{
    char number[256];
    mp_exp_t exponent;
    mpf_get_str(number, &exponent, 10, 78, x);
    if (number[0] == '\0')
    {
        dest = "0e0";
    }
    else
    {
        std::ostringstream s;
        s << "." << number << "e" << exponent;
        dest = s.str();
    }
}

bool bbp_pi(bool const & abortTask,
            std::string const & digitIndex,
            uint32_t const digitStep,
            std::string & piSequence)
{
    unsigned long const mantissa_bits = 132;
    unsigned long const count_offset = 7;
    // settings above gives 32 hexadecimal digits
    unsigned long count = static_cast<unsigned long>(
        floor(log10(pow(2.0, static_cast<double>(mantissa_bits)))));
    count -= count_offset;

    // starting digit
    mpz_t digit;
    mpz_init(digit);
    if (mpz_set_str(digit, digitIndex.c_str(), 10) < 0)
        return false;
    mpz_sub_ui(digit, digit, 1); // subtract the 3 digit
    mpz_add_ui(digit, digit, digitStep);
    
    mpz_t tmpI[TEMPORARY_INTEGERS];
    for (size_t i = 0; i < (sizeof(tmpI) / sizeof(mpz_t)); ++i)
        mpz_init(tmpI[i]);
    mpf_t tmpF[TEMPORARY_FLOATS];
    for (size_t i = 0; i < (sizeof(tmpF) / sizeof(mpf_t)); ++i)
        mpf_init2(tmpF[i], mantissa_bits);
    
    // determine epsilon based on the number of digits required
    mpf_t epsilon;
    mpf_init2(epsilon, mantissa_bits);
    mpf_set_ui(epsilon, 10);
    mpf_pow_ui(epsilon, epsilon, count + count_offset);
    mpf_ui_div(epsilon, 1, epsilon);

    // integer constant
    mpz_t sixteen;
    mpz_init(sixteen);
    mpz_set_ui(sixteen, 16);

    // determine the series
    mpf_t s1, s2, s3, s4;
    mpf_init2(s1, mantissa_bits);
    mpf_init2(s2, mantissa_bits);
    mpf_init2(s3, mantissa_bits);
    mpf_init2(s4, mantissa_bits);
    series(abortTask, s1, 1, tmpI, tmpF, sixteen, digit, epsilon);
    if (abortTask)
        return false;
    series(abortTask, s2, 4, tmpI, tmpF, sixteen, digit, epsilon);
    if (abortTask)
        return false;
    series(abortTask, s3, 5, tmpI, tmpF, sixteen, digit, epsilon);
    if (abortTask)
        return false;
    series(abortTask, s4, 6, tmpI, tmpF, sixteen, digit, epsilon);
    if (abortTask)
        return false;

    // pid = 4. * s1 - 2. * s2 - s3 - s4;
    mpf_mul_ui(s1, s1, 4);
    mpf_mul_ui(s2, s2, 2);
    mpf_t result;
    mpf_init2(result, mantissa_bits);
    mpf_sub(result, s1, s2);
    mpf_sub(result, result, s3);
    mpf_sub(result, result, s4);

    // pid = pid - (int) pid + 1.;
    mpf_t & tmp1 = tmpF[0];
    mpf_floor(tmp1, result);
    mpf_sub(result, result, tmp1);
    mpf_add_ui(result, result, 1);
    mpf_abs(result, result);

    // output the result
    char resultStr[256];
    mp_exp_t exponent;
    mpf_get_str(resultStr, &exponent, 16, 254, result);
    resultStr[count + 1] = '\0'; // cut off any erroneous bits
    piSequence.assign(&resultStr[1]);

    // cleanup
    for (size_t i = 0; i < (sizeof(tmpI) / sizeof(mpz_t)); ++i)
        mpz_clear(tmpI[i]);
    for (size_t i = 0; i < (sizeof(tmpF) / sizeof(mpf_t)); ++i)
        mpf_clear(tmpF[i]);
    mpz_clear(digit);
    mpf_clear(epsilon);
    mpz_clear(sixteen);
    mpf_clear(s1);
    mpf_clear(s2);
    mpf_clear(s3);
    mpf_clear(s4);
    mpf_clear(result);
    return true;
}

