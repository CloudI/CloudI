#ifndef GEPD_PCHAR_LEN_T_H
#define GEPD_PCHAR_LEN_T_H

#include <stdint.h>

/* Memory Usage Concerns
 *
 * pchar_len_t  return values have free() called on the pointer
 *                after the contents has been processed when using pchar_len_t.
 *                To have more explicit control of this, use either
 *                pchar_len_t_free or pchar_len_t_nofree.
 * char *       return values do not have free() called on the pointer
 *                when using pchar. To have more explicit control of this,
 *                use either pchar_free or pchar_nofree.
 *
 * This is done to facilitate different usage which is common to both.
 * If constant strings (char const *) need to be returned, then the return
 * value can be char * which becomes a list of integers in Erlang
 * (an Erlang "string").  Any heap allocated memory should be passed within
 * a pchar_len_t and should use malloc to allocate the pchar variable.
 * The pchar_len_t return value becomes a binary in Erlang.  Any usage that
 * deviates from these typical scenarios should make sure to use the
 * "_free" or "_nofree" macro type declaration suffixes for pchar or pchar_len_t
 * (to make the memory usage explicit and avoid memory leaks or segfaults).
 *
 * GEPD can be easily extended to have separate macro type declarations for
 * new/delete if necessary.
 */
typedef struct pchar_len_t {
    char * pchar;
    uint32_t length;
} pchar_len_t;

#endif /* GEPD_PCHAR_LEN_T_H */
