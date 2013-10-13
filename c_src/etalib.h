// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#ifndef ETALIB_H
#define ETALIB_H

#include "erl_nif.h"
#include <stdbool.h>

typedef struct {
    ERL_NIF_TERM    atom_nan;
    ERL_NIF_TERM    atom_ok;
    ERL_NIF_TERM    atom_error;
} etalib_st;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM make_ok(etalib_st* st, ErlNifEnv* env, ERL_NIF_TERM data);
ERL_NIF_TERM make_error(etalib_st* st, ErlNifEnv* env, const char* error);

bool has_bad_arguments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
double* construct_array_from_list(ErlNifEnv* env, ERL_NIF_TERM opts, unsigned* len);
double extract_option(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, double defValue);

ERL_NIF_TERM ta_sma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_rsi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif // Included ETALIB_H
