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

typedef struct {
    ErlNifEnv*  env;
    etalib_st*  atoms;
    double*    inValues;
    double*    outValues;
    ERL_NIF_TERM* outTerms;
    
    unsigned    inLen;
    int         startIdx;
    int         endIdx;
    int         optInTimePeriod;
    int  outBegIdx;
    int  outNBElement;

} EtaStruct;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM eta_populate_output(EtaStruct* e);
void eta_init(EtaStruct* e, ErlNifEnv* env, const ERL_NIF_TERM argv[]);
void eta_destroy(EtaStruct* e);
bool has_bad_arguments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
double* construct_array_from_list(ErlNifEnv* env, ERL_NIF_TERM opts, unsigned* len);
double extract_option(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, double defValue);

ERL_NIF_TERM ta_sma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_rsi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_wma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_var(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_acos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_asin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_atan(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_cmo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_dema(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_ema(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]); 
ERL_NIF_TERM ta_ceil(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_cos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_cosh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_exp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_floor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_ht_dcperiod(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_ht_dcphase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);


#endif // Included ETALIB_H
