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
    ERL_NIF_TERM    atom_open;
    ERL_NIF_TERM    atom_high;
    ERL_NIF_TERM    atom_low;
    ERL_NIF_TERM    atom_close;
    ERL_NIF_TERM    atom_volume;
} etalib_st;

typedef struct {
    ErlNifEnv*      env;
    etalib_st*      atoms;
    ERL_NIF_TERM*   outTerms;

    double*    inOpen;
    double*    inHigh;
    double*    inLow;
    double*    inClose;
    double*    inVolume;

    double*    outDblValues1;
    double*    outDblValues2;
    double*    outDblValues3;
    int*       outIntValues;
   
    unsigned    inLen;
    int         startIdx;
    int         endIdx;
    int         optInTimePeriod;
    int         outBegIdx;
    int         outNBElement;
} EtaStruct;

ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM eta_populate_output_double(EtaStruct* e, int initPos, const double* outDblValues);
ERL_NIF_TERM eta_populate_output_int(EtaStruct* e, int initPos, const int* outIntValues);
ERL_NIF_TERM extract_atom_option(ErlNifEnv* env, ERL_NIF_TERM opts, ERL_NIF_TERM defValue);

int    eta_init(EtaStruct* e, ErlNifEnv* env, const ERL_NIF_TERM argv[]);
void    eta_destroy(EtaStruct* e);
bool    has_bad_arguments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
double* construct_array_from_list(ErlNifEnv* env, ERL_NIF_TERM opts, unsigned* len);
int     init_input(ErlNifEnv* env, EtaStruct* e, ERL_NIF_TERM atom, const ERL_NIF_TERM* array, double** values);
int     init_input_arrays(ErlNifEnv* env, ERL_NIF_TERM opts, EtaStruct* e);
double  extract_option(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, double defValue);
double* assign_array(EtaStruct* e, ERL_NIF_TERM priceType);

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
ERL_NIF_TERM ta_ht_trendline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_ht_ln(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_ht_log10(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_kama(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_linearreg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_linearreg_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_linearreg_intercept(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_linearreg_slope(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_min(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_midpoint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_mom(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_roc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_rocp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_rocr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_rocr100(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_tema(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_trima(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_trix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_tsf(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_sum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_sin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_sinh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_sqrt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_tan(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ta_tanh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif // Included ETALIB_H
