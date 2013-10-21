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

    ERL_NIF_TERM    atom_sma;
    ERL_NIF_TERM    atom_ema;
    ERL_NIF_TERM    atom_wma;
    ERL_NIF_TERM    atom_dema;
    ERL_NIF_TERM    atom_tema;
    ERL_NIF_TERM    atom_trima;
    ERL_NIF_TERM    atom_kama;
    ERL_NIF_TERM    atom_mama;
    ERL_NIF_TERM    atom_t3;


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

    double*    inValues0;
    double*    inValues1;

    double*    outDblValues0;
    double*    outDblValues1;
    double*    outDblValues2;
    int*       outIntValues0;
    int*       outIntValues1;
   
    unsigned    inLen;
    int         startIdx;
    int         endIdx;
    int         optInTimePeriod;
    int         outBegIdx;
    int         outNBElement;

    double      optInDouble;
} EtaStruct;

#endif // Included ETALIB_H
