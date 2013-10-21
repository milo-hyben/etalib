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

    double*    inValues0;
    double*    inValues1;

    double*    outDblValues0;
    double*    outDblValues1;
    double*    outDblValues2;
    int*       outIntValues;
   
    unsigned    inLen;
    int         startIdx;
    int         endIdx;
    int         optInTimePeriod;
    int         outBegIdx;
    int         outNBElement;

    double      optInDouble;
} EtaStruct;

#endif // Included ETALIB_H
