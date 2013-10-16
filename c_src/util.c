// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#include "etalib.h"
#include <assert.h>
#include <stdbool.h>

ERL_NIF_TERM
make_atom(ErlNifEnv* env, const char* name)
{
    ERL_NIF_TERM ret;
    if(enif_make_existing_atom(env, name, &ret, ERL_NIF_LATIN1)) {
        return ret;
    }
    return enif_make_atom(env, name);
}

bool 
has_bad_arguments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(argc != 2) {
        return true;
    }

    if(!enif_is_list(env, argv[0])) {
        return true;
    }

    if(!enif_is_list(env, argv[1])) {
        return true;
    }

    return false;
}

double* 
construct_array_from_list(ErlNifEnv* env, ERL_NIF_TERM opts, unsigned* len)
{
    double* arr = NULL;
    *len = 0;
    enif_get_list_length(env, opts, len);

    if((*len)==0)
        return arr;

    arr = (double*) enif_alloc((*len) * sizeof(double));

    int pos = 0;
    double dp = 0;
    long int ip = 0;
    ERL_NIF_TERM val;

    while(enif_get_list_cell(env, opts, &val, &opts)) {
        if(enif_is_number(env, val)) {
            if(enif_get_double(env, val, &dp))
                arr[pos] = dp;
            else if(enif_get_long(env, val, &ip))
                    arr[pos] = ip;
        } 
        else {
            arr[pos] = 0;
        }

        pos++;
    }

    return arr;
}

int 
init_input(ErlNifEnv* env, EtaStruct* e, ERL_NIF_TERM atom, const ERL_NIF_TERM* array, double** values)
{
    if(enif_compare(atom, array[0]) == 0
        && enif_is_list(env, array[1]))
    {// tuple contains Open prices
        unsigned tmpLen = 0;
        *values = construct_array_from_list(env, array[1], &tmpLen);

        if(e->inLen != tmpLen && e->inLen != 0) {
            return -1; // we have a problem input arrays are of different size    
        }

        e->inLen = tmpLen;
        e->endIdx = tmpLen-1;
        return 1; // input tuple matching input atom/key
    }

    return 0; // input not matching
}


int
init_input_arrays(ErlNifEnv* env, ERL_NIF_TERM opts, EtaStruct* e)
{
    int result = 0;
    unsigned len = 0;
    enif_get_list_length(env, opts, &len);

    if(len==0)
        return -1;

    ERL_NIF_TERM val;
    int arity;
    const ERL_NIF_TERM* array;

    while(enif_get_list_cell(env, opts, &val, &opts)) {
        if(!enif_is_tuple(env, val))
            continue; // skip no tuple records

        // is tupple e.g. {key, value}
        if(!enif_get_tuple(env, val, &arity, &array))
            continue;        

        if(arity!=2)
            continue;   

        // Open
        result = init_input(env, e, e->atoms->atom_open, array, &(e->inOpen));
        if(result==-1)
            return -1;

        if(result==1)
            continue; // tuple is Open

        // High
        result = init_input(env, e, e->atoms->atom_high, array, &(e->inHigh));
        if(result==-1)
            return -1;

        if(result==1)
            continue; // tuple is High

        // Low
        result = init_input(env, e, e->atoms->atom_low, array, &(e->inLow));
        if(result==-1)
            return -1;

        if(result==1)
            continue; // tuple is Low

        // Close
        result = init_input(env, e, e->atoms->atom_close, array, &(e->inClose));
        if(result==-1)
            return -1;

        if(result==1)
            continue; // tuple is Close

        // Volume
        result = init_input(env, e, e->atoms->atom_volume, array, &(e->inVolume));
        if(result==-1)
            return -1;
    }

    return 1;
}

double 
extract_option(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, double defValue)
{
    double res = defValue;
    int arity;
    double dp = 0;
    long int ip = 0;
    const ERL_NIF_TERM* array;
    ERL_NIF_TERM atom_name = make_atom(env, name);

    ERL_NIF_TERM val;

    while(enif_get_list_cell(env, opts, &val, &opts)) {
        if(!enif_is_tuple(env, val))
            continue;

        if(!enif_get_tuple(env, val, &arity, &array))
            continue;

        if(arity!=2)
            continue;

        if(enif_compare(atom_name, array[0]) != 0)
            continue;

        if(!enif_is_number(env, array[1]))
            continue;            

        if(enif_get_double(env, array[1], &dp))
            res = dp;
        else 
            if(enif_get_long(env, array[1], &ip))
                res = ip;
    }

    return res;
}

ERL_NIF_TERM 
extract_atom_option(ErlNifEnv* env, ERL_NIF_TERM opts, ERL_NIF_TERM defValue)
{
    ERL_NIF_TERM val;
    ERL_NIF_TERM res = defValue;
    while(enif_get_list_cell(env, opts, &val, &opts)) {
        if(enif_is_atom(env, val))
            return val; // found the first atom value in the list
    }

    return res;
}

int
eta_init(EtaStruct* e, ErlNifEnv* env, const ERL_NIF_TERM argv[])
{
    e->env = env;
    e->atoms = enif_priv_data(env);
    e->inOpen = NULL;
    e->inHigh = NULL;
    e->inLow = NULL;
    e->inClose = NULL;
    e->inVolume = NULL;

    e->outDblValues1 = NULL;
    e->outDblValues2 = NULL;
    e->outDblValues3 = NULL;
    e->outIntValues = NULL;

    e->outTerms = NULL;
    e->inLen = 0;
    e->startIdx = 0;
    e->endIdx = 0;
    e->optInTimePeriod = 0;
    e->outBegIdx = 0;
    e->outNBElement = 0;

    return init_input_arrays(env, argv[0], e);
}

void
eta_destroy(EtaStruct* e)
{
    if(e->inOpen != NULL) {
        enif_free(e->inOpen);
        e->inOpen = NULL;
    }

    if(e->inHigh != NULL) {
        enif_free(e->inHigh);
        e->inHigh = NULL;
    }

    if(e->inLow != NULL) {
        enif_free(e->inLow);
        e->inLow = NULL;
    }

    if(e->inClose != NULL) {
        enif_free(e->inClose);
        e->inClose = NULL;
    }

    if(e->inVolume != NULL) {
        enif_free(e->inVolume);
        e->inVolume = NULL;
    }

    if(e->outDblValues1 != NULL) {
        enif_free(e->outDblValues1);
        e->outDblValues1 = NULL;
    }

    if(e->outDblValues2 != NULL) {
        enif_free(e->outDblValues2);
        e->outDblValues2 = NULL;
    }

    if(e->outDblValues3 != NULL) {
        enif_free(e->outDblValues3);
        e->outDblValues3 = NULL;
    }

    if(e->outIntValues != NULL) {
        enif_free(e->outIntValues);
        e->outIntValues = NULL;
    }

    if(e->outTerms != NULL) {
        enif_free(e->outTerms);
        e->outTerms = NULL;
    }
}

ERL_NIF_TERM
eta_populate_output_double(EtaStruct* e, int initPos, const double* outDblValues)
{
    int i;
    for(i = initPos; i < e->inLen; i++) {
        if(i< e->outBegIdx) {
            e->outTerms[i] = e->atoms->atom_nan;
        }
        else {
            e->outTerms[i] = enif_make_double(e->env, outDblValues[i - e->outBegIdx]);
        }
    }
    return enif_make_list_from_array(e->env, e->outTerms, e->inLen);
}

ERL_NIF_TERM
eta_populate_output_int(EtaStruct* e, int initPos, const int* outIntValues)
{
    int i;
    for(i = initPos; i < e->inLen; i++) {
        if(i< e->outBegIdx) {
            e->outTerms[i] = e->atoms->atom_nan;
        }
        else {
            e->outTerms[i] = enif_make_double(e->env, outIntValues[i - e->outBegIdx]);
        }
    }
    return enif_make_list_from_array(e->env, e->outTerms, e->inLen);
}


double* 
assign_array(EtaStruct* e, ERL_NIF_TERM priceType)
{
    if(enif_compare(priceType, e->atoms->atom_open) == 0)
        return e->inOpen;

    if(enif_compare(priceType, e->atoms->atom_high) == 0)
        return e->inHigh;

    if(enif_compare(priceType, e->atoms->atom_low) == 0)
        return e->inLow;

    if(enif_compare(priceType, e->atoms->atom_close) == 0)
        return e->inClose;

    if(enif_compare(priceType, e->atoms->atom_volume) == 0)
        return e->inVolume;

    return NULL;
}

