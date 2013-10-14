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

void
eta_init(EtaStruct* e, ErlNifEnv* env, const ERL_NIF_TERM argv[])
{
    e->env = env;
    e->atoms = enif_priv_data(env);
    e->inValues = NULL;
    e->outValues = NULL;
    e->outTerms = NULL;
    e->inLen = 0;
    e->inValues = construct_array_from_list(env, argv[0], &e->inLen);
    e->outValues = (double*) enif_alloc(e->inLen * sizeof(double));
    e->outTerms = (ERL_NIF_TERM*) enif_alloc(e->inLen * sizeof(ERL_NIF_TERM));

    e->startIdx = 0;
    e->endIdx = e->inLen-1;
}

void
eta_destroy(EtaStruct* e)
{
    if(e->inValues != NULL) {
        enif_free(e->inValues);
    }

    if(e->outValues != NULL) {
        enif_free(e->outValues);
    }

    if(e->outTerms != NULL) {
        enif_free(e->outTerms);
    }
}

ERL_NIF_TERM
eta_populate_output(EtaStruct* e)
{
    int i;
    for(i = 0; i < e->inLen; i++) {
        if(i< e->outBegIdx) {
            e->outTerms[i] = e->atoms->atom_nan;
        }
        else {
            e->outTerms[i] = enif_make_double(e->env, e->outValues[i - e->outBegIdx]);
        }
    }
    return enif_make_list_from_array(e->env, e->outTerms, e->inLen);
}

