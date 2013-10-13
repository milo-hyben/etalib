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

ERL_NIF_TERM
make_ok(etalib_st* st, ErlNifEnv* env, ERL_NIF_TERM value)
{
    return enif_make_tuple2(env, st->atom_ok, value);
}

ERL_NIF_TERM
make_error(etalib_st* st, ErlNifEnv* env, const char* error)
{
    return enif_make_tuple2(env, st->atom_error, make_atom(env, error));
}


ERL_NIF_TERM
make_object(ErlNifEnv* env, ERL_NIF_TERM pairs)
{
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    ERL_NIF_TERM key, val;

    while(enif_get_list_cell(env, pairs, &val, &pairs)) {
        if(!enif_get_list_cell(env, pairs, &key, &pairs)) {
            assert(0 == 1 && "Unbalanced object pairs.");
        }
        val = enif_make_tuple2(env, key, val);
        ret = enif_make_list_cell(env, val, ret);
    }

    return enif_make_tuple1(env, ret);
}

ERL_NIF_TERM
make_array(ErlNifEnv* env, ERL_NIF_TERM list)
{
    ERL_NIF_TERM ret = enif_make_list(env, 0);
    ERL_NIF_TERM item;

    while(enif_get_list_cell(env, list, &item, &list)) {
        ret = enif_make_list_cell(env, item, ret);
    }

    return ret;
}

bool has_bad_arguments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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


double* construct_array_from_list(ErlNifEnv* env, ERL_NIF_TERM opts, unsigned* len)
{
    double* arr = NULL;
    *len = 0;
    enif_get_list_length(env, opts, len);
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

double extract_option(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, double defValue)
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