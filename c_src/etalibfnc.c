// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#include "etalib.h"
#include <ta_libc.h>


ERL_NIF_TERM
ta_sma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(has_bad_arguments(env, argc, argv))
        return enif_make_badarg(env);

    ERL_NIF_TERM ret;
    
    unsigned len = 0;
    TA_Real* inReal = NULL;
    TA_Real* outReal = NULL;
    ERL_NIF_TERM* out_data = NULL;

    inReal = construct_array_from_list(env, argv[0], &len);

    int startIdx = 0;
    int endIdx = len-1;
    int optInTimePeriod = (int)extract_option(env, argv[1], "timeperiod", 2);

    outReal = (TA_Real*) enif_alloc(len * sizeof(TA_Real));
    TA_Integer outBegIdx;
    TA_Integer outNBElement;

    TA_RetCode retCode = TA_SMA( startIdx,
                   endIdx,
                   &inReal[0],
                   optInTimePeriod, 
                   &outBegIdx,
                   &outNBElement,
                   &outReal[0]);

    if( retCode != TA_SUCCESS )
    {
        TA_RetCodeInfo info;
        TA_SetRetCodeInfo( retCode, &info );
        ret = enif_make_tuple2(env, make_atom(env, "error"), enif_make_string(env, info.infoStr, ERL_NIF_LATIN1));
        goto done;
    }    

    ERL_NIF_TERM atom_nan = make_atom(env, "nan");
    out_data = (ERL_NIF_TERM*) enif_alloc(len * sizeof(ERL_NIF_TERM));

    int i;
    for(i = 0; i < len; i++) {
        if(i<outBegIdx) {
            out_data[i] = atom_nan;
        }
        else {
            out_data[i] = enif_make_double(env, outReal[i-outBegIdx]);
        }
    }

    ret = enif_make_list_from_array(env, out_data, len);

done:

    if(inReal != NULL) {
        enif_free(inReal);
    }

    if(outReal != NULL) {
        enif_free(outReal);
    }

    if(out_data != NULL) {
        enif_free(out_data);
    }

    return ret;
}


ERL_NIF_TERM
ta_rsi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    if(has_bad_arguments(env, argc, argv))
        return enif_make_badarg(env);

    ERL_NIF_TERM ret;
    
    unsigned len = 0;
    TA_Real* inReal = NULL;
    TA_Real* outReal = NULL;
    ERL_NIF_TERM* out_data = NULL;

    inReal = construct_array_from_list(env, argv[0], &len);

    int startIdx = 0;
    int endIdx = len-1;
    int optInTimePeriod = (int)extract_option(env, argv[1], "timeperiod", 2);

    outReal = (TA_Real*) enif_alloc(len * sizeof(TA_Real));
    TA_Integer outBegIdx;
    TA_Integer outNBElement;

    TA_RetCode retCode = TA_SMA( startIdx,
                   endIdx,
                   &inReal[0],
                   optInTimePeriod, 
                   &outBegIdx,
                   &outNBElement,
                   &outReal[0]);

    if( retCode != TA_SUCCESS )
    {
        TA_RetCodeInfo info;
        TA_SetRetCodeInfo( retCode, &info );
        ret = enif_make_tuple2(env, make_atom(env, "error"), enif_make_string(env, info.infoStr, ERL_NIF_LATIN1));
        goto done;
    }    

    ERL_NIF_TERM atom_nan = make_atom(env, "nan");
    out_data = (ERL_NIF_TERM*) enif_alloc(len * sizeof(ERL_NIF_TERM));

    int i;
    for(i = 0; i < len; i++) {
        if(i<outBegIdx) {
            out_data[i] = atom_nan;
        }
        else {
            out_data[i] = enif_make_double(env, outReal[i-outBegIdx]);
        }
    }

    ret = enif_make_list_from_array(env, out_data, len);

done:

    if(inReal != NULL) {
        enif_free(inReal);
    }

    if(outReal != NULL) {
        enif_free(outReal);
    }

    if(out_data != NULL) {
        enif_free(out_data);
    }

    return ret;
}