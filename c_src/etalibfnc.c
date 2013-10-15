// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#include "etalib.h"
#include <ta_libc.h>

typedef TA_RetCode (*TA_Function01)(int, int, const double[], int, int*, int*, double[]);  // TA-Lib functions
typedef TA_RetCode (*TA_Function02)(int, int, const double[], int*, int*, double[]);  // TA-Lib functions

ERL_NIF_TERM
eta_generate_results(EtaStruct* e, TA_RetCode retCode)
{
    // check for sucess
    if( retCode != TA_SUCCESS )
    {
        // generate error message
        TA_RetCodeInfo info;
        TA_SetRetCodeInfo( retCode, &info );
        return enif_make_tuple2(e->env, e->atoms->atom_error, enif_make_string(e->env, info.infoStr, ERL_NIF_LATIN1));
    }    

    // generate the output structure
    return enif_make_tuple2(e->env, e->atoms->atom_ok, eta_populate_output(e));
}

ERL_NIF_TERM
call_function_with_one_invalue_and_timeperiod(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], TA_Function01 func)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return enif_make_badarg(env);

    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    // initialise the EtaStruct, extract the inValues
    eta_init(e, env, argv);
    
    // extract option values
    e->optInTimePeriod = (int)extract_option(env, argv[1], "timeperiod", 2);
   
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        &e->inValues[0],
        e->optInTimePeriod, 
        &e->outBegIdx,
        &e->outNBElement,
        &e->outValues[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results(e, retCode);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}

ERL_NIF_TERM
call_function_with_one_invalue(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], TA_Function02 func)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return enif_make_badarg(env);

    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    // initialise the EtaStruct, extract the inValues
    eta_init(e, env, argv);
   
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        &e->inValues[0],
        &e->outBegIdx,
        &e->outNBElement,
        &e->outValues[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results(e, retCode);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}

ERL_NIF_TERM
ta_sma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_SMA);
}

ERL_NIF_TERM
ta_rsi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_RSI);
}

ERL_NIF_TERM
ta_wma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_WMA);
}


ERL_NIF_TERM
ta_acos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_ACOS);
}


ERL_NIF_TERM
ta_asin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_ASIN);
}


ERL_NIF_TERM
ta_atan(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_ATAN);
}


ERL_NIF_TERM
ta_cmo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_CMO);
}


ERL_NIF_TERM
ta_dema(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_DEMA);
}


ERL_NIF_TERM
ta_ema(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_EMA);
}


ERL_NIF_TERM
ta_ceil(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_CEIL);
}

ERL_NIF_TERM
ta_cos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_COS);
}

ERL_NIF_TERM
ta_cosh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_COSH);
}

ERL_NIF_TERM
ta_exp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_EXP);
}

ERL_NIF_TERM
ta_floor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_FLOOR);
}

ERL_NIF_TERM
ta_ht_dcperiod(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_HT_DCPERIOD);
}

ERL_NIF_TERM
ta_ht_dcphase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_HT_DCPHASE);
}

ERL_NIF_TERM
ta_ht_trendline(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_HT_TRENDLINE);
}

ERL_NIF_TERM
ta_ht_ln(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_LN);
}

ERL_NIF_TERM
ta_ht_log10(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue(env, argc, argv, &TA_LOG10);
}


ERL_NIF_TERM
ta_kama(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_KAMA);
}


ERL_NIF_TERM
ta_linearreg(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_LINEARREG);
}


ERL_NIF_TERM
ta_linearreg_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_LINEARREG_ANGLE);
}


ERL_NIF_TERM
ta_linearreg_intercept(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_LINEARREG_INTERCEPT);
}

ERL_NIF_TERM
ta_linearreg_slope(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // call TA-Lib Function
    return call_function_with_one_invalue_and_timeperiod(env, argc, argv, &TA_LINEARREG_SLOPE);
}


ERL_NIF_TERM
ta_var(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return enif_make_badarg(env);

    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    // initialise the EtaStruct, extract the inValues
    eta_init(e, env, argv);
    
    // extract option values
    e->optInTimePeriod = (int)extract_option(env, argv[1], "timeperiod", 2);

    double optInNbDev = extract_option(env, argv[1], "nbdev", 2);
   
    // call TA-Lib function
    TA_RetCode retCode = TA_VAR( 
        e->startIdx,
        e->endIdx,
        &e->inValues[0],
        e->optInTimePeriod, 
        optInNbDev,
        &e->outBegIdx,
        &e->outNBElement,
        &e->outValues[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results(e, retCode);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;
}
