// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#include "etalib.h"
#include <ta_libc.h>

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
ta_sma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_SMA( 
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
ta_rsi(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_RSI( 
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
ta_wma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_WMA( 
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


ERL_NIF_TERM
ta_acos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_ACOS( 
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
ta_asin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_ASIN( 
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
ta_atan(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_ATAN( 
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
ta_cmo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_CMO( 
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
ta_dema(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_DEMA( 
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
ta_ema(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_EMA( 
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
ta_ceil(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_CEIL( 
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
ta_cos(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_COS( 
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
ta_cosh(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_COSH( 
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
ta_exp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_EXP( 
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
ta_floor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_FLOOR( 
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
ta_ht_dcperiod(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_HT_DCPERIOD( 
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
ta_ht_dcphase(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    TA_RetCode retCode = TA_HT_DCPHASE( 
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
