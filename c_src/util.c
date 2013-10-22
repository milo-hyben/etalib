// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#include "etalib.h"
#include "util.h"
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

        if(result==1)
            continue; // tuple is Volume

        // Period
        result = init_input(env, e, e->atoms->atom_period, array, &(e->inPeriod));
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

ERL_NIF_TERM 
extract_atom_option_by_name(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, ERL_NIF_TERM defValue)
{
    ERL_NIF_TERM res = defValue;
    int arity;
    unsigned len;
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

        if(!enif_is_atom(env, array[1]))
            continue;

        if(!enif_get_atom_length(env, array[1], &len, ERL_NIF_LATIN1))
            continue;

        // init array
        char* buf =  (char*) enif_alloc(len * sizeof(char));
        if(!enif_get_atom(env, array[1], buf, len, ERL_NIF_LATIN1))
        {
            enif_free(buf);
            continue;            
        }

        res = make_atom(env, buf);
        enif_free(buf);
        return res;
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
    e->inPeriod = NULL;
    
    e->inValues0 = NULL;
    e->inValues1 = NULL;

    e->outDblValues0 = NULL;
    e->outDblValues1 = NULL;
    e->outDblValues2 = NULL;
    e->outIntValues0 = NULL;
    e->outIntValues1 = NULL;

    e->outTerms = NULL;
    e->inLen = 0;
    e->startIdx = 0;
    e->endIdx = 0;
    e->optInTimePeriod = 0;
    e->outBegIdx = 0;
    e->outNBElement = 0;
    e->optInDouble = 0;

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

    if(e->inPeriod != NULL) {
        enif_free(e->inPeriod);
        e->inPeriod = NULL;
    }

    if(e->outDblValues0 != NULL) {
        enif_free(e->outDblValues0);
        e->outDblValues0 = NULL;
    }

    if(e->outDblValues1 != NULL) {
        enif_free(e->outDblValues1);
        e->outDblValues1 = NULL;
    }

    if(e->outDblValues2 != NULL) {
        enif_free(e->outDblValues2);
        e->outDblValues2 = NULL;
    }

    if(e->outIntValues0 != NULL) {
        enif_free(e->outIntValues0);
        e->outIntValues0 = NULL;
    }

    if(e->outIntValues1 != NULL) {
        enif_free(e->outIntValues1);
        e->outIntValues1 = NULL;
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
            e->outTerms[i] = enif_make_int(e->env, outIntValues[i - e->outBegIdx]);
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

    if(enif_compare(priceType, e->atoms->atom_period) == 0)
        return e->inPeriod;

    return NULL;
}


ERL_NIF_TERM
eta_generate_results_double(
    EtaStruct* e
    , TA_RetCode retCode
    , double* outputValues)
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
    return enif_make_tuple2(e->env, e->atoms->atom_ok, eta_populate_output_double(e, 0, outputValues));
}

ERL_NIF_TERM
eta_generate_results_int(
    EtaStruct* e
    , TA_RetCode retCode
    , int* outputValues)
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
    return enif_make_tuple2(e->env, e->atoms->atom_ok, eta_populate_output_int(e, 0, outputValues));
}

ERL_NIF_TERM 
eta_generate_results_three(
    EtaStruct* e
    , TA_RetCode retCode
    , const char* name0
    , const char* name1
    , const char* name2)
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
    ERL_NIF_TERM results[3];

    results[0] = enif_make_tuple2(e->env, make_atom(e->env, name0), eta_populate_output_double(e, 0, e->outDblValues0));
    results[1] = enif_make_tuple2(e->env, make_atom(e->env, name1), eta_populate_output_double(e, 0, e->outDblValues1));
    results[2] = enif_make_tuple2(e->env, make_atom(e->env, name2), eta_populate_output_double(e, 0, e->outDblValues2));

    return enif_make_list_from_array(e->env, results, 3);
}

ERL_NIF_TERM 
eta_generate_results_two(
    EtaStruct* e
    , TA_RetCode retCode
    , const char* name0
    , const char* name1)
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
    ERL_NIF_TERM results[2];

    results[0] = enif_make_tuple2(e->env, make_atom(e->env, name0), eta_populate_output_double(e, 0, e->outDblValues0));
    results[1] = enif_make_tuple2(e->env, make_atom(e->env, name1), eta_populate_output_double(e, 0, e->outDblValues1));

    return enif_make_list_from_array(e->env, results, 2);
}

ERL_NIF_TERM 
eta_generate_results_two_int(
    EtaStruct* e
    , TA_RetCode retCode
    , const char* name0
    , const char* name1)
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
    ERL_NIF_TERM results[2];

    results[0] = enif_make_tuple2(e->env, make_atom(e->env, name0), eta_populate_output_int(e, 0, e->outIntValues0));
    results[1] = enif_make_tuple2(e->env, make_atom(e->env, name1), eta_populate_output_int(e, 0, e->outIntValues1));

    return enif_make_list_from_array(e->env, results, 2);
}

int 
init_function_input_params(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    // extract option values
    ERL_NIF_TERM priceType = extract_atom_option(env, argv[1], e->atoms->atom_close); // by default work on close
    e->inValues0 = assign_array(e, priceType);
    if(e->inValues0==NULL)
        return 1;

    e->outDblValues0 = (double*) enif_alloc((e->inLen) * sizeof(double));
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}


int 
init_function_input_params_two_out(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    // extract option values
    ERL_NIF_TERM priceType = extract_atom_option(env, argv[1], e->atoms->atom_close); // by default work on close
    e->inValues0 = assign_array(e, priceType);
    if(e->inValues0==NULL)
        return 1;

    e->outDblValues0 = (double*) enif_alloc((e->inLen) * sizeof(double));
    e->outDblValues1 = (double*) enif_alloc((e->inLen) * sizeof(double));
    
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}

int 
init_function_input_params_three_out(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    // extract option values
    ERL_NIF_TERM priceType = extract_atom_option(env, argv[1], e->atoms->atom_close); // by default work on close
    e->inValues0 = assign_array(e, priceType);
    if(e->inValues0==NULL)
        return 1;

    e->outDblValues0 = (double*) enif_alloc((e->inLen) * sizeof(double));
    e->outDblValues1 = (double*) enif_alloc((e->inLen) * sizeof(double));
    e->outDblValues2 = (double*) enif_alloc((e->inLen) * sizeof(double));
    
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}

int 
init_function_input_params_int_out(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    // extract option values
    ERL_NIF_TERM priceType = extract_atom_option(env, argv[1], e->atoms->atom_close); // by default work on close
    e->inValues0 = assign_array(e, priceType);
    if(e->inValues0==NULL)
        return 1;

    e->outIntValues0 = (int*) enif_alloc((e->inLen) * sizeof(double));
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}

int 
init_function_input_params_two_in_arrays(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , const char* inArray0
    , const char* inArray1
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    // extract option values
    ERL_NIF_TERM priceType0 = extract_atom_option_by_name(env, argv[1], inArray0, e->atoms->atom_high); // by default work on High

    e->inValues0 = assign_array(e, priceType0);
    if(e->inValues0==NULL)
        return 1;

    ERL_NIF_TERM priceType1 = extract_atom_option_by_name(env, argv[1], inArray1, e->atoms->atom_low); // by default work on Low

    e->inValues1 = assign_array(e, priceType1);
    if(e->inValues1==NULL)
        return 1;

    e->outDblValues0 = (double*) enif_alloc((e->inLen) * sizeof(double));
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}


int 
init_function_input_params_with_double_out_array(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    e->outDblValues0 = (double*) enif_alloc((e->inLen) * sizeof(double));
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}

int 
init_function_input_params_with_int_out_array(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    e->outIntValues0 = (int*) enif_alloc((e->inLen) * sizeof(double));
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}

int 
init_function_input_params_two_int_out(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , EtaStruct* e)
{
    // check if valid arguments
    if(has_bad_arguments(env, argc, argv))
        return 1;

    // initialise the EtaStruct, extract the inValues
    if(eta_init(e, env, argv)!=1)
        return 1;
    
    // extract option values
    ERL_NIF_TERM priceType = extract_atom_option(env, argv[1], e->atoms->atom_close); // by default work on close
    e->inValues0 = assign_array(e, priceType);
    if(e->inValues0==NULL)
        return 1;

    e->outIntValues0 = (int*) enif_alloc((e->inLen) * sizeof(double));
    e->outIntValues1 = (int*) enif_alloc((e->inLen) * sizeof(double));
    e->outTerms = (ERL_NIF_TERM*) enif_alloc((e->inLen) * sizeof(ERL_NIF_TERM));

    return 0;
}

ERL_NIF_TERM
call_function_with_one_in_array(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , TA_FNC_1_IN_ARRAY func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }
   
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inValues0,
        &e->outBegIdx,
        &e->outNBElement,
        &e->outDblValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_double(e, retCode, e->outDblValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}


ERL_NIF_TERM
call_function_with_one_in_array_and_one_argument(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , const char* argumentName
    , TA_FNC_1_IN_ARRAY_1_ARG func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }

    e->optInTimePeriod = (int)extract_option(env, argv[1], argumentName, 2);
       
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inValues0,
        e->optInTimePeriod, 
        &e->outBegIdx,
        &e->outNBElement,
        &e->outDblValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_double(e, retCode, e->outDblValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}


ERL_NIF_TERM
call_function_with_one_in_array_and_one_argument_int_out(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , const char* argumentName
    , TA_FNC_1_IN_ARRAY_1_ARG_INT_OUT func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_int_out(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }

    e->optInTimePeriod = (int)extract_option(env, argv[1], argumentName, 2);
       
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inValues0,
        e->optInTimePeriod, 
        &e->outBegIdx,
        &e->outNBElement,
        &e->outIntValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_int(e, retCode, e->outIntValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}


ERL_NIF_TERM
call_function_with_two_in_array(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , const char* arrayName0
    , const char* arrayName1
    , TA_FNC_2_IN_ARRAYS func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_two_in_arrays(env, argc, argv, arrayName0, arrayName1, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }

    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inValues0,
        e->inValues1,
        &e->outBegIdx,
        &e->outNBElement,
        &e->outDblValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_double(e, retCode, e->outDblValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}


ERL_NIF_TERM
call_function_with_two_in_array_and_one_argument(
    ErlNifEnv* env, int argc
    , const ERL_NIF_TERM argv[]
    , const char* arrayName0
    , const char* arrayName1
    , const char* argumentName
    , TA_FNC_2_IN_ARRAYS_1_ARG func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_two_in_arrays(env, argc, argv, arrayName0, arrayName1, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }

    e->optInTimePeriod = (int)extract_option(env, argv[1], argumentName, 2);
       
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inValues0,
        e->inValues1,
        e->optInTimePeriod, 
        &e->outBegIdx,
        &e->outNBElement,
        &e->outDblValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_double(e, retCode, e->outDblValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}

ERL_NIF_TERM
call_function_with_three_in_arrays(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , TA_FNC_3_IN_ARRAYS func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_with_double_out_array(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }

    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inHigh,
        e->inLow,
        e->inClose,
        &e->outBegIdx,
        &e->outNBElement,
        &e->outDblValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_double(e, retCode, e->outDblValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}


ERL_NIF_TERM
call_function_with_three_in_arrays_and_one_argument(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , const char* argumentName
    , TA_FNC_3_IN_ARRAYS_1_ARG func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_with_double_out_array(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }

    e->optInTimePeriod = (int)extract_option(env, argv[1], argumentName, 2);
       
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inHigh,
        e->inLow,
        e->inClose,
        e->optInTimePeriod, 
        &e->outBegIdx,
        &e->outNBElement,
        &e->outDblValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_double(e, retCode, e->outDblValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}

ERL_NIF_TERM
call_function_with_four_in_arrays(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , TA_FNC_4_IN_ARRAYS func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_with_int_out_array(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }
   
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inOpen,
        e->inHigh,
        e->inLow,
        e->inClose,
        &e->outBegIdx,
        &e->outNBElement,
        &e->outIntValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_int(e, retCode, e->outIntValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}


ERL_NIF_TERM
call_function_with_four_in_arrays_and_one_argument(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , const char* argumentName
    , TA_FNC_4_IN_ARRAYS_1_ARG func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_with_int_out_array(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }

    e->optInDouble = extract_option(env, argv[1], argumentName, 0);
       
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inOpen,
        e->inHigh,
        e->inLow,
        e->inClose,
        e->optInDouble, 
        &e->outBegIdx,
        &e->outNBElement,
        &e->outIntValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_int(e, retCode, e->outIntValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}

ERL_NIF_TERM
call_function_with_four_in_arrays_out_double(
    ErlNifEnv* env
    , int argc
    , const ERL_NIF_TERM argv[]
    , TA_FNC_4_IN_ARRAYS_OUT_DOUBLE func)
{
    // declare the variables
    EtaStruct eta;
    EtaStruct* e = &eta;

    if(init_function_input_params_with_double_out_array(env, argc, argv, e)==1)
    {// something wrong with input arguments, clean up and return bad argument error
        eta_destroy(e);
        return enif_make_badarg(env);
    }
   
    // call TA-Lib function
    TA_RetCode retCode = func( 
        e->startIdx,
        e->endIdx,
        e->inOpen,
        e->inHigh,
        e->inLow,
        e->inClose,
        &e->outBegIdx,
        &e->outNBElement,
        &e->outDblValues0[0]
    );

    // generate results
    ERL_NIF_TERM results = eta_generate_results_double(e, retCode, e->outDblValues0);

    // clean up
    eta_destroy(e);

    // return the results;
    return results;    
}


int 
extract_ma_type_option(ErlNifEnv* env, EtaStruct* e, ERL_NIF_TERM opts, const char* name, int defValue)
{
    int res = defValue;
    int arity;
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

        if(!enif_is_atom(env, array[1]))
            continue;            

        if(enif_compare(array[1], e->atoms->atom_sma) == 0)
            return 0;

        if(enif_compare(array[1], e->atoms->atom_ema) == 0)
            return 1;

        if(enif_compare(array[1], e->atoms->atom_wma) == 0)
            return 2;

        if(enif_compare(array[1], e->atoms->atom_dema) == 0)
            return 3;

        if(enif_compare(array[1], e->atoms->atom_tema) == 0)
            return 4;

        if(enif_compare(array[1], e->atoms->atom_trima) == 0)
            return 5;

        if(enif_compare(array[1], e->atoms->atom_kama) == 0)
            return 6;

        if(enif_compare(array[1], e->atoms->atom_mama) == 0)
            return 7;

        if(enif_compare(array[1], e->atoms->atom_t3) == 0)
            return 8;

    }

    return res;
}
