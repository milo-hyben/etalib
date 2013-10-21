// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#ifndef UTIL_H
#define UTIL_H

#include "erl_nif.h"
#include <stdbool.h>
#include <ta_libc.h>

// TA-Lib functions with one input array only
typedef TA_RetCode (*TA_FNC_1_IN_ARRAY)(int, int, const double[], int*, int*, double[]);  

// TA-Lib functions with one input array and one indicator specific argument
typedef TA_RetCode (*TA_FNC_1_IN_ARRAY_1_ARG)(int, int, const double[], int, int*, int*, double[]);  

// TA-Lib functions with one input array and one indicator specific argument, output array of integers
typedef TA_RetCode (*TA_FNC_1_IN_ARRAY_1_ARG_INT_OUT)(int, int, const double[], int, int*, int*, int[]);  

// TA-Lib functions with two input arrays only
typedef TA_RetCode (*TA_FNC_2_IN_ARRAYS)(int, int, const double[], const double[], int*, int*, double[]);  

// TA-Lib functions with two input arrays and one indicator specific argument
typedef TA_RetCode (*TA_FNC_2_IN_ARRAYS_1_ARG)(int, int, const double[], const double[], int, int*, int*, double[]);  

// TA-Lib functions with three input arrays only
typedef TA_RetCode (*TA_FNC_3_IN_ARRAYS)(int, int, const double[], const double[], const double[], int*, int*, double[]);

// TA-Lib functions with three input arrays and one indicator specific argument
typedef TA_RetCode (*TA_FNC_3_IN_ARRAYS_1_ARG)(int, int, const double[], const double[], const double[], int, int*, int*, double[]);

// TA-Lib functions with four input arrays (Open, High, Low, Close)
typedef TA_RetCode (*TA_FNC_4_IN_ARRAYS)(int, int, const double[], const double[], const double[], const double[], int*, int*, int[]);

// TA-Lib functions with four input arrays (Open, High, Low, Close) and one indicator specific argument
typedef TA_RetCode (*TA_FNC_4_IN_ARRAYS_1_ARG)(int, int, const double[], const double[], const double[], const double[], double, int*, int*, int[]);

// TA-Lib functions with four input arrays (Open, High, Low, Close), out is double
typedef TA_RetCode (*TA_FNC_4_IN_ARRAYS_OUT_DOUBLE)(int, int, const double[], const double[], const double[], const double[], int*, int*, double[]);


ERL_NIF_TERM make_atom(ErlNifEnv* env, const char* name);
ERL_NIF_TERM eta_populate_output_double(EtaStruct* e, int initPos, const double* outDblValues);
ERL_NIF_TERM eta_populate_output_int(EtaStruct* e, int initPos, const int* outIntValues);

ERL_NIF_TERM eta_generate_results_double(EtaStruct* e, TA_RetCode retCode, double* outputValues);
ERL_NIF_TERM eta_generate_results_int(EtaStruct* e, TA_RetCode retCode, int* outputValues);
ERL_NIF_TERM eta_generate_results_two(EtaStruct* e, TA_RetCode retCode, const char* name0, const char* name1);
ERL_NIF_TERM eta_generate_results_three(EtaStruct* e, TA_RetCode retCode, const char* name0, const char* name1, const char* name2);
ERL_NIF_TERM eta_generate_results_two_int(EtaStruct* e, TA_RetCode retCode, const char* name0, const char* name1);

ERL_NIF_TERM extract_atom_option(ErlNifEnv* env, ERL_NIF_TERM opts, ERL_NIF_TERM defValue);
ERL_NIF_TERM extract_atom_option_by_name(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, ERL_NIF_TERM defValue);

int    eta_init(EtaStruct* e, ErlNifEnv* env, const ERL_NIF_TERM argv[]);
void    eta_destroy(EtaStruct* e);
bool    has_bad_arguments(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
double* construct_array_from_list(ErlNifEnv* env, ERL_NIF_TERM opts, unsigned* len);
int     init_input(ErlNifEnv* env, EtaStruct* e, ERL_NIF_TERM atom, const ERL_NIF_TERM* array, double** values);
int     init_input_arrays(ErlNifEnv* env, ERL_NIF_TERM opts, EtaStruct* e);
double  extract_option(ErlNifEnv* env, ERL_NIF_TERM opts, const char* name, double defValue);
double* assign_array(EtaStruct* e, ERL_NIF_TERM priceType);


int init_function_input_params(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], EtaStruct* e);

int init_function_input_params_two_out(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], EtaStruct* e);

int init_function_input_params_three_out(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], EtaStruct* e);

int init_function_input_params_int_out(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], EtaStruct* e);

int init_function_input_params_two_int_out(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], EtaStruct* e);

int init_function_input_params_two_in_arrays(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
	, const char* inArray0, const char* inArray1, EtaStruct* e);

int init_function_input_params_with_double_out_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], EtaStruct* e);

int  init_function_input_params_with_int_out_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], EtaStruct* e);

// functions with one, two, three, four input arrays, no extra arguments
ERL_NIF_TERM call_function_with_one_in_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, TA_FNC_1_IN_ARRAY func);

ERL_NIF_TERM call_function_with_two_in_array(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, const char* arrayName0, const char* arrayName1, TA_FNC_2_IN_ARRAYS func);

ERL_NIF_TERM call_function_with_three_in_arrays(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, TA_FNC_3_IN_ARRAYS func);

ERL_NIF_TERM call_function_with_four_in_arrays(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, TA_FNC_4_IN_ARRAYS func);

// functions with two, three, four input arrays and extra arguments
ERL_NIF_TERM call_function_with_one_in_array_and_one_argument(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, const char* argumentName, TA_FNC_1_IN_ARRAY_1_ARG func);

ERL_NIF_TERM call_function_with_one_in_array_and_one_argument_int_out(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, const char* argumentName, TA_FNC_1_IN_ARRAY_1_ARG_INT_OUT func);

ERL_NIF_TERM call_function_with_two_in_array_and_one_argument(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, const char* arrayName0, const char* arrayName1, const char* argumentName, TA_FNC_2_IN_ARRAYS_1_ARG func);

ERL_NIF_TERM call_function_with_three_in_arrays_and_one_argument(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, const char* argumentName, TA_FNC_3_IN_ARRAYS_1_ARG func);

ERL_NIF_TERM call_function_with_four_in_arrays_and_one_argument(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, const char* argumentName, TA_FNC_4_IN_ARRAYS_1_ARG func);

ERL_NIF_TERM call_function_with_four_in_arrays_out_double(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]
		, TA_FNC_4_IN_ARRAYS_OUT_DOUBLE func);

int extract_ma_type_option(ErlNifEnv* env, EtaStruct* e, ERL_NIF_TERM opts, const char* name, int defValue);

#endif // Included ETALIB_H