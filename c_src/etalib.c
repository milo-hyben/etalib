// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#include "etalib.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    etalib_st* st = enif_alloc(sizeof(etalib_st));
    if(st == NULL) {
        return 1;
    }

    st->atom_nan = make_atom(env, "nan");
    st->atom_ok = make_atom(env, "ok");
    st->atom_error = make_atom(env, "error");

    *priv = (void*) st;

    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM info)
{
    return load(env, priv, info);
}

static void
unload(ErlNifEnv* env, void* priv)
{
    enif_free(priv);
    return;
}

static ErlNifFunc funcs[] =
{
    {"nif_rsi", 2, ta_rsi},
    {"nif_sma", 2, ta_sma},
    {"nif_wma", 2, ta_wma},
    {"nif_var", 2, ta_var},
    {"nif_acos", 2, ta_acos},
    {"nif_asin", 2, ta_asin},
    {"nif_atan", 2, ta_atan},
    {"nif_cmo", 2, ta_cmo},
    {"nif_dema", 2, ta_dema},
    {"nif_ema", 2, ta_ema},
    {"nif_ceil", 2, ta_ceil},
    {"nif_cos", 2, ta_cos},
    {"nif_cosh", 2, ta_cosh},
    {"nif_exp", 2, ta_exp},
    {"nif_floor", 2, ta_floor},
    {"nif_ht_dcperiod", 2, ta_ht_dcperiod},
    {"nif_ht_dcphase", 2, ta_ht_dcphase}
};

ERL_NIF_INIT(etalib, funcs, &load, &reload, &upgrade, &unload);
