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
    {"nif_sma", 2, ta_sma}
};

ERL_NIF_INIT(etalib, funcs, &load, &reload, &upgrade, &unload);
