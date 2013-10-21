// This file is part of ETALIB released under the MIT license.
// See the LICENSE file for more information.

#include "etalib.h"
#include "etalibfnc.h"
#include "util.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM info)
{
    etalib_st* st = enif_alloc(sizeof(etalib_st));
    if(st == NULL) {
        return 1;
    }

    st->atom_nan    = make_atom(env, "nan");
    st->atom_ok     = make_atom(env, "ok");
    st->atom_error  = make_atom(env, "error");
    st->atom_open   = make_atom(env, "open");
    st->atom_high   = make_atom(env, "high");
    st->atom_low    = make_atom(env, "low");
    st->atom_close  = make_atom(env, "close");
    st->atom_volume = make_atom(env, "volume");
    st->atom_sma    = make_atom(env, "sma");
    st->atom_ema    = make_atom(env, "ema");
    st->atom_wma    = make_atom(env, "wma");
    st->atom_dema   = make_atom(env, "dema");
    st->atom_tema   = make_atom(env, "tema");
    st->atom_trima  = make_atom(env, "trima");
    st->atom_kama   = make_atom(env, "kama");
    st->atom_mama   = make_atom(env, "mama");
    st->atom_t3     = make_atom(env, "t3");

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
    {"nif_ht_dcphase", 2, ta_ht_dcphase},
    {"nif_ht_trendline",2, ta_ht_trendline}, 
    {"nif_ht_ln",2, ta_ht_ln}, 
    {"nif_ht_log10",2, ta_ht_log10}, 
    {"nif_kama",2, ta_kama}, 
    {"nif_linearreg",2, ta_linearreg}, 
    {"nif_linearreg_angle",2, ta_linearreg_angle}, 
    {"nif_linearreg_intercept",2, ta_linearreg_intercept}, 
    {"nif_linearreg_slope" ,2, ta_linearreg_slope}, 
    {"nif_max" ,2, ta_max},
    {"nif_min" ,2, ta_min},
    {"nif_midpoint" ,2, ta_midpoint},
    {"nif_mom" ,2, ta_mom},
    {"nif_roc" ,2, ta_roc},
    {"nif_rocp" ,2, ta_rocp},
    {"nif_rocr" ,2, ta_rocr},
    {"nif_rocr100" ,2, ta_rocr100},
    {"nif_tema" ,2, ta_tema},
    {"nif_trima" ,2, ta_trima},
    {"nif_trix" ,2, ta_trix},
    {"nif_tsf" ,2, ta_tsf},
    {"nif_sum" ,2, ta_sum},
    {"nif_sin" ,2, ta_sin},
    {"nif_sinh" ,2, ta_sinh},
    {"nif_sqrt" ,2, ta_sqrt},
    {"nif_tan" ,2, ta_tan},
    {"nif_tanh" ,2, ta_tanh},
    {"nif_adx" ,2, ta_adx},
    {"nif_adxr" ,2, ta_adxr},
    {"nif_atr" ,2, ta_atr},
    {"nif_cci" ,2, ta_cci},
    {"nif_dx" ,2, ta_dx},
    {"nif_minus_di" ,2, ta_minus_di},
    {"nif_natr" ,2, ta_natr},
    {"nif_plus_di" ,2, ta_plus_di},
    {"nif_willr" ,2, ta_willr},
    {"nif_cdl2crows" ,2, ta_cdl2crows},
    {"nif_cdl3blackcrows" ,2, ta_cdl3blackcrows},
    {"nif_cdl3inside" ,2, ta_cdl3inside},
    {"nif_cdl3linestrike" ,2, ta_cdl3linestrike},
    {"nif_cdl3outside" ,2, ta_cdl3outside},
    {"nif_cdl3starsinsouth" ,2, ta_cdl3starsinsouth},
    {"nif_cdl3whitesoldiers" ,2, ta_cdl3whitesoldiers},
    {"nif_cdladvanceblock" ,2, ta_cdladvanceblock},
    {"nif_cdlbelthold" ,2, ta_cdlbelthold},
    {"nif_cdlbreakaway" ,2, ta_cdlbreakaway},
    {"nif_cdlclosingmarubozu" ,2, ta_cdlclosingmarubozu},
    {"nif_cdlconcealbabyswall" ,2, ta_cdlconcealbabyswall},
    {"nif_cdlcounterattack" ,2, ta_cdlcounterattack},
    {"nif_cdldoji" ,2, ta_cdldoji},
    {"nif_cdldojistar" ,2, ta_cdldojistar},
    {"nif_cdldragonflydoji" ,2, ta_cdldragonflydoji},
    {"nif_cdlengulfing" ,2, ta_cdlengulfing},
    {"nif_cdlgapsidesidewhite" ,2, ta_cdlgapsidesidewhite},
    {"nif_cdlgravestonedoji" ,2, ta_cdlgravestonedoji},
    {"nif_cdlhammer" ,2, ta_cdlhammer},
    {"nif_cdlhangingman" ,2, ta_cdlhangingman},
    {"nif_cdlharami" ,2, ta_cdlharami},
    {"nif_cdlharamicross" ,2, ta_cdlharamicross},
    {"nif_cdlhighwave" ,2, ta_cdlhighwave},
    {"nif_cdlhikkake" ,2, ta_cdlhikkake},
    {"nif_cdlhikkakemod" ,2, ta_cdlhikkakemod},
    {"nif_cdlhomingpigeon" ,2, ta_cdlhomingpigeon},
    {"nif_cdlidentical3crows" ,2, ta_cdlidentical3crows},
    {"nif_cdlinneck" ,2, ta_cdlinneck},
    {"nif_cdlinvertedhammer" ,2, ta_cdlinvertedhammer},
    {"nif_cdlkicking" ,2, ta_cdlkicking},
    {"nif_cdlkickingbylength" ,2, ta_cdlkickingbylength},
    {"nif_cdlladderbottom" ,2, ta_cdlladderbottom},
    {"nif_cdllongleggeddoji" ,2, ta_cdllongleggeddoji},
    {"nif_cdllongline" ,2, ta_cdllongline},
    {"nif_cdlmarubozu" ,2, ta_cdlmarubozu},
    {"nif_cdlmatchinglow" ,2, ta_cdlmatchinglow},
    {"nif_cdlonneck" ,2, ta_cdlonneck},
    {"nif_cdlpiercing" ,2, ta_cdlpiercing},
    {"nif_cdlrickshawman" ,2, ta_cdlrickshawman},
    {"nif_cdlrisefall3methods" ,2, ta_cdlrisefall3methods},
    {"nif_cdlseparatinglines" ,2, ta_cdlseparatinglines},
    {"nif_cdlshootingstar" ,2, ta_cdlshootingstar},
    {"nif_cdlshortline" ,2, ta_cdlshortline},
    {"nif_cdlspinningtop" ,2, ta_cdlspinningtop},
    {"nif_cdlstalledpattern" ,2, ta_cdlstalledpattern},
    {"nif_cdlsticksandwich" ,2, ta_cdlsticksandwich},
    {"nif_cdltakuri" ,2, ta_cdltakuri},
    {"nif_cdltasukigap" ,2, ta_cdltasukigap},
    {"nif_cdlthrusting" ,2, ta_cdlthrusting},
    {"nif_cdltristar" ,2, ta_cdltristar},
    {"nif_cdlunique3river" ,2, ta_cdlunique3river},
    {"nif_cdlupsidegap2crows" ,2, ta_cdlupsidegap2crows},
    {"nif_cdlxsidegap3methods" ,2, ta_cdlxsidegap3methods},
    {"nif_cdlabandonedbaby" ,2, ta_cdlabandonedbaby},
    {"nif_cdldarkcloudcover" ,2, ta_cdldarkcloudcover},
    {"nif_cdleveningdojistar" ,2, ta_cdleveningdojistar},
    {"nif_cdleveningstar" ,2, ta_cdleveningstar},
    {"nif_cdlmathold" ,2, ta_cdlmathold},
    {"nif_cdlmorningdojistar" ,2, ta_cdlmorningdojistar},
    {"nif_cdlmorningstar" ,2, ta_cdlmorningstar},
    {"nif_bop" ,2, ta_bop},
    {"nif_avgprice" ,2, ta_avgprice},
    {"nif_ad" ,2, ta_ad},
    {"nif_aroonosc" ,2, ta_aroonosc},
    {"nif_beta" ,2, ta_beta},
    {"nif_correl" ,2, ta_correl},
    {"nif_minus_dm" ,2, ta_minus_dm},
    {"nif_plus_dm" ,2, ta_plus_dm},
    {"nif_midprice" ,2, ta_midprice},
    {"nif_mult" ,2, ta_mult},
    {"nif_add" ,2, ta_add},
    {"nif_sub" ,2, ta_sub},
    {"nif_div" ,2, ta_div},
    {"nif_obv" ,2, ta_obv},
    {"nif_medprice" ,2, ta_medprice},
    {"nif_trange" ,2, ta_trange},
    {"nif_typprice" ,2, ta_typprice},
    {"nif_wclprice" ,2, ta_wclprice},
    {"nif_maxindex" ,2, ta_maxindex},
    {"nif_minindex" ,2, ta_minindex},
    {"nif_t3" ,2, ta_t3},
    {"nif_ultosc" ,2, ta_ultosc},
    {"nif_ht_trendmode" ,2, ta_ht_trendmode},
    {"nif_mfi" ,2, ta_mfi},
    {"nif_stddev" ,2, ta_stddev},
    {"nif_sar" ,2, ta_sar},
    {"nif_sarext" ,2, ta_sarext},
    {"nif_adosc" ,2, ta_adosc},
    {"nif_ma" ,2, ta_ma},
    {"nif_bbands" ,2, ta_bbands}
};

ERL_NIF_INIT(etalib, funcs, &load, &reload, &upgrade, &unload);
