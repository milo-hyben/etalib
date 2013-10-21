% This file is part of ETALIB released under the MIT license.
% See the LICENSE file for more information.

-module(etalib).
-export([
    sma/2, 
    rsi/2, 
    wma/2, 
    var/2,
    acos/2,
    asin/2,
    atan/2,
    cmo/2,
    dema/2,
    ema/2,
    ceil/2,
    cos/2,
    cosh/2,
    exp/2,
    floor/2,
    ht_dcperiod/2,
    ht_dcphase/2,
    ht_trendline/2,
    ht_ln/2,
    ht_log10/2,
    kama/2,
    linearreg/2,
    linearreg_angle/2,
    linearreg_intercept/2,
    linearreg_slope/2,
    max/2,
    min/2,
    midpoint/2,
    mom/2,
    roc/2,
    rocp/2,
    rocr/2,
    rocr100/2,
    tema/2,
    trima/2,
    trix/2,
    tsf/2,
    sum/2,
    sin/2,
    sinh/2,
    sqrt/2,
    tan/2,
    tanh/2,
    adx/2,
    adxr/2,
    atr/2,
    cci/2,
    dx/2,
    minus_di/2,
    natr/2,
    plus_di/2,
    willr/2,
    cdl2crows/2,
    cdl3blackcrows/2,
    cdl3inside/2,
    cdl3linestrike/2,
    cdl3outside/2,
    cdl3starsinsouth/2,
    cdl3whitesoldiers/2,
    cdladvanceblock/2,
    cdlbelthold/2,
    cdlbreakaway/2,
    cdlclosingmarubozu/2,
    cdlconcealbabyswall/2,
    cdlcounterattack/2,
    cdldoji/2,
    cdldojistar/2,
    cdldragonflydoji/2,
    cdlengulfing/2,
    cdlgapsidesidewhite/2,
    cdlgravestonedoji/2,
    cdlhammer/2,
    cdlhangingman/2,
    cdlharami/2,
    cdlharamicross/2,
    cdlhighwave/2,
    cdlhikkake/2,
    cdlhikkakemod/2,
    cdlhomingpigeon/2,
    cdlidentical3crows/2,
    cdlinneck/2,
    cdlinvertedhammer/2,
    cdlkicking/2,
    cdlkickingbylength/2,
    cdlladderbottom/2,
    cdllongleggeddoji/2,
    cdllongline/2,
    cdlmarubozu/2,
    cdlmatchinglow/2,
    cdlonneck/2,
    cdlpiercing/2,
    cdlrickshawman/2,
    cdlrisefall3methods/2,
    cdlseparatinglines/2,
    cdlshootingstar/2,
    cdlshortline/2,
    cdlspinningtop/2,
    cdlstalledpattern/2,
    cdlsticksandwich/2,
    cdltakuri/2,
    cdltasukigap/2,
    cdlthrusting/2,
    cdltristar/2,
    cdlunique3river/2,
    cdlupsidegap2crows/2,
    cdlxsidegap3methods/2,
    cdlabandonedbaby/2,
    cdldarkcloudcover/2,
    cdleveningdojistar/2,
    cdleveningstar/2,
    cdlmathold/2,
    cdlmorningdojistar/2,
    cdlmorningstar/2,
    bop/2,
    avgprice/2,
    ad/2,
    aroonosc/2,
    beta/2,
    correl/2,
    minus_dm/2,
    plus_dm/2,
    midprice/2,
    v_mult/2,
    v_add/2,
    v_sub/2,
    v_div/2,
    obv/2,
    medprice/2,
    trange/2,
    typprice/2,
    wclprice/2,
    maxindex/2,
    minindex/2,
    t3/2,
    ultosc/2,
    ht_trendmode/2,
    mfi/2,
    stddev/2,
    sar/2,
    sarext/2,
    adosc/2,
    ma/2,
    bbands/2,
    macd/2,
    macdfix/2,
    macdext/2,
    minmaxindex/2
]).

-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

sma(Data, Options) ->
    case nif_sma(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
rsi(Data, Options) ->
    case nif_rsi(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
wma(Data, Options) ->
    case nif_wma(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
var(Data, Options) ->
    case nif_var(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
acos(Data, Options) ->
    case nif_acos(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
asin(Data, Options) ->
    case nif_asin(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
atan(Data, Options) ->
    case nif_atan(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cmo(Data, Options) ->
    case nif_cmo(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
dema(Data, Options) ->
    case nif_dema(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ema(Data, Options) ->
    case nif_ema(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ceil(Data, Options) ->
    case nif_ceil(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cos(Data, Options) ->
    case nif_cos(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cosh(Data, Options) ->
    case nif_cosh(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
exp(Data, Options) ->
    case nif_exp(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
floor(Data, Options) ->
    case nif_floor(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ht_dcperiod(Data, Options) ->
    case nif_ht_dcperiod(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ht_dcphase(Data, Options) ->
    case nif_ht_dcphase(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ht_trendline(Data, Options) ->
    case nif_ht_trendline(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ht_ln(Data, Options) ->
    case nif_ht_ln(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ht_log10(Data, Options) ->
    case nif_ht_log10(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
kama(Data, Options) ->
    case nif_kama(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
linearreg(Data, Options) ->
    case nif_linearreg(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
linearreg_angle(Data, Options) ->
    case nif_linearreg_angle(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
linearreg_intercept(Data, Options) ->
    case nif_linearreg_intercept(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
linearreg_slope(Data, Options) ->
    case nif_linearreg_slope(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
max(Data, Options) ->
    case nif_max(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
min(Data, Options) ->
    case nif_min(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
midpoint(Data, Options) ->
    case nif_midpoint(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
mom(Data, Options) ->
    case nif_mom(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
roc(Data, Options) ->
    case nif_roc(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
rocp(Data, Options) ->
    case nif_rocp(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
rocr(Data, Options) ->
    case nif_rocr(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
rocr100(Data, Options) ->
    case nif_rocr100(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
tema(Data, Options) ->
    case nif_tema(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
trima(Data, Options) ->
    case nif_trima(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
trix(Data, Options) ->
    case nif_trix(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
tsf(Data, Options) ->
    case nif_tsf(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
sum(Data, Options) ->
    case nif_sum(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
sin(Data, Options) ->
    case nif_sin(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
sinh(Data, Options) ->
    case nif_sinh(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
sqrt(Data, Options) ->
    case nif_sqrt(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
tan(Data, Options) ->
    case nif_tan(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
tanh(Data, Options) ->
    case nif_tanh(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.

adx(Data, Options) ->
    case nif_adx(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
adxr(Data, Options) ->
    case nif_adxr(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
atr(Data, Options) ->
    case nif_atr(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cci(Data, Options) ->
    case nif_cci(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
dx(Data, Options) ->
    case nif_dx(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
minus_di(Data, Options) ->
    case nif_minus_di(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
natr(Data, Options) ->
    case nif_natr(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
plus_di(Data, Options) ->
    case nif_plus_di(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
willr(Data, Options) ->
    case nif_willr(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdl2crows(Data, Options)  ->
    case nif_cdl2crows(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdl3blackcrows(Data, Options)  ->
    case nif_cdl3blackcrows(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdl3inside(Data, Options)  ->
    case nif_cdl3inside(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdl3linestrike(Data, Options)  ->
    case nif_cdl3linestrike(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdl3outside(Data, Options)  ->
    case nif_cdl3outside(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdl3starsinsouth(Data, Options)  ->
    case nif_cdl3starsinsouth(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdl3whitesoldiers(Data, Options)  ->
    case nif_cdl3whitesoldiers(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdladvanceblock(Data, Options)  ->
    case nif_cdladvanceblock(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlbelthold(Data, Options)  ->
    case nif_cdlbelthold(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlbreakaway(Data, Options)  ->
    case nif_cdlbreakaway(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlclosingmarubozu(Data, Options)  ->
    case nif_cdlclosingmarubozu(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlconcealbabyswall(Data, Options)  ->
    case nif_cdlconcealbabyswall(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlcounterattack(Data, Options)  ->
    case nif_cdlcounterattack(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdldoji(Data, Options)  ->
    case nif_cdldoji(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdldojistar(Data, Options)  ->
    case nif_cdldojistar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdldragonflydoji(Data, Options)  ->
    case nif_cdldragonflydoji(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlengulfing(Data, Options)  ->
    case nif_cdlengulfing(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlgapsidesidewhite(Data, Options)  ->
    case nif_cdlgapsidesidewhite(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlgravestonedoji(Data, Options)  ->
    case nif_cdlgravestonedoji(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlhammer(Data, Options)  ->
    case nif_cdlhammer(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlhangingman(Data, Options)  ->
    case nif_cdlhangingman(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlharami(Data, Options)  ->
    case nif_cdlharami(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlharamicross(Data, Options)  ->
    case nif_cdlharamicross(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlhighwave(Data, Options)  ->
    case nif_cdlhighwave(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlhikkake(Data, Options)  ->
    case nif_cdlhikkake(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlhikkakemod(Data, Options)  ->
    case nif_cdlhikkakemod(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlhomingpigeon(Data, Options)  ->
    case nif_cdlhomingpigeon(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlidentical3crows(Data, Options)  ->
    case nif_cdlidentical3crows(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlinneck(Data, Options)  ->
    case nif_cdlinneck(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlinvertedhammer(Data, Options)  ->
    case nif_cdlinvertedhammer(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlkicking(Data, Options)  ->
    case nif_cdlkicking(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlkickingbylength(Data, Options)  ->
    case nif_cdlkickingbylength(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlladderbottom(Data, Options)  ->
    case nif_cdlladderbottom(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdllongleggeddoji(Data, Options)  ->
    case nif_cdllongleggeddoji(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdllongline(Data, Options)  ->
    case nif_cdllongline(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlmarubozu(Data, Options)  ->
    case nif_cdlmarubozu(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlmatchinglow(Data, Options)  ->
    case nif_cdlmatchinglow(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlonneck(Data, Options)  ->
    case nif_cdlonneck(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlpiercing(Data, Options)  ->
    case nif_cdlpiercing(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlrickshawman(Data, Options)  ->
    case nif_cdlrickshawman(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlrisefall3methods(Data, Options)  ->
    case nif_cdlrisefall3methods(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlseparatinglines(Data, Options)  ->
    case nif_cdlseparatinglines(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlshootingstar(Data, Options)  ->
    case nif_cdlshootingstar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlshortline(Data, Options)  ->
    case nif_cdlshortline(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlspinningtop(Data, Options)  ->
    case nif_cdlspinningtop(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlstalledpattern(Data, Options)  ->
    case nif_cdlstalledpattern(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlsticksandwich(Data, Options)  ->
    case nif_cdlsticksandwich(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdltakuri(Data, Options)  ->
    case nif_cdltakuri(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdltasukigap(Data, Options)  ->
    case nif_cdltasukigap(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlthrusting(Data, Options)  ->
    case nif_cdlthrusting(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdltristar(Data, Options)  ->
    case nif_cdltristar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlunique3river(Data, Options)  ->
    case nif_cdlunique3river(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlupsidegap2crows(Data, Options)  ->
    case nif_cdlupsidegap2crows(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlxsidegap3methods(Data, Options)  ->
    case nif_cdlxsidegap3methods(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.

cdlabandonedbaby(Data, Options)  ->
    case nif_cdlabandonedbaby(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdldarkcloudcover(Data, Options)  ->
    case nif_cdldarkcloudcover(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdleveningdojistar(Data, Options)  ->
    case nif_cdleveningdojistar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdleveningstar(Data, Options)  ->
    case nif_cdleveningstar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlmathold(Data, Options)  ->
    case nif_cdlmathold(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlmorningdojistar(Data, Options)  ->
    case nif_cdlmorningdojistar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
cdlmorningstar(Data, Options)  ->
    case nif_cdlmorningstar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
bop(Data, Options)  ->
    case nif_bop(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
avgprice(Data, Options)  ->
    case nif_avgprice(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ad(Data, Options)  ->
    case nif_ad(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
aroonosc(Data, Options)  ->
    case nif_aroonosc(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
beta(Data, Options)  ->
    case nif_beta(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
correl(Data, Options)  ->
    case nif_correl(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
minus_dm(Data, Options)  ->
    case nif_minus_dm(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
plus_dm(Data, Options)  ->
    case nif_plus_dm(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
midprice(Data, Options)  ->
    case nif_midprice(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
v_mult(Data, Options)  ->
    case nif_mult(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
v_add(Data, Options)  ->
    case nif_add(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
v_sub(Data, Options)  ->
    case nif_sub(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
v_div(Data, Options)  ->
    case nif_div(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
obv(Data, Options)  ->
    case nif_obv(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
medprice(Data, Options)  ->
    case nif_medprice(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
trange(Data, Options)  ->
    case nif_trange(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
typprice(Data, Options)  ->
    case nif_typprice(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
wclprice(Data, Options)  ->
    case nif_wclprice(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
maxindex(Data, Options)  ->
    case nif_maxindex(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
minindex(Data, Options)  ->
    case nif_minindex(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
t3(Data, Options)  ->
    case nif_t3(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ultosc(Data, Options)  ->
    case nif_ultosc(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
ht_trendmode(Data, Options)  ->
    case nif_ht_trendmode(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
mfi(Data, Options)  ->
    case nif_mfi(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
stddev(Data, Options)  ->
    case nif_stddev(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
sar(Data, Options)  ->
    case nif_sar(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
sarext(Data, Options)  ->
    case nif_sarext(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
adosc(Data, Options)  ->
    case nif_adosc(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end. 
ma(Data, Options)  ->
    case nif_ma(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end. 
bbands(Data, Options)  ->
    case nif_bbands(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
macd(Data, Options)  ->
    case nif_macd(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
macdfix(Data, Options)  ->
    case nif_macdfix(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
macdext(Data, Options)  ->
    case nif_macdext(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.
minmaxindex(Data, Options)  ->
    case nif_minmaxindex(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        IOData ->
            IOData
    end.

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "etalib"), 0).


not_loaded(Line) ->
    erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_sma(_Data, _Options) ->
    ?NOT_LOADED.
nif_rsi(_Data, _Options) ->
    ?NOT_LOADED.
nif_wma(_Data, _Options) ->
    ?NOT_LOADED.
nif_var(_Data, _Options) ->
    ?NOT_LOADED.
nif_acos(_Data, _Options) ->
    ?NOT_LOADED.
nif_asin(_Data, _Options) ->
    ?NOT_LOADED.
nif_atan(_Data, _Options) ->
    ?NOT_LOADED.
nif_cmo(_Data, _Options) ->
    ?NOT_LOADED.
nif_dema(_Data, _Options) ->
    ?NOT_LOADED.
nif_ema(_Data, _Options) ->
    ?NOT_LOADED.
nif_ceil(_Data, _Options) ->
    ?NOT_LOADED.
nif_cos(_Data, _Options) ->
    ?NOT_LOADED.
nif_cosh(_Data, _Options) ->
    ?NOT_LOADED.
nif_exp(_Data, _Options) ->
    ?NOT_LOADED.
nif_floor(_Data, _Options) ->
    ?NOT_LOADED.
nif_ht_dcperiod(_Data, _Options) ->
    ?NOT_LOADED.
nif_ht_dcphase(_Data, _Options) ->
    ?NOT_LOADED.
nif_ht_trendline(_Data, _Options) ->
    ?NOT_LOADED.
nif_ht_ln(_Data, _Options) ->
    ?NOT_LOADED.
nif_ht_log10(_Data, _Options) ->
    ?NOT_LOADED.
nif_kama(_Data, _Options) ->
    ?NOT_LOADED.
nif_linearreg(_Data, _Options) ->
    ?NOT_LOADED.
nif_linearreg_angle(_Data, _Options) ->
    ?NOT_LOADED.
nif_linearreg_intercept(_Data, _Options) ->
    ?NOT_LOADED.
nif_linearreg_slope(_Data, _Options) ->
    ?NOT_LOADED.
nif_max(_Data, _Options) ->
    ?NOT_LOADED.
nif_min(_Data, _Options) ->
    ?NOT_LOADED.
nif_midpoint(_Data, _Options) ->
    ?NOT_LOADED.
nif_mom(_Data, _Options) ->
    ?NOT_LOADED.
nif_roc(_Data, _Options) ->
    ?NOT_LOADED.
nif_rocp(_Data, _Options) ->
    ?NOT_LOADED.
nif_rocr(_Data, _Options) ->
    ?NOT_LOADED.
nif_rocr100(_Data, _Options) ->
    ?NOT_LOADED.
nif_tema(_Data, _Options) ->
    ?NOT_LOADED.
nif_trima(_Data, _Options) ->
    ?NOT_LOADED.
nif_trix(_Data, _Options) ->
    ?NOT_LOADED.
nif_tsf(_Data, _Options) ->
    ?NOT_LOADED.
nif_sum(_Data, _Options) ->
    ?NOT_LOADED.
nif_sin(_Data, _Options) ->
    ?NOT_LOADED.
nif_sinh(_Data, _Options) ->
    ?NOT_LOADED.
nif_sqrt(_Data, _Options) ->
    ?NOT_LOADED.
nif_tan(_Data, _Options) ->
    ?NOT_LOADED.
nif_tanh(_Data, _Options) ->
    ?NOT_LOADED.
nif_adx(_Data, _Options) ->
    ?NOT_LOADED.
nif_adxr(_Data, _Options) ->
    ?NOT_LOADED.
nif_atr(_Data, _Options) ->
    ?NOT_LOADED.
nif_cci(_Data, _Options) ->
    ?NOT_LOADED.
nif_dx(_Data, _Options) ->
    ?NOT_LOADED.
nif_minus_di(_Data, _Options) ->
    ?NOT_LOADED.
nif_natr(_Data, _Options) ->
    ?NOT_LOADED.
nif_plus_di(_Data, _Options) ->
    ?NOT_LOADED.
nif_willr(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdl2crows(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdl3blackcrows(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdl3inside(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdl3linestrike(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdl3outside(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdl3starsinsouth(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdl3whitesoldiers(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdladvanceblock(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlbelthold(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlbreakaway(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlclosingmarubozu(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlconcealbabyswall(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlcounterattack(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdldoji(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdldojistar(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdldragonflydoji(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlengulfing(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlgapsidesidewhite(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlgravestonedoji(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlhammer(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlhangingman(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlharami(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlharamicross(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlhighwave(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlhikkake(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlhikkakemod(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlhomingpigeon(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlidentical3crows(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlinneck(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlinvertedhammer(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlkicking(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlkickingbylength(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlladderbottom(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdllongleggeddoji(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdllongline(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlmarubozu(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlmatchinglow(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlonneck(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlpiercing(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlrickshawman(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlrisefall3methods(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlseparatinglines(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlshootingstar(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlshortline(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlspinningtop(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlstalledpattern(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlsticksandwich(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdltakuri(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdltasukigap(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlthrusting(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdltristar(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlunique3river(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlupsidegap2crows(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlxsidegap3methods(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlabandonedbaby(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdldarkcloudcover(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdleveningdojistar(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdleveningstar(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlmathold(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlmorningdojistar(_Data, _Options) ->
    ?NOT_LOADED.
nif_cdlmorningstar(_Data, _Options) ->
    ?NOT_LOADED.
nif_bop(_Data, _Options) ->
    ?NOT_LOADED.
nif_avgprice(_Data, _Options) ->
    ?NOT_LOADED.
nif_ad(_Data, _Options) ->
    ?NOT_LOADED.
nif_aroonosc(_Data, _Options) ->
    ?NOT_LOADED.
nif_beta(_Data, _Options) ->
    ?NOT_LOADED.
nif_correl(_Data, _Options) ->
    ?NOT_LOADED.
nif_minus_dm(_Data, _Options) ->
    ?NOT_LOADED.
nif_plus_dm(_Data, _Options) ->
    ?NOT_LOADED.
nif_midprice(_Data, _Options) ->
    ?NOT_LOADED.
nif_mult(_Data, _Options) ->
    ?NOT_LOADED.
nif_add(_Data, _Options) ->
    ?NOT_LOADED.
nif_sub(_Data, _Options) ->
    ?NOT_LOADED.
nif_div(_Data, _Options) ->
    ?NOT_LOADED.
nif_obv(_Data, _Options) ->
    ?NOT_LOADED.
nif_medprice(_Data, _Options) ->
    ?NOT_LOADED.
nif_trange(_Data, _Options) ->
    ?NOT_LOADED.
nif_typprice(_Data, _Options) ->
    ?NOT_LOADED.
nif_wclprice(_Data, _Options) ->
    ?NOT_LOADED.
nif_maxindex(_Data, _Options) ->
    ?NOT_LOADED.
nif_minindex(_Data, _Options) ->
    ?NOT_LOADED.
nif_t3(_Data, _Options) ->
    ?NOT_LOADED.
nif_ultosc(_Data, _Options) ->
    ?NOT_LOADED.
nif_ht_trendmode(_Data, _Options) ->
    ?NOT_LOADED.
nif_mfi(_Data, _Options) ->
    ?NOT_LOADED.
nif_stddev(_Data, _Options) ->
    ?NOT_LOADED.
nif_sar(_Data, _Options) ->
    ?NOT_LOADED.
nif_sarext(_Data, _Options) ->
    ?NOT_LOADED.
nif_adosc(_Data, _Options) ->
    ?NOT_LOADED.
nif_ma(_Data, _Options) ->
    ?NOT_LOADED.
nif_bbands(_Data, _Options) ->
    ?NOT_LOADED.
nif_macd(_Data, _Options) ->
    ?NOT_LOADED.
nif_macdfix(_Data, _Options) ->
    ?NOT_LOADED.
nif_macdext(_Data, _Options) ->
    ?NOT_LOADED.    
nif_minmaxindex(_Data, _Options) ->
    ?NOT_LOADED.