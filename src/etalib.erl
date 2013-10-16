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
    tanh/2

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
