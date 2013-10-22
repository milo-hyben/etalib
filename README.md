etalib
==========

Erlang wrapper for [TA-LIB](http://ta-lib.org)

Work in progress ...

Examples:

	Prices = [
		{open,[ 76.09, 76.14, 76.75, 77.1, 77.26, 78.28, 77.69, 78.06, 78.09, 78.72, 78.02, 78.49, 79.56, 79.47, 79.32,79, 78.88, 78.88, 78.74, 78.25, 78.89, 79.13, 79.55, 78.72, 78.7, 78.74, 77.4, 77.8, 78.41, 78.74, 79.73, 79.83, 80]},
		{high,[ 76.44, 76.84, 77.06, 77.34, 77.96, 78.32, 78.11, 78.25, 78.14, 78.72, 78.47, 79.47, 79.69, 79.54, 79.41, 79.25, 79, 79.36, 79.21, 79.09, 79.72, 79.76, 79.72, 79.51, 79.35, 78.81, 77.44, 78.79, 79.29, 79.79, 80.11, 80.42, 80.87]},
		{low, [ 75.54, 75.93, 76.73, 76.05, 77.23, 77.92, 77.59, 77.85, 77.64, 77.67, 77.98, 78.33, 79.36, 78.95, 78.46, 78.57, 78.4, 78.85, 78.59, 78, 78.85, 79.06, 78.3, 78.65, 78.67, 77.19, 76.35, 77.79, 78.4, 78.68, 79.38, 79.79, 79.95]},
		{close, [ 75.9, 76.71, 76.84, 76.93, 77.83, 78.21, 78.06, 78.01, 78.06, 77.85, 78.37, 79.31, 79.5, 78.98, 78.83, 78.83, 78.56, 79.17, 79.07, 78.88, 79.68, 79.65, 78.72, 79.4, 78.74, 77.22, 76.98, 78.61, 79.23, 79.79, 79.48, 80.38, 80.84]},
		{volume, [ 27209900, 27118600, 17809700, 27715100, 25141500, 27604400, 28132900, 18310700, 16763000, 25704300, 27174300, 31645300, 26280900, 26181500, 30305000, 21871300, 22723600, 22406900, 24896000, 27997900, 33024100, 25466800, 42970800, 22852700, 20227100, 43494500, 64489900, 47812400, 25214100, 31438600, 38446600, 46661000, 31916600]}
	]

	Options = [close, {timeperiod,5}, {penetration, 0.5}, {factor, 0.2}, {ma_type, ema}].
	OptionsTimePeriods = [{timeperiod1, 2}, {timeperiod2, 4}, {timeperiod3, 6}, {slow_period, 10}, {fast_period, 3}].

Implemented so far:

	etalib:sma(Prices,Options).
	etalib:wma(Prices,Options).
	etalib:rsi(Prices,Options).
	etalib:var(Prices,Options).
	etalib:acos(Prices,Options).
	etalib:asin(Prices,Options).
	etalib:atan(Prices,Options).
	etalib:cmo(Prices,Options).
	etalib:dema(Prices,Options).
	etalib:ema(Prices,Options).
	etalib:ceil(Prices,Options).
	etalib:cos(Prices,Options).
	etalib:cosh(Prices,Options).
	etalib:exp(Prices,Options).
	etalib:floor(Prices,Options).
	etalib:ht_dcperiod(Prices,Options).
	etalib:ht_dcphase(Prices,Options).
	etalib:ht_trendline(Prices,Options).
	etalib:ht_ln(Prices,Options).
	etalib:ht_log10(Prices,Options).
	etalib:kama(Prices,Options).
	etalib:linearreg(Prices,Options).
	etalib:linearreg_angle(Prices,Options).
	etalib:linearreg_intercept(Prices,Options).
	etalib:linearreg_slope(Prices,Options).
	etalib:max(Prices,Options).
	etalib:min(Prices,Options).
	etalib:midpoint(Prices,Options).
	etalib:mom(Prices,Options).
	etalib:roc(Prices,Options).
	etalib:rocp(Prices,Options).
	etalib:rocr(Prices,Options).
	etalib:rocr100(Prices,Options).
	etalib:tema(Prices,Options).
	etalib:trima(Prices,Options).
	etalib:trix(Prices,Options).
	etalib:tsf(Prices,Options).
	etalib:sum(Prices,Options).
	etalib:sin(Prices,Options).
	etalib:sinh(Prices,Options).
	etalib:sqrt(Prices,Options).
	etalib:tan(Prices,Options).
	etalib:tanh(Prices,Options).
	etalib:adx(Prices,Options).
	etalib:adxr(Prices,Options).
	etalib:atr(Prices,Options).
	etalib:cci(Prices,Options).
	etalib:dx(Prices,Options).
	etalib:minus_di(Prices,Options).
	etalib:natr(Prices,Options).
	etalib:plus_di(Prices,Options).
	etalib:willr(Prices,Options).
	etalib:cdl2crows(Prices,Options).
	etalib:cdl3blackcrows(Prices,Options).
	etalib:cdl3inside(Prices,Options).
	etalib:cdl3linestrike(Prices,Options).
	etalib:cdl3outside(Prices,Options).
	etalib:cdl3starsinsouth(Prices,Options).
	etalib:cdl3whitesoldiers(Prices,Options).
	etalib:cdladvanceblock(Prices,Options).
	etalib:cdlbelthold(Prices,Options).
	etalib:cdlbreakaway(Prices,Options).
	etalib:cdlclosingmarubozu(Prices,Options).
	etalib:cdlconcealbabyswall(Prices,Options).
	etalib:cdlcounterattack(Prices,Options).
	etalib:cdldoji(Prices,Options).
	etalib:cdldojistar(Prices,Options).
	etalib:cdldragonflydoji(Prices,Options).
	etalib:cdlengulfing(Prices,Options).
	etalib:cdlgapsidesidewhite(Prices,Options).
	etalib:cdlgravestonedoji(Prices,Options).
	etalib:cdlhammer(Prices,Options).
	etalib:cdlhangingman(Prices,Options).
	etalib:cdlharami(Prices,Options).
	etalib:cdlharamicross(Prices,Options).
	etalib:cdlhighwave(Prices,Options).
	etalib:cdlhikkake(Prices,Options).
	etalib:cdlhikkakemod(Prices,Options).
	etalib:cdlhomingpigeon(Prices,Options).
	etalib:cdlidentical3crows(Prices,Options).
	etalib:cdlinneck(Prices,Options).
	etalib:cdlinvertedhammer(Prices,Options).
	etalib:cdlkicking(Prices,Options).
	etalib:cdlkickingbylength(Prices,Options).
	etalib:cdlladderbottom(Prices,Options).
	etalib:cdllongleggeddoji(Prices,Options).
	etalib:cdllongline(Prices,Options).
	etalib:cdlmarubozu(Prices,Options).
	etalib:cdlmatchinglow(Prices,Options).
	etalib:cdlonneck(Prices,Options).
	etalib:cdlpiercing(Prices,Options).
	etalib:cdlrickshawman(Prices,Options).
	etalib:cdlrisefall3methods(Prices,Options).
	etalib:cdlseparatinglines(Prices,Options).
	etalib:cdlshootingstar(Prices,Options).
	etalib:cdlshortline(Prices,Options).
	etalib:cdlspinningtop(Prices,Options).
	etalib:cdlstalledpattern(Prices,Options).
	etalib:cdlsticksandwich(Prices,Options).
	etalib:cdltakuri(Prices,Options).
	etalib:cdltasukigap(Prices,Options).
	etalib:cdlthrusting(Prices,Options).
	etalib:cdltristar(Prices,Options).
	etalib:cdlunique3river(Prices,Options).
	etalib:cdlupsidegap2crows(Prices,Options).
	etalib:cdlxsidegap3methods(Prices,Options).
	etalib:cdlabandonedbaby(Prices,Options).
	etalib:cdldarkcloudcover(Prices,Options).
	etalib:cdleveningdojistar(Prices,Options).
	etalib:cdleveningstar(Prices,Options).
	etalib:cdlmathold(Prices,Options).
	etalib:cdlmorningdojistar(Prices,Options).
	etalib:cdlmorningstar(Prices,Options).
    etalib:bop(Prices,Options).
    etalib:avgprice(Prices,Options).
    etalib:ad(Prices,Options).
	etalib:aroonosc(Prices,Options).
	etalib:beta(Prices,Options).
	etalib:correl(Prices,Options).
	etalib:minus_dm(Prices,Options).
	etalib:plus_dm(Prices,Options).
	etalib:midprice(Prices,Options).
	etalib:v_mult(Prices,Options).
	etalib:v_add(Prices,Options).
	etalib:v_sub(Prices,Options).
	etalib:v_div(Prices,Options).
	etalib:obv(Prices,Options).
	etalib:medprice(Prices,Options).
	etalib:trange(Prices,Options).
	etalib:typprice(Prices,Options).
	etalib:wclprice(Prices,Options).
	etalib:maxindex(Prices,Options).
	etalib:minindex(Prices,Options).
	etalib:t3(Prices,Options).
	etalib:ultosc(Prices,OptionsTimePeriods).
	etalib:ht_trendmode(Prices,Options).
	etalib:mfi(Prices,Options).
	etalib:stddev(Prices,Options).
	etalib:sar(Prices,Options).
	etalib:sarext(Prices,Options).
	etalib:adosc(Prices,OptionsTimePeriods).
	etalib:ma(Prices,Options).
	etalib:bbands(Prices,Options).
	etalib:macd(Prices,Options).
	etalib:macdfix(Prices,Options).
	etalib:macdext(Prices,Options).
	etalib:minmaxindex(Prices,Options).

	etalib:mavp(Prices,Options).

TODO:

	Credits
	License
	More Indicators
	Documentation
	Unit Tests