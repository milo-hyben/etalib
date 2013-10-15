etalib
==========

Erlang wrapper for [TA-LIB](http://ta-lib.org)

Work in progress ...

Examples:

	Prices = [10,11,10,12,15,10,11,10,12,15,10,11,10,12,15,12,15].
	Options = [{timeperiod,2}].

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


TODO:

	Credits
	License
	More Indicators
	Documentation
	Unit Tests