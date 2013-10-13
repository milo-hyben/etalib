% This file is part of ETALIB released under the MIT license.
% See the LICENSE file for more information.

-module(etalib).
-export([sma/2, rsi/2]).
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

    
