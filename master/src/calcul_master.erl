-module(calcul_master).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    calcul_master_sup:start_link().

stop(_State) ->
    ok.
