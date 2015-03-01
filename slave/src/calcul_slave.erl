-module(calcul_slave).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    calcul_slave_sup:start_link().

stop(_State) ->
    ok.
