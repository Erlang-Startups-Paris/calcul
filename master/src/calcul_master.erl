-module(calcul_master).
-behaviour(application).
-export([start/2, stop/1]).
-export([sum_squares/2]).

start(normal, []) ->
    calcul_master_sup:start_link().

stop(_State) ->
    ok.

sum_squares(N, M) ->
  Ranges = calcul_master_range:split(N, M, length(nodes())),
  Params = lists:zip(Ranges, nodes()),

  MapperPids = [ spawn_job(Param) || Param <- Params ],

  Results = [ spawn_receive(Pid) || Pid <- MapperPids ],

  lists:foldl(fun(ResultInfo, Sum) -> {_, _, Result} = ResultInfo, Result + Sum end, 0, Results).

spawn_job({{N, M}, Node}) ->
  Answer_to = self(),
  spawn(fun () ->
    {Time, Result} = timer:tc(gen_server, call, [{calcul_slave_server, Node}, {calculate, N, M}]),
    Answer_to ! {self(), Node, Time, Result}
  end).

spawn_receive(Pid) ->
  receive
    {Pid, Node, Time, Result} ->
      {Node, Time, Result}
  end.

