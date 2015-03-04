-module(calcul_master).
-behaviour(application).
-export([start/2, stop/1]).
-export([calculate/2]).

start(normal, []) ->
    calcul_master_sup:start_link().

stop(_State) ->
    ok.

calculate(N, M) ->
  Ranges = calcul_master_range:split(N, M, length(nodes())),
  Params = lists:zip(Ranges, nodes()),

  % Map
  WorkerPids = [ spawn_jobs(Param) || Param <- Params ],
  Results    = [ wait_responses(Pid) || Pid <- WorkerPids ],

  % Reduce
  Result = lists:foldl(
    fun(ResultInfo, Sum) ->
      {_, _, Result} = ResultInfo,
      Result + Sum
    end, 0, Results),

  % Display results and times
  display_results_table(Results, Result),
  Result.

display_results_table(Results, FinalResult) ->
  % Lines
  lists:map(fun(R) ->
    {Node, Time, Result} = R,
    io:format("~n~p | ~pms | ~p", [Node, Time / 1000, Result])
    end, Results),
  % FinalResult
  io:format("~n~nResult: ~p~n~n", [FinalResult]).

spawn_jobs({{N, M}, Node}) ->
  Answer_to = self(),
  spawn(fun () ->
    {Time, Result} = timer:tc(gen_server, call, [{calcul_slave_server, Node}, {calculate, N, M}]),
    Answer_to ! {self(), Node, Time, Result}
  end).

wait_responses(Pid) ->
  receive
    {Pid, Node, Time, Result} ->
      {Node, Time, Result}
  end.
