-module(calcul_slave_lib).

-export([list_of_squares/2, sum_list/1, run/2]).

list_of_squares (From, To) ->
  [X * X || X <- lists:seq(From, To), X rem 2 == 0, X rem 13 == 0].

sum_list (List) ->
  lists:foldl(fun (N, Sum) -> Sum + N end, 0, List).

run (From, To) ->
  sum_list(list_of_squares(From, To)).
