-module (calcul_master_range).
-export ([split/3]).

split(RangeStart, RangeEnd, NbNodes) ->
  ChunkSize = ((RangeEnd - RangeStart) + 1) div NbNodes,
  split_p(RangeStart, RangeEnd, ChunkSize, NbNodes, []).

split_p(RangeStart, RangeEnd, _ChunkSize, 1, Acc) ->
  [{RangeStart, RangeEnd}|Acc];
split_p(RangeStart, RangeEnd, ChunkSize, Iteration, Acc) ->
  split_p(RangeStart, RangeEnd - ChunkSize, ChunkSize, Iteration - 1, [{RangeEnd - ChunkSize + 1,RangeEnd}|Acc]).
