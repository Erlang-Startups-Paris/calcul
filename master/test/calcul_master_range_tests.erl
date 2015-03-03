-module (calcul_master_range_tests).
-include_lib("eunit/include/eunit.hrl").

split_test() ->
    NbNodes = 3,
    RangeStart = 1,
    RangeEnd = 10,
    ?assertEqual(
      [{1,4}, {5,7}, {8,10}],
      calcul_master_range:split(RangeStart, RangeEnd, NbNodes)).
