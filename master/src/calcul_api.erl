-module(calcul_api).

-export([sum_squares/2]).

sum_squares(First_num, Last_num) ->
    Active_nodes = get_active_nodes(),
    gen_server:multi_call(Active_nodes, calcul_slave_server, {calculate, First_num, Last_num}).

get_active_nodes() ->
    lists:foldl(fun(Node, Active_nodes) -> 
			case is_pid(rpc:call(Node, erlang, whereis, [calcul_slave_server])) of 
			    true -> [Node|Active_nodes]; 
			    _ -> [] 
			end 
		end, [], nodes()).
