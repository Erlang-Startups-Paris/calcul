-module(calcul_api).

-export([sum_squares/2]).

sum_squares(First_num, Last_num) ->
    Active_nodes = get_active_nodes(),
    send_calculations(First_num, Last_num, (Last_num - First_num) div length(Active_nodes), Active_nodes, 0).

get_active_nodes() ->
    lists:foldl(fun(Node, Active_nodes) -> 
			case is_pid(rpc:call(Node, erlang, whereis, [calcul_slave_server])) of 
			    true -> [Node|Active_nodes]; 
			    _ -> [] 
			end 
		end, [], nodes()).

send_calculations(First_num, Last_num, _, [Node], Results) ->
    Result = gen_server:call({calcul_slave_server, Node}, {calculate, First_num, Last_num}),
    Result + Results;
send_calculations(First_num, Last_num, Chunk_len, [Node | Active_nodes], Results) ->
    Result = gen_server:call({calcul_slave_server, Node}, {calculate, Last_num - Chunk_len + 1, Last_num}),
    send_calculations(First_num, Last_num - Chunk_len, Chunk_len, Active_nodes, Result + Results).    
