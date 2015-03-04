-module(calcul_master_api).

-export([sum_squares/2]).
-export([sum_squares_seq/2]).

-define(MASTER, master_proc).
-define(SERVER, calcul_master_server).
-define(CLIENT_PROC, calcul_slave_server).

sum_squares(First_num, Last_num) ->
    Time_start = erlang:now(),
    (not is_pid(whereis(?MASTER))) andalso register(?MASTER, self()),
    ?SERVER:reset_status(),
    Active_clients = ?SERVER:get_active_clients(),
    io:fwrite("~nActive clients:~p~n~n", [Active_clients]),
    send_calculations(First_num, Last_num, (Last_num - First_num) div length(Active_clients), Active_clients),
    Result = get_result(),
    Time_diff = timer:now_diff(erlang:now(), Time_start),
    {Result, Time_diff div 1000}.
        
sum_squares_seq(First_num, Last_num) ->
    Time_start = erlang:now(),
    Active_clients = ?SERVER:get_active_clients(),
    io:fwrite("~nActive clients:~p~n~n", [Active_clients]),
    Result = send_calculations_sync(First_num, Last_num, (Last_num - First_num) div length(Active_clients), Active_clients, 0),
    Time_diff = timer:now_diff(erlang:now(), Time_start),
    {Result, Time_diff div 1000}.

send_calculations(First_num, Last_num, _, [Node]) ->
    gen_server:cast({?CLIENT_PROC, Node}, {calculate, First_num, Last_num});
send_calculations(First_num, Last_num, Chunk_len, [Node | Active_clients]) ->
    gen_server:cast({?CLIENT_PROC, Node}, {calculate, Last_num - Chunk_len + 1, Last_num}),
    send_calculations(First_num, Last_num - Chunk_len, Chunk_len, Active_clients).    

send_calculations_sync(First_num, Last_num, _, [Node], Results) ->
    Result = gen_server:call({?CLIENT_PROC, Node}, {calculate, First_num, Last_num}),
    Result + Results;
send_calculations_sync(First_num, Last_num, Chunk_len, [Node | Active_clients], Results) ->
    Result = gen_server:call({?CLIENT_PROC, Node}, {calculate, Last_num - Chunk_len + 1, Last_num}),
    send_calculations_sync(First_num, Last_num - Chunk_len, Chunk_len, Active_clients, Result + Results).    

get_result() ->
    receive
	{result_global, Result} ->
	    Result
    after 10000 ->
	    result_timeout
    end.
