-module(calcul_master_server).
-behaviour(gen_server).

%% API
-export([sum_squares/2]).
-export([sum_squares_seq/2]).
%% gen_server
-export([start_link/0, calculate/3, close/1]).
%% internal
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (SERVER, ?MODULE).
-define (CLIENT_PROC, calcul_slave_server).
-define (RESULT, result_table).
-define (MASTER, master_node).

%%% Client API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

sum_squares(First_num, Last_num) ->
    (not is_pid(whereis(?MASTER))) andalso register(?MASTER, self()),
    gen_server:call(?SERVER, reset),
    Active_clients = gen_server:call(?SERVER, get_active_clients),
    io:fwrite("~nActive clients:~p~n~n", [Active_clients]),
    send_calculations(First_num, Last_num, (Last_num - First_num) div length(Active_clients), Active_clients),
    get_result().
        
sum_squares_seq(First_num, Last_num) ->
    Active_clients = gen_server:call(?SERVER, get_active_clients),
    send_calculations_sync(First_num, Last_num, (Last_num - First_num) div length(Active_clients), Active_clients, 0).



%% Synchronous call
calculate(Pid, N, M) ->
    gen_server:call(Pid, {calculate, N, M}).

%% Synchronous call
close(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([]) ->
    ets:new(?RESULT, [named_table, set]),
    reset_table(?RESULT),
    %% State will contain the list of active clients
    {ok, []}.

reset_table(?RESULT) ->
    ets:delete_all_objects(?RESULT),
    ets:insert(?RESULT, {result_global, 0}).

handle_call(reset, _, _) ->
    reset_table(?RESULT),
    {reply, ok, []};
%% Active clients must be retreived from master process
%% to detect the right active nodes
handle_call(get_active_clients, _, _) ->
    Active_clients = lists:foldl(fun(Node, Active_clients) -> 
					 case is_pid(rpc:call(Node, erlang, whereis, [?CLIENT_PROC])) of 
					     true -> [Node|Active_clients]; 
					     _ -> [] 
					 end 
				 end, [], nodes()),
    {reply, Active_clients, Active_clients};
handle_call(get_results, _, Remaining_clients) ->
    [{result_global, Result}] = ets:lookup(?RESULT, result_global),
    {reply, {Result, Remaining_clients}, Remaining_clients};    
handle_call({calculate, _N, _M}, _From, State) ->
    {reply, empty, State};

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({set_local_result, Result_local, Client}, [Client]) ->
    %% last client call
    io:fwrite("client ~p local result ~p~n", [Client, Result_local]),
    Result_global = store_result(Client, Result_local),
    ?MASTER ! {result_global, Result_global},
    {noreply, []};
handle_cast({set_local_result, Result_local, Client}, Remaining_clients) ->
    io:fwrite("client ~p local result ~p~n", [Client, Result_local]),
    store_result(Client, Result_local),
    {noreply, Remaining_clients -- [Client]};
handle_cast({return, Cat}, State) ->
    {noreply, [Cat|State]}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    io:format("terminate~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

%%% Internal functions
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
store_result(Client, Result_local) ->
    [{result_global, Result_global_old}] = ets:lookup(?RESULT, result_global),
    Result_global = Result_global_old + Result_local,
    ets:insert(?RESULT, [{{result_local, Client}, Result_local},
			 {result_global, Result_global}]),
    Result_global.

get_result() ->
    receive
	{result_global, Result} ->
	    Result
    after 10000 ->
	    result_timeout
    end.
