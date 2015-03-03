-module(calcul_master_server).
-behaviour(gen_server).

-export([sum_squares/2]).
-export([start_link/0, calculate/3, close/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (SERVER, ?MODULE).
-define (CLIENT_PROC, calcul_slave_server).

%%% Client API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

sum_squares(First_num, Last_num) ->
    Active_clients = gen_server:call(?SERVER, get_active_clients),
    send_calculations(First_num, Last_num, (Last_num - First_num) div length(Active_clients), Active_clients, 0).

send_calculations(First_num, Last_num, _, [Node], Results) ->
    Result = gen_server:call({?CLIENT_PROC, Node}, {calculate, First_num, Last_num}),
    Result + Results;
send_calculations(First_num, Last_num, Chunk_len, [Node | Active_clients], Results) ->
    Result = gen_server:call({?CLIENT_PROC, Node}, {calculate, Last_num - Chunk_len + 1, Last_num}),
    send_calculations(First_num, Last_num - Chunk_len, Chunk_len, Active_clients, Result + Results).    


%% Synchronous call
calculate(Pid, N, M) ->
    gen_server:call(Pid, {calculate, N, M}).

%% Synchronous call
close(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([]) ->
    %% State will contain number of active clients
    {ok, 0}.

%% Active clients must be retreived from master process
%% to detect the right active nodes
handle_call(get_active_clients, _, _) ->
    Active_clients = lists:foldl(fun(Node, Active_clients) -> 
					 case is_pid(rpc:call(Node, erlang, whereis, [?CLIENT_PROC])) of 
					     true -> [Node|Active_clients]; 
					     _ -> [] 
					 end 
				 end, [], nodes()),
    {reply, Active_clients, length(Active_clients)};
handle_call({calculate, _N, _M}, _From, State) ->
    {reply, empty, State};

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({return, Cat}, State) ->
    {noreply, [Cat|State]}.

handle_info(_Msg, State) ->
    io:format("Unexpected message: ~n"),
    {noreply, State}.

terminate(normal, _State) ->
    io:format("terminate~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.
