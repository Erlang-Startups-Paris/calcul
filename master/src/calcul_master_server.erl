-module(calcul_master_server).
-behaviour(gen_server).

-export([start_link/0, calculate/3, close/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

%%% Client API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Synchronous call
calculate(Pid, N, M) ->
    gen_server:call(Pid, {calculate, N, M}).

%% Synchronous call
close(Pid) ->
    gen_server:call(Pid, terminate).

%%% Server functions
init([]) -> {ok, []}.

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
