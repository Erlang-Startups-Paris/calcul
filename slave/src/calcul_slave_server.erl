-module(calcul_slave_server).
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
init([]) -> {ok, []}. %% no treatment of info here!

handle_call({calculate, N, M}, _From, State) ->
    {Hour, Min, Sec} = time(),

    io:format("~n  [~ph:~pm:~ps] Runing ~p -> ~p ...", [Hour, Min, Sec, N, M]),

    {Duration, Result} = timer:tc(calcul_slave_lib, run, [N, M]),

    io: format ("~n    => ~p in (~p ms)~n", [Result, Duration/1000]),

    {reply, Result, State};

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

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
