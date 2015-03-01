-module(calcul_master_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
    Server = {calcul_master, {calcul_master_server, start_link, []},
              permanent, 5000, worker, [calcul_master_server]},
    Children = [Server],
    RestartStrategy = {one_for_one, 1, 10},
    {ok, {RestartStrategy, Children}}.
