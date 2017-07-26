-module(nkswarm_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 0, 1}, [
                                {nkswarm_server, 
                                 {nkswarm_server, start_link, []}, permanent, 5000, worker, [nkswarm_server]}]}}.
