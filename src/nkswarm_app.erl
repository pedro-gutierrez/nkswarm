-module(nkswarm_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(SERVER_NAME, "NkSwarm Server").

start(_, _) ->
    io:format("started app ~p~n", [?MODULE]),
    nkswarm_sup:start_link().

stop(_) ->
    ok.
