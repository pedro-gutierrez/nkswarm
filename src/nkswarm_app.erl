-module(nkswarm_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_, _) ->
    nkswarm_sup:start_link().

stop(_) ->
    ok.

