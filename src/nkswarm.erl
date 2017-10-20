-module(nkswarm).
-export([status/0]).

status() ->
    nkswarm_server:status().
