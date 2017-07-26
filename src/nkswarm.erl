-module(nkswarm).
-export([status/0]).

status() ->
    Timeout = nkswarm_config:timeout(),
    gen_server:call(nkswarm_server, status, Timeout+2500).
