-module(nkswarm).
-export([status/0, contact/0]).

status() ->
    gen_server:call(nkswarm_server, status).

contact() ->
    gen_server:call(nkswarm_server, contact).


