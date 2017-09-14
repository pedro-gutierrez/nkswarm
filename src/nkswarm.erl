-module(nkswarm).
-export([status/0, metrics/1]).
-define(HEALTH_COLORS, [red, yellow, green]).

status() ->
    gen_statem:call(nkswarm_server, status).

metrics(specs) ->
    [{gauge, erlang_cluster_size, "The size of the erlang cluster"}];

metrics(values) ->
    #{ nodes := Nodes } = status(),
    [{gauge, erlang_cluster_size, length(Nodes)}].
