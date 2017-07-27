-module(nkswarm).
-export([status/0, metrics/1]).
-define(HEALTH_COLORS, [red, yellow, green]).

status() ->
    Timeout = nkswarm_config:timeout(),
    gen_server:call(nkswarm_server, status, Timeout+2500).

metrics(specs) ->
    [{gauge, erlang_cluster_size, "The size of the erlang cluster"}, 
     {gauge, erlang_cluster_health_status, [color], "The health of the erlang cluster status (red, yellow, green)"}];

metrics(values) ->
    #{ nodes := Nodes, status := Color } = status(),
    [{gauge, erlang_cluster_size, length(Nodes)} | health_status_metrics(Color)].

health_status_metrics(Color) ->
    [{gauge, erlang_cluster_health_status, [Color], 1} | 
     [{gauge, erlang_cluster_health_status, [C], 0} || C <- ?HEALTH_COLORS, C =/= Color ]].
