-module(nkswarm).
-export([status/0, metrics/1]).

status() ->
    Timeout = nkswarm_config:timeout(),
    gen_server:call(nkswarm_server, status, Timeout+2500).

metrics(specs) ->
    [#{ type => gauge, 
        name => erlang_cluster_size, 
        help => "The size of the erlang cluster" }, 
     #{ type => gauge,
        name => erlang_cluster_health_status,
        labels => [color],
        help => "The health of the erlang cluster status (red, yellow, green)"}];

metrics(values) ->
    #{ nodes := Nodes, status := Color } = status(),
    [#{ type => gauge, 
        name => erlang_cluster_size, 
        value =>  length(Nodes) }, 
     #{ type => gauge,
        name => erlang_cluster_health_status,
        labels => [Color],
        value => 1}].
