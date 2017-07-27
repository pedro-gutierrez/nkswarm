-module(nkswarm).
-export([status/0, metrics/1]).

status() ->
    Timeout = nkswarm_config:timeout(),
    gen_server:call(nkswarm_server, status, Timeout+2500).

metrics(specs) ->
    [#{ type => gauge, 
        name => erlang_cluster_size, 
        help => "The size of the erlang cluster" }];

metrics(values) ->
    #{ nodes := Nodes } = status(),
    [#{ type => gauge, 
        name => erlang_cluster_size, 
        value =>  length(Nodes) }].
