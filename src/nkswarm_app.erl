-module(nkswarm_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(SERVER_NAME, "NkSwarm Server").
-define(APP, nkswarm).

start(_, _) ->
    {ok, _} = ensure_contact(),
    nkswarm_sup:start_link().

stop(_) ->
    ok.


ensure_contact() ->
    case get_env(?APP, contact, []) of
        [] -> {ok, standalone};
        Nodes -> 
            ensure_contact(Nodes)
    end.


ensure_contact(Nodes) ->
    Answering = [N || N <- Nodes, net_adm:ping(N) =:= pong],
    case Answering of
        [] -> {error, no_contact_nodes_reachable};
        _ -> 
            DefaultTime = 6000,
            WaitTime = get_env(?APP, contact_timeout, DefaultTime),
            wait_for_nodes(length(Answering), WaitTime)
    end.


wait_for_nodes(MinNodes, WaitTime) ->
    Slices = 10,
    SliceTime = round(WaitTime/Slices),
    wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
    {ok, standalone};

wait_for_nodes(MinNodes, SliceTime, Iterations) -> 
    case length(nodes()) > MinNodes of
        true -> 
            {ok, nodes()};
        false ->
            timer:sleep(SliceTime),
            wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
    end.

get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
        undefined   -> Default;
        {ok, Value} -> Value
    end.

