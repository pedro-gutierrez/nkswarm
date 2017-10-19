-module(nkswarm_svc).
-export([start_services/1, start/0, stop/0]).
-include("nkswarm.hrl").

start_services(_) ->
    Spec = make_service_spec(),
    case nkservice:start(?SRV, Spec) of
        {ok, _} -> ok;
        {error, already_started} -> ok;
        {error, Error} ->
            lager:error("Could not start service: ~p (~p)", [Error, Spec]),
            error(service_start_error)
    end.

start() ->
    Spec = make_service_spec(),
    nkservice:start(?SRV, Spec).

stop() ->
    nkservice:stop(?SRV).

make_service_spec() ->
    #{ callback => nkswarm_callbacks,
       plugins => [],
       discovery_port => nkswarm_app:get(discovery_port),
       discovery_name => nkswarm_app:get(discovery_name),
       beacon_timeout => nkswarm_app:get(beacon_timeout),
       beacon_interval => nkswarm_app:get(beacon_interval),
       debug => []
     }.
