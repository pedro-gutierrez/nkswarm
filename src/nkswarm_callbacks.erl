-module(nkswarm_callbacks).
-export([service_init/2]).
-include("nkswarm.hrl").

service_init(SrvId, #{config := Config}=State) ->
     ?INFO("service init: ~p", [SrvId]),
     nkswarm_server:start(Config),
     {ok, State}.
