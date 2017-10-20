-module(nkswarm_callbacks).
-export([service_init/2]).
-include("nkswarm.hrl").

service_init(_Spec, #{id := SrvId}=State) ->
     ?INFO("service init: ~p", [SrvId]),
     Config = SrvId:config(),
     nkswarm_server:start(Config),
     {ok, State}.
