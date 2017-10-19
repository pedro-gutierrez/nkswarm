-module(nkswarm_callbacks).
-export([service_init/2]).
-include("nkswarm.hrl").

service_init(SrvId, State) ->
     ?INFO("service init: ~p", [SrvId]),
     {ok, State}.
