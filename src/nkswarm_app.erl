-module(nkswarm_app).
-behaviour(application).
-export([start/2, stop/1]).
-define(APP, nkswarm).

start(_, _) ->
    Syntax = #{
      discovery_port => {integer, 1, 65535},
      discovery_name => string,
      beacon_timeout => integer,
      beacon_interval => integer,
      '__defaults' => #{
        discovery_port => 9999,
        beacon_timeout => 1000,
        beacon_interval => 300
       },
      '__mandatory' => [discovery_name]
     },
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Pid} = nkswarm_sup:start_link(),
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("NkSwarm v~s has started.", [Vsn]),
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error(Error)
    end.    

stop(_) ->
    ok.

